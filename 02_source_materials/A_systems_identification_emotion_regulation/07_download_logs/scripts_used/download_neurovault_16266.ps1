param(
    [Parameter(Mandatory = $true)]
    [string]$Root
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$betaRoot = Join-Path $Root '01_first_level_beta_maps_neurovault'
$logRoot = Join-Path $Root '07_download_logs'
New-Item -ItemType Directory -Force -Path $betaRoot, $logRoot | Out-Null

$logFile = Join-Path $logRoot 'neurovault_16266_download.log'
$jsonFile = Join-Path $logRoot 'neurovault_16266_images.json'
$csvFile = Join-Path $logRoot 'neurovault_16266_manifest.csv'

function Write-Log {
    param([string]$Message)
    $stamp = Get-Date -Format 'yyyy-MM-dd HH:mm:ss'
    "[$stamp] $Message" | Tee-Object -FilePath $logFile -Append
}

function Invoke-WithRetry {
    param(
        [scriptblock]$Action,
        [int]$MaxAttempts = 5,
        [int]$InitialDelaySeconds = 3
    )

    for ($attempt = 1; $attempt -le $MaxAttempts; $attempt++) {
        try {
            return & $Action
        } catch {
            if ($attempt -eq $MaxAttempts) {
                throw
            }
            Start-Sleep -Seconds ($InitialDelaySeconds * $attempt)
        }
    }
}

function Get-ConditionName {
    param([string]$FileName)
    if ($FileName -match '_(LookNeg|LookNeu|RegNeg)_Beta\.nii\.gz$') {
        return $Matches[1]
    }
    return 'other'
}

function Get-DatasetName {
    param([string]$FileName)
    if ($FileName -match '^([^_]+)_') {
        return $Matches[1]
    }
    return 'unknown_dataset'
}

Write-Log 'Starting NeuroVault collection 16266 metadata fetch.'

$images = New-Object System.Collections.Generic.List[object]
$next = 'https://neurovault.org/api/collections/16266/images/?limit=100'
while ($next) {
    $page = Invoke-WithRetry -Action {
        Invoke-RestMethod -Uri $next -TimeoutSec 60
    }
    foreach ($item in $page.results) {
        $images.Add($item)
    }
    $next = $page.next
    if ($next) {
        $next = $next -replace '^http:', 'https:'
    }
    Write-Log ("Fetched metadata records: {0}" -f $images.Count)
}

$images | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath $jsonFile -Encoding UTF8
$images |
    Select-Object id, name, file, file_size, map_type, analysis_level, modality, target_template_image, description |
    Export-Csv -LiteralPath $csvFile -NoTypeInformation -Encoding UTF8

Write-Log ("Metadata complete. Image count: {0}. Manifest: {1}" -f $images.Count, $csvFile)

$downloaded = 0
$skipped = 0
$failed = 0

foreach ($image in $images) {
    $uri = [string]$image.file
    $uri = $uri -replace '^http:', 'https:'
    $fileName = [System.IO.Path]::GetFileName(([Uri]$uri).AbsolutePath)
    $dataset = Get-DatasetName -FileName $fileName
    $condition = Get-ConditionName -FileName $fileName
    $destDir = Join-Path $betaRoot ([System.IO.Path]::Combine($dataset, $condition))
    New-Item -ItemType Directory -Force -Path $destDir | Out-Null
    $dest = Join-Path $destDir $fileName

    if ((Test-Path -LiteralPath $dest) -and ((Get-Item -LiteralPath $dest).Length -eq [int64]$image.file_size)) {
        $skipped++
        continue
    }

    try {
        Invoke-WithRetry -Action {
            Invoke-WebRequest -Uri $uri -OutFile $dest -TimeoutSec 180
            $actualSize = (Get-Item -LiteralPath $dest).Length
            if ($actualSize -ne [int64]$image.file_size) {
                throw "Size mismatch for $fileName. Expected $($image.file_size), got $actualSize."
            }
        }
        $downloaded++
        if ((($downloaded + $skipped + $failed) % 25) -eq 0) {
            Write-Log ("Progress: total={0}/{1}, downloaded={2}, skipped={3}, failed={4}" -f ($downloaded + $skipped + $failed), $images.Count, $downloaded, $skipped, $failed)
        }
    } catch {
        $failed++
        Write-Log ("FAILED {0}: {1}" -f $fileName, $_.Exception.Message)
    }
}

Write-Log ("Finished NeuroVault downloads. downloaded={0}, skipped={1}, failed={2}, total={3}" -f $downloaded, $skipped, $failed, $images.Count)
if ($failed -gt 0) {
    exit 2
}
