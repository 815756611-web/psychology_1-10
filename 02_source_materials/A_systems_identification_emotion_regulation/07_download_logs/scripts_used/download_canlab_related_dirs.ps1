param(
    [Parameter(Mandatory = $true)]
    [string]$Root
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$logRoot = Join-Path $Root '07_download_logs'
New-Item -ItemType Directory -Force -Path $logRoot | Out-Null
$logFile = Join-Path $logRoot 'canlab_related_dirs_download.log'
$manifestFile = Join-Path $logRoot 'canlab_related_dirs_manifest.csv'
$script:Manifest = New-Object System.Collections.Generic.List[object]

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

function Get-SafeFileName {
    param([string]$Name)
    foreach ($char in [System.IO.Path]::GetInvalidFileNameChars()) {
        $Name = $Name.Replace($char, '_')
    }
    return $Name
}

function Download-GitHubContents {
    param(
        [string]$ApiUrl,
        [string]$Destination,
        [string]$SourcePath
    )

    New-Item -ItemType Directory -Force -Path $Destination | Out-Null
    $items = Invoke-WithRetry -Action { Invoke-RestMethod -Uri $ApiUrl -TimeoutSec 60 }
    foreach ($item in @($items)) {
        $safeName = Get-SafeFileName -Name $item.name
        $localPath = Join-Path $Destination $safeName
        $sourceItemPath = if ($SourcePath) { "$SourcePath/$($item.name)" } else { $item.name }

        if ($item.type -eq 'dir') {
            Write-Log ("Entering directory {0}" -f $sourceItemPath)
            Download-GitHubContents -ApiUrl $item.url -Destination $localPath -SourcePath $sourceItemPath
            continue
        }

        if ($item.type -ne 'file') {
            continue
        }

        $ok = $false
        if ((Test-Path -LiteralPath $localPath) -and ((Get-Item -LiteralPath $localPath).Length -eq [int64]$item.size)) {
            $ok = $true
        } else {
            Invoke-WithRetry -Action {
                Invoke-WebRequest -Uri $item.download_url -OutFile $localPath -TimeoutSec 240
                $actualSize = (Get-Item -LiteralPath $localPath).Length
                if ($actualSize -ne [int64]$item.size) {
                    throw "Size mismatch for $sourceItemPath. Expected $($item.size), got $actualSize."
                }
                $script:downloadedCount++
            }
            $ok = $true
        }

        $script:Manifest.Add([pscustomobject]@{
            source_repo = 'canlab/Neuroimaging_Pattern_Masks'
            source_path = $sourceItemPath
            size = $item.size
            html_url = $item.html_url
            local_path = $localPath
            downloaded = $ok
        })

        $script:processedCount++
        if (($script:processedCount % 25) -eq 0) {
            Write-Log ("Progress: processed files={0}, downloaded this run={1}" -f $script:processedCount, $script:downloadedCount)
        }
    }
}

$targets = @(
    @{
        Path = 'Atlases_and_parcellations/2022_Hansen_PET_tracer_maps'
        Destination = Join-Path $Root '03_pet_and_neurotransmitter_maps\CANlab_2022_Hansen_PET_tracer_maps'
    },
    @{
        Path = 'Neurosynth_maps'
        Destination = Join-Path $Root '04_neurosynth_and_meta_maps\CANlab_Neurosynth_maps'
    },
    @{
        Path = 'Individual_study_maps/2024_Bo_EmotionRegulation_BayesFactor'
        Destination = Join-Path $Root '05_system_component_maps\CANlab_2024_Bo_EmotionRegulation_BayesFactor'
    }
)

$script:processedCount = 0
$script:downloadedCount = 0
Write-Log 'Starting CANlab related directory downloads.'

foreach ($target in $targets) {
    $encodedPath = ($target.Path -split '/' | ForEach-Object { [uri]::EscapeDataString($_) }) -join '/'
    $apiUrl = "https://api.github.com/repos/canlab/Neuroimaging_Pattern_Masks/contents/$encodedPath`?ref=master"
    Write-Log ("Starting target {0}" -f $target.Path)
    Download-GitHubContents -ApiUrl $apiUrl -Destination $target.Destination -SourcePath $target.Path
}

$script:Manifest | Export-Csv -LiteralPath $manifestFile -NoTypeInformation -Encoding UTF8
Write-Log ("Finished CANlab related downloads. processed files={0}, downloaded this run={1}, manifest={2}" -f $script:processedCount, $script:downloadedCount, $manifestFile)
