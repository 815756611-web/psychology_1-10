param(
    [Parameter(Mandatory = $true)]
    [string]$Root
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

Add-Type -AssemblyName System.Web

$repoOwner = 'canlab'
$repoName = 'Neuroimaging_Pattern_Masks'
$branch = 'master'
$baseTree = "https://github.com/$repoOwner/$repoName/tree/$branch"
$baseRaw = "https://raw.githubusercontent.com/$repoOwner/$repoName/$branch"

$logRoot = Join-Path $Root '07_download_logs'
New-Item -ItemType Directory -Force -Path $logRoot | Out-Null
$logFile = Join-Path $logRoot 'canlab_related_dirs_html_download.log'
$manifestFile = Join-Path $logRoot 'canlab_related_dirs_html_manifest.csv'
$script:Manifest = New-Object System.Collections.Generic.List[object]
$script:VisitedDirs = New-Object 'System.Collections.Generic.HashSet[string]'
$script:Failures = New-Object System.Collections.Generic.List[object]

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

function Encode-PathForUrl {
    param([string]$Path)
    return (($Path -split '/') | ForEach-Object { [uri]::EscapeDataString($_) }) -join '/'
}

function Get-SafeFileName {
    param([string]$Name)
    foreach ($char in [System.IO.Path]::GetInvalidFileNameChars()) {
        $Name = $Name.Replace($char, '_')
    }
    return $Name
}

function Get-GithubDirectoryEntriesFromHtml {
    param([string]$RepoPath)

    $encoded = Encode-PathForUrl -Path $RepoPath
    $url = "$baseTree/$encoded"
    $html = Invoke-WithRetry -Action { (Invoke-WebRequest -Uri $url -UseBasicParsing -TimeoutSec 60).Content }

    $pattern = 'href="/' + [regex]::Escape("$repoOwner/$repoName") + '/(blob|tree)/' + [regex]::Escape($branch) + '/([^"#?]+)"'
    $matches = [regex]::Matches($html, $pattern)
    $entries = @{}
    foreach ($m in $matches) {
        $kind = $m.Groups[1].Value
        $pathEncoded = [System.Web.HttpUtility]::HtmlDecode($m.Groups[2].Value)
        $path = [uri]::UnescapeDataString($pathEncoded)

        if (-not $path.StartsWith("$RepoPath/")) {
            continue
        }

        $relative = $path.Substring($RepoPath.Length + 1)
        if ([string]::IsNullOrWhiteSpace($relative) -or $relative.Contains('/')) {
            continue
        }

        if (-not $entries.ContainsKey($path) -or $kind -eq 'tree') {
            $entries[$path] = [pscustomobject]@{
                kind = $kind
                path = $path
                name = $relative
            }
        }
    }
    return $entries.Values
}

function Download-GitHubDirectoryFromHtml {
    param(
        [string]$RepoPath,
        [string]$Destination
    )

    if (-not $script:VisitedDirs.Add($RepoPath)) {
        return
    }

    New-Item -ItemType Directory -Force -Path $Destination | Out-Null
    Write-Log ("Scanning directory {0}" -f $RepoPath)
    $entries = Get-GithubDirectoryEntriesFromHtml -RepoPath $RepoPath

    foreach ($entry in $entries | Sort-Object kind, name) {
        $safeName = Get-SafeFileName -Name $entry.name
        $localPath = Join-Path $Destination $safeName

        if ($entry.kind -eq 'tree') {
            Download-GitHubDirectoryFromHtml -RepoPath $entry.path -Destination $localPath
            continue
        }

        $rawUrl = "$baseRaw/$(Encode-PathForUrl -Path $entry.path)"
        $ok = $false
        if ((Test-Path -LiteralPath $localPath) -and ((Get-Item -LiteralPath $localPath).Length -gt 0)) {
            $ok = $true
        } else {
            try {
                Write-Log ("Downloading {0}" -f $entry.path)
                Invoke-WithRetry -Action {
                    Invoke-WebRequest -Uri $rawUrl -OutFile $localPath -TimeoutSec 300
                }
                $ok = $true
                $script:DownloadedCount++
            } catch {
                $message = $_.Exception.Message
                Write-Log ("FAILED {0}: {1}" -f $entry.path, $message)
                $script:Failures.Add([pscustomobject]@{
                    source_repo = "$repoOwner/$repoName"
                    source_path = $entry.path
                    raw_url = $rawUrl
                    local_path = $localPath
                    error = $message
                })
            }
        }

        $script:ProcessedCount++
        $script:Manifest.Add([pscustomobject]@{
            source_repo = "$repoOwner/$repoName"
            source_path = $entry.path
            raw_url = $rawUrl
            local_path = $localPath
            downloaded = $ok
        })

        if (($script:ProcessedCount % 25) -eq 0) {
            Write-Log ("Progress: processed files={0}, downloaded this run={1}" -f $script:ProcessedCount, $script:DownloadedCount)
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

$script:ProcessedCount = 0
$script:DownloadedCount = 0
Write-Log 'Starting CANlab related directory downloads via GitHub HTML.'

foreach ($target in $targets) {
    Write-Log ("Starting target {0}" -f $target.Path)
    Download-GitHubDirectoryFromHtml -RepoPath $target.Path -Destination $target.Destination
}

$script:Manifest | Export-Csv -LiteralPath $manifestFile -NoTypeInformation -Encoding UTF8
if ($script:Failures.Count -gt 0) {
    $failureFile = Join-Path $logRoot 'canlab_related_dirs_html_failures.csv'
    $script:Failures | Export-Csv -LiteralPath $failureFile -NoTypeInformation -Encoding UTF8
    Write-Log ("Finished CANlab HTML downloads with failures. processed files={0}, downloaded this run={1}, failures={2}, manifest={3}" -f $script:ProcessedCount, $script:DownloadedCount, $script:Failures.Count, $manifestFile)
    exit 2
}
Write-Log ("Finished CANlab HTML downloads. processed files={0}, downloaded this run={1}, manifest={2}" -f $script:ProcessedCount, $script:DownloadedCount, $manifestFile)
