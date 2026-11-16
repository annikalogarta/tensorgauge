param([string[]]$Tasks = @('testOnly tensorgauge.fp4.E2M1Spec tensorgauge.fp4.FP4GemmReferenceSpec tensorgauge.fp4.FP4ElaborationSpec'))

$ErrorActionPreference = 'Stop'
$taskRoot = Split-Path -Parent $PSScriptRoot
$taskCache = Join-Path $taskRoot '.cache'
$sbtVersion = ((Get-Content -LiteralPath (Join-Path $taskRoot 'project/build.properties') |
    Where-Object { $_ -match '^sbt.version\s*=' }) -replace '^sbt.version\s*=\s*', '').Trim()
$launcher = Join-Path $taskCache "sbt-launch-$sbtVersion.jar"
New-Item -ItemType Directory -Path $taskCache -Force | Out-Null
if (-not (Test-Path -LiteralPath $launcher)) {
    Invoke-WebRequest -UseBasicParsing -Uri "https://repo.maven.apache.org/maven2/org/scala-sbt/sbt-launch/$sbtVersion/sbt-launch-$sbtVersion.jar" -OutFile $launcher
}
$repositories = Join-Path $taskCache 'repositories'
@('[repositories]', 'local', 'maven-central: https://repo.maven.apache.org/maven2') | Set-Content -LiteralPath $repositories
$previousCache = $env:COURSIER_CACHE
try {
    $env:COURSIER_CACHE = Join-Path $taskCache 'coursier'
    Push-Location $taskRoot
    try {
        & java "-Duser.home=$taskCache/java-home" "-Dsbt.global.base=$taskCache/sbt" "-Dsbt.boot.directory=$taskCache/sbt/boot" "-Dsbt.ivy.home=$taskCache/ivy" "-Dsbt.repository.config=$repositories" '-Dsbt.override.build.repos=true' '-Dsbt.supershell=false' -jar $launcher @Tasks
        if ($LASTEXITCODE -ne 0) { throw "sbt failed with exit code $LASTEXITCODE" }
    } finally { Pop-Location }
} finally { $env:COURSIER_CACHE = $previousCache }
