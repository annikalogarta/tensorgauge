param([ValidateSet('software', 'rtl', 'all', 'demo', 'emit', 'evaluation', 'precision')][string]$Mode = 'software')

$ErrorActionPreference = 'Stop'
$taskRoot = Split-Path -Parent $PSScriptRoot
$software = 'testOnly tensorgauge.fp4.E2M1Spec tensorgauge.fp4.FP4GemmReferenceSpec tensorgauge.fp4.FP4ElaborationSpec tensorgauge.fp4.FP4GemmEvaluationSpec tensorgauge.fp4.FP4DotAddReferenceSpec tensorgauge.fp4.FP4DotAddStudySpec'
$rtl = 'testOnly tensorgauge.fp4.FP4RTLSpec tensorgauge.fp4.FP4GemmRTLSpec tensorgauge.fp4.FP4GemmEvaluationRTLSpec tensorgauge.fp4.FP4DotAddRTLSpec'
$tasks = switch ($Mode) {
    'software' { @($software) }
    'rtl' { @($rtl) }
    'all' { @($software, $rtl) }
    'demo' { @('runMain tensorgauge.fp4.RunFP4Gemm') }
    'emit' { @('runMain tensorgauge.EmitDotProducts --systemverilog') }
    'evaluation' { @('runMain tensorgauge.fp4.RunFP4GemmEvaluation') }
    'precision' { @('runMain tensorgauge.fp4.RunFP4DotAddStudy') }
}
if ($Mode -in @('rtl', 'all')) {
    foreach ($tool in @('verilator', 'g++', 'make', 'which')) {
        if (-not (Get-Command $tool -ErrorAction SilentlyContinue)) {
            throw "Missing $tool. Run 'bash scripts/check.sh all' on the Linux server; see docs/running.md."
        }
    }
    Write-Warning 'The Chisel simulator wrapper failed with native Windows shell commands here; Linux is the supported RTL test path.'
}
New-Item -ItemType Directory -Path (Join-Path $taskRoot 'reports') -Force | Out-Null
& (Join-Path $PSScriptRoot 'sbt-local.ps1') -Tasks $tasks 2>&1 |
    Tee-Object -FilePath (Join-Path $taskRoot "reports/check-$Mode.log")
