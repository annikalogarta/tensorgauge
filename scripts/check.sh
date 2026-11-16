#!/usr/bin/env bash
set -euo pipefail

task_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
mode="${1:-software}"
software='testOnly tensorgauge.fp4.E2M1Spec tensorgauge.fp4.FP4GemmReferenceSpec tensorgauge.fp4.FP4ElaborationSpec tensorgauge.fp4.FP4GemmEvaluationSpec tensorgauge.fp4.FP4DotAddReferenceSpec tensorgauge.fp4.FP4DotAddStudySpec'
rtl='testOnly tensorgauge.fp4.FP4RTLSpec tensorgauge.fp4.FP4GemmRTLSpec tensorgauge.fp4.FP4GemmEvaluationRTLSpec tensorgauge.fp4.FP4DotAddRTLSpec'
case "$mode" in
  software) tasks=("$software") ;;
  rtl) tasks=("$rtl") ;;
  all) tasks=("$software" "$rtl") ;;
  demo) tasks=('runMain tensorgauge.fp4.RunFP4Gemm') ;;
  emit) tasks=('runMain tensorgauge.EmitDotProducts --systemverilog') ;;
  evaluation) tasks=('runMain tensorgauge.fp4.RunFP4GemmEvaluation') ;;
  precision) tasks=('runMain tensorgauge.fp4.RunFP4DotAddStudy') ;;
  *) echo 'Usage: bash scripts/check.sh [software|rtl|all|demo|emit|evaluation|precision]' >&2; exit 2 ;;
esac

if [[ "$mode" == rtl || "$mode" == all ]]; then
  for tool in verilator g++ make which; do
    command -v "$tool" >/dev/null || {
      echo "Missing $tool. Load/install the simulator toolchain; see docs/running.md." >&2
      exit 1
    }
  done
fi
mkdir -p "$task_root/reports"
# pipefail preserves test failures even when tee writes a successful log.
bash "$task_root/scripts/sbt-local.sh" "${tasks[@]}" 2>&1 | tee "$task_root/reports/check-$mode.log"
