#!/usr/bin/env bash
set -euo pipefail

task_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
task_cache="$task_root/.cache"
sbt_version="$(sed -n 's/^sbt.version[[:space:]]*=[[:space:]]*//p' "$task_root/project/build.properties" | tr -d '\r')"
launcher="$task_cache/sbt-launch-$sbt_version.jar"
command -v java >/dev/null || { echo 'Install or load a JDK (17 recommended) first.' >&2; exit 1; }
mkdir -p "$task_cache"
if [[ ! -f "$launcher" ]]; then
  command -v curl >/dev/null || { echo 'curl is required to download the sbt launcher.' >&2; exit 1; }
  curl --fail --location --retry 2 --output "$launcher.part" \
    "https://repo.maven.apache.org/maven2/org/scala-sbt/sbt-launch/$sbt_version/sbt-launch-$sbt_version.jar"
  mv -- "$launcher.part" "$launcher"
fi
printf '[repositories]\nlocal\nmaven-central: https://repo.maven.apache.org/maven2\n' > "$task_cache/repositories"
export COURSIER_CACHE="$task_cache/coursier"
cd -- "$task_root"
exec java "-Duser.home=$task_cache/java-home" "-Dsbt.global.base=$task_cache/sbt" \
  "-Dsbt.boot.directory=$task_cache/sbt/boot" "-Dsbt.ivy.home=$task_cache/ivy" \
  "-Dsbt.repository.config=$task_cache/repositories" '-Dsbt.override.build.repos=true' \
  '-Dsbt.supershell=false' -jar "$launcher" "$@"
