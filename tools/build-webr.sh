#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
engine=${CONTAINER_ENGINE:-podman}
out=${1:-"$root/dist/webr"}
mkdir -p "$out"
out=$(cd "$out" && pwd)
stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT
image=$(tr -d '\n' < "$root/WEBR_IMAGE")
if ! "$engine" image inspect "$image" >/dev/null 2>&1; then
  if [[ $(basename "$engine") == podman ]]; then
    "$engine" pull --signature-policy "$root/tools/webr-policy.json" "$image"
  else
    "$engine" pull "$image"
  fi
fi
python3 - "$root" "$out" <<'PY'
import hashlib, json, pathlib, sys, urllib.request
root, out = map(pathlib.Path, sys.argv[1:])
source = json.loads((root / 'tools/webr-sources.json').read_text())['jsonlite']
path = out / 'sources' / ('jsonlite_' + source['version'] + '.tar.gz')
path.parent.mkdir(parents=True, exist_ok=True)
data = path.read_bytes() if path.exists() else urllib.request.urlopen(source['url'], timeout=60).read()
if hashlib.sha256(data).hexdigest() != source['sha256']:
    raise SystemExit('JSON dependency source checksum does not match the pinned source')
if not path.exists():
    path.write_bytes(data)
PY
(cd "$stage" && R CMD build --no-build-vignettes "$root")
mv "$stage"/lm15_*.tar.gz "$stage/source.tar.gz"
# Use the official cross-compiler. Only the clean package source and output
# directory are mounted; credentials and the rest of the workspace are not.
"$engine" run --rm --network=none \
  -v "$stage/source.tar.gz:/source.tar.gz:ro" -v "$out:/output" \
  "$image" Rscript -e 'stopifnot(requireNamespace("jsonlite", quietly = TRUE)); rwasm:::wasm_build("lm15", "/source.tar.gz", "/output", TRUE)'
if [[ ${WEBR_BUILD_DEPENDENCIES:-1} == 1 ]]; then
  "$engine" run --rm --network=none \
    -v "$out/sources:/sources:ro" -v "$out:/output" \
    "$image" Rscript -e 'rwasm:::wasm_build("jsonlite", "/sources/jsonlite_2.0.0.tar.gz", "/output", TRUE)'
  mkdir -p "$out/runtime"
  "$engine" run --rm --network=none -v "$out/runtime:/output" "$image" sh -c 'cp -a /opt/webr/dist/. /output/'
else
  # Development only: reuse dependencies already built with this image.
  test -f "$out/jsonlite_2.0.0.tgz"
  test -f "$out/runtime/R.wasm"
fi
Rscript "$root/tools/prepare-webr-repo.R" "$out"
printf 'WebAssembly packages and matching runtime written to %s\n' "$out"
