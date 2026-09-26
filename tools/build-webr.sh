#!/usr/bin/env bash
# Builds lm15 and its imports for webR with the official cross-compiler image
# (pinned by digest in WEBR_IMAGE), offline: every dependency source is
# pinned by SHA-256 in tools/webr-sources.json and verified before use.
#
#   bash tools/build-webr.sh [output-dir]
#   WEBR_BUILD_DEPENDENCIES=0  reuse dependencies already built in output-dir
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
# Fetch (once) and verify every pinned dependency source; write the build order.
python3 - "$root" "$out" <<'PY'
import hashlib, json, pathlib, sys, urllib.request
root, out = map(pathlib.Path, sys.argv[1:])
sources = {k: v for k, v in json.loads((root / 'tools/webr-sources.json').read_text()).items() if not k.startswith('_')}
(out / 'sources').mkdir(parents=True, exist_ok=True)
order = []
for name, source in sources.items():
    path = out / 'sources' / f"{name}_{source['version']}.tar.gz"
    data = path.read_bytes() if path.exists() else urllib.request.urlopen(source['url'], timeout=60).read()
    if hashlib.sha256(data).hexdigest() != source['sha256']:
        raise SystemExit(f'{name}: source checksum does not match the pinned source')
    if not path.exists():
        path.write_bytes(data)
    order.append(f"{name} {path.name}")
(out / 'sources' / 'ORDER').write_text('\n'.join(order) + '\n')
PY
(cd "$stage" && R CMD build --no-build-vignettes "$root" >/dev/null)
mv "$stage"/lm15_*.tar.gz "$stage/source.tar.gz"
# A compiled lm15 from an earlier version must not be published beside this one.
rm -f "$out"/lm15_*.tgz
cat > "$stage/build.sh" <<'SH'
set -eu
# rwasm installs a package natively before compiling it, to check its
# dependencies; the image's own R lacks some imports, so install them from
# the verified sources first (no network: pak's attempt fails harmlessly).
while read -r name file; do
  if ! Rscript -e "quit(status = !requireNamespace('$name', quietly = TRUE))"; then
    R CMD INSTALL --no-docs --no-html "/sources/$file" >/dev/null
  fi
done < /sources/ORDER
if [ "${WEBR_BUILD_DEPENDENCIES:-1}" = 1 ]; then
  while read -r name file; do
    Rscript -e "rwasm:::wasm_build('$name', '/sources/$file', '/output', TRUE)"
  done < /sources/ORDER
fi
Rscript -e 'rwasm:::wasm_build("lm15", "/source.tar.gz", "/output", TRUE)'
SH
# Only the package source, the verified dependency sources and the output
# directory are mounted; credentials and the rest of the workspace are not.
"$engine" run --rm --network=none -e WEBR_BUILD_DEPENDENCIES="${WEBR_BUILD_DEPENDENCIES:-1}" \
  -v "$stage/source.tar.gz:/source.tar.gz:ro" -v "$stage/build.sh:/build.sh:ro" \
  -v "$out/sources:/sources:ro" -v "$out:/output" \
  "$image" sh /build.sh
if [[ ${WEBR_BUILD_DEPENDENCIES:-1} == 1 ]]; then
  mkdir -p "$out/runtime"
  "$engine" run --rm --network=none -v "$out/runtime:/output" "$image" sh -c 'cp -a /opt/webr/dist/. /output/'
else
  # Development only: reuse dependencies already built with this image.
  while read -r name file; do
    ls "$out/${name}"_*.tgz >/dev/null
  done < "$out/sources/ORDER"
  test -f "$out/runtime/R.wasm"
fi
Rscript "$root/tools/prepare-webr-repo.R" "$out"
printf 'WebAssembly packages and matching runtime written to %s\n' "$out"
