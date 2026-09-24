#!/usr/bin/env python3
"""Run the shared, unmodified contract comparator against the installed R package."""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--contract", type=Path, default=ROOT.parent / "lm15-contract")
    parser.add_argument("--direction", default="all")
    parser.add_argument("--case")
    parser.add_argument("--compare-python", action="store_true", help="Also compare twelve extra request combinations with the Python reference")
    parser.add_argument("--report-dir", type=Path, default=ROOT / "test-results" / "contract")
    args = parser.parse_args()
    contract = args.contract.resolve()
    spec = importlib.util.spec_from_file_location("lm15_contract_check", contract / "harness" / "check.py")
    harness = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = harness
    spec.loader.exec_module(harness)
    directions = list(harness.DIRECTIONS) if args.direction == "all" else args.direction.split(",")
    if any(d not in harness.DIRECTIONS for d in directions):
        parser.error("Unknown direction")
    head = harness.contract_head()
    if head is None or head != (ROOT / "CONTRACT_PIN").read_text().strip():
        parser.error("Contract HEAD must equal CONTRACT_PIN")
    if harness.contract_dirty():
        parser.error("The contract must have no modified tracked files")
    args.report_dir.mkdir(parents=True, exist_ok=True)
    failed = False
    with tempfile.TemporaryDirectory(prefix="lm15-r-library-") as library:
        subprocess.run(["R", "CMD", "INSTALL", "--no-multiarch", "--library=" + library, str(ROOT)], check=True)
        previous = os.environ.get("R_LIBS_USER")
        os.environ["R_LIBS_USER"] = library + (os.pathsep + previous if previous else "")
        shim = harness.Shim("r", ["Rscript", "--vanilla", str(ROOT / "exec" / "lm15-vet.R")], ROOT)
        try:
            harness.check_pin(shim)
            if not shim.sandboxed:
                print("WARNING: OS network isolation unavailable; adapter refuses network calls.", file=sys.stderr)
            reply = shim.call("capabilities")
            if not reply.get("ok"):
                raise RuntimeError("Cannot read adapter capabilities")
            capabilities = reply["result"]
            for direction in directions:
                report = harness.run_direction(shim, direction, args.case, args.report_dir, auth_scope="all")
                harness.write_reports(report, shim, capabilities, args.report_dir)
                counts = report.counts
                print(f"{direction}: {json.dumps(counts)}", flush=True)
                failed |= counts["fail"] > 0
            if args.compare_python:
                probe_spec = importlib.util.spec_from_file_location("lm15_parity_probes", ROOT / "tools" / "parity-probes.py")
                probes = importlib.util.module_from_spec(probe_spec)
                probe_spec.loader.exec_module(probes)
                failed |= probes.run(harness, shim, ROOT.parent / "lm15-python", args.report_dir) > 0
        finally:
            shim.close()
            if previous is None:
                os.environ.pop("R_LIBS_USER", None)
            else:
                os.environ["R_LIBS_USER"] = previous
    return int(failed)


if __name__ == "__main__":
    sys.exit(main())
