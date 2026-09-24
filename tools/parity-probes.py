"""Combinations outside the contract corpus; Python is a comparison, not the oracle."""
from copy import deepcopy
import json


def probes():
    text = {"type": "text", "text": "hello"}
    base = {"model": "gpt-4.1-mini", "messages": [{"role": "user", "parts": [text]}]}
    tool = {"type": "function", "name": "lookup", "parameters": {"type": "object", "properties": {}}}
    for provider, model in [("openai", "gpt-4.1-mini"), ("anthropic", "claude-sonnet-4-6"), ("gemini", "gemini-2.5-flash")]:
        for media in [
            {"type": "image", "media_type": "image/png", "data": "YQ=="},
            {"type": "document", "media_type": "application/pdf", "url": "https://example.test/document.pdf"},
        ]:
            request = deepcopy(base)
            request["model"] = model
            request["tools"] = [tool]
            request["messages"] += [
                {"role": "assistant", "parts": [{"type": "tool_call", "id": "c", "name": "lookup", "input": {"empty": {}, "null": None}}]},
                {"role": "tool", "parts": [{"type": "tool_result", "id": "c", "name": "lookup", "content": [media, {"type": "text", "text": "caption"}]}]},
            ]
            yield f"{provider}-tool-result-{media['type']}", provider, request
    for provider in ["openai", "openai-chat", "anthropic", "gemini"]:
        request = deepcopy(base)
        request["model"] = {"anthropic": "claude-sonnet-4-6", "gemini": "gemini-2.5-flash"}.get(provider, base["model"])
        request["tools"] = [tool]
        request["config"] = {"tool_choice": {"mode": "required", "allowed": ["lookup"]}, "response_format": {"type": "json_schema", "schema": {"type": "object", "properties": {}, "additionalProperties": False}, "strict": True}}
        yield f"{provider}-forced-tool-and-schema", provider, request
    for provider in ["openai-chat", "gemini"]:
        request = deepcopy(base)
        request["model"] = "gemini-2.5-flash" if provider == "gemini" else base["model"]
        request["config"] = {"store": False, "top_k": 7}
        yield f"{provider}-store-and-top-k", provider, request


def run(harness, r_shim, reference_root, report_dir):
    import sys
    import subprocess
    head = subprocess.check_output(["git", "-C", str(reference_root), "rev-parse", "HEAD"], text=True).strip()
    expected_head = (r_shim.cwd / "PYTHON_REFERENCE").read_text().strip()
    dirty = subprocess.check_output(["git", "-C", str(reference_root), "status", "--porcelain", "--untracked-files=no"], text=True).strip()
    if head != expected_head or dirty:
        raise RuntimeError("Python comparisons require a clean checkout at PYTHON_REFERENCE")
    reference = harness.Shim("python-probes", [sys.executable, "-m", "lm15.vet"], reference_root)
    results = []
    try:
        harness.check_pin(reference)
        for name, provider, request in probes():
            fields = dict(provider=provider, canonical_request=request, stream=False, api_key="offline-probe-key")
            expected = reference.call("build_request", **fields)
            actual = r_shim.call("build_request", **fields)
            if expected.get("ok") and actual.get("ok"):
                diff = harness.first_difference(expected["result"], actual["result"])
            else:
                def error(reply):
                    return {"ok": reply.get("ok"), "type": reply.get("error", {}).get("type"), "code": reply.get("error", {}).get("code")}
                diff = harness.first_difference(error(expected), error(actual))
            results.append({"id": name, "status": "pass" if diff is None else "fail", "request": request, "provider": provider,
                            "difference": diff.to_dict() if diff else None})
    finally:
        reference.close()
    (report_dir / "python-probes.json").write_text(json.dumps(results, indent=2) + "\n")
    failures = sum(x["status"] == "fail" for x in results)
    print(f"Python comparison probes: {len(results) - failures} pass, {failures} fail", flush=True)
    return failures
