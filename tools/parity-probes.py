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
    # Surfaces added 2026-09-26: data parts, judgments, the open-model hosts.
    data = {"type": "data", "value": {"note": "caf\u00e9", "n": 1.50, "none": None, "list": []}}
    for provider, model in [("openai", "gpt-5-mini"), ("openai-chat", "gpt-5-mini"), ("anthropic", "claude-haiku-4-5"), ("gemini", "gemini-2.5-flash")]:
        request = deepcopy(base)
        request["model"] = model
        request["tools"] = [tool]
        request["messages"] = [
            {"role": "user", "parts": [text, data]},
            {"role": "assistant", "parts": [{"type": "data", "value": {"ok": True}, "probabilities": {"ok": {"true": 0.75, "false": 0.25}}, "method": "provider_classification"},
                                            {"type": "tool_call", "id": "c", "name": "lookup", "input": {}}]},
            {"role": "tool", "parts": [{"type": "tool_result", "id": "c", "name": "lookup", "content": [data]}]},
        ]
        yield f"{provider}-data-parts-everywhere", provider, request
    levels = {"type": "integer", "description": "How good?", "anyOf": [{"const": 0, "title": "poor"}, {"const": 1, "description": "fine"}, {"const": 2, "title": "great", "description": "very good"}]}
    styles = {"type": "string", "anyOf": [{"const": "a"}, {"const": "b", "description": "the second"}]}
    schema = {"type": "object", "properties": {"q": levels, "s": styles, "free": {"type": "string"}, "yes": {"type": "boolean"}}, "required": ["q", "s", "free", "yes"], "additionalProperties": False}
    for provider, model in [("openai", "gpt-5-mini"), ("anthropic", "claude-haiku-4-5"), ("gemini", "gemini-2.5-flash"), ("groq", "openai/gpt-oss-20b")]:
        for policy in ["if_available", "off"]:
            request = deepcopy(base)
            request["model"] = model
            request["config"] = {"response_format": {"type": "json_schema", "name": "j", "strict": True, "schema": schema}, "probabilities": policy}
            yield f"{provider}-mixed-judgments-{policy}", provider, request
    for provider, model, effort in [("deepinfra", "openai/gpt-oss-120b", "off"), ("together", "openai/gpt-oss-120b", "max"), ("together", "zai-org/GLM-5.3", "off"),
                                    ("fireworks", "accounts/fireworks/models/glm-5p3", "xhigh"), ("parasail", "parasail-qwen3-vl", "minimal")]:
        request = deepcopy(base)
        request["model"] = model
        request["tools"] = [tool]
        request["config"] = {"reasoning": {"effort": effort}, "tool_choice": {"mode": "auto"}, "max_tokens": 64}
        yield f"{provider}-{model.split('/')[-1]}-reasoning-{effort}", provider, request
    for model, mode in [("deepseek-ai/DeepSeek-V3.2-Exp", "required"), ("Qwen/Qwen3-Coder-480B", "required"), ("anthropic/claude-haiku-4-5", "none")]:
        request = deepcopy(base)
        request["model"] = model
        request["tools"] = [tool]
        request["config"] = {"tool_choice": {"mode": mode}}
        yield f"deepinfra-{model.split('/')[-1]}-tool-choice-{mode}", "deepinfra", request


# Differences where R follows the contract and the reference does not. They
# are reported on every run, never counted as failures, and each one names
# the finding it belongs to (IMPLEMENTATION.md, "Findings"). A difference not
# listed here fails the run.
KNOWN_DIFFERENCES = {
    "openai-data-parts-everywhere": "F1: the reference drops an assistant data part on the Responses wire without a record; R sends its JSON text",
    "openai-chat-data-parts-everywhere": "F1: the reference drops an assistant data part on the Chat wire without a record; R sends its JSON text",
}


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
            status = "pass" if diff is None else ("known" if name in KNOWN_DIFFERENCES else "fail")
            results.append({"id": name, "status": status, "known": KNOWN_DIFFERENCES.get(name), "request": request, "provider": provider,
                            "difference": diff.to_dict() if diff else None})
    finally:
        reference.close()
    (report_dir / "python-probes.json").write_text(json.dumps(results, indent=2) + "\n")
    failures = sum(x["status"] == "fail" for x in results)
    known = [x for x in results if x["status"] == "known"]
    print(f"Python comparison probes: {len(results) - failures - len(known)} pass, {len(known)} known differences, {failures} fail", flush=True)
    for x in known:
        print(f"  known: {x['id']}: {x['known']}", flush=True)
    return failures
