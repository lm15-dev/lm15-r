import test from "node:test";
import assert from "node:assert/strict";
import { Lm15WebR } from "../../inst/browser/lm15.mjs";

test("JSON-text completion never parses large canonical integers in JavaScript", async () => {
  const request = '{"model":"test","config":{"max_tokens":9007199254740993}}';
  const response = '{"usage":{"input_tokens":9007199254740993}}';
  const webR = { async evalRString(code) {
    const action = JSON.parse(/^lm15::browser_dispatch\(("[^"]*")/.exec(code)[1]);
    if (action === "prepare") {
      const payload = JSON.parse(Buffer.from(/base64_dec\("([A-Za-z0-9+/=]*)"\)/.exec(code)[1], "base64").toString("utf8"));
      assert.equal(payload.request, request);
      assert.equal(payload.json_only, true);
      return JSON.stringify({ ok: true, result: { method: "POST", url: "https://example.test", headers: {}, body_b64: "e30=" } });
    }
    return JSON.stringify({ ok: true, result: action === "response" ? { response } : {} });
  } };
  const lm = new Lm15WebR(webR, { provider: "openai", apiKey: "test", fetch: async () => new Response("{}") });
  assert.equal(await lm.completeJSON(request), response);
});
