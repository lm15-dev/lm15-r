import test from "node:test";
import assert from "node:assert/strict";
import { Lm15WebR, LM15Error } from "../../inst/browser/lm15.mjs";

function worker(handle) {
  const calls = [];
  return { calls, async evalRString(code) {
    const match = /^lm15::browser_dispatch\(("[^"]*"), ("[^"]*"), rawToChar\(jsonlite::base64_dec\("([A-Za-z0-9+/=]*)"\)\)\)$/.exec(code);
    assert.ok(match, "Only fixed R code and base64 data cross the boundary");
    const action = JSON.parse(match[1]), id = JSON.parse(match[2]);
    const input = JSON.parse(Buffer.from(match[3], "base64").toString("utf8"));
    calls.push({ action, id, input });
    return JSON.stringify({ ok: true, result: await handle(action, input, id) });
  } };
}
const wire = { method: "POST", url: "https://example.test/chat", headers: { authorization: "Bearer test" }, body_b64: "e30=" };
const reply = { model: "test", message: { role: "assistant", parts: [{ type: "text", text: "hi" }] }, finish_reason: "stop" };

test("completion uses browser fetch, safe R data, and cleanup", async () => {
  const request = { model: "test", messages: [{ role: "user", parts: [{ type: "text", text: '\"); system("bad"); #' }] }] };
  const r = worker((action, input) => {
    if (action === "prepare") { assert.deepEqual(input.request, request); return wire; }
    if (action === "response") return { response: reply };
    return {};
  });
  let sent;
  const client = new Lm15WebR(r, { provider: "openai", apiKey: "test", fetch: async (url, options) => {
    sent = options; assert.equal(url, wire.url); return new Response("{}", { status: 200 });
  } });
  assert.deepEqual(await client.complete(request), reply);
  assert.equal(sent.redirect, "error"); assert.equal(sent.credentials, "omit");
  assert.equal(r.calls.at(-1).action, "dispose");
});

test("browser-forbidden required headers refuse before fetch", async () => {
  const r = worker(action => action === "prepare" ? { ...wire, headers: { "user-agent": "required" } } : {});
  const client = new Lm15WebR(r, { provider: "claude-code", apiKey: "test", fetch: () => assert.fail("must not send") });
  await assert.rejects(client.complete({}), error => error instanceof LM15Error && error.code === "unsupported_feature");
  assert.equal(r.calls.at(-1).action, "dispose");
});

test("large responses are cancelled rather than buffered without a bound", async () => {
  const r = worker(action => action === "prepare" ? wire : {});
  const client = new Lm15WebR(r, { provider: "openai", apiKey: "test", maxResponseBytes: 3,
    fetch: async () => new Response("1234") });
  await assert.rejects(client.complete({}), /exceeds maxResponseBytes/);
  assert.equal(r.calls.at(-1).action, "dispose");
  assert.ok(!r.calls.some(c => c.action === "response"));
});

test("breaking a stream aborts fetch and disposes its R state", async () => {
  let cancelled = false, signal;
  const r = worker(action => action === "prepare" ? wire : action === "feed" ? { events: [{ type: "start", model: "test" }] } : {});
  const body = new ReadableStream({ pull(controller) { controller.enqueue(new TextEncoder().encode("data: {}\n\n")); }, cancel() { cancelled = true; } });
  const client = new Lm15WebR(r, { provider: "openai", apiKey: "test", fetch: async (url, options) => { signal = options.signal; return new Response(body); } });
  for await (const event of client.stream({})) { assert.equal(event.type, "start"); break; }
  assert.ok(signal.aborted); assert.ok(cancelled);
  assert.equal(r.calls.at(-1).action, "dispose");
  assert.ok(!r.calls.some(c => c.action === "finish"));
});

test("tagged rotating credentials reach the R codec once per operation", async () => {
  let calls = 0;
  const r = worker((action, input) => {
    if (action === "resource_prepare") { assert.equal(input.api_key.kind, "bearer_token"); return { requests: [wire] }; }
    if (action === "resource_response") return { value: [] };
    return {};
  });
  const client = new Lm15WebR(r, { provider: "azure", apiKey: async () => { calls++; return { kind: "bearer_token", value: "token" }; }, fetch: async () => new Response("{}") });
  assert.deepEqual(await client.models(), []); assert.equal(calls, 1);
});

test("batch submission passes upload results to the submission builder", async () => {
  const r = worker((action, input) => {
    if (action === "resource_prepare") {
      if (input.action === "submit") assert.deepEqual(input.args.upload_body, { id: "file-1" });
      return { requests: [wire] };
    }
    if (action === "resource_response") return { value: { id: "file-1" } };
    return {};
  });
  const client = new Lm15WebR(r, { provider: "openai", apiKey: "test", fetch: async () => new Response("{}") });
  await client.batchSubmit({ requests: [] });
  assert.deepEqual(r.calls.filter(c => c.action === "resource_prepare").map(c => c.input.action), ["upload", "submit"]);
  assert.equal(r.calls.filter(c => c.action === "dispose").length, 2);
});

test("unsafe numbers and cyclic data never reach R", async () => {
  const r = worker(() => ({}));
  const client = new Lm15WebR(r, { provider: "openai", apiKey: "test" });
  await assert.rejects(client.complete({ large: 9007199254740992 }), /exact range/);
  const cycle = {}; cycle.self = cycle;
  await assert.rejects(client.complete(cycle), /cycles/);
  assert.ok(!r.calls.some(c => c.action === "prepare"));
});
