import { chromium } from "playwright-core";
import { createServer } from "node:http";
import { WebSocketServer } from "ws";
import { readFile, realpath, mkdir, writeFile } from "node:fs/promises";
import { resolve, dirname, extname, sep } from "node:path";
import { fileURLToPath } from "node:url";
import assert from "node:assert/strict";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const artifacts = await realpath(resolve(root, "dist/webr"));
const calls = [];
const server = createServer(async (request, response) => {
  const url = new URL(request.url, "http://127.0.0.1");
  const headers = { "Cross-Origin-Opener-Policy": "same-origin", "Cross-Origin-Embedder-Policy": "require-corp", "Cross-Origin-Resource-Policy": "same-origin" };
  try {
    if (url.pathname.startsWith("/provider/")) {
      const chunks = []; let length = 0;
      for await (const chunk of request) { length += chunk.length; if (length > 1024 * 1024) throw Error("Request too large"); chunks.push(chunk); }
      const body = chunks.length ? JSON.parse(Buffer.concat(chunks).toString()) : null;
      calls.push({ path: url.pathname, method: request.method, body });
      if (url.pathname.endsWith("/models")) {
        response.writeHead(200, { ...headers, "content-type": "application/json" });
        response.end('{"data":[{"id":"gpt-browser-test"}]}'); return;
      }
      if (body?.model === "slow") {
        response.writeHead(200, { ...headers, "content-type": "application/json" });
        response.write('{"model":');
        const timer = setTimeout(() => response.end('"slow"}'), 1000);
        response.on("close", () => clearTimeout(timer)); return;
      }
      if (body?.model === "error") {
        response.writeHead(401, { ...headers, "content-type": "application/json" });
        response.end(JSON.stringify({ error: { code: "invalid_api_key", message: request.headers.authorization } })); return;
      }
      if (body?.stream) {
        response.writeHead(200, { ...headers, "content-type": "text/event-stream" });
        response.write('data: {"choices":[{"delta":{"content":"hello"},"finish_reason":null}]}\n\n');
        response.end('data: {"choices":[{"delta":{},"finish_reason":"stop"}],"usage":{"prompt_tokens":2,"completion_tokens":1}}\n\ndata: [DONE]\n\n'); return;
      }
      response.writeHead(200, { ...headers, "content-type": "application/json" });
      response.end('{"model":"gpt-browser-test","choices":[{"message":{"content":"hello"},"finish_reason":"stop"}],"usage":{"prompt_tokens":9007199254740993,"completion_tokens":2}}'); return;
    }
    if (url.pathname === "/") { response.writeHead(200, { ...headers, "content-type": "text/html" }); response.end("<!doctype html><title>lm15 webR integration</title>"); return; }
    const path = url.pathname === "/lm15.mjs" ? resolve(root, "inst/browser/lm15.mjs") : await realpath(resolve(artifacts, "." + decodeURIComponent(url.pathname)));
    if (url.pathname !== "/lm15.mjs" && !path.startsWith(artifacts + sep)) throw Error("Invalid path");
    const body = await readFile(path);
    const type = { ".js": "text/javascript", ".mjs": "text/javascript", ".wasm": "application/wasm", ".json": "application/json" }[extname(path)] ?? "application/octet-stream";
    response.writeHead(200, { ...headers, "content-type": type, "content-length": body.length }); response.end(body);
  } catch { response.writeHead(404, headers); response.end("Not found"); }
});
const liveFrames = [];
const sockets = new WebSocketServer({ noServer: true });
server.on("upgrade", (request, socket, head) => {
  if (!request.url.startsWith("/ws/")) return socket.destroy();
  sockets.handleUpgrade(request, socket, head, connection => sockets.emit("connection", connection));
});
sockets.on("connection", socket => socket.on("message", bytes => {
  const frame = JSON.parse(bytes.toString()); liveFrames.push(frame);
  if (frame.setup) socket.send('{"setupComplete":{}}');
  else socket.send('{"serverContent":{"modelTurn":{"parts":[{"text":"live hello"}]},"turnComplete":true},"usageMetadata":{"promptTokenCount":1,"responseTokenCount":2}}');
}));
await new Promise(done => server.listen(0, "127.0.0.1", done));
const origin = `http://127.0.0.1:${server.address().port}`;
let browser;
try {
  browser = await chromium.launch({ executablePath: process.env.CHROMIUM_BIN ?? "/run/current-system/sw/bin/chromium", headless: true });
  const context = await browser.newContext();
  const external = [];
  await context.route("**/*", route => {
    const url = route.request().url();
    if (url.startsWith(origin + "/") || url.startsWith("blob:") || url.startsWith("data:")) return route.continue();
    external.push(url); return route.abort();
  });
  await context.routeWebSocket("**/*", socket => {
    if (socket.url().startsWith(origin.replace(/^http/, "ws") + "/")) socket.connectToServer();
    else { external.push(socket.url()); socket.close(); }
  });
  const page = await context.newPage();
  page.on("pageerror", error => console.error("Browser error:", error.message));
  await page.goto(origin);
  const result = await page.evaluate(async origin => {
    const { WebR } = await import("/runtime/webr.mjs");
    const { Lm15WebR } = await import("/lm15.mjs");
    const r = new WebR({ baseUrl: origin + "/runtime/", interactive: false });
    await r.init();
    try {
      await r.installPackages("lm15", { repos: origin + "/repo" });
      const rVersion = await r.evalRString("R.version.string");
      const installedBridge = await r.evalRString('paste(readLines(system.file("browser/lm15.mjs", package="lm15"), warn=FALSE), collapse="\\n")');
      const bridgeMatches = installedBridge + "\n" === await (await fetch("/lm15.mjs")).text();
      const integer = await r.evalRString('lm15::as_json(lm15::integer_value("9007199254740993") + 2L)');
      const client = new Lm15WebR(r, { provider: "openai-chat", baseUrl: origin + "/provider/v1", apiKey: "BROWSER-TEST-KEY" });
      const request = { model: "gpt-browser-test", messages: [{ role: "user", parts: [{ type: "text", text: "hello" }] }] };
      const exact = await client.completeJSON(JSON.stringify(request));
      let unsafeRejected = false;
      try { await client.complete(request); } catch (error) { unsafeRejected = /exact range/.test(error.message); }
      const events = [];
      let final;
      for await (const event of client.stream(request, { onResponse: response => { final = response; } })) events.push(event);
      const models = await client.models();
      let secretHidden = false;
      try { await client.complete({ ...request, model: "error" }); } catch (error) { secretHidden = error.code === "auth" && !error.message.includes("BROWSER-TEST-KEY"); }
      const liveClient = new Lm15WebR(r, { provider: "gemini", baseUrl: origin + "/provider/v1", apiKey: "BROWSER-LIVE-KEY" });
      const session = await liveClient.live({ model: "gemini-live-preview" });
      await session.send({ type: "text", text: "hello" });
      const liveEvents = [];
      for await (const event of session.events()) { liveEvents.push(event); if (event.type === "turn_end") break; }
      let liveHeadersRejected = false;
      const openai = new Lm15WebR(r, { provider: "openai", baseUrl: origin + "/provider/v1", apiKey: "BROWSER-TEST-KEY" });
      try { await openai.live({ model: "gpt-realtime" }); } catch (error) { liveHeadersRejected = error.code === "unsupported_feature"; }
      const controller = new AbortController();
      const cancelled = new DOMException("Stopped by the test", "AbortError");
      const slow = new Lm15WebR(r, { provider: "openai-chat", baseUrl: origin + "/provider/v1", apiKey: "BROWSER-TEST-KEY", fetch: async (...args) => {
        const response = await fetch(...args); setTimeout(() => controller.abort(cancelled), 0); return response;
      } });
      let bodyCancelled = false;
      try { await slow.complete({ ...request, model: "slow" }, { signal: controller.signal }); } catch (error) { bodyCancelled = error === cancelled; }
      const liveController = new AbortController();
      const idle = await liveClient.live({ model: "gemini-live-preview" }, { signal: liveController.signal });
      const pending = idle.nextEvent(); liveController.abort(cancelled);
      let liveCancelled = false;
      try { await pending; } catch (error) { liveCancelled = error === cancelled; }
      await idle.close();
      const activeStates = await r.evalRNumber("length(ls(lm15:::.browser_sessions))");
      return { rVersion, integer, exact, unsafeRejected, events, final, models, secretHidden, activeStates, liveEvents, liveHeadersRejected, bodyCancelled, liveCancelled, bridgeMatches };
    } finally { await r.close(); }
  }, origin);
  assert.equal(result.integer, "9007199254740995");
  assert.match(result.exact, /"input_tokens":9007199254740993/);
  assert.match(result.exact, /"total_tokens":9007199254740995/);
  assert.ok(result.unsafeRejected);
  assert.deepEqual(result.events.map(event => event.type), ["start", "delta", "end"]);
  assert.equal(result.final.message.parts[0].text, "hello");
  assert.equal(result.final.usage.total_tokens, 3);
  assert.equal(result.models[0].id, "gpt-browser-test");
  assert.ok(result.secretHidden);
  assert.equal(result.activeStates, 0);
  assert.deepEqual(external, []);
  assert.deepEqual(result.liveEvents.map(event => event.type), ["text", "turn_end"]);
  assert.equal(result.liveEvents[0].text, "live hello");
  assert.ok(result.liveHeadersRejected);
  assert.equal(liveFrames.length, 3);
  assert.ok(result.bodyCancelled);
  assert.ok(result.liveCancelled);
  assert.ok(result.bridgeMatches, "The installed wasm package must contain the tested JavaScript bridge");
  const report = { rVersion: result.rVersion, chromium: browser.version(), assertions: 18, providerRequests: calls.length, externalRequests: external.length, status: "pass" };
  await mkdir(resolve(root, "test-results"), { recursive: true });
  await writeFile(resolve(root, "test-results/webr.json"), JSON.stringify(report, null, 2) + "\n");
  console.log(report);
} finally {
  await browser?.close();
  for (const socket of sockets.clients) socket.terminate();
  await new Promise(done => sockets.close(done));
  await new Promise(done => server.close(done));
}
