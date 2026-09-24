/** Native browser fetch with the R package's canonical codecs in webR.
 * Pass an initialized WebR instance with package lm15 already installed.
 * This module does not persist or log credentials. Codec arguments include
 * base64-encoded credentials: base64 is transport encoding, not encryption.
 */
const encoder = new TextEncoder();
function base64(bytes) {
  let out = "";
  for (let i = 0; i < bytes.length; i += 16384)
    out += String.fromCharCode(...bytes.subarray(i, i + 16384));
  return btoa(out);
}
function unbase64(text) { return Uint8Array.from(atob(text), ch => ch.charCodeAt(0)); }
function assertJSON(value, seen = new Set()) {
  if (value === null || typeof value === "string" || typeof value === "boolean") return;
  if (typeof value === "number") {
    if (!Number.isFinite(value) || (Number.isInteger(value) && !Number.isSafeInteger(value)))
      throw new TypeError("JSON numbers must be finite and integer values must be within JavaScript's exact range");
    return;
  }
  if (typeof value !== "object" || seen.has(value)) throw new TypeError("JSON cannot carry undefined, functions, symbols, bigint, or cycles");
  const prototype = Object.getPrototypeOf(value);
  if (!Array.isArray(value) && prototype !== Object.prototype && prototype !== null)
    throw new TypeError("Use plain objects and arrays for canonical JSON, not class instances");
  seen.add(value);
  for (const item of Object.values(value)) assertJSON(item, seen);
  seen.delete(value);
}
function rJSON(value) {
  assertJSON(value);
  // The only interpolated characters are base64; caller text cannot become
  // executable R syntax. The resulting value is kept in the webR worker.
  return `rawToChar(jsonlite::base64_dec("${base64(encoder.encode(JSON.stringify(value)))}"))`;
}
const forbiddenHeaders = new Set([
  "host", "user-agent", "connection", "content-length", "cookie", "cookie2",
  "origin", "referer", "accept-encoding", "transfer-encoding", "upgrade",
  "te", "trailer", "via", "keep-alive", "expect", "date", "dnt",
  "access-control-request-headers", "access-control-request-method",
]);
export class LM15Error extends Error {
  constructor(detail) {
    super(detail.message);
    this.name = "LM15Error";
    this.code = detail.code;
    this.partial = detail.partial;
    this.events = detail.events ?? [];
  }
}
async function openBrowserLive(wire, { encode, decode, dispose, signal, timeout = 30000, maxQueue = 1024, maxFrameBytes = 32 * 1024 * 1024, createSocket }) {
  for (const limit of [timeout, maxQueue, maxFrameBytes]) if (!Number.isSafeInteger(limit) || limit <= 0) throw new TypeError("Live limits must be positive safe integers");
  if (signal?.aborted) throw signal.reason ?? new DOMException("Aborted", "AbortError");
  if (!createSocket) {
    if (Object.keys(wire.headers).length) throw new LM15Error({ code: "unsupported_feature", message: "This live endpoint requires headers the browser WebSocket API cannot set. Supply an explicit socket connector or use a server-side client." });
    createSocket = request => new WebSocket(request.url);
  }
  let socket;
  try { socket = await createSocket(wire); }
  catch { throw new LM15Error({ code: "transport", message: "Cannot open browser live connection; credential-bearing diagnostics are suppressed." }); }
  socket.binaryType = "arraybuffer";
  let closed = false, failure, waiting, queuedFrames = 0, readyDone = false;
  let chain = Promise.resolve();
  const events = [];
  let resolveReady, rejectReady;
  const ready = new Promise((resolve, reject) => { resolveReady = resolve; rejectReady = reject; });
  const wake = () => { const callback = waiting; waiting = undefined; callback?.(); };
  const fail = error => {
    failure = error;
    if (!readyDone) rejectReady(error);
    closed = true; wake();
    try { socket.close(); } catch {}
  };
  const abort = () => fail(signal.reason ?? new DOMException("Aborted", "AbortError"));
  signal?.addEventListener("abort", abort, { once: true });
  const timer = setTimeout(() => fail(new LM15Error({ code: "timeout", message: "Live connection setup timed out." })), timeout);
  socket.addEventListener("error", () => fail(new LM15Error({ code: "transport", message: "Browser live connection failed." })));
  socket.addEventListener("close", () => {
    closed = true; wake();
    if (!readyDone) rejectReady(new LM15Error({ code: "transport", message: "Live connection closed before setup completed." }));
  });
  const onOpen = () => {
    if (closed) return;
    try {
      for (const frame of wire.setup_frames) socket.send(JSON.stringify(frame));
      if (!wire.wait_for_setup) { readyDone = true; resolveReady(); }
    } catch { fail(new LM15Error({ code: "transport", message: "Sending live setup failed." })); }
  };
  socket.addEventListener("open", onOpen, { once: true });
  socket.addEventListener("message", event => {
    if (closed) return;
    const bytes = typeof event.data === "string" ? encoder.encode(event.data) : new Uint8Array(event.data);
    if (bytes.byteLength > maxFrameBytes || ++queuedFrames > maxQueue) { fail(new LM15Error({ code: "transport", message: "Live incoming data exceeds the configured limit." })); return; }
    chain = chain.then(async () => {
      if (closed) return;
      const result = await decode(bytes);
      let acknowledged = false;
      try { acknowledged = Object.hasOwn(JSON.parse(new TextDecoder().decode(bytes)), "setupComplete"); } catch {}
      if (wire.wait_for_setup && !readyDone) {
        if (result.events.some(e => (typeof e === "string" ? JSON.parse(e) : e).type === "error")) throw new LM15Error({ code: "invalid_request", message: "Provider rejected live setup." });
        if (acknowledged) { readyDone = true; resolveReady(); }
      }
      if (events.length + result.events.length > maxQueue) throw new LM15Error({ code: "transport", message: "Live event queue exceeds the configured limit." });
      events.push(...result.events); wake();
    }).catch(fail).finally(() => { queuedFrames--; });
  });
  if (signal?.aborted) abort();
  if (socket.readyState === 1) queueMicrotask(onOpen);
  const close = async () => {
    signal?.removeEventListener("abort", abort); clearTimeout(timer);
    if (!closed) { closed = true; socket.close(); wake(); }
    await chain; await dispose();
  };
  try { await ready; } catch (error) { await close(); throw error; } finally { clearTimeout(timer); }
  const send = async event => {
    if (closed) throw failure ?? new LM15Error({ code: "transport", message: "Live session is closed." });
    const result = await encode(event);
    try { for (const frame of result.frames) socket.send(JSON.stringify(frame)); }
    catch { fail(new LM15Error({ code: "transport", message: "Live send failed; no replay was attempted." })); throw failure; }
  };
  let consuming = false;
  const nextEvent = async () => {
    if (consuming) throw new TypeError("Only one live event consumer may wait at a time");
    consuming = true;
    try {
      while (!events.length && !closed && !failure) await new Promise(resolve => { waiting = resolve; });
      if (events.length) return events.shift();
      if (failure) throw failure;
      return null;
    } finally { consuming = false; }
  };
  return { send, nextEvent, close, interrupt: () => send({ type: "interrupt" }),
    async *events() { try { for (;;) { const event = await nextEvent(); if (event === null) return; yield event; } } finally { await close(); } } };
}

export class Lm15WebR {
  #webR;
  #options;
  #fetch;
  #maxResponseBytes;
  #queue = Promise.resolve();
  constructor(webR, { provider, apiKey, baseUrl, compat, settings, accountId, fetch: fetchImpl = globalThis.fetch?.bind(globalThis), maxResponseBytes = 128 * 1024 * 1024 } = {}) {
    if (!Number.isSafeInteger(maxResponseBytes) || maxResponseBytes <= 0) throw new TypeError("maxResponseBytes must be a positive safe integer");
    this.#maxResponseBytes = maxResponseBytes;
    if (!provider || (typeof apiKey !== "string" && typeof apiKey !== "function" && (!apiKey || typeof apiKey !== "object")))
      throw new TypeError("provider and an explicit apiKey or credential function are required");
    this.#webR = webR;
    this.#options = Object.fromEntries(Object.entries({ provider, apiKey, base_url: baseUrl, compat, settings, account_id: accountId }).filter(([, value]) => value !== undefined));
    this.#fetch = fetchImpl;
  }
  // Serialize access to this bridge's webR session. An independent HTTP
  // stream may await network data without monopolizing the R worker.
  #dispatch(action, id, value = {}) {
    const work = async () => {
      const code = `lm15::browser_dispatch(${JSON.stringify(action)}, ${JSON.stringify(id)}, ${rJSON(value)})`;
      let text;
      try { text = await this.#webR.evalRString(code); }
      catch { throw new LM15Error({ code: "transport", message: "webR could not complete the codec operation." }); }
      const reply = JSON.parse(text);
      assertJSON(reply);
      if (!reply.ok) throw new LM15Error(reply.error);
      return reply.result;
    };
    const result = this.#queue.then(work, work);
    this.#queue = result.catch(() => {});
    return result;
  }
  async #credentials() {
    const { apiKey, ...rest } = this.#options;
    let key;
    try { key = typeof apiKey === "function" ? await apiKey() : apiKey; }
    catch { throw new LM15Error({ code: "auth", message: "Credential provider failed." }); }
    if (!(typeof key === "string" && key) && !(key && typeof key === "object" && ["api_key", "bearer_token", "aws"].includes(key.kind)))
      throw new TypeError("Credential provider must return a non-empty string or a tagged credential");
    return { ...rest, api_key: key };
  }
  async #prepare(id, request, streaming, jsonOnly = false) {
    return this.#dispatch("prepare", id, { ...await this.#credentials(), request, stream: streaming, json_only: jsonOnly });
  }
  #checkHeaders(wire) {
    for (const name of Object.keys(wire.headers)) {
      const lower = name.toLowerCase();
      if (forbiddenHeaders.has(lower) || lower.startsWith("sec-") || lower.startsWith("proxy-"))
        throw new LM15Error({ code: "unsupported_feature", message: `The browser cannot set required header ${name}; use a server-side client.` });
    }
  }
  async #request(wire, signal) {
    this.#checkHeaders(wire);
    if (signal?.aborted) throw signal.reason ?? new DOMException("Aborted", "AbortError");
    try {
      return await this.#fetch(wire.url, { method: wire.method, headers: wire.headers,
        body: wire.body_b64 ? unbase64(wire.body_b64) : undefined,
        redirect: "error", credentials: "omit", signal });
    } catch {
      if (signal?.aborted) throw signal.reason ?? new DOMException("Aborted", "AbortError");
      throw new LM15Error({ code: "transport", message: "Browser request failed. Check the provider's CORS rules, network connection, and required headers." });
    }
  }
  async #readBody(response, signal) {
    const declared = Number(response.headers.get("content-length"));
    if (declared > this.#maxResponseBytes) {
      await response.body?.cancel().catch(() => {});
      throw new LM15Error({ code: "transport", message: "Response exceeds maxResponseBytes." });
    }
    if (!response.body) return new Uint8Array();
    const reader = response.body.getReader();
    const chunks = []; let size = 0;
    try {
      for (;;) {
        const { done, value } = await reader.read();
        if (done) break;
        size += value.byteLength;
        if (size > this.#maxResponseBytes) throw new LM15Error({ code: "transport", message: "Response exceeds maxResponseBytes." });
        chunks.push(value);
      }
      const body = new Uint8Array(size); let offset = 0;
      for (const chunk of chunks) { body.set(chunk, offset); offset += chunk.byteLength; }
      return body;
    } catch (error) {
      if (signal?.aborted) throw signal.reason ?? new DOMException("Aborted", "AbortError");
      if (error instanceof LM15Error) throw error;
      throw new LM15Error({ code: "transport", message: "Browser response was interrupted during transfer." });
    } finally { await reader.cancel().catch(() => {}); reader.releaseLock(); }
  }
  async operation(surface, action, args = {}, { signal, jsonOnly = false } = {}) {
    const id = crypto.randomUUID();
    try {
      const { requests } = await this.#dispatch("resource_prepare", id, { ...await this.#credentials(), surface, action, args, json_only: jsonOnly });
      const replies = [];
      for (const wire of requests) {
        const response = await this.#request(wire, signal);
        replies.push({ status: response.status, headers: Object.fromEntries(response.headers), body_b64: base64(await this.#readBody(response, signal)) });
        // Do not perform later side effects after an earlier HTTP failure.
        if (!response.ok) break;
      }
      return (await this.#dispatch("resource_response", id, { replies })).value;
    } finally { await this.#dispatch("dispose", id).catch(() => {}); }
  }
  operationJSON(surface, action, argsJSON, options = {}) {
    if (typeof argsJSON !== "string") throw new TypeError("Expected canonical JSON text");
    return this.operation(surface, action, argsJSON, { ...options, jsonOnly: true });
  }
  async live(config, options = {}) {
    const id = crypto.randomUUID();
    try {
      const wire = await this.#dispatch("live_prepare", id, { ...await this.#credentials(), config, json_only: !!options.jsonOnly });
      return await openBrowserLive(wire, { ...options,
        encode: event => this.#dispatch("live_send", id, { event }),
        decode: bytes => this.#dispatch("live_receive", id, { body_b64: base64(bytes) }),
        dispose: () => this.#dispatch("dispose", id).catch(() => {}) });
    } catch (error) { await this.#dispatch("dispose", id).catch(() => {}); throw error; }
  }
  models(options) { return this.operation("models", "list", {}, options); }
  imageGenerate(request, options) { return this.operation("image", "generate", { request }, options); }
  speechGenerate(request, options) { return this.operation("speech", "generate", { request }, options); }
  fileUpload(request, options) { return this.operation("files", "upload", { request }, options); }
  fileGet(id, options) { return this.operation("files", "get", { id }, options); }
  fileList(args = {}, options) { return this.operation("files", "list", args, options); }
  fileDelete(id, options) { return this.operation("files", "delete", { id }, options); }
  async fileDownload(id, options) { return unbase64((await this.operation("files", "download", { id }, options)).bytes_b64); }
  cacheCreate(prefix, args = {}, options) { return this.operation("cache", "create", { ...args, prefix }, options); }
  cacheGet(id, options) { return this.operation("cache", "get", { id }, options); }
  cacheList(args = {}, options) { return this.operation("cache", "list", args, options); }
  cacheUpdate(id, ttl_seconds, options) { return this.operation("cache", "update", { id, ttl_seconds }, options); }
  cacheDelete(id, options) { return this.operation("cache", "delete", { id }, options); }
  async batchSubmit(request, options) {
    const upload_body = await this.operation("batch", "upload", { request }, options);
    return this.operation("batch", "submit", { request, upload_body }, options);
  }
  batchStatus(id, options) { return this.operation("batch", "status", { id }, options); }
  batchCancel(id, options) { return this.operation("batch", "cancel", { id }, options); }
  batchList(args = {}, options) { return this.operation("batch", "list", args, options); }
  async batchResults(id, options) {
    const job = await this.batchStatus(id, options);
    if (!["completed", "failed", "cancelled", "expired"].includes(job.status)) throw new LM15Error({ code: "invalid_request", message: "Batch is not finished; poll its status first." });
    return this.operation("batch", "result_fetches", { id, status_body: job.provider_data }, options);
  }
  videoGenerate(request, options) { return this.operation("video", "submit", { request }, options); }
  videoStatus(id, options) { return this.operation("video", "status", { id }, options); }
  videoList(args = {}, options) { return this.operation("video", "list", args, options); }
  async videoResult(id, options) {
    const job = await this.videoStatus(id, options);
    if (job.status !== "completed") throw new LM15Error({ code: "invalid_request", message: "Video is not completed; poll its status first." });
    return this.operation("video", "result_fetch", { id, status_body: job.provider_data }, options);
  }
  completeJSON(requestJSON, options = {}) {
    if (typeof requestJSON !== "string") throw new TypeError("Expected canonical JSON text");
    return this.complete(requestJSON, { ...options, jsonOnly: true });
  }
  streamJSON(requestJSON, options = {}) {
    if (typeof requestJSON !== "string") throw new TypeError("Expected canonical JSON text");
    return this.stream(requestJSON, { ...options, jsonOnly: true });
  }
  async complete(request, { signal, jsonOnly = false } = {}) {
    const id = crypto.randomUUID();
    try {
      const wire = await this.#prepare(id, request, false, jsonOnly);
      const response = await this.#request(wire, signal);
      const body = await this.#readBody(response, signal);
      const parsed = await this.#dispatch("response", id, { status: response.status,
        headers: Object.fromEntries(response.headers), body_b64: base64(body) });
      return parsed.response;
    } finally { await this.#dispatch("dispose", id).catch(() => {}); }
  }
  /** Yield canonical events. Early iterator exit aborts the fetch and frees
   * the R state without draining the response or pretending it completed.
   */
  async *stream(request, { signal, onResponse, jsonOnly = false } = {}) {
    const id = crypto.randomUUID();
    const controller = new AbortController();
    const abort = () => controller.abort(signal?.reason);
    signal?.addEventListener("abort", abort, { once: true });
    if (signal?.aborted) abort();
    let reader;
    try {
      const wire = await this.#prepare(id, request, true, jsonOnly);
      const response = await this.#request(wire, controller.signal);
      if (!response.ok) {
        await this.#dispatch("response", id, { status: response.status,
          headers: Object.fromEntries(response.headers), body_b64: base64(await this.#readBody(response, controller.signal)) });
        return;
      }
      if (!response.body) throw new LM15Error({ code: "transport", message: "Browser response has no readable body." });
      reader = response.body.getReader();
      let size = 0;
      for (;;) {
        let chunk;
        try { chunk = await reader.read(); }
        catch {
          if (controller.signal.aborted) throw controller.signal.reason ?? new DOMException("Aborted", "AbortError");
          throw new LM15Error({ code: "transport", message: "Browser stream ended during transfer." });
        }
        if (chunk.done) break;
        size += chunk.value.byteLength;
        if (size > this.#maxResponseBytes) throw new LM15Error({ code: "transport", message: "Stream exceeds maxResponseBytes." });
        let reply;
        try { reply = await this.#dispatch("feed", id, { body_b64: base64(chunk.value) }); }
        catch (error) { for (const event of error.events ?? []) yield event; throw error; }
        for (const event of reply.events) yield event;
      }
      let final;
      try { final = await this.#dispatch("finish", id); }
      catch (error) { for (const event of error.events ?? []) yield event; throw error; }
      for (const event of final.events) yield event;
      if (onResponse) await onResponse(final.response);
    } finally {
      signal?.removeEventListener("abort", abort);
      controller.abort();
      if (reader) { await reader.cancel().catch(() => {}); reader.releaseLock(); }
      await this.#dispatch("dispose", id).catch(() => {});
    }
  }
}
