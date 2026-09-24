import { createServer } from "node:https";
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { WebSocketServer } from "ws";
import { spawn } from "node:child_process";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import assert from "node:assert/strict";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const cert = resolve(root, "tests/fixtures/tls/localhost-cert.pem");
const key = resolve(root, "tests/fixtures/tls/test-only-key.pem");
let upgrades = 0, pongs = 0, sentSecrets = [];
const server = createServer({ cert: await readFile(cert), key: await readFile(key) });
const sockets = new WebSocketServer({ noServer: true });
server.on("tlsClientError", () => {});
server.on("upgrade", (request, socket, head) => {
  upgrades++; sentSecrets.push(request.headers.authorization);
  if (request.url === "/denied") {
    socket.end("HTTP/1.1 401 Unauthorized\r\nContent-Length: 16\r\nConnection: close\r\n\r\nTEST-ONLY-SECRET"); return;
  }
  sockets.handleUpgrade(request, socket, head, connection => sockets.emit("connection", connection));
});
sockets.on("connection", socket => {
  socket.on("error", () => {});
  socket.on("pong", () => { pongs++; });
  socket.send('{"text":', { fin: false }); socket.ping("test-control");
  socket.send('"hello"}', { fin: true });
  socket.on("message", bytes => socket.send(bytes.toString()));
});
await new Promise(done => server.listen(0, "127.0.0.1", done));
const port = server.address().port;
const code = `
pkgload::load_all(${JSON.stringify(root)}, quiet = TRUE)
url <- "wss://localhost:${port}/"
headers <- list(authorization = "TEST-ONLY-SECRET")
refused <- tryCatch(websocket_connect(url, headers, timeout=3), error=identity)
stopifnot(inherits(refused, "TransportError"))
stopifnot(!grepl("TEST-ONLY-SECRET", conditionMessage(refused), fixed=TRUE))
wrong_host <- tryCatch(websocket_connect("wss://127.0.0.1:${port}/", headers, timeout=3, ca_bundle=${JSON.stringify(cert)}), error=identity)
stopifnot(inherits(wrong_host, "TransportError"))
socket <- websocket_connect(url, headers, timeout=3, ca_bundle=${JSON.stringify(cert)})
tryCatch({
  stopifnot(rawToChar(socket$receive(3)) == '{"text":"hello"}')
  text <- strrep("large-frame-", 20000)
  socket$send(text)
  stopifnot(rawToChar(socket$receive(3)) == text)
}, finally=socket$close())
socket$close()
denied <- tryCatch(websocket_connect(paste0(url, "denied"), headers, timeout=3, ca_bundle=${JSON.stringify(cert)}), error=identity)
stopifnot(inherits(denied, "AuthError"), denied$status == 401L)
small <- websocket_connect(url, headers, timeout=3, max_frame_bytes=4L, ca_bundle=${JSON.stringify(cert)})
tryCatch({
  refused <- tryCatch(small$receive(3), error=identity)
  stopifnot(inherits(refused, "TransportError"))
}, finally=small$close())
cat("VERIFIED_TLS_OK\\n")
`;
try {
  const result = await new Promise((resolve, reject) => {
    const child = spawn("Rscript", ["--vanilla", "-e", code], { cwd: root, stdio: ["ignore", "pipe", "pipe"] });
    let stdout = "", stderr = "";
    child.stdout.on("data", data => { stdout += data; });
    child.stderr.on("data", data => { stderr += data; });
    child.on("error", reject);
    child.on("close", status => resolve({ status, stdout, stderr }));
  });
  if (result.status !== 0) throw Error(`Native WebSocket test failed:\n${result.stdout}\n${result.stderr}`);
  assert.match(result.stdout, /VERIFIED_TLS_OK/);
  assert.equal(upgrades, 3, "Untrusted certificates and wrong hostnames must fail before any HTTP credentials are sent");
  assert.deepEqual(sentSecrets, ["TEST-ONLY-SECRET", "TEST-ONLY-SECRET", "TEST-ONLY-SECRET"]);
  assert.ok(pongs >= 1, "Control frames must not corrupt fragmented data");
  assert.ok(!result.stdout.includes("TEST-ONLY-SECRET"));
  const report = { status: "pass", certificateRejection: true, hostnameRejection: true, trustedTLS: true, fragmentedMessages: true, largeMessages: true, pingPong: true, frameLimit: true, failedHandshakeBodyHidden: true, credentialLeak: false };
  await mkdir(resolve(root, "test-results"), { recursive: true });
  await writeFile(resolve(root, "test-results/native-websocket.json"), JSON.stringify(report, null, 2) + "\n");
  console.log(report);
} finally {
  for (const socket of sockets.clients) socket.terminate();
  await new Promise(done => sockets.close(done));
  await new Promise(done => server.close(done));
}
