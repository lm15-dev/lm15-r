# 2026-11-01 — live traffic through lm15 for R 2.0.0

```bash
set -a; . ../.env; set +a
Rscript tools/live-smoke.R --managed --out receipts/2026-11-01-live-smoke
```

lm15-r 2.0.0 (contract pin `cfc3edc`), installed from this checkout. The
first paid traffic from R beyond one OpenAI call (2026-10-06): fourteen
API-key bindings and six connections saved in the shared lm15 store,
used through `new_router(auth = local_auth())`. 106 checks: **84 ok, 22
not run to the end, none of them an R defect** (below). Receipts were
shortened after the run (picture data replaced by its length and SHA-256,
model catalogs cut to five entries, a long Codex instruction echo
dropped); `check_secrecy.py --root` passes on the directory.

## What each check does

- **hello**: `complete()` and `stream()` of one request agree in text and
  finish reason; the text deltas concatenate to the streamed text; usage
  obeys MAP-19 (cache, reasoning and audio counts within their wholes,
  total = input + output, input and output known).
- **order**: a `json_schema` listing `reasoning` before `answer`; the body
  sent keeps that order, `parse_json()` reads the answer (51).
- **tools**: a function tool; `complete()` and `stream()` both return a
  whole call, the result goes back, the model answers with it.
- **image**: the camera-trap JPEG (`website/public/docs-media/badger.jpg`).
- **reasoning**: a reasoning setting; reasoning tokens within output.
- **cache**: a ~11,000-token system prompt twice. Anthropic: the cache was
  written then read, `input_tokens` equals Anthropic's new + cache read +
  cache write (MAP-19). Codex: counts equal OpenAI's own; this time the
  provider read no cache, so OpenAI's cache-read path was not exercised
  live. Both: `estimate_cost()` equals the price worked by hand.
- **models**: `list_models()`. **bad-key**: a wrong key is `AuthError` on
  every provider and the key never appears in the message.
- **judgments** (TypeSafe): two declared judgments with
  `probabilities = "required"` (each distribution sums to 1), and a
  bounding box on the photo with `jev-1.14.0`: top 0.38, bottom 0.70,
  left 0.85, right 0.99, the box Python recorded for the website.

## Results

| Binding | Model | Result |
|---|---|---|
| anthropic | claude-haiku-4-5 | 8 of 8 ok (hello, order, tools, image, reasoning, cache, models, bad-key) |
| gemini | gemini-2.5-flash | 7 of 7 ok |
| groq | openai/gpt-oss-20b | 6 of 6 ok |
| openrouter | openai/gpt-4.1-nano | 6 of 6 ok |
| zai | glm-5.3-flash | 5 of 5 ok; `order`: the schema dropped and recorded (Z.AI accepts `json_schema` and does not apply it) |
| meta | muse-spark-1.3 | 5 of 5 ok |
| moonshotai | kimi-k3 | 5 of 5 ok |
| parasail | Llama-3.3-70B-Instruct | 5 of 5 ok |
| typesafe | jev-latest, jev-1.14.0 | 3 of 3 ok |
| deepseek | deepseek-v4-flash | 4 of 5; `order`: DeepSeek answers 400 "This response_format type is unavailable now", the same for lm15-python 2.0.0 today and for Go on 2026-09-26 |
| openai | gpt-5-mini | models and bad-key ok; every model call 429 "You have no credits remaining" (the account) |
| deepinfra, together | | models and bad-key ok; model calls 402, no balance (the accounts) |
| fireworks | | bad-key ok; everything else refused, account suspended |
| claude-code (account) | claude-haiku-4-5 | 4 of 4 ok |
| openai-codex (account) | gpt-5.6-sol | 7 of 7 ok (hello, order, tools, image, reasoning, cache, models); gpt-5.5, the model of earlier runs, is no longer offered to ChatGPT accounts |
| openrouter (account) | openai/gpt-4.1-nano | 4 of 4 ok |
| github-copilot (account) | gpt-4.1 | 4 of 4 ok |
| meta (account) | muse-spark-1.3 | 4 of 4 ok |
| xai (account) | grok-4.7 | not run: renewal refused, see below |

## The xAI connection

The saved xAI connection's access token had expired; R renewed it, and
`auth.x.ai` answered 400 to the refresh token. Following the contract
(`spec/auth-managed.md`, `credential_rejected`), R dropped the credential
and reports "sign in again"; the connection is gone from the shared
store. R's renewal request is the same as lm15-python's
(`lm15/login/flows/xai.py`: same token URL, client id and form fields),
and Go renewed this connection on 2026-09-26, the last recorded use. A
refresh token unused for five weeks is the likely cause; it is not
proven. A fresh xAI sign-in, from any language, restores it.

## Not covered

Paid OpenAI API-key traffic (no credits; the Responses wire was covered
through Codex), DeepInfra, Together AI and Fireworks AI model calls, xAI,
Google Cloud, Azure and AWS (no cloud credentials in this run), live
WebSocket sessions, files, batches and media generation (provisional).
