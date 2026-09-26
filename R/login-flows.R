# Provider login flows and the descriptor table (AUTH-13, AUTH-18, AUTH-20).
#
# Account flows are written per provider (their protocols differ in ways a
# generic OAuth machine would hide badly); api_key, env, external, cloud and
# local recipes are generated from the provider registry so every route the
# router knows can be connected the same way. Client ids, URLs and scopes
# are the values recorded once in lm15-contract auth/managed/profiles.json.
# Nothing here reads a credential, the network or the environment until a
# flow runs.

.named_credentials <- c("platform", "workload", "environment", "cli")

# ---- values ---------------------------------------------------------------
.field <- function(id, label, type = "text", required = TRUE, options = list(), help = NULL)
  list(id = id, label = label, type = type, required = required, options = options, help = help)
.method <- function(id, label, kind, flow, availability = "supported", reason = NULL, fields = list(), delivery = character(),
                    subscription = FALSE, billing_note = NULL, guidance = NULL)
  structure(list(id = id, label = label, kind = kind, flow = flow, availability = availability, reason = reason, fields = fields,
    delivery = delivery, subscription = subscription, billing_note = billing_note, guidance = guidance), class = "lm15_login_method")
print.lm15_login_method <- function(x, ...) { cat("<lm15 login method ", x$id, ": ", x$label, " (", x$availability, ")>\n", sep = ""); invisible(x) }
.descriptor <- function(id, label, service, routes, methods, console_url = NULL, docs_url = NULL)
  structure(list(id = id, label = label, service = service, routes = routes, methods = methods, console_url = console_url, docs_url = docs_url), class = "lm15_provider_descriptor")
print.lm15_provider_descriptor <- function(x, ...) {
  cat("<lm15 provider ", x$id, ": ", length(x$methods), " login method", if (length(x$methods) != 1L) "s", ">\n", sep = "")
  invisible(x)
}
.login_result <- function(material, label, renewal = "refresh_token", account_label = NULL, settings = list())
  list(material = material, label = label, renewal = renewal, account_label = account_label, settings = settings)
.request_auth_value <- function(credential = NULL, headers = list(), base_url = NULL, account_id = NULL, named = NULL)
  structure(list(credential = credential, headers = headers, base_url = base_url, account_id = account_id, named = named), class = "lm15_request_auth")
print.lm15_request_auth <- function(x, ...) { cat("<lm15 request authentication; credential redacted>\n"); invisible(x) }
str.lm15_request_auth <- function(object, ...) { print.lm15_request_auth(object); invisible(NULL) }

.ms_token <- function(ms) .json_number(sprintf("%.0f", ms))
.num <- function(x) { if (is.null(x) || is.logical(x)) return(NULL); v <- suppressWarnings(as.numeric(unclass(x))); if (length(v) == 1L && is.finite(v)) v else NULL }
.nonempty <- function(x) .nonempty_string(.json_str(x))
.oauth_material <- function(access, refresh, expires_in_s, now_ms, extra = list()) {
  m <- json_object(type = "oauth", access = access)
  if (!is.null(refresh) && nzchar(refresh)) m$refresh <- refresh
  m$issued_at <- .ms_token(now_ms)
  if (!is.null(expires_in_s) && expires_in_s > 0) { m$lifetime_s <- as.double(expires_in_s); m$expires <- .ms_token(now_ms + expires_in_s * 1000) }
  for (k in names(extra)) m[[k]] <- extra[[k]]
  m
}
.material_expiry_ms <- function(material) .num(material$expires)
.now_ms <- function(ctx) round(ctx$wall() * 1000)
.https_url <- function(value, what) {
  parts <- if (!is.null(.json_str(value))) .url_parts(value) else NULL
  if (is.null(parts) || parts$scheme != "https" || !nzchar(parts$host)) .deny(paste(what, "returned an untrusted verification URL"))
  value
}
.http_url <- function(value) {
  parts <- if (!is.null(.json_str(value)) && nzchar(value)) .url_parts(value) else NULL
  if (!is.null(parts) && parts$scheme %in% c("https", "http") && nzchar(parts$host)) value else NULL
}
.random_urlsafe <- function(n) { .crypto(); .base64url(openssl::rand_bytes(n)) }
.random_hex <- function(n) { .crypto(); paste(as.character(openssl::rand_bytes(n)), collapse = "") }

.default_expiry <- function(material) if (identical(material$type, "api_key")) "never" else .material_expiry_ms(material)
.default_lifetime <- function(material) {
  life <- .num(material$lifetime_s)
  if (!is.null(life) && life > 0) return(life)
  issued <- .num(material$issued_at); expires <- .material_expiry_ms(material)
  if (!is.null(issued) && !is.null(expires)) return(max((expires - issued) / 1000, 0))
  NULL
}
.flow <- function(login, renew, request_auth, expiry = .default_expiry, lifetime = .default_lifetime)
  list(login = login, renew = renew, request_auth = request_auth, expiry = expiry, lifetime = lifetime)
.device_step <- function(status, value = NULL, interval = NULL) list(status = status, value = value, interval = interval)

# ---- xAI (RFC 8628 device code) -------------------------------------------
.xai_client <- "b1a00492-073a-47ea-816f-4c329264a828"
.xai_token_url <- "https://auth.x.ai/oauth2/token"
.xai_tokens <- function(body, now_ms, previous_refresh = NULL) {
  access <- .nonempty(body$access_token)
  if (is.null(access)) .deny("xAI token response carried no access token")
  refresh <- .nonempty(body$refresh_token) %||% previous_refresh  # xAI may omit it when it does not rotate
  .oauth_material(access, refresh, .positive(body$expires_in) %||% 3600, now_ms)
}
.xai_flow <- .flow(
  login = function(ctx, method, settings, answers) {
    reply <- .http_form(ctx, "https://auth.x.ai/oauth2/device/code", list(client_id = .xai_client, scope = "openid profile email offline_access grok-cli:access api:access", referrer = "lm15"))
    if (!reply$ok) .deny(paste0("xAI refused to start a device authorization (HTTP ", reply$status, ")"))
    b <- reply$body; device_code <- .nonempty(b$device_code); user_code <- .nonempty(b$user_code)
    if (is.null(device_code) || is.null(user_code)) .deny("xAI device authorization response is missing required fields")
    verification <- .https_url(b$verification_uri, "xAI")
    target <- if (!is.null(.nonempty(b$verification_uri_complete))) .https_url(b$verification_uri_complete, "xAI") else verification
    interval <- .positive(b$interval); expires_in <- .positive(b$expires_in)
    ctx$notify(.notice("device_code", user_code = user_code, verification_url = target, expires_in_s = expires_in %||% 900, interval_s = interval %||% 5))
    poll <- function() {
      r <- .http_form(ctx, .xai_token_url, list(grant_type = "urn:ietf:params:oauth:grant-type:device_code", client_id = .xai_client, device_code = device_code))
      if (r$ok) return(.device_step("complete", .xai_tokens(r$body, .now_ms(ctx))))
      error <- .json_str(r$body$error) %||% ""
      if (error == "authorization_pending") return(.device_step("pending"))
      if (error == "slow_down") return(.device_step("slow_down", interval = .positive(r$body$interval)))
      if (error %in% c("access_denied", "authorization_denied")) return(.device_step("denied"))
      if (error == "expired_token") return(.device_step("expired"))
      .deny(paste0("xAI device token polling failed (HTTP ", r$status, ")"))
    }
    .login_result(.run_device_flow(ctx, poll, interval, expires_in), "xAI subscription")
  },
  renew = function(ctx, material, settings) {
    refresh <- .nonempty(material$refresh)
    if (is.null(refresh)) .deny("xAI credential has no refresh token")
    r <- .http_form(ctx, .xai_token_url, list(grant_type = "refresh_token", client_id = .xai_client, refresh_token = refresh))
    if (!r$ok) .deny(if (r$status %in% c(400L, 401L, 403L)) paste0("xAI rejected the refresh token (HTTP ", r$status, ")") else paste0("xAI refresh failed (HTTP ", r$status, ")"), status = r$status, provider_code = r$oauth_error, stage = "renewal")
    .login_result(.xai_tokens(r$body, .now_ms(ctx), refresh), "xAI subscription")
  },
  request_auth = function(material, settings) .request_auth_value(bearer_token(material$access)))

# ---- Claude (authorization code, hosted return page or loopback) ----------
.claude_client <- "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
.claude_token_url <- "https://platform.claude.com/v1/oauth/token"
.claude_scope <- "org:create_api_key user:profile user:inference user:sessions:claude_code user:mcp_servers user:file_upload"
.claude_tokens <- function(body, now_ms) {
  access <- .nonempty(body$access_token); refresh <- .nonempty(body$refresh_token)
  if (is.null(access) || is.null(refresh)) .deny("Claude token response is missing required fields")
  .oauth_material(access, refresh, .positive(body$expires_in), now_ms)
}
.claude_flow <- .flow(
  login = function(ctx, method, settings, answers) {
    hosted <- method$id == "browser"
    ctx$check()
    verifier <- .random_urlsafe(32L)
    challenge <- pkce_challenge(verifier)
    state <- .random_urlsafe(32L)
    listener <- NULL
    redirect_uri <- if (hosted) "https://platform.claude.com/oauth/code/callback" else "http://localhost:53692/callback"
    if (!hosted) {
      listener <- .loopback_listener("/callback", state, port = 53692L, redirect_host = "localhost")
      if (is.null(listener)) ctx$notify(.notice("info", message = "Could not listen on port 53692; paste the full redirect URL when the browser finishes."))
    }
    on.exit(if (!is.null(listener)) listener$close(), add = TRUE)
    query <- list(code = "true", client_id = .claude_client, response_type = "code", redirect_uri = redirect_uri, scope = .claude_scope,
      code_challenge = challenge, code_challenge_method = "S256", state = state)
    url <- paste0(if (hosted) "https://claude.com/cai/oauth/authorize" else "https://claude.ai/oauth/authorize", "?", .form_encode(query))
    ctx$notify(.notice("auth_url", url = url, instructions = if (hosted) "Sign in to Claude in your browser. On the Authentication code page, copy the whole displayed code (including #state) and paste it here. The full return URL also works. Your browser may be on another machine; no localhost connection is needed." else "Sign in to Claude in your browser. If the local callback cannot be reached, interrupt and paste the full redirect URL (or code#state)."))
    prompt <- .prompt("manual_code", "return", "Paste the full code#state or return URL here", accepted = "the full return URL, or code#state (a bare code without state is not accepted)")
    returned <- .await_return(ctx, listener, prompt, function(pasted) .parse_manual_return(pasted, state, FALSE, .url_parts(redirect_uri)$path, redirect_uri))
    ctx$check()
    ctx$notify(.notice("progress", stage = "exchange", message = "Exchanging the authorization code..."))
    r <- .http_json(ctx, .claude_token_url, json_object(grant_type = "authorization_code", code = returned$code, redirect_uri = redirect_uri, client_id = .claude_client, code_verifier = verifier, state = state))
    if (!r$ok) .deny(paste0("Claude authorization-code exchange failed: ", .failure_summary(r), ". The authorization code will not be retried automatically."), status = r$status, provider_code = r$oauth_error, stage = "exchange")
    ctx$check()
    .login_result(.claude_tokens(r$body, .now_ms(ctx)), "Claude subscription")
  },
  renew = function(ctx, material, settings) {
    refresh <- .nonempty(material$refresh)
    if (is.null(refresh)) .deny("Claude credential has no refresh token")
    r <- .http_json(ctx, .claude_token_url, json_object(grant_type = "refresh_token", client_id = .claude_client, refresh_token = refresh))
    if (!r$ok) .deny(paste0("Claude token renewal failed: ", .failure_summary(r)), status = r$status, provider_code = r$oauth_error, stage = "renewal")
    .login_result(.claude_tokens(r$body, .now_ms(ctx)), "Claude subscription")
  },
  request_auth = function(material, settings) .request_auth_value(bearer_token(material$access)))

# ---- ChatGPT / Codex (browser with loopback, or device) --------------------
.codex_client <- "app_EMoamEEZ73f0CkXaXp7hrann"
.codex_auth <- "https://auth.openai.com"
.codex_tokens <- function(body, now_ms) {
  access <- .nonempty(body$access_token); refresh <- .nonempty(body$refresh_token)
  if (is.null(access) || is.null(refresh)) .deny("ChatGPT token response is missing required fields")
  lifetime <- .positive(body$expires_in)
  if (is.null(lifetime)) {
    exp <- .num(.jwt_claims(access)$exp)
    if (!is.null(exp)) { lifetime <- max((exp * 1000 + 5 * 60 * 1000 - now_ms) / 1000, 0); if (lifetime == 0) lifetime <- NULL }
  }
  account <- .nonempty(.jwt_claims(access)[["https://api.openai.com/auth"]]$chatgpt_account_id)
  if (is.null(account)) .deny("ChatGPT token carries no account id")
  extra <- list(accountId = account)
  if (!is.null(.nonempty(body$id_token))) extra$id_token <- body$id_token
  .oauth_material(access, refresh, lifetime, now_ms, extra)
}
.codex_exchange <- function(ctx, code, verifier, redirect_uri) {
  ctx$notify(.notice("progress", stage = "exchange", message = "Exchanging the authorization code..."))
  r <- .http_form(ctx, paste0(.codex_auth, "/oauth/token"), list(grant_type = "authorization_code", client_id = .codex_client, code = code, code_verifier = verifier, redirect_uri = redirect_uri))
  if (!r$ok) .deny(paste0("ChatGPT rejected the authorization code (HTTP ", r$status, ")"), status = r$status, provider_code = r$oauth_error, stage = "exchange")
  material <- .codex_tokens(r$body, .now_ms(ctx))
  .login_result(material, "ChatGPT subscription", account_label = material$accountId)
}
.codex_flow <- .flow(
  login = function(ctx, method, settings, answers) {
    if (method$id == "device") {
      r <- .http_json(ctx, paste0(.codex_auth, "/api/accounts/deviceauth/usercode"), json_object(client_id = .codex_client))
      if (!r$ok) .deny(if (r$status == 404L) "ChatGPT device-code login is not enabled for this server; use the browser method" else paste0("ChatGPT refused to start a device authorization (HTTP ", r$status, ")"))
      b <- r$body; device_id <- .nonempty(b$device_auth_id); user_code <- .nonempty(b$user_code)
      interval <- b$interval
      if (!is.null(.json_str(interval))) interval <- suppressWarnings(as.numeric(trimws(interval)))
      if (is.null(device_id) || is.null(user_code)) .deny("ChatGPT device authorization response is missing required fields")
      interval_s <- { v <- .num(interval); if (!is.null(v) && v >= 0) v else NULL }
      ctx$notify(.notice("device_code", user_code = user_code, verification_url = paste0(.codex_auth, "/codex/device"), expires_in_s = 15 * 60, interval_s = interval_s %||% 5))
      poll <- function() {
        p <- .http_json(ctx, paste0(.codex_auth, "/api/accounts/deviceauth/token"), json_object(device_auth_id = device_id, user_code = user_code))
        if (p$ok) {
          code <- .nonempty(p$body$authorization_code); verifier <- .nonempty(p$body$code_verifier)
          if (is.null(code) || is.null(verifier)) .deny("ChatGPT device token response is missing required fields")
          return(.device_step("complete", list(code = code, verifier = verifier)))
        }
        if (p$status %in% c(403L, 404L)) return(.device_step("pending"))
        error <- p$body$error; code <- if (.is_object(error)) .json_str(error$code) else .json_str(error)
        if (identical(code, "deviceauth_authorization_pending")) return(.device_step("pending"))
        if (identical(code, "slow_down")) return(.device_step("slow_down"))
        .deny(paste0("ChatGPT device authorization failed (HTTP ", p$status, ")"))
      }
      got <- .run_device_flow(ctx, poll, interval_s, 15 * 60)
      return(.codex_exchange(ctx, got$code, got$verifier, paste0(.codex_auth, "/deviceauth/callback")))
    }
    pkce <- generate_pkce(); state <- .random_hex(16L)
    listener <- .loopback_listener("/auth/callback", state, port = 1455L, redirect_host = "localhost")
    if (is.null(listener)) ctx$notify(.notice("info", message = "Could not listen on port 1455; paste the redirect URL when the browser finishes."))
    on.exit(if (!is.null(listener)) listener$close(), add = TRUE)
    redirect <- "http://localhost:1455/auth/callback"
    query <- list(response_type = "code", client_id = .codex_client, redirect_uri = redirect, scope = "openid profile email offline_access",
      code_challenge = pkce$challenge, code_challenge_method = "S256", state = state, id_token_add_organizations = "true", codex_cli_simplified_flow = "true", originator = "lm15")
    ctx$notify(.notice("auth_url", url = paste0(.codex_auth, "/oauth/authorize?", .form_encode(query)), instructions = "Sign in to ChatGPT in your browser. If the browser is on another machine, interrupt and paste the final redirect URL back here."))
    prompt <- .prompt("manual_code", "return", "Paste the redirect URL here", accepted = "the full redirect URL, or code#state")
    returned <- .await_return(ctx, listener, prompt, function(pasted) .parse_manual_return(pasted, state, FALSE, "/auth/callback"))
    .codex_exchange(ctx, returned$code, pkce$verifier, redirect)
  },
  renew = function(ctx, material, settings) {
    refresh <- .nonempty(material$refresh)
    if (is.null(refresh)) .deny("ChatGPT credential has no refresh token")
    r <- .http_form(ctx, paste0(.codex_auth, "/oauth/token"), list(grant_type = "refresh_token", refresh_token = refresh, client_id = .codex_client))
    if (!r$ok) .deny(paste0("ChatGPT rejected the refresh token (HTTP ", r$status, ")"), status = r$status, provider_code = r$oauth_error, stage = "renewal")
    body <- r$body
    if (is.null(.nonempty(body$refresh_token))) body$refresh_token <- refresh  # OpenAI may omit it when it does not rotate
    material <- .codex_tokens(body, .now_ms(ctx))
    .login_result(material, "ChatGPT subscription", account_label = material$accountId)
  },
  request_auth = function(material, settings) {
    account <- .nonempty(material$accountId) %||% .nonempty(.jwt_claims(material$access)[["https://api.openai.com/auth"]]$chatgpt_account_id)
    .request_auth_value(bearer_token(material$access), headers = if (!is.null(account)) list("chatgpt-account-id" = account) else list(), account_id = account)
  })

# ---- GitHub Copilot (device code, then a Copilot token) -------------------
.copilot_client <- "Iv1.b507a08c87ecfe98"
.copilot_headers <- list("User-Agent" = "GitHubCopilotChat/0.35.0", "Editor-Version" = "vscode/1.107.0", "Editor-Plugin-Version" = "copilot-chat/0.35.0", "Copilot-Integration-Id" = "vscode-chat")
.copilot_domain <- function(settings) {
  raw <- trimws(settings$enterprise_domain %||% "")
  if (!nzchar(raw)) return("github.com")
  parts <- .url_parts(if (grepl("://", raw, fixed = TRUE)) raw else paste0("https://", raw))
  host <- parts$host %||% ""
  if (!nzchar(host) || !grepl("^[a-z0-9.-]+$", host)) .deny("invalid GitHub Enterprise domain")
  host
}
.copilot_base_url <- function(material, settings) {
  token <- .json_str(material$access) %||% ""
  domain <- .copilot_domain(settings)
  m <- regmatches(token, regexec("proxy-ep=([^;]+)", token))[[1L]]
  if (length(m) == 2L) {
    api_host <- sub("^proxy\\.", "api.", tolower(trimws(m[[2L]])))
    allowed <- if (domain == "github.com") ".githubcopilot.com" else c(paste0(".", domain), ".githubcopilot.com")
    if (grepl("^[a-z0-9.-]+$", api_host) && any(vapply(allowed, function(s) endsWith(api_host, s), logical(1)))) return(paste0("https://", api_host))
  }
  if (domain != "github.com") return(paste0("https://copilot-api.", domain))
  "https://api.individual.githubcopilot.com"
}
.copilot_exchange <- function(ctx, github_token, settings) {
  r <- .http_get(ctx, paste0("https://api.", .copilot_domain(settings), "/copilot_internal/v2/token"), headers = c(list(Authorization = paste("Bearer", github_token)), .copilot_headers))
  if (r$status %in% c(401L, 403L)) .deny("GitHub rejected the token for Copilot; sign in again", status = r$status, stage = "exchange")
  if (!r$ok) .deny(paste0("Copilot token exchange failed (HTTP ", r$status, ")"), status = r$status, stage = "exchange")
  token <- .nonempty(r$body$token); expires_at <- .num(r$body$expires_at)
  if (is.null(token) || is.null(expires_at)) .deny("Copilot token response is missing required fields")
  now_ms <- .now_ms(ctx); expires_ms <- round(expires_at * 1000)
  json_object(type = "oauth", access = token, refresh = github_token, issued_at = .ms_token(now_ms), lifetime_s = max((expires_ms - now_ms) / 1000, 1), expires = .ms_token(expires_ms))
}
.copilot_flow <- .flow(
  login = function(ctx, method, settings, answers) {
    merged <- settings
    if (nzchar(answers$enterprise_domain %||% "")) merged$enterprise_domain <- answers$enterprise_domain
    domain <- .copilot_domain(merged)
    ua <- list("User-Agent" = .copilot_headers[["User-Agent"]])
    r <- .http_form(ctx, paste0("https://", domain, "/login/device/code"), list(client_id = .copilot_client, scope = "read:user"), headers = ua)
    if (!r$ok) .deny(paste0("GitHub refused to start a device authorization (HTTP ", r$status, ")"))
    b <- r$body; device_code <- .json_str(b$device_code); user_code <- .json_str(b$user_code); verification <- .json_str(b$verification_uri)
    if (is.null(device_code) || is.null(user_code) || is.null(verification)) .deny("GitHub device authorization response is missing required fields")
    vparts <- .url_parts(verification)
    if (is.null(vparts) || !vparts$scheme %in% c("https", "http") || !nzchar(vparts$host)) .deny("GitHub returned an untrusted verification URL")
    interval <- .positive(b$interval); expires <- .positive(b$expires_in)
    ctx$notify(.notice("device_code", user_code = user_code, verification_url = verification, expires_in_s = expires %||% 900, interval_s = interval %||% 5))
    poll <- function() {
      p <- .http_form(ctx, paste0("https://", domain, "/login/oauth/access_token"), list(client_id = .copilot_client, device_code = device_code, grant_type = "urn:ietf:params:oauth:grant-type:device_code"), headers = ua)
      token <- .nonempty(p$body$access_token)
      if (!is.null(token)) return(.device_step("complete", token))
      error <- .json_str(p$body$error) %||% ""
      if (error == "authorization_pending") return(.device_step("pending"))
      if (error == "slow_down") return(.device_step("slow_down", interval = .positive(p$body$interval)))
      if (error == "expired_token") return(.device_step("expired"))
      if (error == "access_denied") return(.device_step("denied"))
      .deny(paste0("GitHub device authorization failed (HTTP ", p$status, ")"))
    }
    github_token <- .run_device_flow(ctx, poll, interval, expires)
    ctx$notify(.notice("progress", stage = "exchange", message = "Exchanging the GitHub token for a Copilot token..."))
    material <- .copilot_exchange(ctx, github_token, merged)
    .login_result(material, if (domain == "github.com") "GitHub Copilot" else paste0("GitHub Copilot (", domain, ")"), renewal = "remint",
      settings = if (domain != "github.com") list(enterprise_domain = domain) else list())
  },
  renew = function(ctx, material, settings) {
    github_token <- .nonempty(material$refresh)
    if (is.null(github_token)) .deny("Copilot credential has no GitHub token to renew with")
    .login_result(.copilot_exchange(ctx, github_token, settings), "GitHub Copilot", renewal = "remint")
  },
  request_auth = function(material, settings) .request_auth_value(bearer_token(material$access), headers = .copilot_headers, base_url = .copilot_base_url(material, settings)))

# ---- Kimi Code (device code) ----------------------------------------------
.kimi_client <- "17e5f671-d194-4dfb-9706-5516cb48c098"
.kimi_host <- function(settings) sub("/+$", "", settings$oauth_host %||% "https://auth.kimi.com")
.kimi_tokens <- function(body, now_ms) {
  access <- .nonempty(body$access_token); refresh <- .nonempty(body$refresh_token)
  if (is.null(access) || is.null(refresh)) .deny("Kimi Code token response is missing required fields")
  .oauth_material(access, refresh, .positive(body$expires_in), now_ms)
}
.kimi_flow <- .flow(
  login = function(ctx, method, settings, answers) {
    host <- .kimi_host(settings)
    r <- .http_form(ctx, paste0(host, "/api/oauth/device_authorization"), list(client_id = .kimi_client))
    if (!r$ok) .deny(paste0("Kimi Code refused to start a device authorization (HTTP ", r$status, ")"))
    b <- r$body; device_code <- .nonempty(b$device_code); user_code <- .nonempty(b$user_code)
    verification <- .http_url(b$verification_uri_complete) %||% .http_url(b$verification_uri)
    if (is.null(device_code) || is.null(user_code) || is.null(verification)) .deny("Kimi Code device authorization response is missing required fields")
    interval <- .positive(b$interval); expires <- .positive(b$expires_in) %||% (15 * 60)
    ctx$notify(.notice("device_code", user_code = user_code, verification_url = verification, expires_in_s = expires, interval_s = interval %||% 5))
    poll <- function() {
      p <- .http_form(ctx, paste0(host, "/api/oauth/token"), list(client_id = .kimi_client, device_code = device_code, grant_type = "urn:ietf:params:oauth:grant-type:device_code"))
      if (p$ok && !is.null(.json_str(p$body$access_token))) return(.device_step("complete", .kimi_tokens(p$body, .now_ms(ctx))))
      error <- .json_str(p$body$error) %||% ""
      if (error == "authorization_pending") return(.device_step("pending"))
      if (error == "slow_down") return(.device_step("slow_down", interval = .positive(p$body$interval)))
      if (error == "expired_token") return(.device_step("expired"))
      if (error == "access_denied") return(.device_step("denied"))
      .deny(paste0("Kimi Code device token request failed (HTTP ", p$status, ")"))
    }
    material <- .run_device_flow(ctx, poll, interval, expires)
    .login_result(material, "Kimi Code subscription", settings = if (host != "https://auth.kimi.com") list(oauth_host = host) else list())
  },
  renew = function(ctx, material, settings) {
    refresh <- .nonempty(material$refresh)
    if (is.null(refresh)) .deny("Kimi Code credential has no refresh token")
    r <- .http_form(ctx, paste0(.kimi_host(settings), "/api/oauth/token"), list(client_id = .kimi_client, grant_type = "refresh_token", refresh_token = refresh))
    if (r$status %in% c(401L, 403L) || identical(.json_str(r$body$error), "invalid_grant")) .deny(paste0("Kimi Code rejected the refresh token (HTTP ", r$status, ")"), status = r$status, provider_code = r$oauth_error, stage = "renewal")
    if (!r$ok) .abort(paste0("Kimi Code rate-limited the token refresh (HTTP ", r$status, ")"), "rate_limit", "kimi-code", status = r$status)
    .login_result(.kimi_tokens(r$body, .now_ms(ctx)), "Kimi Code subscription")
  },
  request_auth = function(material, settings) .request_auth_value(bearer_token(material$access)))

# ---- Meta (device code, then a minted Model API key) -----------------------
.meta_client <- "1031625952748946"
.meta_mint <- function(ctx, identity) {
  ctx$notify(.notice("progress", stage = "exchange", message = "Enabling Meta Model API access..."))
  r <- .http_json(ctx, "https://api.meta.ai/muse-code/key", json_object(), headers = list(Authorization = paste("Bearer", identity), "x-api-version" = "1.0.0"))
  if (r$status %in% c(401L, 403L)) .deny("Meta session is no longer valid; sign in again", status = r$status, stage = "exchange")
  if (!r$ok) .deny(paste0("Meta API key mint failed (HTTP ", r$status, ")"), status = r$status, stage = "exchange")
  key <- .nonempty(r$body$api_key)
  if (is.null(key)) { action <- .http_url(r$body$action_url); .deny(paste0("Meta did not issue an API key", if (!is.null(action)) paste0("; complete setup at ", action))) }
  now_ms <- .now_ms(ctx); life <- 24 * 60 * 60
  json_object(type = "oauth", access = key, refresh = identity, issued_at = .ms_token(now_ms), lifetime_s = as.double(life), expires = .ms_token(now_ms + life * 1000))
}
.meta_flow <- .flow(
  login = function(ctx, method, settings, answers) {
    r <- .http_form(ctx, "https://auth.meta.com/oidc/device/authorization/", list(client_id = .meta_client))
    if (!r$ok) .deny(paste0("Meta refused to start a device authorization (HTTP ", r$status, ")"))
    b <- r$body; device_code <- .nonempty(b$device_code); user_code <- .nonempty(b$user_code)
    verification <- .http_url(b$verification_uri_complete) %||% .http_url(b$verification_uri)
    if (is.null(device_code) || is.null(user_code) || is.null(verification)) .deny("Meta device authorization response is missing required fields")
    interval <- .positive(b$interval); expires <- .positive(b$expires_in)
    ctx$notify(.notice("device_code", user_code = user_code, verification_url = verification, expires_in_s = expires %||% 900, interval_s = interval %||% 5))
    poll <- function() {
      p <- .http_form(ctx, "https://auth.meta.com/oidc/device/token/", list(grant_type = "urn:ietf:params:oauth:grant-type:device_code", device_code = device_code, client_id = .meta_client))
      token <- .nonempty(p$body$access_token)
      if (p$ok && !is.null(token)) return(.device_step("complete", token))
      error <- .json_str(p$body$error) %||% ""
      if (error == "authorization_pending") return(.device_step("pending"))
      if (error == "slow_down") return(.device_step("slow_down", interval = .positive(p$body$interval)))
      if (error == "access_denied") return(.device_step("denied"))
      if (error == "expired_token") return(.device_step("expired"))
      .deny(paste0("Meta device token request failed (HTTP ", p$status, ")"))
    }
    identity <- .run_device_flow(ctx, poll, interval, expires)
    .login_result(.meta_mint(ctx, identity), "Meta (Muse subscription)", renewal = "remint")
  },
  renew = function(ctx, material, settings) {
    identity <- .nonempty(material$refresh)
    if (is.null(identity)) .deny("Meta credential has no identity token to re-mint with")
    .login_result(.meta_mint(ctx, identity), "Meta (Muse subscription)", renewal = "remint")
  },
  request_auth = function(material, settings) .request_auth_value(api_key(material$access)))

# ---- OpenRouter (authorization code minting a user-controlled key) ---------
.openrouter_flow <- .flow(
  login = function(ctx, method, settings, answers) {
    pkce <- generate_pkce()
    path <- paste0("/oauth/callback/", .random_urlsafe(24L))
    listener <- .loopback_listener(path, NULL, port = 0L)
    on.exit(if (!is.null(listener)) listener$close(), add = TRUE)
    callback <- listener$redirect_uri %||% paste0("http://127.0.0.1", path)
    query <- list(callback_url = callback, code_challenge = pkce$challenge, code_challenge_method = "S256")
    ctx$notify(.notice("auth_url", url = paste0("https://openrouter.ai/auth?", .form_encode(query)), instructions = "Sign in to OpenRouter in your browser and approve the key. If the browser is on another machine, interrupt and paste the final redirect URL back here."))
    prompt <- .prompt("manual_code", "return", "Paste the redirect URL or code here", accepted = "the full redirect URL, or the code")
    returned <- .await_return(ctx, listener, prompt, function(pasted) .parse_manual_return(pasted, NULL, TRUE, path))
    ctx$notify(.notice("progress", stage = "exchange", message = "Exchanging the code for an API key..."))
    r <- .http_json(ctx, "https://openrouter.ai/api/v1/auth/keys", json_object(code = returned$code, code_verifier = pkce$verifier, code_challenge_method = "S256"))
    if (!r$ok) .deny(paste0("OpenRouter rejected the authorization code (HTTP ", r$status, ")"), status = r$status, stage = "exchange")
    key <- .nonempty(r$body$key)
    if (is.null(key)) .deny("OpenRouter returned no key")
    .login_result(json_object(type = "api_key", key = key, minted = TRUE), "OpenRouter (minted key)", renewal = "none")
  },
  renew = function(ctx, material, settings) .login_result(material, "OpenRouter (minted key)", renewal = "none"),
  request_auth = function(material, settings) .request_auth_value(api_key(material$key)),
  expiry = function(material) "never")

# ---- recipes: connections that are recipes, not tokens --------------------
.external_sources <- list(
  "claude-code-cli" = c("claude-code", "your Claude Code login (~/.claude/.credentials.json)"),
  "codex-cli" = c("openai-codex", "your Codex CLI login (~/.codex/auth.json)"),
  "pi-xai" = c("xai", "your Pi agent xAI login (~/.pi/agent/auth.json)"))
.external_path <- function(source) switch(source, "pi-xai" = file.path(.auth_home(NULL), ".pi", "agent", "auth.json"), NULL)
.external_credential <- function(source) {
  route <- .external_sources[[source]][[1L]]
  load_local_credential(route, path = .external_path(source))
}
.recipe_flow <- function(provider) { force(provider); .flow(
  login = function(ctx, method, settings, answers) {
    id <- method$id
    if (id == "api_key") {
      key <- answers$key %||% ""
      if (!is.character(key) || !nzchar(trimws(key))) .deny("no API key was entered")
      return(.login_result(json_object(type = "api_key", key = trimws(key)), paste(provider, "API key"), renewal = "none"))
    }
    if (id == "env") {
      name <- answers$name %||% ""
      if (!nzchar(name)) .deny("no environment variable was chosen")
      return(.login_result(json_object(type = "env", name = name), paste0(provider, " key from $", name), renewal = "recipe"))
    }
    if (startsWith(id, "external:")) {
      source <- substring(id, 10L)
      if (is.null(.external_sources[[source]])) .deny("unknown external source")
      .external_credential(source)  # fail now, typed, if that tool has no login
      return(.login_result(json_object(type = "external", source = source), paste(provider, "via", .external_sources[[source]][[2L]]), renewal = "external"))
    }
    if (id == "cloud") {
      named <- answers$named %||% ""
      if (!named %in% .named_credentials) .deny(paste("choose one of", paste(.named_credentials, collapse = ", ")))
      return(.login_result(json_object(type = "cloud", named = named), paste(provider, "via", named, "identity"), renewal = "recipe"))
    }
    if (id == "local") {
      base_url <- answers$base_url %||% settings$base_url %||% ""
      return(.login_result(json_object(type = "local", base_url = base_url, key = answers$key %||% "local"), paste(provider, "local server"), renewal = "none",
        settings = if (nzchar(base_url)) list(base_url = base_url) else list()))
    }
    stop("unknown recipe method", call. = FALSE)
  },
  renew = function(ctx, material, settings) .login_result(material, provider, renewal = .json_str(material$type) %||% "none"),
  request_auth = function(material, settings) {
    kind <- .json_str(material$type) %||% ""
    if (kind == "api_key") return(.request_auth_value(api_key(material$key)))
    if (kind == "env") {
      value <- Sys.getenv(material$name, unset = "")
      if (!nzchar(value)) .deny(paste0("$", material$name, " is not set in this process's environment"))
      return(.request_auth_value(api_key(value)))
    }
    if (kind == "external") {
      source <- material$source
      info <- .external_credential(source)
      if (source == "codex-cli") return(.request_auth_value(info$credential, headers = if (!is.null(info$account_id)) list("chatgpt-account-id" = info$account_id) else list(), account_id = info$account_id))
      return(.request_auth_value(info$credential))
    }
    if (kind == "local") return(.request_auth_value(api_key(.nonempty(material$key) %||% "local"), base_url = .nonempty(material$base_url)))
    if (kind == "cloud") return(.request_auth_value(named = material$named))
    .deny("unknown connection material")
  },
  expiry = function(material) if ((.json_str(material$type) %||% "") %in% c("api_key", "env", "local", "cloud")) "never" else NULL) }

# ---- the table ------------------------------------------------------------
.account_flows <- list(
  xai = list(flow = .xai_flow, descriptor = .descriptor("xai", "xAI", "xAI", "xai", list(
    .method("device", "Sign in with SuperGrok or X Premium", "account", "device_code", delivery = "device", subscription = TRUE,
      billing_note = "Subscription access per xAI's own recommendation (2026-09-01); the API key path is metered.")), console_url = "https://console.x.ai")),
  "claude-code" = list(flow = .claude_flow, descriptor = .descriptor("claude-code", "Claude (subscription)", "Anthropic", "claude-code", list(
    .method("browser", "Sign in with Claude (paste code from hosted page)", "account", "authorization_code", "unverified",
      "LM15 hosted login, inference, persistence and early renewal observed 2026-09-23; permission and billing remain unverified", delivery = "manual", subscription = TRUE,
      billing_note = "Provider permission and included usage must be verified separately for your account."),
    .method("loopback", "Sign in with Claude (local browser callback)", "account", "authorization_code", "unverified", "no live LM15 receipt for this local callback flow",
      delivery = c("loopback", "manual"), subscription = TRUE, billing_note = "Provider permission and included usage must be verified separately for your account.")), docs_url = "https://docs.claude.com")),
  "openai-codex" = list(flow = .codex_flow, descriptor = .descriptor("openai-codex", "ChatGPT (subscription)", "OpenAI", "openai-codex", list(
    .method("browser", "Sign in with ChatGPT (browser)", "account", "authorization_code", "unverified", "Browser login, inference, persistence and early renewal observed 2026-09-23; provider permission and billing remain unverified", delivery = c("loopback", "manual"), subscription = TRUE),
    .method("device", "Sign in with ChatGPT (device code, for SSH/headless)", "account", "device_code", "unverified", "Device login and inference observed 2026-09-23; provider permission and billing remain unverified", delivery = "device", subscription = TRUE)))),
  "github-copilot" = list(flow = .copilot_flow, descriptor = .descriptor("github-copilot", "GitHub Copilot", "GitHub", "github-copilot", list(
    .method("device", "Sign in with GitHub (Copilot subscription)", "account", "device_code", "unverified", "Login, catalog, inference, persistence and early renewal observed 2026-09-23; permission review pending",
      fields = list(.field("enterprise_domain", "GitHub Enterprise domain (blank for github.com)", required = FALSE, help = "e.g. company.ghe.com")), delivery = "device", subscription = TRUE,
      billing_note = "Some models require enabling on your account first; LM15 does not change that setting during login.")))),
  "kimi-code" = list(flow = .kimi_flow, descriptor = .descriptor("kimi-code", "Kimi Code (subscription)", "Moonshot AI", "kimi-code", list(
    .method("device", "Sign in with Kimi Code (subscription)", "account", "device_code", "unverified", "no live receipt yet", delivery = "device", subscription = TRUE)))),
  meta = list(flow = .meta_flow, descriptor = .descriptor("meta", "Meta", "Meta", c("meta", "meta-chat", "meta-anthropic"), list(
    .method("device", "Sign in with Meta (Muse subscription)", "account", "device_code", "unverified", "no live receipt yet", delivery = "device", subscription = TRUE,
      billing_note = "Minted Model API keys are tied to the Muse subscription; verify entitlement on your account.")), console_url = "https://dev.meta.ai")),
  openrouter = list(flow = .openrouter_flow, descriptor = .descriptor("openrouter", "OpenRouter", "OpenRouter", "openrouter", list(
    .method("browser", "Sign in with OpenRouter (creates an API key for this app)", "account", "authorization_code", "unverified", "Login, key limit, inference and persistence observed 2026-09-23; broader support review pending",
      delivery = c("loopback", "manual"), billing_note = "The minted key spends your OpenRouter credits like any other key.")), console_url = "https://openrouter.ai/keys")))

.service_labels <- c(anthropic = "Anthropic", "claude-code" = "Anthropic", openai = "OpenAI", "openai-chat" = "OpenAI", "openai-codex" = "OpenAI", gemini = "Google",
  vertex = "Google Cloud", "vertex-anthropic" = "Google Cloud", "vertex-express" = "Google Cloud", azure = "Microsoft Azure", "azure-chat" = "Microsoft Azure",
  "azure-anthropic" = "Microsoft Azure", "aws-anthropic" = "AWS", "bedrock-anthropic" = "AWS", "bedrock-chat" = "AWS", "bedrock-mantle-chat" = "AWS", meta = "Meta",
  "meta-chat" = "Meta", "meta-anthropic" = "Meta", moonshotai = "Moonshot AI", "moonshotai-anthropic" = "Moonshot AI", "moonshotai-responses" = "Moonshot AI",
  "kimi-code" = "Moonshot AI", deepseek = "DeepSeek", "deepseek-anthropic" = "DeepSeek", groq = "Groq", openrouter = "OpenRouter", xai = "xAI", zai = "Z.AI",
  typesafe = "TypeSafe", ollama = "Local", vllm = "Local", sglang = "Local", "github-copilot" = "GitHub", deepinfra = "DeepInfra", together = "Together AI",
  fireworks = "Fireworks AI", parasail = "Parasail")

.recipe_methods <- function(provider) {
  out <- list()
  for (source in names(.external_sources)) if (.external_sources[[source]][[1L]] == provider)
    out[[length(out) + 1L]] <- .method(paste0("external:", source), paste("Use", .external_sources[[source]][[2L]]), "account", "source_recipe", subscription = TRUE,
      billing_note = "Whatever that tool's login is entitled to; LM15 reads and renews it in place and copies nothing.", guidance = "Sign in with that tool first if it says no credential is present.")
  d <- tryCatch(.definition(provider), error = function(e) NULL)
  if (is.null(d) || isTRUE(d$declared)) return(out)  # connection-only routes have no registry recipes
  access <- d$access
  if (access$credential_policy %in% c("aws-chain", "azure-chain", "gcp-chain"))
    out[[length(out) + 1L]] <- .method("cloud", "Use a named cloud identity", "cloud_identity", "source_recipe",
      fields = list(.field("named", "Identity", "select", options = lapply(.named_credentials, .option))), billing_note = "Billed to that cloud account.")
  if (!is.null(d$placeholder_key)) {
    out[[length(out) + 1L]] <- .method("local", "Local server (no key needed)", "local_server", "source_recipe", fields = list(.field("base_url", "Server URL", required = FALSE)))
    return(out)
  }
  if (access$credential_policy %in% c("key", "oauth-unless-explicit", "connection", "aws-chain", "azure-chain", "gcp-chain")) {
    out[[length(out) + 1L]] <- .method("api_key", "Paste an API key", "api_key", "form", fields = list(.field("key", "API key", "secret")),
      guidance = if (!is.null(d$console_url)) paste("Create one at", d$console_url), billing_note = "Metered per token by the provider.")
    keys <- unlist(access$env_keys)
    if (length(keys)) out[[length(out) + 1L]] <- .method("env", paste0("Use the key in $", keys[[1L]], " from the environment"), "api_key", "source_recipe",
      fields = list(.field("name", "Environment variable", "select", options = lapply(keys, function(k) .option(k, paste0("$", k))))),
      billing_note = "Metered per token by the provider; the variable's value is read at request time, never saved.")
  }
  out
}

.login_table <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    table <- list()
    for (provider in sort(unique(c(providers(), names(.account_flows))))) {
      account <- .account_flows[[provider]]
      recipes <- .recipe_methods(provider)
      descriptor <- if (!is.null(account)) {
        a <- account$descriptor
        own <- Filter(function(m) m$kind == "account" && !startsWith(m$id, "external:"), a$methods)
        d <- tryCatch(.definition(provider), error = function(e) NULL)
        .descriptor(a$id, a$label, a$service, a$routes, c(own, recipes), console_url = a$console_url %||% d$console_url, docs_url = a$docs_url)
      } else {
        d <- .definition(provider)
        .descriptor(provider, provider, unname(.service_labels[provider]) %|NA|% provider, provider, recipes, console_url = d$console_url)
      }
      table[[provider]] <- list(descriptor = descriptor, recipe = .recipe_flow(provider), account = account$flow)
    }
    # Radius: its model protocol is not implemented, so login alone would
    # advertise a model connection that cannot make a request (AUTH-26).
    table$radius <- list(descriptor = .descriptor("radius", "Radius", "Radius", character(), list(
      .method("browser", "Sign in with Radius", "account", "authorization_code", "unavailable", "Radius's model protocol is not implemented in lm15; login without inference would be a false 'supported' claim"))),
      recipe = .recipe_flow("radius"), account = NULL)
    cache <<- table
    table
  }
})
.login_provider_ids <- function() sort(names(.login_table()))
.login_entry <- function(provider) .login_table()[[canonical_provider(provider)]]
.flow_for_method <- function(provider, method_id) {
  e <- .login_entry(provider)
  if (!is.null(e$account) && any(vapply(e$descriptor$methods, function(m) m$id == method_id && m$kind == "account" && !startsWith(m$id, "external:"), logical(1)))) return(e$account)
  e$recipe
}
.flow_for_material <- function(provider, material) {
  e <- .login_entry(provider)
  kind <- .json_str(material$type) %||% ""
  if (kind %in% c("api_key", "env", "external", "local", "cloud") && !isTRUE(material$minted)) return(e$recipe)
  e$account %||% e$recipe
}
