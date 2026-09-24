auth_fixture <- function() {
  home <- tempfile("lm15-auth-"); dir.create(home)
  list(home = home, path = file.path(home, "credentials.json"), env = c(HOME = home, LM15_LOCK_DIR = file.path(home, "locks")))
}
auth_body <- function(access = "old-access", refresh = "old-refresh", expires = 1) {
  json_object(other = json_object(preserve = NULL), xai = json_object(type = "oauth", access = access, refresh = refresh, expires = expires))
}
