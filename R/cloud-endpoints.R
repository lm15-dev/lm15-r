.cloud_resource_endpoint <- function(definition, env) {
  key <- switch(definition$access$backend, `azure-openai` = "AZURE_OPENAI_ENDPOINT", `azure-foundry` = "ANTHROPIC_FOUNDRY_BASE_URL", NULL)
  if (is.null(key)) return(NULL)
  value <- .auth_env(env, key)
  if (nzchar(value)) value else NULL
}
.cloud_endpoint_root <- function(definition, endpoint) {
  if (!grepl("^https://[^/@?#[:space:]]+(?:/[^?#[:space:]]*)?$", endpoint, perl = TRUE)) .abort("Cloud endpoint must be HTTPS without userinfo, query, or fragment.", "not_configured", definition$id)
  endpoint <- sub("/+$", "", endpoint)
  if (definition$access$backend == "azure-openai") {
    if (endsWith(endpoint, "/openai/v1")) return(endpoint)
    if (endsWith(endpoint, "/openai")) return(paste0(endpoint, "/v1"))
    return(paste0(endpoint, "/openai/v1"))
  }
  if (definition$access$backend == "azure-foundry") {
    if (endsWith(endpoint, "/anthropic/v1")) return(endpoint)
    if (endsWith(endpoint, "/anthropic")) return(paste0(endpoint, "/v1"))
    return(paste0(endpoint, "/anthropic/v1"))
  }
  .abort("This host requires a resource name, not an endpoint URL.", "not_configured", definition$id)
}
