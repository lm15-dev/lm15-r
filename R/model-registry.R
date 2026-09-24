new_model_registry <- function(models = list(), ...) {
  .check_dots(...)
  entries <- list()
  key <- function(provider, id) paste0(nchar(provider, type = "bytes"), ":", provider, id)
  add <- function(model, replace = TRUE) {
    model <- validate(model)
    if (!inherits(model, "lm15_ModelInfo")) stop("Expected model_info().", call. = FALSE)
    .coerce_field(replace, "bool", "replace", FALSE)
    id <- key(model$provider, model$id)
    if (!replace && id %in% names(entries)) stop("Model is already registered.", call. = FALSE)
    entries[[id]] <<- model
    invisible(model)
  }
  get <- function(provider, model) {
    .string(provider, "provider"); .string(model, "model")
    exact <- entries[[key(provider, model)]]
    if (!is.null(exact)) return(exact)
    candidates <- Filter(function(info) info$provider == provider && model %in% unlist(info$aliases), entries)
    if (length(candidates)) candidates[[length(candidates)]] else NULL
  }
  list_models <- function(provider = NULL) {
    out <- unname(entries)
    if (is.null(provider)) out else Filter(function(info) info$provider == provider, out)
  }
  resolve <- function(model, provider = NULL) {
    .string(model, "model")
    if (!is.null(provider)) return(get(provider, model))
    matches <- Filter(function(info) info$id == model || model %in% unlist(info$aliases), entries)
    if (length(matches) == 1L) matches[[1L]] else NULL
  }
  for (model in models) add(model)
  structure(list(add = add, get = get, resolve = resolve, list = list_models,
    providers = function() sort(unique(vapply(entries, function(info) info$provider, "")), method = "radix")), class = "lm15_model_registry")
}
print.lm15_model_registry <- function(x, ...) { cat("<lm15 model registry: ", length(x$list()), " models>\n", sep = ""); invisible(x) }
str.lm15_model_registry <- function(object, ...) { print.lm15_model_registry(object); invisible(NULL) }

# R packages can ship inst/lm15/model-catalog.json. Discovery reads data, not
# arbitrary package startup code. Named zero-argument loaders can be supplied
# explicitly when an application needs a computed catalog.
discover_model_registry <- function(..., catalogs = NULL, libraries = .libPaths()) {
  .check_dots(...)
  if (is.null(catalogs)) {
    catalogs <- list()
    for (library in libraries) {
      directories <- list.dirs(library, full.names = TRUE, recursive = FALSE)
      for (directory in directories) {
        name <- basename(directory); path <- file.path(directory, "lm15", "model-catalog.json")
        if (is.null(catalogs[[name]]) && file.exists(path)) catalogs[[name]] <- path
      }
    }
  }
  if (!is.list(catalogs) || (length(catalogs) && (is.null(names(catalogs)) || any(!nzchar(names(catalogs))) || anyDuplicated(names(catalogs))))) stop("catalogs must be a uniquely named list of paths or functions.", call. = FALSE)
  registry <- new_model_registry()
  for (name in sort(names(catalogs), method = "radix")) {
    models <- tryCatch({
      source <- catalogs[[name]]
      data <- if (is.function(source)) source() else {
        .string(source, "catalog path")
        if (is.na(file.info(source)$size) || file.info(source)$size > 32 * 1024^2) stop("Invalid catalog size.")
        .json_decode(paste(readLines(source, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
      }
      if (!.is_array(data)) stop("Catalog must be an array.")
      lapply(data, function(value) {
        out <- if (inherits(value, "lm15_ModelInfo")) validate(value) else from_dict(value, "model_info")
        if (!inherits(out, "lm15_ModelInfo")) stop("Invalid model entry.")
        out
      })
    }, error = function(e) {
      warning(paste0("Model catalog '", name, "' could not be read and was skipped. Catalog diagnostics are suppressed."), call. = FALSE)
      NULL
    })
    for (model in models) {
      exact <- Filter(function(info) info$id == model$id, registry$list(model$provider))
      if (!length(exact)) registry$add(model, replace = FALSE)
    }
  }
  registry
}

estimate_cost <- function(pricing, usage, ...) {
  .check_dots(...); pricing <- validate(pricing); usage <- validate(usage)
  if (!inherits(pricing, "lm15_InferencePricing") || !inherits(usage, "lm15_Usage")) stop("Expected inference_pricing() and usage().", call. = FALSE)
  total <- 0
  for (dimension in c("input", "output", "cache_read", "cache_write")) {
    rate <- pricing[[paste0(dimension, "_per_million")]]
    count <- usage[[paste0(dimension, "_tokens")]]
    if (!is.null(rate) && !is.null(count)) total <- total + as.double(count) * rate / 1000000
  }
  # Unknown dimensions contribute nothing: this is a lower bound, not a bill.
  total
}
