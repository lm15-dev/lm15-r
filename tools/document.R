#!/usr/bin/env Rscript
# Writes man/*.Rd from tools/reference.R. Usage sections come from the
# installed functions themselves, so they cannot drift from the code; titles,
# descriptions, arguments, values and examples are written by hand in
# tools/reference.R. The run fails if an export has no page, a page names a
# function that does not exist, or any argument lacks a description.
#
#   R CMD INSTALL --library=<lib> . && Rscript tools/document.R <lib>
args <- commandArgs(trailingOnly = TRUE)
lib <- if (length(args)) args[[1L]] else .libPaths()[[1L]]
ns <- asNamespace(loadNamespace("lm15", lib.loc = lib))
exports <- sort(getNamespaceExports(ns))
source("tools/reference.R", local = (ref <- new.env()))
pages <- ref$pages; common <- ref$common_args

escape <- function(x) gsub("%", "\\%", x, fixed = TRUE)
usage_of <- function(name) {
  fn <- get(name, envir = ns)
  text <- paste(trimws(deparse(args(fn), width.cutoff = 500L)), collapse = " ")
  text <- sub("\\) +NULL$", "", sub("^function \\(", "", text))
  # Split on the commas at depth zero (defaults may hold calls and strings).
  parts <- character(); depth <- 0L; quote <- ""; start <- 1L; chars <- strsplit(text, "")[[1L]]
  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    if (nzchar(quote)) { if (ch == quote && chars[[max(1L, i - 1L)]] != "\\") quote <- ""; next }
    if (ch %in% c("\"", "'")) quote <- ch
    else if (ch %in% c("(", "[", "{")) depth <- depth + 1L
    else if (ch %in% c(")", "]", "}")) depth <- depth - 1L
    else if (ch == "," && depth == 0L) { parts <- c(parts, trimws(substr(text, start, i - 1L))); start <- i + 1L }
  }
  if (nzchar(trimws(text))) parts <- c(parts, trimws(substr(text, start, nchar(text))))
  lines <- character(); line <- paste0(name, "(")
  for (i in seq_along(parts)) {
    piece <- paste0(parts[[i]], if (i < length(parts)) ",")
    if (nchar(line) + nchar(piece) + 1L > 78L && !grepl("\\($", line)) { lines <- c(lines, line); line <- paste0("  ", piece) }
    else line <- paste0(line, if (!grepl("\\($", line)) " ", piece)
  }
  escape(c(lines, paste0(line, ")")))
}

problems <- character()
seen <- character()
dir.create("man", showWarnings = FALSE)
unlink(list.files("man", pattern = "\\.Rd$", full.names = TRUE))
for (page in pages) {
  missing_fns <- setdiff(page$functions, exports)
  if (length(missing_fns)) problems <- c(problems, paste0(page$name, ": not exported: ", paste(missing_fns, collapse = ", ")))
  fns <- intersect(page$functions, exports)
  seen <- c(seen, fns)
  arg_names <- unique(unlist(lapply(fns, function(f) names(formals(get(f, envir = ns))))))
  described <- c(page$args, common[setdiff(names(common), names(page$args))])
  undocumented <- setdiff(arg_names, names(described))
  if (length(undocumented)) problems <- c(problems, paste0(page$name, ": undocumented arguments: ", paste(undocumented, collapse = ", ")))
  extra <- setdiff(names(page$args), arg_names)
  if (length(extra)) problems <- c(problems, paste0(page$name, ": describes arguments no function has: ", paste(extra, collapse = ", ")))
  rd <- c(paste0("\\name{", page$name, "}"),
    paste0("\\alias{", unique(c(page$name, fns, page$aliases)), "}"),
    paste0("\\title{", page$title, "}"),
    "\\description{", page$description, "}")
  if (length(fns)) rd <- c(rd, "\\usage{", unlist(lapply(fns, function(f) c(usage_of(f), ""))), "}")
  if (length(arg_names)) {
    rd <- c(rd, "\\arguments{")
    for (a in arg_names) rd <- c(rd, paste0("\\item{", if (a == "...") "\\dots" else a, "}{", described[[a]], "}"))
    rd <- c(rd, "}")
  }
  if (!is.null(page$details)) rd <- c(rd, "\\details{", page$details, "}")
  rd <- c(rd, "\\value{", page$value, "}")
  if (!is.null(page$seealso)) rd <- c(rd, "\\seealso{", page$seealso, "}")
  if (!is.null(page$examples)) rd <- c(rd, "\\examples{", page$examples, "}")
  if (isTRUE(page$internal)) rd <- c(rd, "\\keyword{internal}")
  if (!is.null(page$docType)) rd <- c(rd, paste0("\\docType{", page$docType, "}"))
  writeLines(rd, file.path("man", paste0(page$name, ".Rd")), useBytes = TRUE)
}
unpaged <- setdiff(exports, seen)
if (length(unpaged)) problems <- c(problems, paste0("exports with no page: ", paste(unpaged, collapse = ", ")))
twice <- unique(seen[duplicated(seen)])
if (length(twice)) problems <- c(problems, paste0("functions on two pages: ", paste(twice, collapse = ", ")))
if (length(problems)) { writeLines(problems, stderr()); quit(status = 1L) }
cat(length(pages), "pages,", length(exports), "exports documented\n")
