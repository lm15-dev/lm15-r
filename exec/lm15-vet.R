#!/usr/bin/env Rscript
# Newline-delimited contract transformations. The package's vet adapter
# installs a transport that refuses every network operation.
input <- file("stdin", open = "r")
repeat {
  line <- readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) break
  if (!nzchar(trimws(line))) next
  cat(lm15::vet_handle(line), "\n", sep = "")
  flush.console()
}
close(input)
