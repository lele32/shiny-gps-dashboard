# Compatibility entrypoint for the former beta launcher.
# The production app and beta now use the same modular runtime so fixes do not
# drift between two copies of the GPS workspace.

beta_root <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(error) NULL)
  if (is.character(source_file) && length(source_file) == 1L && nzchar(source_file)) {
    dirname(normalizePath(source_file, mustWork = FALSE))
  } else {
    getwd()
  }
})

source(file.path(beta_root, "app.R"), local = environment())
