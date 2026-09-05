# testthat runs with the working directory at tests/testthat, so every path
# into the project has to be resolved from the repository root.
proj_path <- function(...) {
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  if (!file.exists(file.path(root, "server.R"))) root <- normalizePath(".", mustWork = FALSE)
  file.path(root, ...)
}
