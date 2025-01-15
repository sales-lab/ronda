# Copyright (C) 2024 Gabriele Sales
# MIT License


#' Retrieve custom build details for specific packages.
#'
#' @param pkg A length-one vector containing a package name.
#' @return A list with the following entries, any of which could be `NULL`:
#'   \item{compiler}{Package-specific compiler.}
#'   \item{build_deps}{Build dependencies.}
#'   \item{run_deps}{Runtime dependencies.}
#'   \item{script}{Build script.}
lookup_custom <- function(pkg) {
  if (!is.character(pkg)) {
    cli_abort("`pkg` should be a character vector.")
  } else if (length(pkg) != 1) {
    cli_abort("`pkg` should contain a single entry.")
  }

  sysdeps <- lookup_sysdeps(pkg)

  list(compiler = custom_compiler[[pkg]],
       build_deps = c(custom_build_deps[[pkg]], sysdeps),
       run_deps = c(custom_run_deps[[pkg]], sysdeps),
       script = custom_script(pkg))
}

custom_compiler <- list(
  torch = "cuda-compiler"
)

custom_build_deps <- list(
  xml2 = c("liblzma-devel", "zlib")
)

custom_run_deps <- list(
  torch = "cuda",
  xml2 = "liblzma"
)

custom_script <- function(pkg) {
  filename <- paste0(pkg, ".sh")
  tryCatch({
      path <- fs::path_package("ronda", "extdata", filename)
      readLines(path)
    },
    EEXIST = \(e) return(NULL)
  )
}
