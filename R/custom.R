# Copyright (C) 2024-2025 Gabriele Sales
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

  list(compiler = apply_constraints(custom_compiler[[pkg]]),
       build_deps = apply_constraints(c(custom_build_deps[[pkg]], sysdeps)),
       run_deps = apply_constraints(c(custom_run_deps[[pkg]], sysdeps)),
       script = custom_script(pkg))
}

custom_compiler <- list(
  torch = "cuda-compiler"
)

custom_build_deps <- list(
  Cairo = c("xorg-xproto", "xorg-kbproto", "xorg-renderproto", "xorg-xextproto"),
  systemfonts = "expat"
)

custom_run_deps <- list(
  torch = "cuda",
  xml2 = "liblzma"
)

apply_constraints <- function(pkgs) {
  purrr::map_chr(pkgs, \(pkg) {
    cstr <- custom_constraints[[pkg]]
    if (is.null(cstr)) pkg else paste(pkg, cstr, sep = " ")
  })
}

custom_constraints <- list(
  libxml2 = "<2.14"
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
