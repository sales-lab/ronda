#' Retrieve package information for all packages on CRAN and Bioconductor.
#'
#' @return A `pkg_info` object.
#' @export
all_package_info <- function() {
  pkgs <- list_packages()
  builtins <- tools::standard_package_names()$base
  stopifnot(!any(rownames(pkgs) %in% builtins))

  deps <-
    tools::package_dependencies(rownames(pkgs), db = pkgs) |>
    purrr::map(\(ds) ds[!ds %in% builtins])

  structure(list(pkgs = pkgs, deps = deps), class = "pkg_info")
}

list_packages <- function() {
  ap <- purrr::map(
    unique(c(getOption("repos"), BiocManager::repositories())),
    \(r) utils::available.packages(repos = r)
  )
  do.call(rbind, ap)
}

#' Print function for `pkg_info` objects.
#'
#' @param x A `pkg_info` object.
#' @param ... Other arguments, ignored.
#' @export
print.pkg_info <- function(x, ...) {
  cat("pkg_info for", nrow(x$pkgs), "packages.\n")
}

#' Subset package information, keeping track of all required dependencies.
#'
#' @param x A `pkg_info` object.
#' @param subset A vector of package names to be selected.
#' @param ... Other arguments, ignored.
#' @return Another pkg_info, including required packages and their dependencies.
#' @export
subset.pkg_info <- function(x, subset, ...) {
  queue <- subset
  ns <- subset

  while (length(queue) > 0) {
    wset <- queue
    queue <- character()

    for (p in wset) {
      ds <- x$deps[[p]]
      ds <- setdiff(ds, x$builtins)
      ds <- setdiff(ds, ns)
      queue <- c(queue, ds)
      ns <- c(ns, ds)
    }
  }

  structure(
    list(pkgs = x$pkgs[ns, , drop = FALSE],
         deps = x$deps[ns],
         builtins = x$builtins),
    class = "pkg_info")
}

#' Extract package names.
#'
#' @param x A `pkg_info` object.
#' @return A vector of package names.
#' @export
names.pkg_info <- function(x) {
  rownames(x$pkgs)
}

#' Retrieve the dependencies of a package.
#'
#' @param pkg_info A `pkg_info` object.
#' @param pkg A length-one vector containing  a package name.
#' @return A vector of dependency names.
#'
#' @importFrom cli cli_abort
pkg_deps <- function(pkg_info, pkg) {
  if (!is.character(pkg)) {
    cli_abort("`pkg` should be a character vector.")
  } else if (length(pkg) != 1) {
    cli_abort("`pkg` should contain a single entry.")
  }

  pkg_info$deps[[pkg]]
}
