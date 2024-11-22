#' Retrieve package information for all packages on CRAN and Bioconductor.
#'
#' @return A `pkg_info` object.
#' @export
all_package_info <- function() {
  pkgs <- list_packages()
  deps <- tools::package_dependencies(rownames(pkgs), db = pkgs)
  builtins <- tools::standard_package_names()
  structure(list(pkgs = pkgs, deps = deps, builtins = builtins$base),
            class = "pkg_info")
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
