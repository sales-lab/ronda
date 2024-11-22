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
    c(getOption("repos"), BiocManager::repositories()["BioCsoft"]),
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
