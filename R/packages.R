#' Retrieve package information for all packages on CRAN and Bioconductor.
#'
#' @return A matrix, as returned by [available.packages()].
#' @export
all_package_info <- function() {
  ap <- purrr::map(
    c(getOption("repos"), BiocManager::repositories()["BioCsoft"]),
    \(r) utils::available.packages(repos = r)
  )
  do.call(rbind, ap)
}

#' Retrieve dependency information for all packages available on CRAN and on
#' Bioconductor.
#'
#' @param pkg_info Package information retrieved by [all_package_info()].
#' @return A named list representing all dependencies of each package.
#' @export
all_package_deps <- function(pkg_info) {
  deps <- tools::package_dependencies(rownames(pkg_info), db = pkg_info)
  builtins <- unlist(tools::standard_package_names())
  drop_builtins(deps, builtins)
}

drop_builtins <- function(deps, builtins) {
  ns <- setdiff(names(deps), builtins)
  purrr::map(deps[ns], \(ps) setdiff(ps, builtins))
}
