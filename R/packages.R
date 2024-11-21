#' Retrieve dependency information for all packages available on CRAN and on
#' Bioconductor.
#'
#' @return A named list representing all dependencies of each package.
#' @export
all_package_deps <- function() {
  ap <- purrr::map(
    c(getOption("repos"), BiocManager::repositories()["BioCsoft"]),
    \(r) utils::available.packages(repos = r)
  )
  ap <- do.call(rbind, ap)
  tools::package_dependencies(rownames(ap), db = ap)
}
