#' Computes a build schedule respecting inter-package dependencies.
#'
#' @param packages A list representing packages dependencies.
#' @return Vector of packages names in build order.
#' @export
build_schedule <- function(packages) {
  builtins <- unlist(tools::standard_package_names())
  deps <- drop_builtins(packages, builtins)
  mapping <- intern_package_names(names(deps))
  ideps <- intern_dependencies(deps, mapping)
  ts <- toposort::topological_sort(ideps, dependency_type = "follows")
  names(mapping[ts])
}

drop_builtins <- function(deps, builtins) {
  ns <- setdiff(names(deps), builtins)
  purrr::map(deps[ns], \(ps) setdiff(ps, builtins))
}

intern_package_names <- function(packages) {
  mapping <- seq_along(packages)
  names(mapping) <- packages
  mapping
}

intern_dependencies <- function(deps, mapping) {
  purrr::map(deps, \(ns) purrr::keep(mapping[ns], \(x) !is.na(x)))
}
