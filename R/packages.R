#' Build a package tree including all packages on CRAN and Bioconductor.
#'
#' @return A `pkg_tree` object.
#' @export
all_packages <- function() {
  pkgs <- list_packages()
  builtins <- tools::standard_package_names()$base
  stopifnot(!any(rownames(pkgs) %in% builtins))

  deps <-
    tools::package_dependencies(rownames(pkgs), db = pkgs) |>
    purrr::map(\(ds) ds[!ds %in% builtins])

  structure(list(pkgs = pkgs, deps = deps), class = "pkg_tree")
}

list_packages <- function() {
  ap <- purrr::map(
    suppressMessages(
      unique(c(getOption("repos"), BiocManager::repositories()))
    ),
    \(r) utils::available.packages(repos = r)
  )
  do.call(rbind, ap)
}

#' Print function for `pkg_tree` objects.
#'
#' @param x A `pkg_tree` object.
#' @param ... Other arguments, ignored.
#' @export
print.pkg_tree <- function(x, ...) {
  cat("Package tree including ", nrow(x$pkgs), "packages.\n")
}

#' Subset a package tree, keeping track of all required dependencies.
#'
#' @param x A `pkg_tree` object.
#' @param subset A vector of package names to be selected.
#' @param ... Other arguments, ignored.
#' @return Another `pkg_tree`, including required packages and their
#'     dependencies.
#' @export
subset.pkg_tree <- function(x, subset, ...) {
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
    list(pkgs = x$pkgs[ns, , drop = FALSE], deps = x$deps[ns]),
    class = "pkg_tree"
  )
}

#' Extract package names.
#'
#' @param x A `pkg_tree` object.
#' @return A vector of package names.
#' @export
names.pkg_tree <- function(x) {
  rownames(x$pkgs)
}

#' Retrieve the dependencies of a package.
#'
#' @param tree A `pkg_tree` object.
#' @param pkg A length-one vector containing a package name.
#' @return A vector of dependency names.
#'
#' @importFrom cli cli_abort
pkg_deps <- function(tree, pkg) {
  if (!is.character(pkg)) {
    cli_abort("`pkg` should be a character vector.")
  } else if (length(pkg) != 1) {
    cli_abort("`pkg` should contain a single entry.")
  }

  tree$deps[[pkg]]
}

#' List package repositories.
#'
#' @param tree A `pkg_tree` object.
#' @param pkg A vector of package names.
#' @return A vector of repositories corresponding to the provided packages.
pkg_repos <- function(tree, pkg) {
  tree$pkgs[pkg, "Repository"]
}

#' List package versions.
#'
#' @param tree A `pkg_tree` object.
#' @param pkg A vector of package names.
#' @return A vector of package versions.
pkg_versions <- function(tree, pkg) {
  tree$pkgs[pkg, "Version"]
}

#' Retrieve package information.
#'
#' @param tree A `pkg_tree` object.
#' @param pkg A length-one vector containing a package name.
#' @return A list including all available package information.
pkg_info <- function(tree, pkg) {
  as.list(tree$pkgs[pkg, , drop = TRUE])
}
