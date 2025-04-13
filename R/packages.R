# Copyright (C) 2024-2025 Gabriele Sales
# MIT License


#' Build a package tree including all packages on CRAN and Bioconductor.
#'
#' @return A `pkg_tree` object.
#'
#' @export
all_packages <- function() {
  pkgs <- list_packages()
  builtins <- tools::standard_package_names()$base
  stopifnot(!any(rownames(pkgs) %in% builtins))

  deps <-
    tools::package_dependencies(rownames(pkgs), db = pkgs) |>
    purrr::map(\(ds) ds[!ds %in% builtins])

  broken <- packages_with_missing_deps(deps)
  if (sum(broken) > 0) {
    cli_warn(c(
            "Some packages declare unknown dependencies.",
      "i" = "Ignoring {sum(broken)} packages{?s}."
    ))
  }

  pkgs <- pkgs[!broken, , drop = FALSE]
  deps <- deps[!broken]
  structure(list(pkgs = pkgs, deps = deps), class = "pkg_tree")
}

list_packages <- function() {
  ap <- purrr::map(
    suppressMessages(
      unique(c(getOption("repos"), BiocManager::repositories()))
    ),
    \(r) utils::available.packages(repos = r, fields = "Description")
  )
  do.call(rbind, ap)
}

#' @importFrom stats setNames
packages_with_missing_deps <- function(deps) {
  ns <- names(deps)
  broken <- setNames(logical(length(ns)), ns)

  dep_tbl <- data.frame(
    pkg = rep.int(ns, purrr::map_int(deps, length)),
    dep = unlist(deps)
  )

  miss <- unique(unlist(deps))
  miss <- data.frame(miss = miss[!(miss %in% ns)])

  repeat {
    matches <- dplyr::inner_join(
      dep_tbl,
      miss,
      by = dplyr::join_by("dep" == "miss")
    )
    if (nrow(matches) == 0) {
      break
    } else {
      miss <- data.frame(miss = matches$pkg) |> dplyr::distinct()
      broken[miss$miss] <- TRUE
    }
  }

  broken
}

#' Print function for `pkg_tree` objects.
#'
#' @param x A `pkg_tree` object.
#' @param ... Other arguments, ignored.
#'
#' @export
print.pkg_tree <- function(x, ...) {
  cat("Package tree including", nrow(x$pkgs), "packages.\n")
}

#' Subset a package tree, keeping track of all required dependencies.
#'
#' @param x A `pkg_tree` object.
#' @param subset A vector of package names to be selected.
#' @param ... Other arguments, ignored.
#' @return Another `pkg_tree`, including required packages and their
#'     dependencies.
#'
#' @importFrom utils head
#' @export
subset.pkg_tree <- function(x, subset, ...) {
  check_type(subset, "character")
  check_length(subset, c(0, NA), interval = TRUE)
  check_contents(subset, Negate(is.na))

  if (!all(subset %in% x$pkgs)) {
    invalid <- setdiff(subset, x$pkgs)
    some <- paste(head(invalid), collapse = ", ")
    cli_abort(c(
      "Some of the requested packages do not exist.",
      "i" = "For instance: {some}"
    ))
  }

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

  stopifnot(all(ns %in% rownames(x$pkgs)))
  structure(
    list(pkgs = x$pkgs[ns, , drop = FALSE], deps = x$deps[ns]),
    class = "pkg_tree"
  )
}

#' Extract package names.
#'
#' @param x A `pkg_tree` object.
#' @return A vector of package names.
#'
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
#' @export
pkg_deps <- function(tree, pkg) {
  check_class(tree, "pkg_tree")
  check_string(pkg)
  check_content(pkg, Negate(is.na))

  tree$deps[[pkg]]
}

#' List package repositories.
#'
#' @param tree A `pkg_tree` object.
#' @param pkg A vector of package names.
#' @return A vector of repositories corresponding to the provided packages.
#'
#' @export
pkg_repos <- function(tree, pkg) {
  check_class(tree, "pkg_tree")
  check_type(pkg, "character")
  check_contents(pkg, Negate(is.na))

  tree$pkgs[pkg, "Repository"]
}

#' List package versions.
#'
#' @param tree A `pkg_tree` object.
#' @param pkg A vector of package names.
#' @return A vector of package versions.
#'
#' @export
pkg_versions <- function(tree, pkg) {
  check_class(tree, "pkg_tree")
  check_type(pkg, "character")
  check_contents(pkg, Negate(is.na))

  tree$pkgs[pkg, "Version"]
}

#' Retrieve package information.
#'
#' @param tree A `pkg_tree` object.
#' @param pkg A length-one vector containing a package name.
#' @param download Download package tarball to extract full metadata.
#' @return A list including all available package information.
#'
#' @export
pkg_info <- function(tree, pkg, download = FALSE) {
  check_class(tree, "pkg_tree")
  check_string(pkg)
  check_content(pkg, Negate(is.na))

  l <- as.list(tree$pkgs[pkg, , drop = TRUE])

  if (download) {
    d <- fetch_description(pkg, tree$pkgs)
    l$Title <- d$Title
    l$Description <- d$Description
    l$URL <- d$URL
  } else {
    l$Title <- pkg
    l$Description <- ""
    l$URL <- ""
  }

  l
}

#' @importFrom utils download.packages
fetch_description <- function(name, pkgs) {
  path <- download.packages(
    name, tempdir(), available = pkgs, quiet = TRUE
  )[, 2]

  filename <- paste0(name, "/DESCRIPTION")
  res <- processx::run(
    "tar", c("--extract", "--file", path, "--to-stdout", filename),
    echo = FALSE
  )

  conn <- textConnection(res$stdout)
  df <- read.dcf(conn)
  stopifnot(nrow(df) == 1)
  as.list(df[1, ])
}

format_description <- function(descr) {
  lines <- unlist(strsplit(descr, "\n"))
  sub("^\\s*", "", lines)
}

#' Build a package tree including all Bioconductor packages
#'
#' @return A `pkg_tree` object.
#'
#' @importFrom stringr str_detect 
#' @export
bioc_packages <- function() {
  tree <- all_packages()
  sel <- str_detect(tree$pkgs[, "Repository"], "bioconductor.org")
  subset(tree, tree$pkgs[sel, "Package"])
}

packages_for_test <- function(deps) {
  pkgs <- data.frame(
    Package = names(deps),
    Version = "1.0",
    row.names = names(deps)
  )
  structure(list(pkgs = as.matrix(pkgs), deps = deps), class = "pkg_tree")
}
