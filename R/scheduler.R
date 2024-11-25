#' Computes a build schedule respecting inter-package dependencies.
#'
#' @param tree Packages to build.
#' @return A `build_sched` object.
#' @export
build_schedule <- function(tree) {
  pkgs <- names(tree)

  counts <- rep.int(0, length(pkgs))
  names(counts) <- pkgs

  rdeps <- list()

  for (p in pkgs) {
    ds <- pkg_deps(tree, p)

    counts[p] <- length(ds)
    for (d in ds) {
      rdeps[[d]] <- c(rdeps[[d]], p)
    }
  }

  structure(list(counts = counts, rdeps = rdeps), class = "build_sched")
}

#' Print method for `build_sched` objects.
#'
#' @param x A `build_sched` object.
#' @param ... Other arguments, ignored.
#' @export
print.build_sched <- function(x, ...) {
  cat("Build schedule for", length(x$counts), "packages.\n")
}

#' List packages that can be build now.
#'
#' @param build_sched A build schedule.
#' @return A vector of package names.
buildable_pkgs <- function(build_sched) {
  cs <- build_sched$counts
  names(cs)[cs == 0]
}

#' Set packages as built.
#'
#' @param build_sched A build schedule.
#' @param pkgs A vector of package names.
#' @return An updated version of the build schedule.
set_built_pkgs <- function(build_sched, pkgs) {
  cs <- build_sched$counts
  cs[pkgs] <- -1
  for (p in pkgs) {
    rds <- build_sched$rdeps[[p]]
    cs[rds] <- cs[rds] - 1
  }

  structure(list(counts = cs, rdeps = build_sched$rdeps), class = "build_sched")
}
