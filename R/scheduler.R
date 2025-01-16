# Copyright (C) 2024 Gabriele Sales
# MIT License


#' Computes a build schedule respecting inter-package dependencies.
#'
#' @param tree Packages to build.
#' @return A `build_sched` object.
#'
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
#'
#' @importFrom cli cli_text cli_ul cli_li cli_end
#' @export
print.build_sched <- function(x, ...) {
  cli_text(glue::glue("Build schedule for {length(x$counts)} packages."))
  ul <- cli_ul()
  cli_li(glue::glue("Already built: {sum(x$counts < 0)}"))
  cli_li(glue::glue("Ready to build: {sum(x$counts == 0)}"))
  cli_end(ul)
}

#' List packages that can be build now.
#'
#' @param build_sched A build schedule.
#' @return A vector of package names.
#'
#' @export
buildable_pkgs <- function(build_sched) {
  cs <- build_sched$counts
  names(cs)[cs == 0]
}

#' Set packages as built.
#'
#' @param build_sched A build schedule.
#' @param pkgs A vector of package names.
#' @return An updated version of the build schedule.
#'
#' @export
set_built_pkgs <- function(build_sched, pkgs) {
  if (length(pkgs) == 0) {
    return(build_sched)
  }

  cs <- build_sched$counts
  all_pkgs <- names(cs)

  clean <- find_clean(build_sched, setdiff(all_pkgs, pkgs))
  cs[clean] <- -1
  for (p in clean) {
    rds <- build_sched$rdeps[[p]]
    cs[rds] <- max(cs[rds] - 1, -1)
  }

  structure(list(counts = cs, rdeps = build_sched$rdeps), class = "build_sched")
}

find_clean <- function(build_sched, dirty) {
  mask <- rep(FALSE, length(build_sched$count))
  names(mask) <- names(build_sched$count)
  mask[dirty] <- TRUE

  dirty_count <- sum(mask)
  repeat {
    ps <- names(mask[mask])
    for (p in ps) {
      rds <- build_sched$rdeps[[p]]
      mask[rds] <- TRUE
    }

    s <- sum(mask)
    if (s > dirty_count) {
      dirty_count <- s
    } else {
      ps <- names(mask[!mask])
      return(ps)
    }
  }
}
