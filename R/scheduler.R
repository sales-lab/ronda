# Copyright (C) 2024-2025 Gabriele Sales
# MIT License


#' Computes a build schedule respecting inter-package dependencies.
#'
#' @param tree Packages to build.
#' @return A `build_sched` object.
#'
#' @export
build_schedule <- function(tree) {
  check_class(tree, "pkg_tree")

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
  check_class(build_sched, "build_sched")

  cs <- build_sched$counts
  names(cs)[cs == 0]
}

#' Mark packages already built at their most recent version.
#'
#' @param build_sched A build schedule.
#' @param pkgs A vector of package names.
#' @return An updated version of the build schedule.
#'
#' @export
mark_pkgs_up_to_date <- function(build_sched, pkgs) {
  check_class(build_sched, "build_sched")
  check_class(pkgs, "character")
  check_contents(pkgs, Negate(is.na))

  if (length(pkgs) == 0) {
    return(build_sched)
  }

  clean <- find_clean_pkgs(build_sched, pkgs)
  mark_clean(build_sched, clean)
}

# Identify packages that belong to the up-to-date set and, either directly or
# indirectly, depend only on other packages that are also up-to-date.
find_clean_pkgs <- function(build_sched, up_to_date) {
  cs <- build_sched$counts
  rdeps <- build_sched$rdeps

  stopifnot(all(up_to_date %in% names(cs)))

  clean <- names(cs) %in% up_to_date
  names(clean) <- names(cs)
  clean_num <- 0
  while (sum(clean) != clean_num) {
    clean_num <- sum(clean)

    for (p in names(cs)[!clean]) {
      rs <- rdeps[[p]]
      if (!is.null(rs)) {
        clean[rs] <- FALSE
      }
    }
  }

  names(cs)[clean]
}

mark_clean <- function(build_sched, pkgs) {
  if (length(pkgs) == 0) {
    return(build_sched)
  }

  cs <- build_sched$counts
  rdeps <- build_sched$rdeps

  for (p in pkgs) {
    if (cs[p] >= 0) {
      cs[p] <- -1

      rs <- rdeps[[p]]
      if (!is.null(rs)) {
        cs[rs] <- ifelse(cs[rs] < 0, -1, cs[rs] - 1)
      }
    }
  }

  build_sched$counts <- cs
  return(build_sched)
}


#' Record freshly built packages.
#'
#' @param build_sched A build schedule.
#' @param pkgs A vector of package names.
#' @return An updated version of the build schedule.
#'
#' @export
mark_built_pkgs <- function(build_sched, pkgs) {
  check_class(build_sched, "build_sched")
  check_type(pkgs, "character")
  check_contents(pkgs, Negate(is.na))

  if (length(pkgs) == 0) {
    return(build_sched)
  }

  cs <- build_sched$counts
  rdeps <- build_sched$rdeps

  stopifnot(all(pkgs %in% names(cs)))

  cs[pkgs] <- -1

  for (p in pkgs) {
    rs <- rdeps[[p]]

    # If package `p` is a dependency of another package that has been marked
    # as built, we need to trigger a rebuild of the dependent package as well.
    # That's the reason why we set the count to 0 if it was -1.
    cs[rs] <- ifelse(cs[rs] == -1, 0, cs[rs] - 1)
  }

  build_sched$counts <- cs
  return(build_sched)
}
