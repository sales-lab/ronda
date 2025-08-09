# Copyright (C) 2024-2025 Gabriele Sales
# MIT License


#' Build a set of packages, including all of their dependencies.
#'
#' Packages already available in the local Conda channel are skipped.
#'
#' @param pkgs A vector of package names, or a `pkg_tree` object.
#' @param log_dir Write logs to specified directory, defaulting to current
#'                directory.
#'
#' @export
ronda_build <- function(pkgs, log_dir = getwd()) {
  if (!inherits(pkgs, "pkg_tree")) {
    check_type(pkgs, "character")
    check_length(pkgs, c(0, NA), interval = TRUE)
    check_contents(pkgs, Negate(is.na))
  }
  check_string(log_dir)

  tree <- if (inherits(pkgs, "pkg_tree")) pkgs else subset(all_packages(), pkgs)
  bs <-
    build_schedule(tree) |>
    mark_pkgs_up_to_date(match_local_packages(tree, local_channel()))

  built_pkgs <- character(0)
  failed_pkgs <- character(0)

  repeat { 
    pkgs <- buildable_pkgs(bs)
    if (length(pkgs) == 0) {
      break
    }

    succ <- character()
    fail <- character()
    for (p in pkgs) {
      step <- function() {
        cli_progress_step(
          paste0("Building ", p),
          msg_done = paste0("Built ", p),
          msg_failed = paste0("Failed ", p)
        )
        conda_build(p, tree, log_dir = log_dir)
      }

      res <- try(step(), silent = TRUE)
      if (inherits(res, "try-error")) {
        fail <- c(fail, p)
      } else {
        succ <- c(succ, p)
      }
    }

    built_pkgs <- c(built_pkgs, succ)
    failed_pkgs <- c(failed_pkgs, fail)
    bs <-
      bs |>
      mark_failed_pkgs(fail) |>
      mark_built_pkgs(succ)
  }

  fail_count <- length(failed_pkgs)
  if (fail_count > 0) {
    cli_warn(c(
      "x" = "Build failed.",
      "i" = "There was an error building {fail_count} package{?s}."
    ))
  }

  invisible(list(
    built = built_pkgs,
    failed = failed_pkgs
  ))
}

match_local_packages <- function(tree, channel) {
  local_pkgs <- channel_packages(channel)
  if (nrow(local_pkgs) == 0) {
    return(character())
  }

  tree_names <- names(tree)
  qnames <- qualified_names(tree_names, tree)
  tree_pkgs <- data.frame(
    name = tree_names,
    package = qnames,
    version = conda_canonize_version(pkg_versions(tree, tree_names)),
    row.names = NULL
  )

  tbl <- merge(tree_pkgs, local_pkgs, by = "package",
               suffixes = c("_tree", "_local"))
  sel <- tbl["version_tree"] == tbl["version_local"]
  tbl[sel, "name"]
}
