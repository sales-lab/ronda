# Copyright (C) 2024 Gabriele Sales
# MIT License


#' Build a set of packages, including all of their dependencies.
#'
#' Packages already available in the local Conda channel are skipped.
#'
#' @param pkgs A vector of package names.
#'
#' @importFrom cli cli_progress_step
#' @export
ronda_build <- function(pkgs) {
  tree <-
    all_packages() |>
    subset(pkgs)

  bs <-
    build_schedule(tree) |>
    mark_pkgs_up_to_date(match_local_packages(tree, local_channel()))

  repeat { 
    pkgs <- buildable_pkgs(bs)
    if (length(pkgs) == 0) {
      break
    }

    for (p in pkgs) {
      cli_progress_step(
        paste0("Building ", p),
        msg_done = paste0("Built ", p)
      )
      conda_build(p, tree)
    }

    bs <- mark_built_pkgs(bs, pkgs)
  }
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
    version = pkg_versions(tree, tree_names),
    row.names = NULL
  )

  tbl <- merge(tree_pkgs, local_pkgs, by = "package",
               suffixes = c("_tree", "_local"))
  sel <- tbl["version_tree"] == tbl["version_local"]
  tbl[sel, "name"]
}
