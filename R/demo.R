#' Run a demonstration of the system, building the graphite package and its
#' dependencies.
#'
#' @importFrom cli cli_progress_step
#' @export
demo_build <- function() {
  tree <- all_packages() |> subset("tidyverse")

  local <- local_channel()
  bs <- build_schedule(tree)
  bs <- set_built_pkgs(bs, find_local_packages(tree, local))

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

    bs <- set_built_pkgs(bs, pkgs)
  }
}

find_local_packages <- function(tree, channel) {
  local_pkgs <- channel_packages(channel)

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
