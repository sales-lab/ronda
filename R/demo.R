#' Run a demonstration of the system, building the graphite package and its
#' dependencies.
#'
#' @importFrom cli cli_progress_step
#' @export
demo_build <- function() {
  tree <- all_packages() |> subset("tidyverse")

  bs <- build_schedule(tree)
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
