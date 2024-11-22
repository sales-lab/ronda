#' Run a demonstration of the system, building the graphite package and its
#' dependencies.
#'
#' @export
demo_build <- function() {
  pis <- all_package_info() |> subset("graphite")

  bs <- build_schedule(pis)
  repeat {
    pkgs <- buildable_pkgs(bs)
    if (length(pkgs) == 0) {
      break
    }

    for (p in pkgs) {
      conda_build(p, pis)
    }

    bs <- set_built_pkgs(bs, pkgs)
  }
}
