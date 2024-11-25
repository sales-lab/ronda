#' Read the manifest of a Conda channel.
#'
#' @param path Root directory of the channel
#' @return A `conda_channel` object.
conda_channel <- function(path) {
  manifest_path <- fs::path_join(c(path, "channeldata.json"))
  manifest <- jsonlite::read_json(manifest_path)
  structure(list(pkgs = purrr::map_chr(manifest$packages, \(p) p$version)),
            class = "conda_channel")
}

#' Print method for `conda_channel` objects.
#'
#' @param x A `conda_channel` object.
#' @param ... Other arguments, ignored.
#' @export
print.conda_channel <- function(x, ...) {
  cat("Conda channel including", length(x$pkgs), "packages.\n")
}

#' Load the local Conda channel.
#'
#' @return A `conda_channel` object.
#'
#' @importFrom cli cli_abort
local_channel <- function() {
  prefix <- Sys.getenv("CONDA_PREFIX")
  if (!fs::is_dir(prefix)) {
    cli_abort(c(
      "x" = "Conda environment is not active.",
      "i" = "Have you tried running {.code conda activate base}?"
    ))
  }

  local_dir <- fs::path_join(c(prefix, "conda-bld"))
  if (!fs::is_dir(local_dir)) {
    cli_abort(c(
      "x" = "Missing directory for local Conda channel.",
      "i" = "Expected path: {.path local_dir}"
    ))
  }

  conda_channel(local_dir)
}

#' List packages available from a channel.
#'
#' @param channel A `conda_channel` object.
#' @return A data.frame with two columns: `package` name and `version`.
channel_packages <- function(channel) {
  data.frame(package = names(channel$pkgs), version = channel$pkgs,
             row.names = NULL)
}
