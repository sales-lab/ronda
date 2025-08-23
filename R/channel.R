# Copyright (C) 2024-2025 Gabriele Sales
# MIT License


#' Read the manifest of a Conda channel.
#'
#' @param path Root directory of the channel
#' @return A `conda_channel` object.
#'
#' @export
conda_channel <- function(path) {
  check_string(path)

  if (!fs::is_dir(path)) {
    return(empty_channel())
  }

  pkgs <-
    fs::dir_map(path = path, type = "directory", fun = \(d) {
      manifest_path <- fs::path_join(c(d, "repodata.json"))

      # TODO: consider handling this as an error
      if (!fs::is_file(manifest_path)) {
        return(NULL)
      }

      manifest <- jsonlite::read_json(manifest_path)
      pkgs <- manifest$`packages.conda`
      data.frame(
        name = purrr::map_chr(pkgs, \(p) p$name),
        version = purrr::map_chr(pkgs, \(p) p$version),
        build = purrr::map_int(pkgs, \(p) p$build_number)
      )
    }) |>
    purrr::list_rbind()

  is_r_pkg <- stringr::str_detect(pkgs$name, "^(r|bioconductor)-")
  pkgs <- pkgs[is_r_pkg, , drop = FALSE]
  if (nrow(pkgs) == 0) {
    return(empty_channel())
  }

  ext_version <- paste(pkgs$version, pkgs$build, sep = ".")
  pkgs$version <- package_version(pkgs$version)
  pkgs$ext_version <- package_version(ext_version)
  pkgs <- dplyr::slice_max(pkgs, ext_version, n = 1L,
                           by = "name", with_ties = FALSE)
  rownames(pkgs) <- pkgs$name
  pkgs$name <- NULL
  pkgs$ext_version <- NULL

  structure(list(pkgs = pkgs), class = "conda_channel")
}

#' Print method for `conda_channel` objects.
#'
#' @param x A `conda_channel` object.
#' @param ... Other arguments, ignored.
#'
#' @export
print.conda_channel <- function(x, ...) {
  if (nrow(x$pkgs) == 0) {
    cat("Empty Conda channel.\n")
  } else {
    cat("Conda channel including", nrow(x$pkgs), "packages.\n")
  }
}

#' Load the local Conda channel.
#'
#' @return A `conda_channel` object.
#'
#' @export
local_channel <- function() {
  local_dir <- conda_artifact_dir()
  if (!fs::is_dir(local_dir)) {
    empty_channel()
  }

  conda_channel(local_dir)
}

empty_channel <- function() {
  structure(list(pkgs = data.frame()), class = "conda_channel")
}

#' List packages available from a channel.
#'
#' @param channel A `conda_channel` object.
#' @return A data.frame containing package versions and build numbers, with
#'   package names used as row names.
#'
#' @export
channel_packages <- function(channel) {
  check_class(channel, "conda_channel")
  channel$pkgs
}

#' Lookup the build number of a package.
#'
#' @param channel A `conda_channel` object.
#' @param pkg The package name.
#' @return The build version as an integer if the package exists, or `NA`.
#'
#' @export
conda_build_num <- function(pkg, channel) {
  check_string(pkg)
  check_class(channel, "conda_channel")

  pkgs <- channel$pkgs
  if (nrow(pkgs) == 0) NA_integer_ else channel$pkgs[pkg, "build"]
}
