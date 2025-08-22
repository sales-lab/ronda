# Copyright (C) 2024-2025 Gabriele Sales
# MIT License


#' Lookup the system dependencies of a package.
#'
#' @param pkg A vector of package names.
#' @return A vector of system dependencies.
#'
#' @importFrom pkgdepends sysreqs_install_plan
#' @export
lookup_sysdeps <- function(pkg) {
  check_string(pkg)

  p <- sysreqs_install_plan(pkg)
  sr <- p$packages$sysreq
  unlist(purrr::map(sr, \(r) {
    x <- sysreqs_conv[[r]]
    if (is.null(x)) r else x
  }))
}

sysreqs_conv <- list(
  gnumake = "make",
  java = "openjdk",
  libicu = "icu",
  libjpeg = "libjpeg-turbo",
  x11 = "xorg-libx11"
)
