#' Build an R package with Conda.
#'
#' This function works on the assumption tha all dependencies are already
#' available.
#'
#' @param pkg Package name.
#' @param pkg_info A `pkg_info` object.
#'
#' @export
conda_build <- function(pkg, pkg_info) {
  build_dir <- create_build_dir(pkg)
  create_recipe(pkg, pkg_info, build_dir)
  run_build(build_dir)
}

create_build_dir <- function(pkg) {
  dir <- fs::path_join(c(tempdir(), pkg))
  fs::dir_create(dir)
  withr::defer_parent(fs::dir_delete(dir))
  dir
}

create_recipe <- function(pkg, pkg_info, dir) {
  qname <- qualified_names(pkg, pkg_info)

  # TODO: build an helper for this?
  info <- pkg_info$pkgs[pkg, ]

  repo <- info[["Repository"]]
  version <- info[["Version"]]
  url <- glue::glue("{repo}/{pkg}_{version}.tar.gz")
  md5 <- info[["MD5sum"]]
  license <- info[["License"]]

  compiler <- if (info[["NeedsCompilation"]] == "yes") compiler_spec else ""

  deps <-
    pkg_deps(pkg_info, pkg) |>
    qualified_names(pkg_info) |>
    purrr::map_chr(\(p) paste0("    - ", p)) |>
    paste(collapse = "\n")

  content <- glue::glue(recipe_template)
  writeLines(content, fs::path_join(c(dir, "meta.yaml")))
}

qualified_names <- function(pkgs, pkg_info) {
  repos <- pkg_repos(pkg_info, pkgs)
  prefixes <- ifelse(grepl("bioconductor", repos), "bioconductor", "r")
  paste(prefixes, tolower(pkgs), sep = "-")
}

recipe_template <- "
package:
  name: '{qname}'
  version: '{version}'

source:
  url: '{url}'
  md5: '{md5}'

build:
  number: 0
  merge_build_host: true
  script: R CMD INSTALL --build .
  rpaths:
    - lib/R/lib/
    - lib/

requirements:
  build:
{compiler}
{deps}
  host:
    - r-base
{deps}
  run:
    - r-base
{deps}

about:
  summary: Summary.
  license: '{license}'

extra:
  recipe-maintainers:
    - gbrsales
"

compiler_spec <- "
    - cross-r-base {{ r_base }}  # [build_platform != target_platform]
    - autoconf  # [unix]
    - \"{{ compiler('c') }}\"  # [unix]
    - \"{{ compiler('cxx') }}\"  # [unix]
    - posix  # [win]
"

#' @importFrom cli cli_abort
run_build <- function(dir) {
  pkg <- fs::path_file(dir)
  parent <- fs::path_dir(dir)
  res <- processx::run("conda", c("build", "--R", r_version(), pkg),
                       error_on_status = FALSE, wd = parent, echo = TRUE)

  if (res$status != 0) {
    cli_abort(c(
      "x" = "Build failed.",
      "i" = "There was an error building the {pkg} package."
    ))
  }
}

r_version <- function() {
  major <- R.version$major
  minor <- R.version$minor
  paste(major, minor, sep = ".")
}
