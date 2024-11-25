#' Build an R package with Conda.
#'
#' This function works on the assumption that all dependencies are already
#' available.
#'
#' @param pkg Package name.
#' @param tree A `pkg_tree` object.
#'
#' @export
conda_build <- function(pkg, tree) {
  build_dir <- create_build_dir(pkg)
  sysdeps <- lookup_sysdeps(pkg)
  recipe <- create_recipe(pkg, tree, sysdeps, build_dir)
  run_build(build_dir, recipe)
  return(pkg)
}

create_build_dir <- function(pkg) {
  dir <- fs::path_join(c(tempdir(), pkg))
  fs::dir_create(dir)
  withr::defer_parent(fs::dir_delete(dir))
  dir
}

create_recipe <- function(pkg, tree, sysdeps, dir) {
  qname <- qualified_names(pkg, tree)

  info <- pkg_info(tree, pkg)

  repo <- info$Repository
  version <- info$Version
  version_safe <- sub("-", ".", version, fixed = TRUE)
  url <- glue::glue("{repo}/{pkg}_{version}.tar.gz")
  md5 <- info$MD5sum
  license <- info$License

  compiler <- if (info$NeedsCompilation == "yes") compiler_spec else ""

  deps <-
    pkg_deps(tree, pkg) |>
    qualified_names(tree) |>
    append(x = _, sysdeps) |>
    purrr::map_chr(\(p) paste0("    - ", p)) |>
    paste(collapse = "\n")

  content <- glue::glue(recipe_template)
  writeLines(content, fs::path_join(c(dir, "meta.yaml")))

  content
}

qualified_names <- function(pkgs, tree) {
  repos <- pkg_repos(tree, pkgs)
  prefixes <- ifelse(grepl("bioconductor", repos), "bioconductor", "r")
  paste(prefixes, tolower(pkgs), sep = "-")
}

recipe_template <- "
package:
  name: '{qname}'
  version: '{version_safe}'

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
run_build <- function(dir, recipe) {
  pkg <- fs::path_file(dir)
  parent <- fs::path_dir(dir)

  log_file <- paste0(pkg, ".log")
  res <- processx::run(
    "conda", c("build", "--R", r_version(), pkg),
    error_on_status = FALSE, wd = parent, stderr_to_stdout = TRUE,
    stdout = log_file
  )

  if (res$status != 0) {
    log <- file(log_file, open = "at")
    writeLines(c("", "==> Recipe", recipe), log)
    close(log)

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
