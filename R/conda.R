# Copyright (C) 2024-2025 Gabriele Sales
# MIT License


#' Build an R package with Conda.
#'
#' This function works on the assumption that all dependencies are already
#' available.
#'
#' When `dry-run` is `TRUE`, the function generates the recipe file but does not
#' initiate the build process. Instead, it returns the path to the temporary
#' directory containing the recipe.
#'
#' @param pkg Package name.
#' @param tree A `pkg_tree` object.
#' @param dry_run Enable dry run when `TRUE`.
#' @param log_dir Write logs to specified directory, defaulting to current
#'                directory.
#'
#' @export
conda_build <- function(pkg, tree, dry_run = FALSE, log_dir = getwd()) {
  check_string(pkg)
  check_class(tree, "pkg_tree")
  check_bool(dry_run)
  check_string(log_dir)

  build_dir <- create_build_dir(pkg, dry_run)
  custom <- lookup_custom(pkg)
  recipe <- create_recipe(pkg, tree, custom, build_dir)

  if (dry_run) {
    return(build_dir)
  } else {
    run_build(build_dir, recipe, log_dir)
    return(pkg)
  }
}

create_build_dir <- function(pkg, dry_run) {
  dir <- fs::path_join(c(tempdir(), pkg))
  fs::dir_create(dir)
  if (!dry_run) withr::defer_parent(fs::dir_delete(dir))
  dir
}

create_recipe <- function(pkg, tree, custom, dir) {
  qname <- qualified_names(pkg, tree)

  info <- pkg_info(tree, pkg, download = TRUE)

  repo <- info$Repository
  version <- info$Version
  version_safe <- sanitize_version(version)
  url <- glue::glue("{repo}/{pkg}_{version}.tar.gz")
  md5 <- info$MD5sum
  summary <- format_block(info$Title)
  description <- format_block(info$Description)
  home <- if (is.null(info$URL)) ""
          else glue::glue("  home: '{format_entry(info$URL)}'")
  license <- format_entry(info$License)

  if (info$NeedsCompilation == "yes") {
    compiler <- compiler_spec
    noarch <- ""
  } else {
    compiler <- ""
    noarch <- "  noarch: generic"
  }
  pkg_config <- if (info$NeedsCompilation == "yes") "pkg-config" else NULL
  custom_compiler <- format_deps(custom$compiler)

  r_deps <- pkg_deps(tree, pkg)
  r_vers <- pkg_versions(tree, r_deps)
  base_deps <- paste(qualified_names(r_deps, tree),
                     sanitize_version(r_vers),
                     sep = " >=")

  build_deps <- c(base_deps, custom$build_deps, pkg_config) |> format_deps()
  run_deps <- c(base_deps, custom$run_deps) |> format_deps()

  if (is.null(custom$script)) {
    script <- "  script: R CMD INSTALL --build ."
  } else {
    script <- ""
    writeLines(custom$script, fs::path_join(c(dir, "build.sh")))
  }

  content <- glue::glue(recipe_template)
  writeLines(content, fs::path_join(c(dir, "meta.yaml")))

  content
}

#' @importFrom stringr str_replace_all fixed
sanitize_version <- function(version) {
  str_replace_all(version, fixed("-"), ".")
}

#' @importFrom stringr str_replace_all str_squish
format_entry <- function(entry) {
  entry |>
    str_replace_all(fixed("\n"), " ") |>
    str_squish() |>
    quote_string()
}

recipe_description <- function(descr) {
  descr |>
    purrr::map_chr(\(l) paste0("    ", l)) |>
    paste(collapse = "\n")
}

#' @importFrom stringr str_length str_replace_all fixed
quote_string <- function(value) {
  str_replace_all(value, fixed("'"), "''")
}

#' @importFrom stringr str_split_1 fixed
format_block <- function(lines) {
  lines |>
    purrr::map(\(l) str_split_1(l, fixed("\n"))) |>
    unlist() |>
    str_prepend("    ") |>
    paste(collapse = "\n")
}

str_prepend <- function(strs, prefix) {
  paste0(prefix, strs)
}

format_deps <- function(deps) {
  lines <- purrr::map_chr(deps,  \(x) paste0("    - ", x))
  paste(lines, collapse = "\n")
}

#' Transform R package names in the format required by Conda.
#'
#' @param pkgs A vector of package names.
#' @param tree A `pkg_tree` object.
#' @return A vector of transformed package names.
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
{noarch}
{script}
  rpaths:
    - lib/R/lib/
    - lib/

requirements:
  build:
{compiler}
{custom_compiler}
    - liblzma-devel
    - zlib
{build_deps}
  host:
    - r-base
    - liblzma-devel
    - zlib
{build_deps}
  run:
    - r-base
{run_deps}

about:
  summary: >
{summary}
  description: >
{description}
{home}
  license: '{license}'

extra:
  recipe-maintainers:
    - gbrsales
"

compiler_spec <- "
    - autoconf
    - \"{{ compiler('c') }}\"
    - \"{{ compiler('cxx') }}\"
"

#' @importFrom cli cli_abort
run_build <- function(dir, recipe, log_dir) {
  pkg <- fs::path_file(dir)
  parent <- fs::path_dir(dir)

  log_file <- fs::path_join(c(log_dir, paste0(pkg, ".log")))
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
