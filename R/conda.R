# Copyright (C) 2024-2025 Gabriele Sales
# MIT License

conda_artifact_dir <- function() {
  bld_dir <- Sys.getenv("CONDA_BLD_PATH")
  if (bld_dir != "") {
    return(bld_dir)
  }
  
  prefix <- Sys.getenv("CONDA_PREFIX")
  if (!fs::is_dir(prefix)) {
    cli_abort(c(
      "x" = "Conda environment is not active.",
      "i" = "Have you tried running {.code conda activate base}?"
    ))
  }

  fs::path_join(c(prefix, "conda-bld"))
}

conda_clear_build_dir <- function() {
  bld_dir <- fs::path_join(c(conda_artifact_dir(), "bld"))
  if (fs::is_dir(bld_dir)) {
    fs::dir_delete(bld_dir)
  }
}


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

  artifact_dir <- conda_artifact_dir()
  if (!fs::is_dir(artifact_dir)) {
    fs::dir_create(artifact_dir)
    index_channel(artifact_dir, log = NULL)
  }
  
  build_dir <- create_build_dir(pkg, artifact_dir, dry_run)
  custom <- lookup_custom(pkg)
  recipe <- create_recipe(pkg, tree, custom, build_dir)

  if (dry_run) {
    return(build_dir)
  } else {
    run_build(build_dir, recipe, log_dir)
    return(pkg)
  }
}

create_build_dir <- function(pkg, artifact_dir, dry_run) {
  dir <- fs::path_join(c(tempdir(), pkg))
  fs::dir_create(dir)
  fs::link_create(artifact_dir, fs::path_join(c(tempdir(), "output")))
  if (!dry_run) withr::defer_parent(fs::dir_delete(dir))
  dir
}

create_recipe <- function(pkg, tree, custom, dir) {
  qname <- qualified_names(pkg, tree)

  info <- pkg_info(tree, pkg, download = TRUE)

  repo <- info$Repository
  version <- info$Version
  version_safe <- conda_canonize_version(version)
  url <- glue::glue("{repo}/{pkg}_{version}.tar.gz")
  md5 <- info$MD5sum
  summary <- format_block(info$Title)
  description <- format_block(info$Description)
  home <- first_url(info$URL)
  home <- if (is.null(home)) ""
          else glue::glue("  homepage: '{quote_string(home)}'")

  # TODO: fix licence handling
  license <- ""
  # license <- format_license(info$License)

  if (info$NeedsCompilation == "yes") {
    compiler <- compiler_spec
    noarch <- ""
  } else {
    compiler <- ""
    noarch <- "  noarch: generic"
  }
  pkg_config <- if (info$NeedsCompilation == "yes") format_deps("pkg-config") else ""
  custom_compiler <- format_deps(custom$compiler)

  r_deps <- pkg_deps(tree, pkg)
  r_vers <- pkg_versions(tree, r_deps)
  base_deps <- paste(qualified_names(r_deps, tree),
                     conda_canonize_version(r_vers),
                     sep = " >=")

  build_deps <- c(base_deps, custom$build_deps) |> format_deps()
  run_deps <- c(base_deps, custom$run_deps) |> format_deps()

  if (is.null(custom$script)) {
    script <- "  script: R CMD INSTALL --build ."
  } else {
    script <- ""
    writeLines(custom$script, fs::path_join(c(dir, "build.sh")))
  }

  content <- glue::glue(recipe_template)
  writeLines(content, fs::path_join(c(dir, "recipe.yaml")))

  if (info$NeedsCompilation == "yes") {
    writeLines(build_config, fs::path_join(c(dir, "variants.yaml")))
  }

  content
}

#' @importFrom stringr str_replace_all fixed
conda_canonize_version <- function(version) {
  str_replace_all(version, fixed("-"), ".")
}

#' @importFrom stringr str_replace_all str_squish
format_entry <- function(entry) {
  entry |>
    str_replace_all(fixed("\n"), " ") |>
    str_squish() |>
    quote_string()
}

first_url <- function(value) {
  url <- value |> stringr::str_extract("^[^, \\s]+")
  p <- tryCatch(curl::curl_parse_url(url), error = \(e) NULL)
  if (is.null(p)) p else quote_string(url)
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
    purrr::map(quote_curly) |>
    str_prepend("    ") |>
    paste(collapse = "\n")
}

quote_curly <- function(str) {
  if (str_detect(str, fixed("{{"))) {
    paste0("{% raw %}", str, "{% endraw %}")
  } else {
    str
  }
}

str_prepend <- function(strs, prefix) {
  paste0(prefix, strs)
}

#' @importFrom stringr str_extract
format_license <- function(value) {
  value |> str_extract("^\\S+") |> quote_string()
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

context:
  build: 0

build:
  number: ${{{{ build|int + (microarch_level|int) * 100 }}}}
{noarch}
{script}

requirements:
  build:
    - if: microarch_level|int > 0
      then: x86_64-microarch-level ==${{{{ microarch_level }}}}
{compiler}
{custom_compiler}
{pkg_config}
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
  # license: '{license}'

extra:
  recipe-maintainers:
    - gbrsales
"

compiler_spec <- "
    - autoconf
    - ${{ compiler('c') }}
    - ${{ compiler('cxx') }}
"

build_config <- "
microarch_level:
  - if: not(unix and x86_64)
    then:
      - 0
    else:
      - 1
"

run_build <- function(dir, recipe, log_dir) {
  pkg_name <- fs::path_file(dir)
  work_dir <- fs::path_dir(dir)
  
  log <- file(fs::path_join(c(log_dir, paste0(pkg_name, ".log"))), open = "at")
  withr::defer(close(log))
  
  log_recipe(recipe, log)
  build_package(pkg_name, recipe, work_dir, log)
  index_channel(fs::path_join(c(work_dir, "output")), log)
}

log_recipe <- function(recipe, log) {
  writeLines(c("==> Recipe", recipe, ""), log)
}

build_package <- function(pkg_name, recipe, work_dir, log) {
  writeLines("==> Build Log", log)
  logger <- function(line, proc) writeLines(line, log)
  
  res <- processx::run(
    "rattler-build",
    c("build", "--recipe", pkg_name, "--wrap-log-lines", "false"),
    error_on_status = FALSE, wd = work_dir, stderr_to_stdout = TRUE,
    stdout = "|", stdout_line_callback = logger
  )
  writeLines("", log)

  if (res$status != 0) {
    cli_abort(c(
      "x" = "Build failed.",
      "i" = "There was an error building the {pkg_name} package."
    ))
  }
}

index_channel <- function(path, log) {
  args <- c("fs", "--write-zst", "false", "--write-shards", "false",
            "--quiet", ".")
  res <- processx::run(
    "rattler-index", args, error_on_status = FALSE, wd = path,
    stderr_to_stdout = TRUE, stdout = if (is.null(log)) "" else "|"
  )

  if (res$status != 0) {
    if (!is.null(log)) {
      writeLines(c("==> Indexing Log", res$stdout), log)
    }

    cli_abort(c(
      "x" = "Indexing failed.",
      "i" = "There was an error producing the channel index."
    ))
  }
}
