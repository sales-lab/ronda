# Copyright (C) 2024-2025 Gabriele Sales
# MIT License


#' Detect the microarchitecture of the host
#'
#' @return The integer representing the microarchitecture level.
#'
#' @export
microarch_detect <- function() {
  value <- get0("value", envir = the_microarch)
  if (is.null(value)) {
    value <- callr::r(query_microarch)
    if (is.null(value)) {
      cli_abort(c(
        "x" = "Failed to detect the microarchitectural level of the host.",
        "i" = "Have you installed {.pkg conda-build}?"
      ))
    }

    assign("value", value, envir = the_microarch)
  }

  value
}

query_microarch <- function() {
  res <- try({
    path <- Sys.which("python")
    reticulate::use_python(path)

    reticulate::py_run_string("
import archspec.cpu

name_mapping = {
  'x86_64': 1,
  'x86_64_v2': 2,
  'x86_64_v3': 3,
  'x86_64_v4': 4,
  'ppc64le': 8,
  'power8le': 8,
  'power9le': 9,
  'power10le': 10,
}

levels = {}
for arch_name, arch in archspec.cpu.TARGETS.items():
    if arch.family.name not in ('x86_64', 'ppc64le'):
        continue
    levels[(arch_name, arch.family.name)] = max(
        1,
        name_mapping.get(arch_name, 0),
        *(name_mapping.get(parent.name, 0) for parent in (arch.ancestors or ()))
    )

levels = {arch[0]: level for arch, level in sorted(
    levels.items(), key=lambda kv: kv[1])}

level = levels[archspec.cpu.host().name]
    ")
  })

  if (inherits(res, "try-error")) NULL else reticulate::py$level
}

the_microarch <- new.env(parent = emptyenv())
