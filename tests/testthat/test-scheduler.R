expect_schedule <- function(sched, turns) {
  sched <- purrr::reduce(turns, .init = sched, \(sched, pkgs) {
    expect_setequal(buildable_pkgs(sched), pkgs)
    mark_built_pkgs(sched, pkgs)
  })
  expect_length(buildable_pkgs(sched), 0)
}

test_that("independent packages build in parallel", {
  deps <- list(a = character(), b = character())
  tree <- packages_for_test(deps)
  sched <- build_schedule(tree)
  expect_schedule(sched, list(c("a", "b")))
})

test_that("dependent packages build sequentially", {
  deps <- list(a = character(), b = "a")
  tree <- packages_for_test(deps)
  sched <- build_schedule(tree)
  expect_schedule(sched, list("a", "b"))
})

test_that("up to date packages are not rebuilt", {
  deps <- list(
    a = character(),
    b = character(),
    c = c("a", "b")
  )
  tree <- packages_for_test(deps)
  sched <- build_schedule(tree) |> mark_pkgs_up_to_date("a")
  expect_schedule(sched, list("b", "c"))
})

test_that("up to date packages are rebuild because of dependencies", {
  deps <- list(
    a = character(),
    b = character(),
    c = c("a", "b"),
    d = "c",
    e = "c"
  )
  tree <- packages_for_test(deps)
  sched <- build_schedule(tree) |> mark_pkgs_up_to_date(c("a", "b"))
  expect_schedule(sched, list("c", c("d", "e")))
})

test_that("failed builds are not retried", {
  deps <- list(
    a = character(),
    b = "a",
    c = "b"
  )
  tree <- packages_for_test(deps)

  sched <- build_schedule(tree) |> mark_failed_pkgs("b")
  expect_equal(buildable_pkgs(sched), "a")

  sched <- mark_built_pkgs(sched, "a")
  expect_equal(buildable_pkgs(sched), character())
})
