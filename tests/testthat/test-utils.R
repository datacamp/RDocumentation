context("utils")

test_that("get_package_from_URL works", {
  url <- "https://www.rdocumentation.org/packages/base/versions/3.3.1/topics/mean"
  expect_equal(get_package_from_URL(url), "base")
})

test_that("check_base_package",{
  expect_equal(check_package("base", "0.0.0"), 0)
  expect_equal(check_package("base", "5.6.0"), -1)
  expect_equal(check_package("non-existing-package", my_version), 1)
})


