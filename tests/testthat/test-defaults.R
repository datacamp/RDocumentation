context("defaults")

test_that("autoloads",{
  disable_autoload()
  expect_false(autoload_line %in% readLines(get_r_profile()))
  enable_autoload()
  expect_true(autoload_line %in% readLines(get_r_profile()))
  disable_autoload()
  expect_false(autoload_line %in% readLines(get_r_profile()))
})

test_that("overrides", {
  disable_override()
  expect_false(override_line %in% readLines(get_r_profile()))
  enable_override()
  expect_true(override_line %in% readLines(get_r_profile()))
  disable_override()
  expect_false(override_line %in% readLines(get_r_profile()))
})
