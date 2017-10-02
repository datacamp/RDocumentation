context("defaults")

test_that("overrides", {
  disable_rdocs()
  expect_false(rdocs_active())
  enable_rdocs()
  expect_true(rdocs_active())
  disable_rdocs()
  expect_false(rdocs_active())
})

disable_autoload()
disable_rdocs()