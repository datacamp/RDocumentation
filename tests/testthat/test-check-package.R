context("check_package")

test_that("check_base_package",{
	expect_equal(check_package("base", "0.0.0"), 0)
})

test_that("check_bad_version_base",{
  expect_equal(check_package("base", "5.6.0"), -1)
})

test_that("check_non-existing-package",{
	expect_equal(check_package("non-existing-package", my_version), 1)
})


