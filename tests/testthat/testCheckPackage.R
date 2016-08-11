library(Rdocumentation)
context("check_package")

test_that("check_base_package",{
	expect_equal(check_package("base"), (packageVersion("base")))
})

test_that("check_non-existing-package",{
	expect_equal(check_package("non-existing-package"),FALSE)
})