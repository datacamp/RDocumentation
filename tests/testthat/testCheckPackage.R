library(RDocumentation)
context("check_package")

test_that("check_base_package",{
	expect_equal(check_package("base","3.3.1"),0)
})

test_that("check_non-existing-package",{
	expect_equal(check_package("non-existing-package",3),1)
})

test_that("check_bad_version_base",{
	expect_equal(check_package("base","3.3.0"),-1)
})

test_that("check_updated_version_base",{
	expect_equal(check_package("base","3.4.0"),0)
})