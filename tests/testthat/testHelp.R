library(Rdocumentation)
context("help")

test_that("help_non_installed_package",{
	test_env()
	expect_equal(help(centre,ftsa),NULL)
	expect_equal(help(package=ftsa),NULL)
})

test_that