library(Rdocumentation)
context("help")

test_that("help_non_installed_package",{
	expect_equal(help(centre,ftsa),NULL)
	expect_equal(help(package=ftsa),NULL)
})
