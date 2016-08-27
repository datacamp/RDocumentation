library(Rdocumentation)
context("help")

test_that("help_normal",{
	expect_equal(help(mean,base),NULL)
	expect_equal(help(package=utils),NULL)
})

test_that("help_non_installed_package",{
	expect_equal(help(package="non_installed"),NULL)
})

test_that("help_topic_non_installed_package",{
	expect_equal(help("no_topic","non_installed"),NULL)
})
