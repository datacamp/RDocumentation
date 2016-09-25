context("help")

test_that("help_normal",{
	# expect_equal(help(mean,base),invisible())
	# expect_equal(help(package=utils),invisible())
})

test_that("help_non_installed_package",{
	# RDocumentation will not find this package
	expect_error(help(package="non_installed"))
})

test_that("help_topic_non_installed_package",{
	# RDocumenation will not find the package, but can still give similar topics, contrary to the local function that would just throw an error
	expect_equal(help("no_topic","non_installed"),invisible())
})

