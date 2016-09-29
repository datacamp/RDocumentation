context("help")

options(RDocs.override = TRUE)

# test_that("help_normal",{
#   expect_equal(help(mean), NULL)
# 	expect_equal(help(mean,base), NULL)
# })

test_that("help_non_installed_package",{
	# RDocumentation will not find this package
	expect_error(help(package = "non_installed"))
})

test_that("help_topic_non_installed_package",{
	# RDocumenation will not find the package, but can still give similar topics, contrary to the local function that would just throw an error
	expect_equal(help("no_topic","non_installed"), NULL)
})

test_that("help_search",{
  expect_equal(help.search("whatever"), NULL)
  expect_equal(help.search("help"), NULL)
})

test_that("test_with_fields",{
  expect_equal(help.search("help",fields=c("title")), NULL)
  expect_equal(help.search("this",fields=c("title","alias")), NULL)
  expect_equal(help.search("testing",fields=c("title","alias","concept")), NULL)
})

options(RDocs.override = FALSE)