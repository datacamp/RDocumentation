library(RDocumentation)
context("help.search")

test_that("help_search",{
	expect_equal(help.search("whatever"),NULL)
	expect_equal(help.search("help"),NULL)
});

test_that("test_with_fields",{
	expect_equal(help.search("help",fields=c("title")),NULL)
	expect_equal(help.search("this",fields=c("title","alias")),NULL)
	expect_equal(help.search("testing",fields=c("title","alias","concept")),NULL)
})