context("help.search")

test_that("help_search",{
	expect_equal(help.search("whatever"),invisible())
	expect_equal(help.search("help"),invisible())
});

test_that("test_with_fields",{
	expect_equal(help.search("help",fields=c("title")),invisible())
	expect_equal(help.search("this",fields=c("title","alias")),invisible())
	expect_equal(help.search("testing",fields=c("title","alias","concept")),invisible())
})