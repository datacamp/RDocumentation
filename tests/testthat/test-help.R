context("help")

options(RDocs.override = TRUE)

test_that("help works", {
  # package + topic exists: base::mean
  with_mock(
    view_help = identity,
    expect_equal(help(mean, base), list(packages = "base",
                                        topic_names = "mean",
                                        topic = "mean",
                                        called_function = "help")),
    expect_equal(help(mean), list(packages = "base",
                                  topic_names = "mean",
                                  topic = "mean",
                                  called_function = "help"))
  )
  
  # package exists, but topic not in there: utils::mean
  with_mock(
    view_help = identity,
    expect_equal(help(mean, utils), list(packages = "utils",
                                         topic_names = "",
                                         topic = "mean",
                                         called_function = "help"))
  )
  
  # package nor topic exists: asdfasdf::asdf
  with_mock(
    view_help = identity,
    expect_equal(help(asdf, asdfasdf), list(packages = "asdfasdf",
                                            topic_names = "",
                                            topic = "asdf",
                                            called_function = "help"))
  )
})

test_that("help.search works", {
  with_mock(
    view_help = identity,
    expect_true(is.list(help.search("mean"))),
    expect_equal(help.search("mean")$pattern, "mean"),
    expect_equal(help.search("mean")$fields, "aliases,concept,title"),
    expect_equal(help.search("mean")$type, "regexp"),
    expect_equal(help.search("mean")$agrep, NULL),
    expect_true(help.search("mean")$ignore.case),
    expect_true(is.character(help.search("mean")$matching_titles)),
    expect_true(is.character(help.search("mean")$matching_packages))
  )
  
  with_mock(
    view_help = identity,
    expect_true(is.list(help.search("help", fields= "title"))),
    expect_equal(help.search("help", fields = "title")$pattern, "help"),
    expect_equal(help.search("help", fields = "title")$fields, "title"),
    expect_equal(help.search("help", fields = "title")$type, "regexp"),
    expect_equal(help.search("help", fields = "title")$agrep, NULL),
    expect_true(help.search("help", fields = "title")$ignore.case),
    expect_true(is.character(help.search("help", fields = "title")$matching_titles)),
    expect_true(is.character(help.search("help", fields = "title")$matching_packages))
  )
  
  with_mock(
    view_help = identity,
    expect_true(is.list(help.search("help", fields= c("title", "alias")))),
    expect_equal(help.search("help", fields = c("title", "alias"))$pattern, "help"),
    expect_equal(help.search("help", fields = c("title", "alias"))$fields, "title,aliases"),
    expect_equal(help.search("help", fields = c("title", "alias"))$type, "regexp"),
    expect_equal(help.search("help", fields = c("title", "alias"))$agrep, NULL),
    expect_true(help.search("help", fields = c("title", "alias"))$ignore.case),
    expect_true(is.character(help.search("help", fields = c("title", "alias"))$matching_titles)),
    expect_true(is.character(help.search("help", fields = c("title", "alias"))$matching_packages))
  )
})

options(RDocs.override = FALSE)