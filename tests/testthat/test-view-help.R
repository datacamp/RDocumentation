context("view_help")

test_that("normal use", {
  expect_that(view_help(body = list(packages = "base",
                                    topic_names = "mean",
                                    topic = "mean",
                                    tried_all_packages = FALSE,
                                    help_type = "html",
                                    called_function = "help"),
                        arg1 = ""))
})
