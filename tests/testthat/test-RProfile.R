context("get_r_profile")

test_that("Rprofile_file_exists",{
  get_r_profile()
	expect_that(file.exists(file.path(Sys.getenv("HOME"),".Rprofile")), is_true())
})

test_that("is_home_directory",{
	expect_equal(get_r_profile(),file.path(Sys.getenv("HOME"),".Rprofile"))
})