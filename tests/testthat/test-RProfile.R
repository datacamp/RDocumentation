context("getRProfile")

test_that("Rprofile_file_exists",{
	getRProfile()
	expect_that(file.exists(file.path(Sys.getenv("HOME"),".Rprofile")), is_true())
})

test_that("is_home_directory",{
	expect_equal(getRProfile(),file.path(Sys.getenv("HOME"),".Rprofile"))
})