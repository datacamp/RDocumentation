library(Rdocumentation)
context("login")

test_that("can_login",{
	dir.create(paste0(find.package("Rdocumentation"),"/config"),showWarnings = FALSE,recursive=TRUE)
	write('username=accountForTestsRdocumentation&password=TestingRdocumenation123', file = paste0(find.package("Rdocumentation"),'/config/creds.txt'))
	expect_that(login(), prints_text("logging you in to RDocumentation"))
})

test_that("bad_credentials",{
	dir.create(paste0(find.package("Rdocumentation"),"/config"),showWarnings = FALSE,recursive=TRUE)
	write('username=accountForTestsRdocumentation&password=Testingrdocumenation123', file = paste0(find.package("Rdocumentation"),'/config/creds.txt'))
	expect_that(login(), prints_text("there is something wrong with your credentials, please try logging in to the site in the help panel"))
})