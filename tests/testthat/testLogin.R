library(RDocumentation)
context("login")

test_that("can_login",{
	dir.create(paste0(find.package("RDocumentation"),"/config"),showWarnings = FALSE,recursive=TRUE)
	write('username=accountForTestsRdocumentation&password=TestingRdocumenation123', file = paste0(find.package("RDocumentation"),'/config/creds.txt'))
	expect_that(login(), prints_text("logging you in to RDocumentation"))
})

test_that("bad_credentials",{
	dir.create(paste0(find.package("RDocumentation"),"/config"),showWarnings = FALSE,recursive=TRUE)
	write('username=accountForTestsRdocumentation&password=Testingrdocumenation123', file = paste0(find.package("RDocumentation"),'/config/creds.txt'))
	expect_that(login(), prints_text("there is something wrong with your credentials, please try logging in to the site in the help panel"))
})

# test_that("wrong_address",{
# 	#overwrite the url, to make a connection error happen
# 	assign("rdocs_url",function(){
# 		return ("")
# 	},envir=environment(login))
# 	dir.create(paste0(find.package("Rdocumentation"),"/config"),showWarnings = FALSE,recursive=TRUE)
# 	write('username=accountForTestsRdocumentation&password=TestingRdocumenation123', file = paste0(find.package("Rdocumentation"),'/config/creds.txt'))
# 	expect_that(login(), prints_text("Could not log you in, something is wrong with your internet connection or Rdocumentation is offline"))
# })