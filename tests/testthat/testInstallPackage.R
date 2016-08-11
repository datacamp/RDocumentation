library(Rdocumentation)
context("install_package")

test_that("install_package_cran",{
	install_package("ash",1)
	test_that("ash" %in% rownames(installed.packages()),is_true())
})

# no automated test for bioconductor, as this requires user-interaction
# test_that("install_package_bioconductor",{
# 	install_package("AnnotationDbi",2)
# 	test_that("AnnotationDbi" %in% rownames(installed.packages()),is_true())
# })

# no automated test for github, as this requires user-interaction
# test_that("install_package_github",{
# 	install_package("forcats",3)
# 	test_that("forcats" %in% rownames(installed.packages()),is_true())
# })

test_that("install_package_part_of_R",{
	test_that(install_package("utils",4), prints_text("Can not install this package, you need to upgrade your R installation"))
	test_that("utils" %in% rownames(installed.packages()),is_true())
})