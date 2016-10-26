utils::globalVariables("biocLite")
#' Install a package from CRAN, BioConductor, or GitHub
#'
#' @param mypkg the name of the package you want to install
#' @param type the type of the package, type 1 means the package comes from CRAN, type 2 packages are from BioConductor, type 3 packages are from GitHub and type 4 packages are by default part of R.
#' @examples
#' \dontrun{
#'   install_package("dplyr", 1)
#'   install_package("RDocumentation", 3)
#' }
#' 
#' @export
#' @importFrom githubinstall githubinstall
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
install_package <- function(mypkg, type){
  if (type == 1) {
    #CRAN
    install.packages(mypkg, repos = "http://cran.rstudio.com/");
  }
  else if (type == 2) {
    # bioconductor
    source("https://bioconductor.org/biocLite.R")
    if (!is.element(mypkg, installed.packages()[ ,1])) {
      biocLite(mypkg)
    } else {
      biocLite("BiocUpgrade")
    }
  } else if (type == 3) {
    githubinstall(mypkg)
  } else if (type == 4) {
    cat("RDocumentation cannot install this package, you need to upgrade your R installation")
  } else {
    cat("RDocumentation could not install this package; something went wrong.")
  }
} 