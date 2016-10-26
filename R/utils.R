rdocs_url <- "http://www.rdocumentation.org/"
rdocs_dir <- file.path(system.file(package = "RDocumentation"), "doc")
html_file <- file.path(rdocs_dir, "index.html")
cred_path <- file.path(system.file(package = "RDocumentation"), "config", "creds.txt")
  
#' Check if a package is installed for the user.
#'
#' @param pkg Name of the package
#' @param version the latest version to be checked
#' 
#' @return 1 if the package is not installed; -1 if the package is not up to date; 0 if the package if the package is installed and up to date.
#' 
#' @examples
#' \dontrun{
#' check_package("RDocumentation","0.2")
#' check_package("utils","3.3.1")
#' }
#' 
#' @export
#' @importFrom utils packageVersion
check_package <- function(pkg, version) {
  if (!(pkg %in% installed.packages()[ ,1])) {
    return (1)
  } else {
    if (packageVersion(pkg) >= version) {
      return(0)
    } else {
      return(-1)
    }
  }
}


#' Redirects the viewer to the RDocumentation help page.
#'
#' @export
#' @importFrom utils tail
hideViewer <- function(){
  help(package = "RDocumentation")
}

#' Get package URL
#' 
#' @param url The url to get the package for
#' 
#' @export
get_package_from_URL <- function(url){
  parsing <- substring(url, 18, nchar(url) - 18)
  parts <- strsplit(parsing, "/")
  return (as.character(parts[[1]][3]))
}

get_r_profile <- function(){
  if (!file.exists(file.path(Sys.getenv("HOME"),".Rprofile"))) {
    file.create(file.path(Sys.getenv("HOME"), ".Rprofile"), quiet=TRUE)
  }
  Rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")
  return (Rprofile)
}

concat <- function(x) {
  paste(x, collapse = ",")
}
