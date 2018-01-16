rdocs_url <- "http://www.rdocumentation.org/"
get_rdocs_dir <- function() {
  file.path(system.file(package = "RDocumentation"), "doc")
}
get_html_file <- function() {
  file.path(get_rdocs_dir(), "index.html")
}

autoload_line_old <- "options(defaultPackages = c(getOption('defaultPackages'), 'RDocumentation'))"
autoload_line <- paste("if(isTRUE('RDocumentation' %in% rownames(utils::installed.packages())))", autoload_line_old)

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
#' @importFrom utils packageVersion installed.packages
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
#' @importFrom utils tail
hideViewer <- function(){
  help(package = "RDocumentation")
}

# Get package from url
get_package_from_URL <- function(url){
  parsing <- substring(url, 18, nchar(url) - 18)
  parts <- strsplit(parsing, "/")
  return (as.character(parts[[1]][3]))
}

get_r_profile <- function(){
  Rprofile <- file.path(Sys.getenv("HOME"), ".Rprofile")
  return (file(Rprofile, "w+"))
}

concat <- function(x) {
  paste(x, collapse = ",")
}

is_in_profile <- function(targets) {
  if (!file.exists(file.path(Sys.getenv("HOME"),".Rprofile"))) {
    return (FALSE)
  }
  any(targets %in% readLines(get_r_profile()))
}

add_to_profile <- function(the_line, old_lines = "") {
  rprofile <- get_r_profile()
  lines <- readLines(rprofile)
  # Keep all non-matching lines and append line
  write(c(lines[!lines %in% c(the_line, old_lines)], the_line), file = rprofile)
}

remove_from_profile <- function(the_line, old_lines = "") {
  if (!file.exists(file.path(Sys.getenv("HOME"),".Rprofile"))) {
    return ()
  }
  rprofile <- get_r_profile()
  lines <- readLines(rprofile)
  # Keep all non-matching lines
  write(lines[!lines %in% c(the_line, old_lines)], file = rprofile)
}

says_yes <- function(msg) {
  yesses <- c("y", "yes", "yeah", "sure", "ok")
  nos <- c("n", "no", "not", "nah", "none", "non")
  resp <- ""
  while (!(tolower(resp) %in% c(yesses, nos))) {
    resp <- readline(msg)
  }
  if (tolower(resp) %in% c(yesses)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

