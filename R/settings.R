#' Settings
#' 
#' Automatically load RDocumentation and make it active, so that it overrides the default 
#' documentation?
#' 
#' When you load in the RDocumentation package, you'll be prompted to autoload 
#' the package whenever you start R. When RDocumentation is loaded, it will automatically
#' override the default help functionality with documentation from RDocumentation.org.
#' 
#' The autoloading is done by modifying the .RProfile file that is in your working directory.
#' 
#' With \code{enable_autoload} and \code{disable_autoload} you can update the
#' .RProfile file to enable or disable the automatic loading of RDocumentation
#' when you start R. With \code{enable_rdocs} and \code{disable_rdocs} you can enable or 
#' disable the overriding of the default help viewer.
#' 
#' @name defaults

#' @export
#' @rdname defaults
enable_autoload <- function(){
  add_to_profile(autoload_line, autoload_line_old)
  return (invisible())
}

#' @export
#' @rdname defaults
makeDefault <- enable_autoload # for backwards compatibility

#' @export
#' @rdname defaults
disable_autoload <- function() {
  remove_from_profile(autoload_line, autoload_line_old)
}

is_autoload <- function() {
  is_in_profile(c(autoload_line, autoload_line_old))
}

ask_questions <- function() {
  if (interactive()) {
    if (!is_autoload()) {
      ask_autoload()
    }
  }
}

ask_autoload <- function() {
  msg <- "Do you want to automatically load RDocumentation when you start R? [y|n] "
  if (says_yes(msg)) {
    enable_autoload()
    message(paste("Congratulations!",
                  "R will now use RDocumentation to display your help files.",
                  "If you're offline, R will just display your local documentation.",
                  "To avoid automatically loading the RDocumentation package, use disable_autoload().", 
                  "If you don't want the ? and help functionality to show RDocumentation pages, use disable_override().",
                  sep = "\n"))
  } else {
    disable_autoload()
  }
}

## Whether or not to enable RDocs

#' @export
#' @rdname defaults
enable_rdocs <- function() {
  options(rdocs_active = TRUE)
}

#' @export
#' @rdname defaults
disable_rdocs <- function() {
  options(rdocs_active = FALSE)
}

rdocs_active <- function() {
  getOption("rdocs_active", default = FALSE)
}
