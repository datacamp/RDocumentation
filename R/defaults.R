#' Autoloads and overrides
#' 
#' Automatically load RDocumentation and have it override the default 
#' documentation?
#' 
#' When you load in the RDocumentation package, you'll be prompted to autoload 
#' the package whenever you start R, and whether you want to override the 
#' default help functionality with documentation from RDocumentation.org.
#' 
#' It does by modyfing the .RProfile file that is in your working directory.
#' 
#' With \code{enable_autoload} and \code{disable_autoload} you can update the
#' .RProfile file to enable or disable the automatic loading of RDocumentation
#' when you start R. With \code{enable_override} and \code{disable_override} you
#' can update the .RProfile file to enable or disable the overriding of the
#' default help viewer.
#' 
#' @name defaults

#' @export
#' @rdname defaults
enable_autoload <- function(){
  add_to_profile(autoload_line)
  return (invisible())
}

#' @export
#' @rdname defaults
makeDefault <- enable_autoload # for backwards compatibility

#' @export
#' @rdname defaults
disable_autoload <- function() {
  remove_from_profile(autoload_line)
}

#' @export
#' @rdname defaults
enable_override <- function() {
  add_to_profile(override_line)
  # Enable is in the current session as well!
  options(RDocs.override = TRUE)
}

#' @export
#' @rdname defaults
disable_override <- function() {
  remove_from_profile(override_line)
  # Disable it in the current session as well!
  options(RDocs.override = FALSE)
}


autoload_line <- "options(defaultPackages = c(getOption('defaultPackages'), 'RDocumentation'))"
override_line <- "options(RDocs.override = TRUE)"

is_autoload <- function() {
  is_in_profile(autoload_line)
}

is_override <- function() {
  getOption("RDocs.override", default = FALSE)
}

is_in_profile <- function(the_line) {
  the_line %in% readLines(get_r_profile())
}

add_to_profile <- function(the_line) {
  rprofile <- get_r_profile()
  lines <- readLines(rprofile)
  write(c(lines[lines != the_line], the_line), file = rprofile)
}

remove_from_profile <- function(the_line) {
  rprofile <- get_r_profile()
  lines <- readLines(rprofile)
  write(lines[lines != the_line], file = rprofile)
}

ask_questions <- function() {
  if (interactive()) {
    if (!is_autoload()) {
      ask_autoload()
    }
    if (!is_override()) {
      ask_override()
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

ask_override <- function(){
  # msg <- "When you load RDocumentation, shall I override the default ? and help functionality to show RDocs documentation instead? [y|n] "
  # ifelse(says_yes(msg), enable_override(), disable_override())
  enable_override() # don't ask, just do it!
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
