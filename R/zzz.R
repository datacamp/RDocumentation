.onAttach <- function(libName,pkgName) {
  options(RDocumentation.timeOut = 3)
  options(help_type = "html")
  
  if (interactive()) {
    ask_questions()
  } else {
    # just do the override
    enable_override()
  }
}

