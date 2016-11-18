.onAttach <- function(libName,pkgName) {
  options(RDocumentation.timeOut = 3)
  options(help_type = "html")
  enable_rdocs()
  ask_questions()
}

