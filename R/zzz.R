.onAttach <- function(libName,pkgName){
    options(RDocumentation.timeOut = 3)
    options(help_type = "html")
    Rprofile <- getRProfile()
    names <- scan(Rprofile, what=character(),quiet=TRUE)
    if (!any(grepl("Rdocumentation", names))) {
       #view_help(list(called_function="make_default"),"","")
    }
}