.onLoad <- function(libName,pkgName){
    op <- options()
    op.Rdocumentation <- list(
        Rdocumentation.timeOut = 3
    )
    toset <- !(names(op.Rdocumentation) %in% names(op))
    if(any(toset)) options(op.Rdocumentation[toset])
    login()
    Rprofile <- .getRProfile()
    names <- scan(Rprofile, what=character(),quiet=TRUE)
    if (length(grep("Rdocumentation",names)) == 0){
        .view_help(paste0(rdocs_url(),"rstudio/make_default?viewer_pane=1"),"DEFAULT",FALSE,"","")
    }
}