.onLoad <- function(libName,pkgName){
    op <- options()
    op.Rdocumentation <- list(
        Rdocumentation.timeOut = 3
    )
    toset <- !(names(op.Rdocumentation) %in% names(op))
    if(any(toset)) options(op.Rdocumentation[toset])
    Rprofile <- .getRProfile()
    names <- scan(Rprofile, what=character(),quiet=TRUE)
    if (length(grep("Rdocumentation",names)) == 0){
        .view_help(list(called_function="make_default"),"","")
    }
}