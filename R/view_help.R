#' View the help
#' 
#' @param body body of the POST request to RDocumentation
#' @param arg1 Same as arg1 of utils function
#' @param arg2 Same as arg2 of utils function
#' 
#' @details Leverage https://www.rdocumentation.org/packages/tools/versions/3.3.1/topics/startDynamicHelp to render html page into the help pane
#' As the page are rendered by the internal RStudio Server, it tricks RStudio into thinking that page is from the same origin as the other elements
#' in RStudio IDE, which allows us to communicate from the javascript to the /rpc/... endpoint 
#' 
#' @export
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom httr content_type_json
#' @importFrom httr timeout
#' @importFrom httr cookies
#' @importFrom httr add_headers
#' @importFrom rjson toJSON
#' @importFrom utils browseURL
#' @importFrom utils read.table
view_help <- function(body, arg1, arg2){
  go_to_url <- paste0(rdocs_url, "rstudio/view?viewer_pane=1")
  rdocs_dir <- file.path(system.file(package = "RDocumentation"), "doc")
  if(!dir.exists(rdocs_dir)) dir.create(rdocs_dir)
  html_file <- file.path(rdocs_dir, "index.html")
  if ( exists("package_not_local", envir = prototype)) {
    package_not_local = prototype$package_not_local
  } else {
    package_not_local = ""
  }
  assign("package_not_local", "", envir = prototype)
  tryCatch({
    r <- POST(go_to_url, add_headers(Accept = "text/html"), config = (content_type_json()), body = rjson::toJSON(body), encode = "json", timeout(getOption("RDocumentation.timeOut")))
    if (status_code(r) == 200) {
      cred_path <- file.path(rdocs_dir, "config", "creds.txt")
      if (file.exists(cred_path) && file.info(cred_path)$size > 0) {
        creds <- as.character(read.table(cred_path, header = FALSE)$V1)
      } else {
        creds <- ""
      }
      writeBin(content(r, "raw"), html_file)
      p <- tools::startDynamicHelp(NA)
      browser <- getOption("browser")
      url <- paste0("http://127.0.0.1:", p, "/library/RDocumentation/doc/index.html?viewer_pane=1&Rstudio_port=",
                    as.character(Sys.getenv("RSTUDIO_SESSION_PORT")), "&RS_SHARED_SECRET=", as.character(Sys.getenv("RS_SHARED_SECRET")), "&", creds)
      browseURL(url, browser)
      return(invisible())
    } else{
      stop("bad return status")
    }
  },
  error = function(e){
    if (package_not_local != "") {
      stop(paste0("package ", package_not_local, " is not in your local library"))
    }
    if (body$called_function == "help" || body$called_function == "help_search") {
      return(baseenv()$`class<-`(arg1, arg2))
    } else if (body$called_function == "find_package") {
      #this line will throw an error if the package does not exist before falling back on the original help function
      base::find.package(get_package_from_URL(arg1))
      return(utils::browseURL(arg1, arg2))
    } else {
      stop(e)
    }
  })
}
