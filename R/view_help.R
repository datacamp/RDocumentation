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
  
  # create doc directory if doesn't exist yet
  dir.create(rdocs_dir, showWarnings = FALSE)
  
  if ( exists("package_not_local", envir = prototype)) {
    package_not_local <- prototype$package_not_local
  } else {
    package_not_local <- ""
  }
  assign("package_not_local", "", envir = prototype)
  
  tryCatch({
    go_to_url <- paste0(rdocs_url, "rstudio/view?viewer_pane=1")
    resp <- POST(go_to_url,
                 add_headers(Accept = "text/html"),
                 config = (content_type_json()),
                 body = rjson::toJSON(body),
                 encode = "json",
                 timeout(getOption("RDocumentation.timeOut")))
    
    if (status_code(resp) == 200) {
      writeBin(content(resp, "raw"), html_file)
      browser <- getOption("browser")
      p <- tools::startDynamicHelp(NA)
      url <- build_local_url(p)
      browseURL(url, browser)
      return(invisible())
    } else{
      stop("bad return status")
    }
  },
  error = function(e){
    print(e)
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

#' @importFrom httr parse_url
build_local_url <- function(p) {
  url <- sprintf("http://127.0.0.1:%s/library/RDocumentation/doc/index.html", p)
  append <- "?viewer_pane=1"
  rstudio_port <- Sys.getenv("RSTUDIO_SESSION_PORT")
  if (nchar(rstudio_port) > 0) {
    append <- c(append, paste0("Rstudio_port=", rstudio_port))
  }
  shared_secret <- Sys.getenv("RS_SHARED_SECRET")
  if (nchar(shared_secret) > 0) {
    append <- c(append, paste0("RS_SHARED_SECRET=", shared_secret))
  }
  if (file.exists(cred_path) && file.info(cred_path)$size > 0) {
    creds <- paste0(readLines(cred_path), collapse = "")
    comps <- parse_url(paste0("?", creds))$query[c("username", "password")]
    append <- c(append, paste0(names(comps), "=", unlist(comps, use.names = FALSE)))
  }
  url <- paste0(url, paste0(append, collapse = "&"))
  return(url)
}