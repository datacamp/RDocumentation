#' View the help
#' 
#' @param body body of the POST request to RDocumentation
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
#' @importFrom httr user_agent
#' @importFrom httr content_type_json
#' @importFrom httr timeout
#' @importFrom httr cookies
#' @importFrom httr add_headers
#' @importFrom rjson toJSON
#' @importFrom utils browseURL
#' @importFrom utils read.table
view_help <- function(body){
  # create doc directory if doesn't exist yet
  dir.create(rdocs_dir, showWarnings = FALSE)
  go_to_url <- paste0(rdocs_url, "rstudio/view?viewer_pane=1")
  resp <- POST(go_to_url,
               add_headers(Accept = "text/html"),
               user_agent("rstudio"),
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
  # If in RStudio, send along creds.
  if (nchar(Sys.getenv("RSTUDIO")) > 0 && file.exists(cred_path) && file.info(cred_path)$size > 0) {
    creds <- paste0(readLines(cred_path), collapse = "")
    if (creds != "") {
      comps <- parse_url(paste0("?", creds))$query[c("sid")]
      append <- c(append, paste0(names(comps), "=", unlist(comps, use.names = FALSE)))
    }
  }
  url <- paste0(url, paste0(append, collapse = "&"))
  return(url)
}