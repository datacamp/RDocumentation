get_help_search_body <- function(paths) {
  lut <- c(alias = "aliases", concept = "concept", keyword = "keywords", name = "name", title = "title")
  body <- paths
  body$fields <- concat(lut[body$fields])
  body$matching_titles <- concat(unique(body$matches$Topic))
  body$matching_packages <- concat(unique(body$matches$Package))
  body$called_function <- "help_search"
  body[c("lib.loc", "matches", "types", "package")] <- NULL
  body
}

get_help_body <- function(paths, package = "", topic = "") {
  if (!length(paths)) {
    # no documentation found locally, use specified package and topic names
    packages <- if (length(package) == 0) "" else package
    topic_names <- ""
    topic <- if (length(topic) == 0) "" else topic
  } else {
    # documentation was found
    split <- strsplit(paths, "/")
    packages <- sapply(split, function(x) return(x[length(x)-2]))
    topic_names <- sapply(split, tail, n = 1)
    topic <- attr(paths, "topic")
  }
  body <- list(packages = concat(packages),
          topic_names = concat(topic_names),
          topic = topic,
          called_function = "help")
  body
}

get_find_package_body <- function(package) {
  list(called_function = "find_package", package_name = package)
}

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
  dir.create(get_rdocs_dir(), showWarnings = FALSE)
  go_to_url <- paste0(rdocs_url, "rstudio/view?viewer_pane=1")
  resp <- POST(go_to_url,
               add_headers(Accept = "text/html"),
               user_agent("rstudio"),
               config = (content_type_json()),
               body = rjson::toJSON(body),
               encode = "json",
               timeout(getOption("RDocumentation.timeOut")))
  if (status_code(resp) == 200) {
    writeBin(content(resp, "raw"), get_html_file())
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
  url <- paste0(url, paste0(append, collapse = "&"))
  return(url)
}