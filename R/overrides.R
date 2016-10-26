#' Documentation on RDocumentation or via the normal help system if offline
#'
#' Wrapper functions around the default help functions from the \code{utils} package. If online, you'll be redirected to RDocumentation. If you're offline, you'll fall back onto your locally installed documentation files.
#'
#' @param ... the arguments you'd pass to the default \code{utils} function with the same name
#'
#' @details for slow internet connections, a timeout can be set for getting the page of RDocumentation via \code{options("RDocumentation.timeOut" = nb_of_seconds)} the default timeout is 3 seconds.
#' @name documentation

#' @rdname documentation
#' @export
#' @importFrom proto proto
#' @importFrom utils help
help <- function(...) {
  mc <- match.call(utils::help)
  topic <- as.character(mc$topic)
  package <- as.character(mc$package)
  paths <- tryCatch({
    utils::help(...)  
  }, error = function(e) {
    if (grepl("there is no package called", e$message)) {
      return(character(0))
    } else {
      stop(e)
    }
  })
  tryCatch({
    if (!isTRUE(is_override())) {
      stop("rdocs not active")
    }
    get_help(paths, package, topic)
  }, error = function(e) {
    print(e)
    paths
  })
}

#' @rdname documentation
#' @export
`?` <- function(...){
  paths <- utils::`?`(...)
  tryCatch({
    if (!isTRUE(is_override())) {
      stop("rdocs not active")
    }
    get_help(paths)
  }, error = function(e) {
    paths
  })
}

#' @rdname documentation
#' @export
#' @importFrom utils help.search
help.search <- function(...) {
  paths <- utils::help.search(...)
  tryCatch({
    if (!isTRUE(is_override())) {
      stop("rdocs not active")
    }
    get_help_search(paths)
  }, error = function(e) {
    paths
  })
}

get_help_search <- function(paths) {
  lut <- c(alias = "aliases", concept = "concept", keyword = "keywords", name = "name", title = "title")
  body <- paths
  body$fields <- concat(lut[body$fields])
  body$matching_titles <- concat(unique(body$matches$Topic))
  body$matching_packages <- concat(unique(body$matches$Package))
  body$called_function <- "help_search"
  body[c("lib.loc", "matches", "types", "package")] <- NULL
  view_help(body)
}

get_help <- function(paths, package = "", topic = "") {
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
  view_help(body)
}



