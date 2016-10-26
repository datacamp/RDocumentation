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
  paths <- utils::help(...)
  tryCatch({
    if (!isTRUE(is_override())) {
      stop("rdocs not active")
    }
    get_help(paths, as.character(match.call(utils::help)$package))
  }, error = function(e) {
    paths
  })
}

#' @rdname documentation
#' @export
#' @importFrom proto proto
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
#' @importFrom proto proto
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

get_help <- function(paths, package = "") {
  if (!length(paths)) {
    # no documentation found locally - figure out package name from help call, if any
    packages <- if (length(package) == 0) "" else package
    topic_names <- ""
  } else {
    # documentation was found
    split <- strsplit(paths, "/")
    packages <- sapply(split, function(x) return(x[length(x)-2]))
    topic_names <- sapply(split, tail, n = 1)
  }
  
  body <- list(packages = concat(packages),
               topic_names = concat(topic_names),
               topic = attributes(paths)$topic,
               called_function = "help")
  view_help(body)
}





