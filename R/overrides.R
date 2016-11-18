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
  package <- as.character(mc$package)
  topic <- as.character(mc$topic)

  if (length(topic) == 0 && length(package) != 0) {
    body <- get_find_package_body(package)
    view_help_wrap(body, utils::help(...))
  } else {
    paths <- tryCatch({
      utils::help(...)
    }, error = function(e) {
      if (grepl("there is no package called", e$message)) {
        return(character(0))
      } else {
        stop(e)
      }
    })
    body <- get_help_body(paths, package, topic)
    view_help_wrap(body, paths)
  }
}

#' @rdname documentation
#' @export
`?` <- function(...){
  paths <- utils::`?`(...)
  body <- get_help_body(paths)
  view_help_wrap(body, paths)
}

#' @rdname documentation
#' @export
#' @importFrom utils help.search
help.search <- function(...) {
  paths <- utils::help.search(...)
  body <- get_help_search_body(paths)
  view_help_wrap(body, paths)
}