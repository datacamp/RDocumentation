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
  with_override({
    mc <- match.call(utils::help)
    package <- as.character(mc$package)
    topic <- as.character(mc$topic)

    if (length(topic) == 0 && length(package) != 0) {
      body <- get_find_package_body(package)
      view_help(body)
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
      view_help(body)
    }
  }, alternative = utils::help(...))
}

#' @rdname documentation
#' @export
`?` <- function(...){
  with_override({
    paths <- utils::`?`(...)
    body <- get_help_body(paths)
    view_help(body)
  }, alternative = utils::`?`(...))
}

#' @rdname documentation
#' @export
#' @importFrom utils help.search
help.search <- function(...) {
  with_override({
    paths <- utils::help.search(...)
    body <- get_help_search_body(paths)
    view_help(body)
  }, alternative = utils::help.search())
}

with_override <- function(code, alternative) {
  tryCatch({
    force(code)
  }, error = function(e) {
    force(alternative)
  })
}