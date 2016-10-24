# Overwrites the class<- function, converts help answers to json and sends them to RDocumentation
`.class.help<-` <- function(package, value) {
  
  concat <- function(x) {
    paste(x, collapse = ",")
  }
  
  print("======= package")
  print(package)
  print("======= value")
  print(value)
  print("=======")
  if (value == "help_files_with_topic") {
    if (any(grepl("/", package))) {
      # package was find locally
      split <- strsplit(package, "/")
      packages <- sapply(split, function(x) return(x[length(x)-2]))
      topic_names <- sapply(split, tail, n = 1)
    } else {
      # package wasn't found locally
      packages <- package
      topic_names <- ""
    }
    body <- list(packages = concat(packages),
                 topic_names = concat(topic_names),
                 topic = attributes(package)$topic,
                 called_function = "help")
    str(body)
  } else {
    str(package)
    lut <- c(alias = "aliases", concept = "concpet", keyword = "keywords", name = "name", title = "title")
    body <- package
    body$fields <- lut[body$fields]
    attributes(body$fields) <- NULL
    body$matching_titles <- concat(unique(body$matches$Topic))
    body$matching_packages <- concat(unique(body$matches$Package))
    body$called_function <- "help_search"
    body[c("lib.loc", "matches")] <- NULL
    
    str(body)
  }
  return (view_help(body, package, value))
}

find.package.help <- function(packages, lib, verbose = FALSE) {
  tryCatch({
    return (base::find.package(packages, lib, verbose))
  }, error = function(cond){
    # packages are not found locally, just return their names
    return (packages)
  })
}

index.search.help <- function(topic, paths, firstOnly = FALSE) {
  res <- utils:::index.search(topic, paths, firstOnly)
  if (length(res) == 0) {
    if (any(grepl("/", paths))) {
      return(sapply(strsplit(paths, "/"), tail, n = 1))
    } else {
      return("paths")
    }
  } else {
    return(res)
  }
}

# Prototype = childEnvironment of the utils-package environment
prototype <- proto(environment(help), 
                   # browseURL = browseUrl.help, 
                   `class<-` = `.class.help<-`, 
                   find.package = find.package.help,
                   index.search = index.search.help,
                   help = utils::help, 
                   help.search = utils::help.search,
                   `?` = utils::`?`)


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
help <- function(...){
  if (is_override()) {
    tryCatch({
      invisible(with(prototype, help)(...))  
    }, error = function(e) {
      utils::help(...)
    })
  } else {
    utils::help(...)
  }
}

#' @rdname documentation
#' @export
#' @importFrom proto proto
#' @importFrom utils help.search
help.search <- function(...) {
  if (is_override()) {
    tryCatch({
      invisible(with(prototype, help.search)(...))
    }, error = function(e) {
      print(e)
      print(content(e, "text"))
      utils::help.search(...)
    })
    
  } else {
    utils::help.search(...)
  }
}

#' @rdname documentation
#' @export
#' @importFrom proto proto
`?` <- function(...){
  if (is_override()) {
    tryCatch({
      invisible(with(prototype, `?`)(...))
    }, error = function(e) {
      print(e)
      utils::`?`(...)
    })
  } else {
    utils::`?`(...)
  }
}


