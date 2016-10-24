# Overwrites the class<- function, converts help answers to json and sends them to RDocumentation
`.class.help<-` <- function(package, value) {
  concat <- function(x) {
    paste(x, collapse = ",")
  }
  
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
  } else {
    lut <- c(alias = "aliases", concept = "concept", keyword = "keywords", name = "name", title = "title")
    body <- package
    body$fields <- concat(lut[body$fields])
    body$matching_titles <- concat(unique(body$matches$Topic))
    body$matching_packages <- concat(unique(body$matches$Package))
    body$called_function <- "help_search"
    body[c("lib.loc", "matches", "types", "package")] <- NULL
  }
  return (view_help(body))
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
  
  # Copied from utils:::index.search()
  res <- character()
  for (p in paths) {
    if (file.exists(f <- file.path(p, "help", "aliases.rds"))) 
      al <- readRDS(f)
    else if (file.exists(f <- file.path(p, "help", "AnIndex"))) {
      foo <- scan(f, what = list(a = "", b = ""), sep = "\t", 
                  quote = "", na.strings = "", quiet = TRUE)
      al <- structure(foo$b, names = foo$a)
    }
    else next
    f <- al[topic]
    if (is.na(f)) 
      next
    res <- c(res, file.path(p, "help", f))
    if (firstOnly) 
      break
  }
  
  # Interpret res object
  if (length(res) == 0) {
    # index.search failed to find something meaningful.
    # paths comes from find.package.help; can be actual paths, or simply package names
    if (any(grepl("/", paths))) {
      # if a path, return all last elements
      return(sapply(strsplit(paths, "/"), tail, n = 1))
    } else {
      # else, just return package names
      return(paths)
    }
  } else {
    return(res)
  }
}

# Create proto object that has utils environment as parent
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
      utils::`?`(...)
    })
  } else {
    utils::`?`(...)
  }
}


