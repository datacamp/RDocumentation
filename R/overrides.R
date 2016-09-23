browseUrl.help <- function(url, browser) {
  body = list(package_name = get_package_from_URL(url), called_function = "find_package")
  return (view_help(body, url, browser))
}

# Overwrites the class<- function, converts help answers to json and sends them to RDocumentation
`.class.help<-` <- function(package, value) {
  if (value == "help_files_with_topic"){
    if (!exists("package_not_local", envir = environment(help)) || environment(help)$package_not_local == ""){
      packages <- lapply(package,function(path){
        temp = strsplit(path, "/")[[1]]
        return (temp[length(temp)-2])
      })
      topic_names <- lapply(package, function(path){
        temp = strsplit(path, "/")[[1]]
        return (tail(temp, n = 1))
      })
    }
    else{
      packages <- environment(help)$package_not_local
      topic_names <- ""
    }     
    body <- list(packages = as.character(paste(packages,sep = "",collapse = ",")), topic_names = as.character(paste(topic_names, sep = "", collapse = ",")),
                call = as.character(paste(attributes(package)$call, sep = "", collapse = ",")), topic = as.character(attributes(package)$topic),
                tried_all_packages = as.character(attributes(package)$tried_all_packages), help_type = as.character(attributes(package)$type), called_function="help")
  } else {
    hsearch_db_fields <- c("alias", "concept", "keyword", "name", "title")
    elas_search_db_fields <- c("aliases","concept","keywords","name","title")
    fields = lapply(package$fields, function(e){
      return (elas_search_db_fields[which(hsearch_db_fields == e)])
    })
    body <- list(query = as.character(package[1]), fields = as.character(paste(fields, sep = "", collapse = ",")),
                 type = as.character(package[3]), agrep = as.character(package[4]), ignore_case = as.character(package[5]),
                 types = as.character(paste(package$types, sep = "", collapse = ",")), package = as.character(package[7]),
                 matching_titles = as.character(gsub(" ", "", toString(unique(package$matches$Topic)), fixed = TRUE)),
                 matching_packages = as.character(gsub(" ", "", toString(unique(package$matches$Package)), fixed = TRUE)), called_function="help_search")
  }
  return (view_help(body, package, value))
}

# This find.package replacement function makes sure we can save the packagename to search it online, instead of returning an error.
find.package.help <- function(packages, lib, verbose = FALSE){
    tryCatch({
        return (base::find.package(packages, lib, verbose))
        },
    error = function(cond){
        #because we go over functioncalls and need access in the other function, we need to store this information internally
        assign("package_not_local", packages, envir = environment(help))
        return ("")
    })
}

# Prototype = childEnvironment of the utils-package environment
prototype <- proto(environment(help), 
                   browseURL = browseUrl.help, 
                   `class<-` = `.class.help<-`, 
                   find.package = find.package.help, 
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
    returned <- with(prototype, help)(...)
    if (length(returned) == 0) {
        invisible()
    } else{
        return(returned)
    }
}

#' @rdname documentation
#' @export
#' @importFrom proto proto
#' @importFrom utils help.search
help.search <- function(...){
    returned <- with(prototype, help.search)(...)
    if (length(returned) == 0) {
        invisible()
    }
    else{
        return (returned)
    }
}

#' @rdname documentation
#' @export
#' @importFrom proto proto
`?` <- function(...){
  returned <- with(prototype, `?`)(...)
  if (length(returned) == 0) {
    invisible()
  } else{
    return(returned)
  }
}


