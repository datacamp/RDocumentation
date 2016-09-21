browseUrl.help <- function(url, browser){ 
  body = list( package_name = get_package_from_URL(url), called_function="find_package")
  return (view_help(body, url, browser))
}


# Overwrites the class<- function, converts help answers to json and sends them to RDocumentation
`.class.help<-` <- function(package, value){
    if (value == "help_files_with_topic"){
        if (!exists("package_not_local", envir = environment(help)) || environment(help)$package_not_local == ""){
            packages<-lapply(package,function(path){
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
        body = list(packages = as.character(paste(packages,sep = "",collapse = ",")), topic_names = as.character(paste(topic_names, sep = "", collapse = ",")),
                           call = as.character(paste(attributes(package)$call, sep = "", collapse = ",")), topic = as.character(attributes(package)$topic),
                           tried_all_packages = as.character(attributes(package)$tried_all_packages), help_type = as.character(attributes(package)$type), called_function="help")
                           go_to_url = paste0(RDocumentation:::rdocs_url, "rstudio/normal/help?viewer_pane=1")
    }
    else{
        hsearch_db_fields <- c("alias", "concept", "keyword", "name", "title")
        elas_search_db_fields <- c("aliases","concept","keywords","name","title")
        fields = lapply(package$fields, function(e){
            return (elas_search_db_fields[which(hsearch_db_fields == e)])
        })
        body = list(query = as.character(package[1]), fields = as.character(paste(fields, sep = "", collapse = ",")),
                           type = as.character(package[3]), agrep = as.character(package[4]), ignore_case = as.character(package[5]),
                           types = as.character(paste(package$types, sep = "", collapse = ",")), package = as.character(package[7]),
                           matching_titles = as.character(gsub(" ", "", toString(unique(package$matches$Topic)), fixed = TRUE)),
                           matching_packages = as.character(gsub(" ", "", toString(unique(package$matches$Package)), fixed = TRUE)), called_function="help_search")
    }
    return (view_help(body, package, value))
}


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
    go_to_url = paste0(rdocs_url, "rstudio/view?viewer_pane=1")
    temp_dir <- system.file(package = "RDocumentation")
    html_file <- file.path(temp_dir, "index.html")

    if ( exists("package_not_local", envir = prototype)) {
        package_not_local = prototype$package_not_local
    }
    else{
        package_not_local = ""
    }
    assign("package_not_local", "", envir = prototype)
    tryCatch({
        r <- POST(go_to_url, add_headers(Accept = "text/html"), config = (content_type_json()), body = rjson::toJSON(body), encode = "json", timeout(getOption("RDocumentation.timeOut")))
        if (status_code(r) == 200) {
            if (file.exists(paste0(find.package("RDocumentation"),"/config/creds.txt")) && file.info(paste0(find.package("RDocumentation"),"/config/creds.txt"))$size > 0) {
                creds <- as.character(read.table(paste0(find.package("RDocumentation"), "/config/creds.txt"), header = FALSE)$V1)
            }
            else{
                creds = ""
            }
            writeBin(content(r, "raw"), html_file)
            p <- tools::startDynamicHelp(NA)
            browser <- getOption("browser")
            browseURL(paste0("http://127.0.0.1:", p, html_file, "?viewer_pane=1&Rstudio_port=",
                as.character(Sys.getenv("RSTUDIO_SESSION_PORT")), "&RS_SHARED_SECRET=", as.character(Sys.getenv("RS_SHARED_SECRET")), "&", creds), browser)
            return (invisible())
        }
        else{
            stop("bad return status")
        }
        
    },
    error = function(cond){
        if (package_not_local != "") {
            stop(paste0("package ", package_not_local, " is not in your local library"))
        }
        if (body$called_function == "help" || body$called_function == "help_search") {
            return (baseenv()$`class<-`(arg1, arg2))
        }
        else if (body$called_function == "find_package") {
            #this line will throw an error if the package does not exist before falling back on the original help function
            base::find.package(get_package_from_URL(arg1))
            return(utils::browseURL(arg1, arg2))
        }        
    })
    
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
#' \code{help} contacts RDocumentation for help given aliases and packages
#'
#' @param package a name or character vector giving the packages to look into for documentation, or \code{NULL}.
#' By default, all packages whose namespaces are loaded are used. To avoid a name being deparsed use e.g. \code{(pkg_ref)} (see the examples).
#' @param lib.loc a character vector of directory names of R libraries, or \code{NULL}. The default value of \code{NULL} corresponds to all libraries currently known.
#' If the default is used, the loaded packages are searched before the libraries. This is not used for HTML help (see ‘Details’ from \code{\link[utils]{help}}).
#' When no local matches are found, RDocumentation will ignore local libraries and search in the online database.
#' @param verbose logical; if \code{TRUE}, the file name is reported.
#' @param try.all.packages logical; see Note on \code{\link[utils]{help}}.
#' @param help_type only works if the user is offline, otherwise documentation is viewed on RDocumentation.org in the help-panel
#' character string: the type of help required. Possible values are code{"text"}, \code{"html"} and code{"pdf"}. Case is ignored, and partial matching is allowed.
#' @examples
#' \dontrun{
#' help(package=RDocumentation)
#' help(strsplit,base)
#' }
#' 
#' @seealso \url{http://www.RDocumentation.org} for the online version of the documentation, \code{\link[RDocumentation]{help.search}} for finding help on vague topics or \code{\link[utils]{help}} for 
#' documentation of the offline help.
#'
#' @details for slow internet connections, a timeout can be set for getting the page of RDocumentation via options("RDocumentation.timeOut" = \code{nb_of_seconds}) the default timeout is 3 seconds
#'
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

#' Search the Help System on RDocumentation or local if offline
#'
#'Allows for searching the help system for documentation matching a given character string in the (file) name, alias, title, concept or keyword entries (or any combination thereof),
#'using either fuzzy matching(\code{\link[base]{agrep}}) or regular expression (\code{\link[base]{regex}}) matching.
#'Names and titles of the matched help entries are displayed nicely formatted. Vignette names, titles and keywords and demo names and titles may also be searched.
#'
#' @param pattern a character string to be matched in the specified fields. If this is given, the arguments \code{apropos}, \code{keyword}, and \code{whatis} are ignored.
#' @param fields a character vector specifying the fields of the help database to be searched. The entries must be abbreviations of \code{"name"}, \code{"title"}, \code{"alias"}, \code{"concept"}, and \code{"keyword"},
#' corresponding to the help page's (file) name, its title, the topics and concepts it provides documentation for, and the keywords it can be classified to.
#' See below for details and how vignettes and demos are searched.
#' @param apropos a character string to be matched in the help page topics and title.
#' @param keyword a character string to be matched in the help page ‘keywords’. ‘Keywords’ are really categories: the standard categories are listed in file ‘R.home("doc")/KEYWORDS’
#' (see also the example) and some package writers have defined their own. If \code{keyword} is specified, \code{agrep} defaults to \code{FALSE}.
#' @param whatis a character string to be matched in the help page topics.
#' @param ignore.case a logical. If \code{TRUE}, case is ignored during matching; if \code{FALSE}, pattern matching is case sensitive. If no local matches are found, the online search is used, which is always non case-sensitive.
#' @param package a character vector with the names of packages to search through, or \code{NULL} in which case all available packages in the library trees specified by \code{lib.loc} are searched.
#' if no matches are found, the RDocumentation.org database is searched.
#' @param lib.loc a character vector describing the location of R library trees to search through, or \code{NULL}. The default value of \code{NULL} corresponds to all libraries currently known. If no matches are found in the given library,
#' the online search of RDocumentation is used.
#' @param help.db a character string giving the file path to a previously built and saved help database, or \code{NULL}.
#' @param verbose logical; if \code{TRUE}, the search process is traced. Integer values are also accepted, with \code{TRUE} being equivalent to \code{2},
#' ' and \code{1} being less verbose. On Windows a progress bar is shown during rebuilding, and on Unix a heartbeat is shown for \code{verbose = 1} and a package-by-package list for \code{verbose >= 2}.
#' @param rebuild a logical indicating whether the help database should be rebuilt. This will be done automatically if \code{lib.loc} or the search path is changed, 
#' or if \code{package} is used and a value is not found.
#' @param agrep if \code{NULL} (the default unless \code{keyword} is used) and the character string to be matched consists of alphanumeric characters,
#' whitespace or a dash only, approximate (fuzzy) matching via \code{\link[base]{agrep}} is used unless the string has fewer than 5 characters; 
#' otherwise, it is taken to contain a regular expression (\code{\link[base]{regex}}) to be matched via \code{\link[base]{grep}}. If \code{FALSE},
#' approximate matching is not used. Otherwise, one can give a numeric or a list specifying the maximal distance for the approximate match, see argument \code{max.distance} in the documentation for agrep.
#' @param use_UTF8 logical: should results be given in UTF-8 encoding? Also changes the meaning of regexps in \code{agrep} to be Perl regexps. This does not have effect when the online database is used.
#' @param types a character vector listing the types of documentation to search. The entries must be abbreviations of \code{"vignette"}, \code{"help"} or \code{"demo"}. Results will be presented in the order specified. 
#' @examples
#' \dontrun{
#' help.search("linear models")    # In case you forgot how to fit linear
#'                                 # models
#' help.search("non-existent topic")
#' 
#' ??utils::help  # All the topics matching "help" in the utils package
#'
#' help.search("print")            # All help pages with topics or title
#'                                 # matching 'print'
#' help.search(apropos = "print")  # The same
#'
#' help.search(keyword = "hplot",fields=c("alias"))  # All help pages documenting high-level
#'                                                   # plots.
#' file.show(file.path(R.home("doc"), "KEYWORDS"))  # show all keywords
#' 
#' ## Help pages with documented topics starting with 'try'.
#' help.search("\\btry", fields = "alias")
#' }
#' 
#' @seealso \url{http://www.RDocumentation.org} for the online version of the documentation, \code{\link[RDocumentation]{help}} for finding help on non-vague topics or \code{\link[utils]{help.search}} for 
#' documentation of the offline help.
#'
#' @details for slow internet connections, a timeout can be set for getting the page of RDocumentation via options("RDocumentation.timeOut" = \code{nb_of_seconds}) the default timeout is 3 seconds
#'
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

#' RDocumentation shortcuts
#'
#' These functions provide access to RDocumentation. Documentation on a topic with name name (typically, an R object or a data set) can be displayed by either help("name") or ?name.
#'
#' @param topic Usually, a name or character string specifying the topic for which help is sought. Alternatively, a function call to ask for documentation on a corresponding S4 method: 
#' see the section on S4 method documentation in \code{\link[utils]{?}}. The calls pkg::topic and pkg:::topic are treated specially, and look for help on topic in package pkg.
#' @param type the special type of documentation to use for this topic; for example, if the type is class, documentation is provided for the class with name topic. 
#' See the Section ‘S4 Method Documentation in \code{\link[utils]{?}}’ for the uses of type to get help on formal methods, including methods?function and method?call.
#'
#' This is a shortcut to help and uses its default type of help.
#' Some topics need to be quoted (by backticks) or given as a character string. There include those which cannot syntactically appear on their own such as unary and binary operators, function and control-flow reserved words (including if, else for, in, repeat, while, break and next. The other reserved words can be used as if they were names, for example TRUE, NA and Inf.
#' @examples
#' \dontrun{
#' ?lapply
#' 
#' # for specials, quotes/backticks are needed
#' ?"for"                  
#' ?`+`
#' 
#' # information about data set "women"
#' ?women
#' }
#' 
#' @export
#' @importFrom proto proto
`?` <- function(...){
    returned <- with(prototype, `?`)(...)
    if (length(returned) == 0) {
        invisible()
    }
    else{
        return (returned)
    }
}


