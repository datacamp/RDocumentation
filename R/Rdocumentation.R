
#' Return the url for RDocumentation
#'
#' @return The url for RDocumentation
#' @examples
#' rdocs_url()
#' @export
rdocs_url <- function(){
  return ("http://staging.rdocumentation.org/")
}
.onLoad <- function(libName,pkgeName){
    login()
    Rprofile <- .getRProfile()
    names <- scan(Rprofile, what=character(),quiet=TRUE)
    if (length(grep("Rdocumentation",names)) == 0){
        .view_help(paste0(rdocs_url(),"rstudio/make_default?viewer_pane=1"),"DEFAULT",FALSE,"","")
    }
}
.getRProfile<-function(){
    if(!file.exists(file.path(Sys.getenv("HOME"),".Rprofile"))){    
        file.create(file.path(Sys.getenv("HOME"),".Rprofile"),quiet=TRUE)
    }
    Rprofile<-file.path(Sys.getenv("HOME"),".Rprofile")
    return (Rprofile)
}
#' Log you in to RDocumentation if credentials are saved from last time
#'
#' Logs you in to RDocumentation if credentials from a previous session where saved. To save credentials, please login using the login button from a random help page on RDocumentation
#'
#' @examples
#' login()
#' @export
#' @importFrom httr POST
#' @importFrom httr status_code
#' @importFrom httr content_type
#' @importFrom rjson toJSON
#' @importFrom utils read.table
login<-function(){ 
    # The jsonlite package turns every variable into an array, which the /login doesn't accept, so the json is parsed with rjson
    if(file.exists(paste0(find.package("Rdocumentation"),"/config/creds.txt")) && file.info(paste0(find.package("Rdocumentation"),"/config/creds.txt"))$size > 0){
        creds <- read.table(paste0(find.package("Rdocumentation"),"/config/creds.txt"), header = FALSE)
        go_to_url = paste0(rdocs_url(),"login")

        tryCatch({
                r <- POST(go_to_url, body = as.character(creds$V1), content_type("application/x-www-form-urlencoded"))
                if (length(grep("Invalid Username or password",content(r,"text"))) > 0) {
                    cat("there is something wrong with your credentials, please try logging in to the site in the help panel")
                }
                else{
                    cat("logging you in to RDocumentation")
                }
            },
            error = function(cond){
                cat("Could not log you in, something is wrong with your internet connection or RDocumentation is offline")
            }
        )
    }
    else{
         dir.create(paste0(find.package("Rdocumentation"),"/config"), showWarnings = FALSE, recursive = TRUE)
    }

}
#' Makes the RDocumentation package the default help-package.
#'
#' @export
makeDefault <- function(){
    Rprofile <- .getRProfile()
    write("options(defaultPackages = c(getOption('defaultPackages'), 'RDocumentation'))", file = Rprofile, append = TRUE)
    hideViewer()
    return (invisible())
}
#' Redirects the viewer to the RDocumentation help page.
#'
#' @export
#' @importFrom utils tail
hideViewer <- function(){
    help(package = "RDocumentation")
}

# Overwrites the class<- function, converts help answers to json and sends them to RDocumentation
# @importFrom rjson toJSON
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
        body = rjson::toJSON(list(packages = as.character(paste(packages,sep = "",collapse = ",")), topic_names = as.character(paste(topic_names, sep = "", collapse = ",")),
                           call = as.character(paste(attributes(package)$call, sep = "", collapse = ",")), topic = as.character(attributes(package)$topic),
                           tried_all_packages = as.character(attributes(package)$tried_all_packages), help_type = as.character(attributes(package)$type)))
                           go_to_url = paste0(Rdocumentation::rdocs_url(), "rstudio/normal/help?viewer_pane=1")
    }
    else{
        hsearch_db_fields <- c("alias", "concept", "keyword", "name", "title")
        elas_search_db_fields <- c("aliases","concept","keywords","name","title")
        fields = lapply(package$fields, function(e){
            return (elas_search_db_fields[which(hsearch_db_fields == e)])
        })
        body = rjson::toJSON(list(query = as.character(package[1]), fields = as.character(paste(fields, sep = "", collapse = ",")),
                           type = as.character(package[3]), agrep = as.character(package[4]), ignore_case = as.character(package[5]),
                           types = as.character(paste(package$types, sep = "", collapse = ",")), package = as.character(package[7]),
                           matching_titles = as.character(gsub(" ", "", toString(unique(package$matches$Topic)), fixed = TRUE)),
                           matching_packages = as.character(gsub(" ", "", toString(unique(package$matches$Package)), fixed = TRUE))))
                           go_to_url = paste0(Rdocumentation::rdocs_url(),"rstudio/search/help?viewer_pane=1")
    }
    return (.view_help(go_to_url, body, TRUE, package, value))
}

#' @export
.browseUrl.help <- function(url, browser){
    parsing = substring(url,18, nchar(url)-18)
    parts = strsplit(parsing, "/")
    go_to_url = paste0(Rdocumentation::rdocs_url(), "rstudio/package/", parts[[1]][3], "?viewer_pane=1")
    return (.view_help(go_to_url, "", FALSE, url, browser))
}
#' @export
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom httr content_type_json
#' @importFrom rjson toJSON
#' @importFrom utils browseURL
.view_help <- function(go_to_url, body, post, arg1, arg2){
    tempDir <- paste0(find.package("Rdocumentation"), "/doc")
    htmlFile <- file.path(tempDir, "index.html")
    if (!file.exists(tempDir)){
        dir.create(tempDir)
    }
    if (exists("package_not_local", envir=environment(help))){
        package_not_local = environment(help)$package_not_local
    }
    else{
        package_not_local = ""
    }
    assign("package_not_local", "", envir = environment(help))
    tryCatch({
        if(post){
            r <- POST(go_to_url, config = (content_type_json()), body = body, encode = "json")
        }
        else{
            r <- GET(go_to_url)
            
        }
        if(status_code(r) == 200){
            writeBin(content(r, "raw"), htmlFile)
            p <- tools::startDynamicHelp(NA)
            browser <- getOption("browser")
            browseURL(paste0("http://127.0.0.1:", p, "/library/RDocumentation/doc/index.html?viewer_pane=1&Rstudio_port=",
                as.character(Sys.getenv("RSTUDIO_SESSION_PORT")), "&RS_SHARED_SECRET=", as.character(Sys.getenv("RS_SHARED_SECRET"))), browser)
            return (invisible())
        }
        else{
            stop("bad return status")
        }
        
    },
    error = function(cond){
        if (package_not_local != ""){
            stop(paste0("package ", package_not_local, " is not in your local library"))
        }
        if (post){
            return (baseenv()$`class<-`(arg1,arg2))
        }
        else{
            if (body == "DEFAULT"){
                p <- tools::startDynamicHelp(NA)
                browser <-  getOption("browser")
                if (file.exists(paste0(find.package("Rdocumentation"), "/default.html"))){
                    tempDir <- paste0(find.package("Rdocumentation"), "/doc")
                    htmlFile <- file.path(tempDir, "index.html")
                    cssFile <- file.path(tempDir, "default.css")
                    file.copy(paste0(find.package("Rdocumentation"), "/default.html"), htmlFile, overwrite = TRUE)
                    file.copy(paste0(find.package("Rdocumentation"), "/css/default.css"), cssFile, overwrite = TRUE)
                    browseURL(paste0("http://127.0.0.1:", p, "/library/rdocumentation/doc/index.html?viewer_pane=1&Rstudio_port=",
                                     as.character(Sys.getenv("RSTUDIO_SESSION_PORT")), "&RS_SHARED_SECRET=", as.character(Sys.getenv("RS_SHARED_SECRET"))), browser)
                }
            }
            else{
                return(utils::browseURL(arg1, arg2))
            }
        }        
    })
    
}
#' Check if a package is installed for the user.
#'
#' @param mypkg Name of the package
#' @param version the latest version to be checked
#' @return FALSE if the package is not installed, otherwise the versionnumber of the package
#' @examples
#' check_package("RDocumentation","0.2")
#' check_package("utils","3.3.1")
#' @export
#' @importFrom utils packageVersion
check_package <- function(mypkg,version){
    if (!is.element(mypkg, installed.packages()[ ,1])){
        return (1)
    }
    else{
        i = 1
        testPackage <- unlist(strsplit(as.character(packageVersion(mypkg)),"[.]"))
        testVersion <- unlist(strsplit(as.character(version),"[.]"))
        while  (i <= length(testPackage)){
            if (length(testVersion) < i){
                return (-1)
            }
            if (as.numeric(testVersion[i]) < as.numeric(testPackage[i])){
                return (-1)
            }
            if (as.numeric(testVersion[i]) > as.numeric(testPackage[i])){
                return (0)
            }
            i = i + 1
        }
        return (0)
    }
} 
#' Installs the given package
#'
#' @param mypkg the name of the package you want to install
#' @param type the type of the package, type 1 means the package comes from CRAN, type 2 packages are from bioconductor, type 3 packages are from github and type 4 packages are by default part of R.
#' @examples
#' ## Not run
#' ## install_package("dplyr",1)
#' ## install_package("RDocumentation",3)
#' @export
#' @importFrom githubinstall githubinstall
#' @importFrom BiocInstaller biocLite
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
install_package <- function(mypkg, type){
    if (type == 1){
        #CRAN
        install.packages(mypkg,repos="http://cran.rstudio.com/");
    }
    else if (type == 2){
        #bioconductor
        source("https://bioconductor.org/biocLite.R")
        if (!is.element(mypkg, installed.packages()[ ,1])){
            biocLite(c(mypkg))
        }
        else{
            biocLite("BiocUpgrade")
        }         
    }
    else if (type == 3){
        #github
        githubinstall::githubinstall(mypkg)

    }
    else if (type == 4){
        cat("Can not install this package, you need to upgrade your R installation")
    }
    else{
        cat("Something went wrong, could not install this package")
    }
} 

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

#' Documentation on RDocumentation or via the normal help system if offline
#'
#'\code{help} contacts RDocumentation for help given aliases and packages
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
#' help(package=RDocumentation)
#' help(strsplit,base)
#' @seealso \url{http://www.RDocumentation.org} for the online version of the documentation, \code{\link[RDocumentation]{help.search}} for finding help on vague topics or \code{\link[utils]{help}} for 
#' documentation of the offline help.
#' @export
#' @importFrom proto proto
#' @importFrom utils help
help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help, `class<-` = `.class.help<-`, find.package = find.package.help), help)
this.help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help, `class<-` = `.class.help<-`, find.package = find.package.help), help)
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
#' help.search("linear models")    # In case you forgot how to fit linear
#'                                 # models
#' help.search("non-existent topic")
#' 
#' ??utils::help  # All the topics matching "help" in the utils package
#'
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
#' @seealso \url{http://www.rdocumentation.org} for the online version of the documentation, \code{\link[RDocumentation]{help}} for finding help on non-vague topics or \code{\link[utils]{help.search}} for 
#' documentation of the offline help.
#' @export
#' @importFrom proto proto
#' @importFrom utils help.search
help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`), help.search)
this.help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`), help.search)

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
#¼ ?lapply
#'
#' ?"for"                  # but quotes/backticks are needed
#' ?`+`
#'
#' ?women                  # information about data set "women"
#' 
#' ## Not run: 
#' # require(methods)
#' # ## define a S4 generic function and some methods
#' # combo <- function(x, y) c(x, y)
#' # setGeneric("combo")
#' # setMethod("combo", c("numeric", "numeric"), function(x, y) x+y)
#' # 
#' # ## assume we have written some documentation
#' # ## for combo, and its methods ....
#' # 
#' # ?combo  # produces the function documentation
#' # 
#' # methods?combo  # looks for the overall methods documentation
#' # 
#' # method?combo("numeric", "numeric")  # documentation for the method above
#' # 
#' # ?combo(1:10, rnorm(10))  # ... the same method, selected according to
#' #                          # the arguments (one integer, the other numeric)
#' # 
#' # ?combo(1:10, letters)    # documentation for the default method
#' # ## End(Not run)
#' @keywords documentation
#' @export
#' @importFrom proto proto
`?` <- with(proto(environment(help), `?` = utils::`?`, help = this.help, help.search = this.help.search), `?`)             