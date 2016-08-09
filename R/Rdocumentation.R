
#' Return the url for Rdocumentation
#'
#' @return The url for Rdocumentation
#' @examples
#' rdocs_url()
#' @export
rdocs_url <- function(){
  return("http://staging.rdocumentation.org/")
}
.onLoad<-function(libName,packageName){
    login()
    #locate Rprofile file
        #search it for Rdocumentation
    Rprofile<-.getRProfile()
    names <- scan(Rprofile, what=character(),quiet=TRUE)
    if(length(grep("Rdocumentation",names))==0){
        .view_help(paste0(rdocs_url(),"rstudio/make_default"),"DEFAULT",FALSE,"","")
    }
}
.getRProfile<-function(){
    if(!file.exists(file.path(Sys.getenv("HOME"),".Rprofile"))){
        file.create(file.path(Sys.getenv("HOME"),".Rprofile"),quiet=TRUE)
    }
    Rprofile<-file.path(Sys.getenv("HOME"),".Rprofile")
    return (Rprofile)
}
#' Log you in to Rdocumentation if credentials are saved from last time
#'
#' Logs you in to Rdocumentation if credentials from a previous session where saved. To save credentials, please login using the login button from a random help page on Rdocumentation.
#'
#' @examples
#' login()
#' @export
#' @importFrom httr POST
#' @importFrom httr status_code
#' @importFrom rjson toJSON
login<-function(){
    library(rjson)
    library(httr)  
    #the jsonlite package turns every variable into an array, which the /login doesn't accept, so the json is parsed with rjson
    if(file.exists(paste0(find.package("Rdocumentation"),"/config/creds.txt")) && file.info(paste0(find.package("Rdocumentation"),"/config/creds.txt"))$size>0){
        creds<-read.table(paste0(find.package("Rdocumentation"),"/config/creds.txt"),header=FALSE)
        go_to_url=paste0(rdocs_url(),"login")

        tryCatch({
                r <- POST(go_to_url,body=as.character(creds$V1),content_type("application/x-www-form-urlencoded"))
                if(status_code(r)!=200){
                    packageStartupMessage("there is something wrong with your credentials, please try logging in to the site in the help panel")
                }
                else{
                    packageStartupMessage("logging you in to Rdocumentation")
                }
            },
            error=function(cond){
                print(cond)
                packageStartupMessage("Could not log you in, something is wrong with your internet connection or Rdocumentation is offline")
            }
        )
    }
    else{
         dir.create(paste0(find.package("Rdocumentation"),"/config"),showWarnings = FALSE,recursive=TRUE)
    }

}
#' Makes the Rdocumentation package the default help-package.
#'
#' @export
makeDefault<-function(){
    Rprofile<-.getRProfile()
    write("options(defaultPackages = c(getOption('defaultPackages'), 'Rdocumentation'))" ,file=Rprofile,append=TRUE)
    return (invisible())
}
#' Redirects the viewer to the Rdocumentation help page.
#'
#' @export
hideViewer<-function(){
    help(package=Rdocumentation)
}

#overwrites the class<- function, converts help answers to json and sends them to Rdocumentation
`.class.help<-`<-function(package,value){
    library(rjson)
    if(value== "help_files_with_topic"){
        if(!exists("package_not_local",envir=environment(help)) || environment(help)$package_not_local==""){
            packages<-lapply(package,function(path){
            temp = strsplit(path,"/")[[1]]
            return (temp[length(temp)-2])
            })
            topic_names<-lapply(package,function(path){
                temp = strsplit(path,"/")[[1]]
                return (tail(temp,n=1))
            })
        }
        else{
            packages<-environment(help)$package_not_local
            topic_names<-attributes(package)$topic
            assign("package_not_local","",envir=environment(help))
        }        
        body= toJSON(list(packages=as.character(paste(packages,sep="",collapse=",")),topic_names=as.character(paste(topic_names,sep="",collapse=",")),
        call=as.character(paste(attributes(package)$call,sep="",collapse=",")),topic=as.character(attributes(package)$topic),
        tried_all_packages=as.character(attributes(package)$tried_all_packages),help_type=as.character(attributes(package)$type)))
        go_to_url=paste0(Rdocumentation::rdocs_url(),"rstudio/normal/help?viewer_pane=1")
    }
    else{
        hsearch_db_fields <-c("alias", "concept", "keyword", "name", "title")
        elas_search_db_fields <-c("aliases","concept","keywords","name","title")
        fields = lapply(package$fields,function(e){
            return (elas_search_db_fields[which(hsearch_db_fields==e)])
        })
        body= toJSON(list(query=as.character(package[1]),fields=as.character(paste(fields,sep="",collapse=",")),
        type=as.character(package[3]),agrep=as.character(package[4]),ignore_case=as.character(package[5]),
        types=as.character(paste(package$types,sep="",collapse=",")),package=as.character(package[7]),
        matching_titles=as.character(gsub(" ", "", toString(unique(package$matches$Topic)), fixed = TRUE)),
        matching_packages=as.character(gsub(" ", "", toString(unique(package$matches$Package)), fixed = TRUE))))
        go_to_url=paste0(Rdocumentation::rdocs_url(),"rstudio/search/help?viewer_pane=1")
    }
    return (.view_help(go_to_url,body,TRUE,package,value))
}

#' @export
.browseUrl.help<-function(url,browser){
    parsing = substring(url,18,nchar(url)-18)
    parts = strsplit(parsing,"/")
    go_to_url=paste0(Rdocumentation::rdocs_url(),"rstudio/package/",parts[[1]][3],"?viewer_pane=1")
    return (.view_help(go_to_url,"",FALSE,url,browser))
}
#' @export
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom httr content_type_json
#' @importFrom rjson toJSON
.view_help<-function(go_to_url,body,post,arg1,arg2){
    tempDir <- paste0(find.package("Rdocumentation"),"/doc")
    htmlFile <- file.path(tempDir, "index.html")
    if(!file.exists(tempDir)){
        dir.create(tempDir)
    }
    tryCatch({
        if(post){
            r <- POST(go_to_url,config=(content_type_json()),body =body,encode="json")
        }
        else{
            r <- GET(go_to_url)
            
        }
        if(status_code(r)==200){
            writeBin(content(r,'raw'),htmlFile)
            p <- tools::startDynamicHelp(NA)
            browser <-  getOption("browser")
            browseURL(paste0("http://127.0.0.1:", p, "/library/Rdocumentation/doc/index.html?viewer_pane=1&Rstudio_port=",
                as.character(Sys.getenv("RSTUDIO_SESSION_PORT")),"&RS_SHARED_SECRET=",as.character(Sys.getenv("RS_SHARED_SECRET"))),browser)
            return (invisible())
        }
        else{
            stop("bad return status")
        }
        
    },
    error=function(cond){
        print("Could not reach Rdocumentation, either your internet connection is bad or Rdocumentation is offline")
        if(post){
            return (baseenv()$`class<-`(arg1,arg2))
        }
        else{
            if(body=="DEFAULT"){
                p <- tools::startDynamicHelp(NA)
                browser <-  getOption("browser")
                if(file.exists(paste0(find.package("Rdocumentation"),"/default.html"))){
                    tempDir <- paste0(find.package("Rdocumentation"),"/doc")
                    htmlFile <- file.path(tempDir, "index.html")
                    cssFile<-file.path(tempDir,'default.css')
                    file.copy(paste0(find.package("Rdocumentation"),"/default.html"),htmlFile,overwrite=TRUE)
                    file.copy(paste0(find.package("Rdocumentation"),"/css/default.css"),cssFile,overwrite=TRUE)
                    browseURL(paste0("http://127.0.0.1:", p, "/library/Rdocumentation/doc/index.html?viewer_pane=1&Rstudio_port=",
                        as.character(Sys.getenv("RSTUDIO_SESSION_PORT")),"&RS_SHARED_SECRET=",as.character(Sys.getenv("RS_SHARED_SECRET"))),browser)
                }
            }
            else{
                return(utils::browseURL(arg1,arg2))
            }
        }        
    })
    
}
#' Check if a package is installed for the user.
#'
#' @param mypkg Name of the package
#' @return FALSE if the package is not installed, otherwise the versionnumber of the package
#' @examples
#' check_package("Rdocumentation")
#' check_package("utils")
#' @export
check_package <- function(mypkg){
    if(!is.element(mypkg, installed.packages()[,1])){
        print("The package for this topic is not installed, try running:")
        print(paste0("install.packages('",mypkg,"')"))
        print("Or check the installation notes for the package with:")
        print(paste0("help(package=",mypkg,")"))
        return (FALSE)
    }
    else{
        return (packageVersion(mypkg))
    }
} 
#' Installs the given package
#'
#' @param mypkg the name of the package you want to install
#' @param type the type of the package, type 1 means the package comes from CRAN, type 2 packages are from bioconductor, type 3 packages are from github and type 4 packages are by default part of R.
#' @examples
#' install_package("dplyr",1)
#' install_package("Rdocumentation",3)
#' @export
install_package <- function(mypkg,type){
    if(type==1){
        #CRAN
        install.packages(mypkg);
    }
    else if(type==2){
        #bioconductor
        source("https://bioconductor.org/biocLite.R")
        if(!is.element(mypkg, installed.packages()[,1])){
            biocLite(c(mypkg))
        }
        else{
            biocLite("BiocUpgrade")
        }         
    }
    else if(type==3){
        #github,needs url
        if(!is.element("githubinstall", installed.packages()[,1])){
            install.packages("githubinstall")
        }
        library(githubinstall)
        githubinstall(mypkg)

    }
    else if(type==4){
        print("Can not install this package, you need to upgrade your R installation")
    }
    else{
        print("Something went wrong, could not install this package")
    }
} 

find.package.help<-function(packages,lib, verbose = FALSE){
    tryCatch({
        return (base::find.package(packages,lib,verbose))
        },
    error=function(cond){
        #because we go over functioncalls and need access in the other function, we need to store this information internally
        assign("package_not_local",packages,envir=environment(help))
        return ("")
    })
}

library(proto)
#' Documentation on Rdocumentation or via the normal help system if offline
#'
#'\code{help} contacts Rdocumentation for help given aliases and packages
#'
#' @param topic usually, a name or character string specifying the topic for which help is sought.
#' A character string (enclosed in explicit single or double quotes) is always taken as naming a \code{topic}.
#' If the value of \code{topic} is a length-one character vector the topic is taken to be the value of the only element.
#' Otherwise topic must be a name or a reserved word (if syntactically valid) or character string. See ‘Details’ for what happens if this is omitted.
#' @param package a name or character vector giving the packages to look into for documentation, or \code{NULL}.
#' By default, all packages whose namespaces are loaded are used. To avoid a name being deparsed use e.g. \code{(pkg_ref)} (see the examples).
#' @param lib.loc a character vector of directory names of R libraries, or \code{NULL}. The default value of \code{NULL} corresponds to all libraries currently known.
#' If the default is used, the loaded packages are searched before the libraries. This is not used for HTML help (see ‘Details’).
#' When no local matches are found, Rdocumentation will ignore local libraries and search in the online database.
#' @param verbose logical; if \code{TRUE}, the file name is reported.
#' @param try.all.packages logical; see \code{Note}.
#' @param help_type character string: the type of help required. Possible values are code{"text"}, \code{"html"} and code{"pdf"}. Case is ignored, and partial matching is allowed.
#' @examples
#' help(package=Rdocumentation)
#' help(centre,ftsa)
#' @export
#' @importFrom proto proto
help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help,`class<-` = `.class.help<-`,find.package=find.package.help),help)
this.help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help,`class<-` = `.class.help<-`),help)
#' Search the Help System on Rdocumentation or local if offline
#'
#'Allows for searching the help system for documentation matching a given character string in the (file) name, alias, title, concept or keyword entries (or any combination thereof),
#'using either fuzzy matching or regular expression matching.
#'Names and titles of the matched help entries are displayed nicely formatted. Vignette names, titles and keywords and demo names and titles may also be searched.
#'
#' @param pattern a character string to be matched in the specified fields. If this is given, the arguments \code{apropos}, \code{keyword}, and \code{whatis} are ignored.
#' @param fields a character vector specifying the fields of the help database to be searched. The entries must be abbreviations of "name", "title", "alias", "concept", and "keyword",
#' corresponding to the help page's (file) name, its title, the topics and concepts it provides documentation for, and the keywords it can be classified to.
#' See below for details and how vignettes and demos are searched.
#' @param apropos a character string to be matched in the help page topics and title.
#' @param keyword a character string to be matched in the help page ‘keywords’. ‘Keywords’ are really categories: the standard categories are listed in file ‘R.home("doc")/KEYWORDS’
#' (see also the example) and some package writers have defined their own. If keyword is specified, agrep defaults to FALSE.
#' @param ignore.case a logical. If \code{TRUE}, case is ignored during matching; if \code{FALSE}, pattern matching is case sensitive. If no local matches are found, the online search is used, which is always non case-sensitive.
#' @param package a character vector with the names of packages to search through, or \code{NULL} in which case all available packages in the library trees specified by lib.loc are searched.
#' @param lib.loc a character vector describing the location of R library trees to search through, or \code{NULL}. The default value of \code{NULL} corresponds to all libraries currently known. If no matches are found in the given library,
#' the online search of Rdocumentation is used.
#' @param help.db a character string giving the file path to a previously built and saved help database, or \code{NULL}.
#' @param verbose logical; if TRUE, the search process is traced. Integer values are also accepted, with TRUE being equivalent to 2, and 1 being less verbose. On Windows a progress bar is shown during rebuilding, and on Unix a heartbeat is shown for verbose = 1 and a package-by-package list for verbose >= 2.
#' @param rebuild a logical indicating whether the help database should be rebuilt. This will be done automatically if lib.loc or the search path is changed, or if package is used and a value is not found.
#' @param agrep if NULL (the default unless keyword is used) and the character string to be matched consists of alphanumeric characters, whitespace or a dash only, approximate (fuzzy) matching via agrep is used unless the string has fewer than 5 characters; otherwise, it is taken to contain a regular expression to be matched via grep. If FALSE, approximate matching is not used. Otherwise, one can give a numeric or a list specifying the maximal distance for the approximate match, see argument max.distance in the documentation for agrep.
#' @param use_UTF8 logical: should results be given in UTF-8 encoding? Also changes the meaning of regexps in agrep to be Perl regexps.
#' @param types a character vector listing the types of documentation to search. The entries must be abbreviations of "vignette" "help" or "demo". Results will be presented in the order specified.
#' @param field a single value of fields to search.
#'
#' Upon installation of a package, a pre-built help.search index is serialized as ‘hsearch.rds’ in the ‘Meta’ directory (provided the package has any help pages). Vignettes are also indexed in the ‘Meta/vignette.rds’ file. These files are used to create the help search database via hsearch_db.
#' The arguments apropos and whatis play a role similar to the Unix commands with the same names.
#' Searching with agrep = FALSE will be several times faster than the default (once the database is built). However, approximate searches should be fast enough (around a second with 5000 packages installed).
#' If possible, the help database is saved in memory for use by subsequent calls in the session.
#' Note that currently the aliases in the matching help files are not displayed.
#' As with ?, in ?? the pattern may be prefixed with a package name followed by :: or ::: to limit the search to that package.
#' For help files, \keyword entries which are not among the standard keywords as listed in file ‘KEYWORDS’ in the R documentation directory are taken as concepts. For standard keyword entries different from internal, the corresponding descriptions from file ‘KEYWORDS’ are additionally taken as concepts. All \concept entries used as concepts. 
#' Vignettes are searched as follows. The "name" and "alias" are both the base of the vignette filename, and the "concept" entries are taken from the \VignetteKeyword entries. Vignettes are not classified using the help system "keyword" classifications. Demos are handled similarly to vignettes, without the "concept" search.
#' 
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
#' help.search(keyword = "hplot")  # All help pages documenting high-level
#'                                 # plots.
#' file.show(file.path(R.home("doc"), "KEYWORDS"))  # show all keywords
#'
#' ## Help pages with documented topics starting with 'try'.
#' help.search("\\btry", fields = "alias")
#' @export
#' @importFrom proto proto
help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`),help.search)
this.help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`),help.search)

#' Rdocumentation shortcuts
#'
#' These functions provide access to Rdocumentation. Documentation on a topic with name name (typically, an R object or a data set) can be displayed by either help("name") or ?name.
#'
#' @param topic Usually, a name or character string specifying the topic for which help is sought. Alternatively, a function call to ask for documentation on a corresponding S4 method: see the section on S4 method documentation. The calls pkg::topic and pkg:::topic are treated specially, and look for help on topic in package pkg.
#' @param type the special type of documentation to use for this topic; for example, if the type is class, documentation is provided for the class with name topic. See the Section ‘S4 Method Documentation’ for the uses of type to get help on formal methods, including methods?function and method?call.
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
`?` <- with(proto(environment(help), `?` = utils::`?`,help=this.help,help.search=this.help.search),`?`)             