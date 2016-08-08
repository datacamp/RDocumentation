
#' @export
rdocs_url <- function(){
  return("http://www.rdocumentation.org/")
}
.onLoad<-function(libName,packageName){
    login()
    #locate Rprofile file
    #search it for Rdocumentation
}
.getRProfile<-function(){
    if(!file.exists(file.path(Sys.getenv("HOME"),".Rprofile"))){
        file.create(file.path(Sys.getenv("HOME"),".Rprofile"),quiet=TRUE)
    }
    Rprofile<-file.path(Sys.getenv("HOME"),".Rprofile")
    return (Rprofile)
}
#' @export
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
#' @export
makeDefault<-function(){
    Rprofile<-.getRProfile()
    write("options(defaultPackages = c(getOption('defaultPackages'), 'Rdocumentation'))" ,file=Rprofile,append=TRUE)
    return (invisible())
}
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

#TODO make a package url that doesn't return JSON
.browseUrl.help<-function(url,browser){
    parsing = substring(url,18,nchar(url)-18)
    parts = strsplit(parsing,"/")
    go_to_url=paste0(Rdocumentation::rdocs_url(),"rstudio/package/",parts[[1]][3],"?viewer_pane=1")
    return (.view_help(go_to_url,"",FALSE,url,browser))
}
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
#' @export
help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help,`class<-` = `.class.help<-`,find.package=find.package.help),help)
this.help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help,`class<-` = `.class.help<-`),help)
#' @export
help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`),help.search)
this.help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`),help.search)
#' @export
`?` <- with(proto(environment(help), `?` = utils::`?`,help=this.help,help.search=this.help.search),`?`)             