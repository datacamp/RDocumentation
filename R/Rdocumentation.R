#' @export
rdocs_url <- function(){
  return("http://localhost:1337/")
}
.onLoad<-function(libName,packageName){
    login()
}
#' @export
login<-function(){
    library(rjson)
    library(httr)  
    #the jsonlite package turns every variable into an array, which the /login doesn't accept, so the json is parsed with rjson
    if(file.exists(paste0(.libPaths()[1],"/Rdocumentation/config/creds.txt")) && file.info(paste0(.libPaths()[1],"/Rdocumentation/config/creds.txt"))$size>0){
        creds<-read.table(paste0(.libPaths()[1],"/Rdocumentation/config/creds.txt"),header=FALSE)
        go_to_url=paste0(rdocs_url(),"login")

        tryCatch({
                r <- POST(go_to_url,body=as.character(creds$V1),content_type("application/x-www-form-urlencoded"))
                if(status_code(r)!=200){
                    print("there is something wrong with your credentials, please try logging in to the syte in the help panel")
                }
                else{
                    print("logging you in to Rdocumentation")
                }
            },
            error=function(cond){
                print(cond)
                print("Could not log you in, something is wrong with your internet connection or Rdocumentation is offline")
            }
        )
    }
    else{
         dir.create(paste0(.libPaths()[1],"/Rdocumentation/config"),showWarnings = FALSE,recursive=TRUE)
    }
}

#overwrites the class<- function, converts help answers to json and sends them to Rdocumentation
`.class.help<-`<-function(package,value){
    library(rjson)
    if(value== "help_files_with_topic"){
        packages<-lapply(package,function(path){
            temp = strsplit(path,"/")[[1]]
            return (temp[length(temp)-2])
        })
        topic_names<-lapply(package,function(path){
            temp = strsplit(path,"/")[[1]]
            return (tail(temp,n=1))
        })
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
    return (.view_help(go_to_url,NULL,FALSE,url,browser))
}
.view_help<-function(go_to_url,body,post,arg1,arg2){
    tempDir <- paste0(.libPaths()[1],"/Rdocumentation/doc")
    htmlFile <- file.path(tempDir, "index.html")
    if(!file.exists(tempDir)){
        dir.create(tempDir)
    }
    tryCatch({
        if(post){
            r <- POST(go_to_url,config=(content_type_json()),body =body,encode="json")
            writeBin(content(r,'raw'),htmlFile)
        }
        else{
            r <- GET(go_to_url)
            writeBin(content(r,'raw'),htmlFile)
        }
        p <- tools::startDynamicHelp(NA)
        browser <-  getOption("browser")
        browseURL(paste0("http://127.0.0.1:", p, "/library/Rdocumentation/doc/index.html?viewer_pane=1&Rstudio_port=",
            as.character(Sys.getenv("RSTUDIO_SESSION_PORT")),"&RS_SHARED_SECRET=",as.character(Sys.getenv("RS_SHARED_SECRET"))),browser)
        return (invisible())
    },
    error=function(cond){
        print("Could not reach Rdocumentation, either your internet connection is bad or Rdocumentation is offline")
        if(post){
            return (baseenv()$`class<-`(arg1,arg2))
        }
        else{
            return(utils::browseURL(arg1,arg2))
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
        return (TRUE)
    }
} 

library(proto)
#' @export
help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help,`class<-` = `.class.help<-`),help)
this.help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help,`class<-` = `.class.help<-`),help)
#' @export
help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`),help.search)
this.help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`),help.search)
#' @export
`?` <- with(proto(environment(help), `?` = utils::`?`,help=this.help,help.search=this.help.search),`?`)         