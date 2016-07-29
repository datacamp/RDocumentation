#' @export
rdocs_url <- function(){
  return("http://localhost:1337/")
}

.pkgglobalenv <- new.env(parent=emptyenv())

#overwrites the class<- function, converts help answers to json and sends them to Rdocumentation
`.class.help<-`<-function(package,value){
    if(exists('s_id',envir=.pkgglobalenv)==FALSE){
        session_id=""
    }
    else{
        session_id=.pkgglobalenv$s_id
    }
    library(jsonlite)
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
        tried_all_packages=as.character(attributes(package)$tried_all_packages),help_type=as.character(attributes(package)$type),
        XS_Secret=as.character(Sys.getenv("RS_SHARED_SECRET")), Rstudio_port= as.character(Sys.getenv("RSTUDIO_SESSION_PORT"))))
        go_to_url=paste0(Rdocumentation::rdocs_url(),"rstudio/normal/help")
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
        matching_packages=as.character(gsub(" ", "", toString(unique(package$matches$Package)), fixed = TRUE)),
        XS_Secret=as.character(Sys.getenv("RS_SHARED_SECRET")), Rstudio_port= as.character(Sys.getenv("RSTUDIO_SESSION_PORT"))))
        go_to_url=paste0(Rdocumentation::rdocs_url(),"rstudio/search/help")
    }
    return (.view_help(go_to_url,body,session_id,TRUE))
}

#TODO make a package url that doesn't return JSON
.browseUrl.help<-function(url,browser){
     if(exists('s_id',envir=.pkgglobalenv)==FALSE){
        session_id=""
    }
    else{
        session_id=.pkgglobalenv$s_id
    }
    parsing = substring(url,18,nchar(url)-18)
    parts = strsplit(parsing,"/")
    go_to_url=paste0(Rdocumentation::rdocs_url(),"rstudio/package/",parts[[1]][3])
    return (.view_help(go_to_url,NULL,session_id,FALSE))
}
.view_help<-function(go_to_url,body,session_id,post){
    print(session_id)
    tempDir <- paste0(.libPaths()[1],"/Rdocumentation/doc")
    htmlFile <- file.path(tempDir, "index.html")
    dir.create(tempDir)
    if(post){
        r <- POST(go_to_url,set_cookies("sails.sid=" = as.character(session_id)),config=(content_type_json()),add_headers(Cookie= paste0("sails.sid=",as.character(session_id))),body =body,encode="json")
        print(content(r))
        print(content(r)$cookies)
        writeBin(content(r,'raw'),htmlFile)
    }
    else{
        print(session_id)
        print(as.character(session_id));
        r <- GET(go_to_url,set_cookies("sails.sid=" = as.character(session_id)),add_headers(Cookie= paste0("sails.sid=",as.character(session_id))))
        print(content(r)$cookies)
        writeBin(content(r,'raw'),htmlFile)
    }
    p <- tools::startDynamicHelp(NA)
    browser <-  getOption("browser")
    browseURL(paste0("http://127.0.0.1:", p, "/library/Rdocumentation/doc/index.html?viewer_pane=1&Rstudio_port=",
        as.character(Sys.getenv("RSTUDIO_SESSION_PORT")),"&RS_SHARED_SECRET=",as.character(Sys.getenv("RS_SHARED_SECRET")),
        "&S_id=",as.character(session_id),"&URL=",go_to_url),browser)
    return (invisible())
}
library(proto)
#TODO:check if browse_url in subfunction also fails
#' @export
help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help,`class<-` = `.class.help<-`),help)
this.help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help,`class<-` = `.class.help<-`),help)
#' @export
help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`),help.search)
this.help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`),help.search)
#' @export
`?` <- with(proto(environment(help), `?` = utils::`?`,help=this.help,help.search=this.help.search),`?`) 