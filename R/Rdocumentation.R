rdocs_url=function(){
  return("http://localhost:1337/")
}

#overwrites the class<- function, converts help answers to json and sends them to Rdocumentation
`.class.help<-`<-function(package,value){
    if(value== "help_files_with_topic"){
        packages<-lapply(package,function(path){
            temp = strsplit(path,"/")[[1]]
            return (temp[length(temp)-2])
        })
        topic_names<-lapply(package,function(path){
            temp = strsplit(path,"/")[[1]]
            return (tail(temp,n=1))
        })
        body= paste0('{"packages":"',as.character(paste(packages,sep="",collapse=",")),
        '","topic_names":"',as.character(paste(topic_names,sep="",collapse=",")),
        '","call":"',as.character(paste(attributes(package)$call,sep="",collapse=",")),
        '","topic":"',as.character(attributes(package)$topic),
        '","tried_all_packages":"',as.character(attributes(package)$tried_all_packages),
        '","help_type":"',as.character(attributes(package)$type),
        '"}')
        go_to_url=paste0(rdocs_url(),"rstudio/normal/help")
    }
    else{
        hsearch_db_fields <-c("alias", "concept", "keyword", "name", "title")
        elas_search_db_fields <-c("aliases","concept","keywords","name","title")
        fields = lapply(package$fields,function(e){
            return (elas_search_db_fields[which(hsearch_db_fields==e)])
        })
        body= paste0('{"query":"',as.character(package[1]), 
        '","fields":"',as.character(paste(fields,sep="",collapse=",")),
        '","type":"',as.character(package[3]),
        '","agrep":"',as.character(package[4]),
        '","ignore_case":"',as.character(package[5]),
        '","types":"',as.character(paste(package$types,sep="",collapse=",")),
        '","package":"',as.character(package[7]),
        '","matching_titles":"',as.character(gsub(" ", "", toString(unique(package$matches$Topic)), fixed = TRUE)),
        '","matching_packages":"',as.character(gsub(" ", "", toString(unique(package$matches$Package)), fixed = TRUE)),
        '"}')
        go_to_url=paste0(rdocs_url(),"rstudio/search/help")
    }
    print(body)
    return (.view_help(go_to_url,body,TRUE))
}

#TODO make a package url that doesn't return JSON
.browseUrl.help<-function(url,browser){
    parsing = substring(url,18,nchar(url)-18)
    print(parsing);
    parts = strsplit(parsing,"/")
    print(parts)
    go_to_url=paste0(rdocs_url(),"rstudio/package/",parts[[1]][3])
    return (.view_help(go_to_url,NULL,FALSE))
}
.view_help<-function(go_to_url,body,post){
    print(go_to_url)
    viewer <- getOption("viewer")
    if (!is.null(viewer)){
        tempDir <- tempfile()
        dir.create(tempDir)
        htmlFile <- file.path(tempDir, "index.html")
        if(post){
            r <- POST(go_to_url,config=(content_type_json()),body =body,encode="json")
            writeBin(content(r,'raw'),htmlFile)
        }
        else{
            download.file(go_to_url, destfile = htmlFile,method='internal',quiet=TRUE)
        }
        viewer(htmlFile)
    }
    else{
        utils::browseURL(go_to_url)
    }
    return (invisible())
}
library(proto)
#TODO:check if browse_url in subfunction also fails
help <- with(proto(environment(help), help = utils::help, browseURL = .browseUrl.help,`class<-` = `.class.help<-`),help)
help.search <- with(proto(environment(help), help.search = utils::help.search, `class<-` = `.class.help<-`),help.search)
`?` <- with(proto(environment(`?`), `?` = utils::`?`,help=help,help.search=help.search),`?`)