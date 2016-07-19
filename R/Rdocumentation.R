rdocs_url = function(){
  return("http://localhost:1337/link/")
  #return("http://rdocsv2-app-staging.us-west-1.elasticbeanstalk.com/")
}

help_path <- function(function_name){ 
  paste0(rdocs_url(),"#",function_name)  
}

get_help = function(function_name){
  browseURL(help_path(function_name))
}

help <- function(function_name,package_name=NULL){
  args = as.list(environment());
  if( all( sapply(args,FUN="is.null"))){
    stop("Please provide a function, package,... to search for")
  } 

  #go_to_url = paste0(rdocs_url(),"advanced_search?utf8=✓", 
            #"&q=",as.character(all_fields), 
            #"&package_name=", as.character(package_name), 
            #"&function_name=", as.character(function_name),
            #"&title=", as.character(title), 
            #"&description=", as.character(description), 
            #"&author=", as.character(author) )
  if(!is.null(function_name)){
    if(!is.null(package_name)){
      go_to_url=paste0(rdocs_url(),as.character(function_name),"/",as.character(package_name),"/ordered")
    }
    else{
      go_to_url=paste0(rdocs_url(),as.character(function_name),"/ordered")
    }
    viewer <- getOption("viewer")
    if (!is.null(viewer)){
      tempDir <- tempfile()
      dir.create(tempDir)
      htmlFile <- file.path(tempDir, "index.html")
      download.file(go_to_url, destfile = htmlFile,method='internal',quiet=TRUE)
      viewer(htmlFile)
    }
    else{
      utils::browseURL(go_to_url)
    }
  }
  else{
    print("please input a valid function name and or package name")
  }  
}

`?` <- function (e1, e2) 
{
  
  if (missing(e2)) {
    type <- NULL
    topicExpr <- substitute(e1)
  }
  else {
    type <- substitute(e1)
    topicExpr <- substitute(e2)
  }
  
  # Capture the "??" situation and do a global search 
  search <- (is.call(topicExpr) && topicExpr[[1L]] == "?")
  
  if(search){
    return( help( topicExpr[[2L]] ) )
  }
  
  if(substr(as.character(topicExpr),2,2)[[1]]=="?"){
    topicExpr <- substr(as.character(topicExpr),3,nchar(topicExpr)) 
  }
  
  get_help(as.character(topicExpr))
}