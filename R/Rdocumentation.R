rdocs_url = function(){
  return("http://localhost:1337/link/")
  #return("http://rdocsv2-app-staging.us-west-1.elasticbeanstalk.com/")
}

help <- function(function_name,package_name=NULL){
  args = as.list(environment());
  if( all( sapply(args,FUN="is.null"))){
    stop("Please provide a function, package,... to search for")
  } 
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

`?` <- function (e1, e2=NULL) 
{
  help(e1,e2)
}