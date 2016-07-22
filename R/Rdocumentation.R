rdocs_url = function(){
  return("http://localhost:1337/link/")
  #return("http://rdocsv2-app-staging.us-west-1.elasticbeanstalk.com/")
}
rdocs_package_url=function(){
  return("http://localhost:1337/")
}

help <- function(topic=NULL,package=NULL,try.all.packages = getOption("help.try.all.packages")){
  ischar <- tryCatch(is.character(topic) && length(topic) == 
        1L, error = identity)
  if (inherits(ischar, "error")) 
      ischar <- FALSE
  if (!ischar) {
      reserved <- c("TRUE", "FALSE", "NULL", "Inf", "NaN", 
          "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
      stopic <- deparse(substitute(topic))
      if (!is.name(substitute(topic)) && !stopic %in% reserved) 
          stop("'topic' should be a name")
      if(!stopic %in% reserved){
        topic <- stopic
      }      
  }
  ischar <- tryCatch(is.character(package) && length(topic) == 
        1L, error = identity)
    if (inherits(ischar, "error")) 
        ischar <- FALSE
    if (!ischar) {
      reserved <- c("TRUE", "FALSE", "NULL", "Inf", "NaN", 
          "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
        spackage <- deparse(substitute(package))
        if (!is.name(substitute(package))) 
          stop("'package' should be a name")
        if(!spackage %in% reserved){
          package<-spackage   
        }          
    }
  if ((!is.null(topic)) && is.null(package) &&  try.all.packages==FALSE) {
      for (p in search()){
        if(topic %in% ls(pos=p)){
            package = as.character(p)
        
      }
    }
  }
  else if(is.null(package) && try.all.packages){
    packages=c()
    for (lib in .libPaths()) {
            packages <- c(packages,.packages(TRUE, lib))
        }
  }
  else if(!is.null(package)){
     
  }
  if(!is.null(topic)){
    if(!is.null(package)){
      go_to_url=paste0(rdocs_url(),as.character(topic),"/",as.character(package),"/ordered")
    }
    else if(try.all.packages){
      array =""
      for(p in packages){
        array = paste0(array,p,",")
      }
      array = substr(array,1,nchar(array)-1)  
      go_to_url=paste0(rdocs_url(),as.character(topic),"/",array,"/ordered")
    }
    else{
      go_to_url=paste0(rdocs_url(),as.character(topic),"/",as.character(package),"/ordered")
    }
  }
  else if (!is.null(package)){
    go_to_url=paste0(rdocs_url(),"NULL/",as.character(package),"/ordered")
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

`?` <- function (e1, e2=NULL) 
{
  help(e1,e2)
}
