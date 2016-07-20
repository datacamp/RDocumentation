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
# =======
  
#   if (missing(e2)) {
#     type <- NULL
#     topicExpr <- substitute(e1)
#   }
#   else {
#     type <- substitute(e1)
#     topicExpr <- substitute(e2)
#   }
  
#   # Capture the "??" situation and do a global search 
#   search <- (is.call(topicExpr) && topicExpr[[1L]] == "?")
  
#   if(search){
#     return( help( topicExpr[[2L]] ) )
#   }
  
#   if(substr(as.character(topicExpr),2,2)[[1]] == "?"){
#     topicExpr <- substr(as.character(topicExpr),3,nchar(topicExpr)) 
#   }
#   get_help(as.character(topicExpr))
# }

# `?` <- function (e1, e2) 
# {
#   if (missing(e2)) {
#     type <- NULL
#     topicExpr <- substitute(e1)
#   }
#   else {
#     type <- substitute(e1)
#     topicExpr <- substitute(e2)
#   }
#   search <- (is.call(topicExpr) && topicExpr[[1L]] == "?")
  
#   if(search){
#     return( help(topicExpr[[2L]]) )
#   }
  
#   if(substr(as.character(topicExpr),2,2)[[1]] == "?"){
#     topicExpr <- substr(as.character(topicExpr),3,nchar(topicExpr)) 
#     get_help(as.character(topicExpr))
#   }

#   if (is.call(topicExpr) && (topicExpr[[1L]] == "::" || topicExpr[[1L]] == 
#                              ":::")) {
#     package <- as.character(topicExpr[[2L]])
#     topicExpr <- topicExpr[[3L]]
#   }
#   else package <- NULL
#   if (search) {
#     if (is.null(type))
#       return(eval(substitute(help.search(TOPIC, package = PACKAGE),
#                              list(TOPIC = as.character(topicExpr), PACKAGE = package))))
#     else return(eval(substitute(help.search(TOPIC, fields = FIELD,
#                                             package = PACKAGE), list(TOPIC = as.character(topicExpr),
#                                                                      FIELD = as.character(type), PACKAGE = package))))
#   }
#   else {
#     if (is.null(type)) {
#       if (is.call(topicExpr))
#         return(.helpForCall(topicExpr, parent.frame()))
#       topic <- if (is.name(topicExpr))
#         as.character(topicExpr)
#       else e1
#       return(eval(substitute(help(TOPIC, package = PACKAGE),
#                              list(TOPIC = topic, PACKAGE = package))))
#     }
#     else {
#       type <- if (is.name(type))
#         as.character(type)
#       else e1
#       topic <- if (is.name(topicExpr))
#         as.character(topicExpr)
#       else {
#         if (is.call(topicExpr) && identical(type, "method"))
#           return(.helpForCall(topicExpr, parent.frame(),
#                               FALSE))
#         e2
#       }
#       if (type == "package")
#         package <- topic
#       h <- .tryHelp(topicName(type, topic), package = package)
#       if (is.null(h)) {
#         if (is.language(topicExpr))
#           topicExpr <- deparse(topicExpr)
#         stop(gettextf("no documentation of type %s and topic %s (or error in processing help)",
#                       sQuote(type), sQuote(topicExpr)), domain = NA)
#       }
#       h
#     }
#   }
# }
