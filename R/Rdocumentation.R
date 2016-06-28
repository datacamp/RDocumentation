rdocs_url = function(){
  #return("http://rdocs-v2-staging.herokuapp.com")
  return("http://rdocumentation.org")
}

help_path <- function(function_name){ 
  paste0(rdocs_url(),"/search?q=",function_name)
}

get_help <- function(function_name){
  viewer <- getOption("viewer")
  temp <- tempfile(fileext = ".html")
  download.file(help_path(function_name), temp, quiet = TRUE)
  viewer(temp)
}

help <- function( all_fields=NULL, package_name=NULL, function_name=NULL,
                  title=NULL, description=NULL, author=NULL ){
  args = as.list(environment());
  if( all( sapply(args,FUN="is.null"))){
    stop("Please provide a function, package,... to search for")
  } 
  
  # Search if no package name
  if(is.null(package_name))
    return(get_help(all_fields))
  
  # Otherwise query api
  else {
  topic_json <- fromJSON(paste0(rdocs_url(), "/api/",
                                "packages/",as.character(package_name),
                                "/topics/", as.character(all_fields)) )
  go_to_url = paste0(rdocs_url(), topic_json$uri)
 
  viewer <- getOption("viewer")
  temp <- tempfile(fileext = ".html")
  download.file(go_to_url, temp, quiet = TRUE)
  viewer(temp)
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
  
  if(substr(as.character(topicExpr),2,2)[[1]] == "?"){
    topicExpr <- substr(as.character(topicExpr),3,nchar(topicExpr)) 
  }
  get_help(as.character(topicExpr))
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
  search <- (is.call(topicExpr) && topicExpr[[1L]] == "?")
  
  if(search){
    return( help(topicExpr[[2L]]) )
  }
  
  if(substr(as.character(topicExpr),2,2)[[1]] == "?"){
    topicExpr <- substr(as.character(topicExpr),3,nchar(topicExpr)) 
    get_help(as.character(topicExpr))
  }

  if (is.call(topicExpr) && (topicExpr[[1L]] == "::" || topicExpr[[1L]] == 
                             ":::")) {
    package <- as.character(topicExpr[[2L]])
    topicExpr <- topicExpr[[3L]]
  }
  else package <- NULL
  if (search) {
    if (is.null(type))
      return(eval(substitute(help.search(TOPIC, package = PACKAGE),
                             list(TOPIC = as.character(topicExpr), PACKAGE = package))))
    else return(eval(substitute(help.search(TOPIC, fields = FIELD,
                                            package = PACKAGE), list(TOPIC = as.character(topicExpr),
                                                                     FIELD = as.character(type), PACKAGE = package))))
  }
  else {
    if (is.null(type)) {
      if (is.call(topicExpr))
        return(.helpForCall(topicExpr, parent.frame()))
      topic <- if (is.name(topicExpr))
        as.character(topicExpr)
      else e1
      return(eval(substitute(help(TOPIC, package = PACKAGE),
                             list(TOPIC = topic, PACKAGE = package))))
    }
    else {
      type <- if (is.name(type))
        as.character(type)
      else e1
      topic <- if (is.name(topicExpr))
        as.character(topicExpr)
      else {
        if (is.call(topicExpr) && identical(type, "method"))
          return(.helpForCall(topicExpr, parent.frame(),
                              FALSE))
        e2
      }
      if (type == "package")
        package <- topic
      h <- .tryHelp(topicName(type, topic), package = package)
      if (is.null(h)) {
        if (is.language(topicExpr))
          topicExpr <- deparse(topicExpr)
        stop(gettextf("no documentation of type %s and topic %s (or error in processing help)",
                      sQuote(type), sQuote(topicExpr)), domain = NA)
      }
      h
    }
  }
}
