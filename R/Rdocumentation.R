rdocs_url = function(){
  return("http://rdocs-staging.herokuapp.com/")
}

help_path <- function(function_name){ 
  paste0(rdocs_url(),"#",function_name)  
}

get_help = function(function_name){
  browseURL(help_path(function_name))
}

help <- function( all_fields=NULL, package_name=NULL, function_name=NULL,
                  title=NULL, description=NULL, author=NULL ){
  args = as.list(environment());
  if( all( sapply(args,FUN="is.null"))){
    stop("Please provide a function, package,... to search for")
  } 

  go_to_url = paste0(rdocs_url(),"advanced_search?utf8=✓", 
            "&q=",as.character(all_fields), 
            "&package_name=", as.character(package_name), 
            "&function_name=", as.character(function_name),
            "&title=", as.character(title), 
            "&description=", as.character(description), 
            "&author=", as.character(author) )
  browseURL(go_to_url)
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