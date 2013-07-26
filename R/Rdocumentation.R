help_path <- function(function_name){
 paste0("http://www.rdocumentation.org/#",function_name)  
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

  if(substr(as.character(topicExpr),2,2)[[1]]=="?"){
    topicExpr <- substr(as.character(topicExpr),3,nchar(topicExpr)) 
  }
  
  get_help(as.character(topicExpr))
}

get_help = function(function_name){
  browseURL(help_path(function_name))
}

help <- function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
                  try.all.packages = getOption("help.try.all.packages"), help_type = getOption("help_type")) 
{
  types <- c("text", "html", "pdf")
  if (!missing(package)) {
    if (missing(topic)) {
      if (!missing(package)) {
        DM.temp.help <<- help_path(package=package,FUN="00index")
        return()
      }
    }
  }
  ischar <- tryCatch(is.character(topic) && length(topic) == 
                       1L, error = identity)
  if (inherits(ischar, "error")) 
    ischar <- FALSE
  if (!ischar) {
    reserved <- c("TRUE", "FALSE", "NULL", "Inf", "NaN", 
                  "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
    stopic <- deparse(substitute(topic))
    if (!is.name(substitute(topic)) && !stopic %in% reserved) 
      stop("'topic' should be a name, length-one character vector or reserved word")
    topic <- stopic
  }
  help_type <- if (!length(help_type)){ "text"
  }else{ match.arg(tolower(help_type), types) }
  paths <-   utils:::index.search(topic, find.package(package, lib.loc, verbose = verbose))
  
  if( length(paths) == 0){ 
    DM.temp.help <<- help_path(package="",FUN="")
    stop(paste("No documentation for ‘",topic," ’ in specified packages and libraries: you could try ‘??",topic,"’. (But not yet on DataMind, since that's not implemented yet ;-).",sep=""))    
  }else{
    splitted.path  <- strsplit(paths,"/")
    L <- length( splitted.path[[1]] )    
    DM.temp.help <<- help_path( package=splitted.path[[1]][L-2], FUN=splitted.path[[1]][L] )
    utils:::help(topic)
    return()
  }
}

vignette <- function (topic, package = NULL, lib.loc = NULL, all = TRUE) 
{
  if (is.null(package)) {
    package <- .packages(all.available = all, lib.loc)
    paths <- find.package(package, lib.loc, quiet = TRUE)
  }
  else paths <- find.package(package, lib.loc)
  paths <- paths[file_test("-d", file.path(paths, "doc"))]
  vignettes <- lapply(paths, function(dir) {
    tools::list_files_with_type(file.path(dir, "doc"), "vignette")
  })
  if (!missing(topic)) {
    topic <- topic[1L]
    vignettes <- as.character(unlist(vignettes))
    vidx <- (tools::file_path_sans_ext(basename(vignettes)) == topic)
    if (any(vidx) ){
      pdf  <- sub("\\.[[:alpha:]]+$", ".pdf", vignettes)
      pidx <- file_test("-f", pdf)
      ok <- vidx & pidx
      if (any(ok)) {
        idx <- min(which(ok))
        if (sum(ok) > 1) {
          warning(gettextf("vignette %s found more than once,\nusing the one found in %s", sQuote(topic), sQuote(dirname(pdf[idx]))), call. = FALSE, domain = NA)
        }
        DM.temp.help <<- vignette_path( pdf[idx])   # CHANGED
        return() # CHANGED
      }
      else {
        #TODO: what now :-)?
      }
    }
    else warning(gettextf("vignette %s not found", sQuote(topic)), 
                 call. = FALSE, domain = NA)
  }
  if (missing(topic)){
    warning("Please specify what vignette you would like to consult.")
  }
} 