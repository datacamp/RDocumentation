rdocs_url = function(){
  return("http://localhost:1337/link/")
  #return("http://rdocsv2-app-staging.us-west-1.elasticbeanstalk.com/")
}
rdocs_package_url=function(){
  return("http://localhost:1337/")
}

help <- function(topic,package=NULL,try.all.packages = getOption("help.try.all.packages")){
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
        if (!is.name(substitute(package))&& !spackage %in% reserved) 
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
      go_to_url=paste0(rdocs_url(),as.character(topic),"/ordered")
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


#beneath is the original help.search function to mimic
whatever <-function (pattern, fields = c("alias", "concept", "title"), apropos, 
    keyword, whatis, ignore.case = TRUE, package = NULL, lib.loc = NULL, 
    help.db = getOption("help.db"), verbose = getOption("verbose"), 
    rebuild = FALSE, agrep = NULL, use_UTF8 = FALSE, types = getOption("help.search.types")) 
{
    hsearch_db_fields <-c("alias", "concept", "keyword", "name", "title")
    hsearch_db_types <- c("help", "vignette", "demo")
    .wrong_args <- function(args) gettextf("argument %s must be a single character string", 
        sQuote(args))
    #FALSE = 0, TRUE = 2
    if (is.logical(verbose)) 
        verbose <- 2 * as.integer(verbose)
    #afrep is fuzzy matching or a regexp if less han 5 characters
    fuzzy <- agrep
    #character string to be matched specified
    if (!missing(pattern)) {
        #check validity
        if (!is.character(pattern) || (length(pattern) > 1L)) 
            stop(.wrong_args("pattern"), domain = NA)
        #match given fields with fields in db
        i <- pmatch(fields, hsearch_db_fields)
        if (anyNA(i)) 
            stop("incorrect field specification")
        else fields <-hsearch_db_fields[i]
    }
    #there is a string for the page topic and title specified
    else if (!missing(apropos)) {
        #check validity
        if (!is.character(apropos) || (length(apropos) > 1L)) 
            stop(.wrong_args("apropos"), domain = NA)
        else {
            pattern <- apropos
            fields <- c("alias", "title")
        }
    }
    #keyword for keyword fields is specified
    else if (!missing(keyword)) {
        #check validity
        if (!is.character(keyword) || (length(keyword) > 1L)) 
            stop(.wrong_args("keyword"), domain = NA)
        else {
            pattern <- keyword
            fields <- "keyword"
            #set default if not already set
            if (is.null(fuzzy)) 
                fuzzy <- FALSE
        }
    }
    #string to match in help page topics is specified
    else if (!missing(whatis)) {
        #check validity
        if (!is.character(whatis) || (length(whatis) > 1)) 
            stop(.wrong_args("whatis"), domain = NA)
        else {
            pattern <- whatis
            fields <- "alias"
        }
    }
    else {
        stop("do not know what to search")
    }
    if (!missing(help.db)) 
        warning("argument 'help.db' is deprecated")
    #check types of documentation to search
    i <- pmatch(types, hsearch_db_types)
    if (anyNA(i)) 
        stop("incorrect type specification")
    else types <- hsearch_db_types[i]
    db <- hsearch_db(package, lib.loc, types, verbose, rebuild, 
        use_UTF8)
    lib.loc <- attr(db, "LibPaths")
    #match columns of help db
    if (!identical(sort(types), sort(attr(db, "Types")))) {
        db$Base <- db$Base[!is.na(match(db$Base$Type, types)), 
            ]
        db[-1L] <- lapply(db[-1L], function(e) {
            e[!is.na(match(e$ID, db$Base$ID)), ]
        })
    }
    #check package, must contain list of packages to search
    if (!is.null(package)) {
        #match packages with packages in DB
        pos_in_hsearch_db <- match(package, unique(db$Base[, 
            "Package"]), nomatch = 0L)
        if (any(pos_in_hsearch_db) == 0L) 
            print(gettextf("WARNING :no information in the local database for package %s: might be a typing error", 
                sQuote(package[pos_in_hsearch_db == 0][1L])), 
                domain = NA)
        db[] <- lapply(db, function(e) {
            e[!is.na(match(e$Package, package)), ]
        })
    }
    #verbose logging
    if (verbose >= 2L) {
        message("Database of ", NROW(db$Base), " help objects (", 
            NROW(db$Aliases), " aliases, ", NROW(db$Concepts), 
            " concepts, ", NROW(db$Keywords), " keywords)", domain = NA)
        flush.console()
    }
    if (!length(db$Base)) 
        return(invisible())
    #fuzzy matching if more than 4 characters in pattern and the string consists of only letters, numbers, and dashes
    if (is.null(fuzzy) || is.na(fuzzy)) 
        fuzzy <- (grepl("^([[:alnum:]]|[[:space:]]|-)+$", pattern) && 
            (nchar(pattern, type = "c") > 4L))
    if (is.logical(fuzzy)) {
        if (fuzzy) 
            max.distance <- 0.1
    }
    else if (is.numeric(fuzzy) || is.list(fuzzy)) {
        max.distance <- fuzzy
        fuzzy <- TRUE
    }
    else stop("incorrect 'agrep' specification")
    dbBase <- db$Base
    search_fun <- if (fuzzy) {
        function(x) {
            agrep(pattern, x, ignore.case = ignore.case, max.distance = max.distance)
        }
    }
    else {
        function(x) {
            grep(pattern, x, ignore.case = ignore.case, perl = use_UTF8)
        }
    }
    search_db_results <- function(p, f, e) data.frame(Position = p, 
        Field = f, Entry = e, stringsAsFactors = FALSE)
    search_db_field <- function(field) {
        switch(field, alias = {
            aliases <- db$Aliases$Alias
            matched <- search_fun(aliases)
            search_db_results(match(db$Aliases$ID[matched], dbBase$ID), 
                rep.int(field, length(matched)), aliases[matched])
        }, concept = {
            concepts <- db$Concepts$Concept
            matched <- search_fun(concepts)
            search_db_results(match(db$Concepts$ID[matched], 
                dbBase$ID), rep.int(field, length(matched)), 
                concepts[matched])
        }, keyword = {
            keywords <- db$Keywords$Keyword
            matched <- search_fun(keywords)
            search_db_results(match(db$Keywords$ID[matched], 
                dbBase$ID), rep.int(field, length(matched)), 
                keywords[matched])
        }, name = {
            matched <- search_fun(dbBase$Name)
            search_db_results(matched, rep.int("Name", length(matched)), 
                dbBase$Name[matched])
        }, title = {
            matched <- search_fun(dbBase$Title)
            search_db_results(matched, rep.int("Title", length(matched)), 
                dbBase$Title[matched])
        })
    }
    matches <- NULL
    for (f in fields) matches <- rbind(matches, search_db_field(f))
    matches <- matches[order(matches$Position), ]
    db <- cbind(dbBase[matches$Position, c("Topic", "Title", 
        "Name", "ID", "Package", "LibPath", "Type"), drop = FALSE], 
        matches[c("Field", "Entry")])
    rownames(db) <- NULL
    if (verbose >= 2L) {
        n_of_objects_matched <- length(unique(db[, "ID"]))
        message(sprintf(ngettext(n_of_objects_matched, "matched %d objects locally.", 
            "matched %d objects locally."), n_of_objects_matched), domain = NA)
        flush.console()
    }
    print(db)
    print(length(unique(db[,"ID"])))
    if(length(unique(db[,"ID"])) == 1){
        go_to_url=paste0(rdocs_package_url(),"packages/",as.character(db[,"Package"][1]),"/versions/",as.character(packageVersion(db[,"Package"][1])),"/topics/",db[,"Topic"][1])
    }
    else if(length(unique(db[,"ID"])) > 1) {
        go_to_url = paste0(rdocs_package_url(),"topics/",as.character(gsub(" ", "", toString(unique(db[,"Topic"])), fixed = TRUE)),"/packages/",as.character(gsub(" ", "", toString(unique(db[,"Package"])), fixed = TRUE)),"/help")
    }
    else{
        if (verbose >= 2L) {
        message("no local functions found, searching all packages on Rdocumentation")
        flush.console()
        }
        if(max.distance<1){
            max.distance = ceiling(max.distance*nchar(pattern))
        }
        go_to_url = paste0(rdocs_url(),as.character(pattern),"/",as.character(gsub(" ", "", toString(fields), fixed = TRUE)),"/",as.character(fuzzy),"/",as.character(max.distance),"/",as.character(ignore.case),"/ordered")
    }
    print(go_to_url)
    if (verbose >= 2L) {
        message("retrieving content")
        flush.console()
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