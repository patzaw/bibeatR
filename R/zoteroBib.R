###############################################################################@
#' Get Zotero bibiliography from cloud
#'
#' Internal function (not exported)
#'
#' @param userID Zotero user identifier
#' @param key Zotero key
#' @param latestVersion A numeric indicating the version after which
#' modified records should be taken (Default: NA ==> take all records)
#' @param excludedTypes the types of record to exclude (default: "attachment")
#' @param by Number of records to be taken at once (default: 100 (maximum
#' value for Zotero API))
#' @param verbose If TRUE messages regarding the download of records are
#' displayed.
#'
#' @importFrom httr GET add_headers content
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr bind_cols bind_rows
#'
#' @return A [tibble::tibble]  with a `latestVersion` single numeric attribute
#' and the following fields:
#' - **key**: the Zotero internal key of the record
#' - **bib**: bibtex representation of the record
#' - **type**: the type of the record
#' - **id**: the record identifier
#' - **title**: the record title
#' - **journal**: the record journal
#' - **year**: publication year
#' - **authors**: the record authors
#' - **pmid**: PubMed identifier
#' - **doi**: Digital Object Identifier
#' - **url**: the record URL
#'
getZoteroBib <- function(
   baseURL="https://api.zotero.org",
   userID, key,
   latestVersion=NA,
   excludedTypes="attachment",
   by=100,
   verbose=FALSE
){
   i <- 0
   n <- 1
   bibTable <- c()
   version <- c()
   while(n>0){
      if(verbose){
         message("Getting records ", i, "->", i+by)
      }
      r <- GET(
         glue(
            "{baseURL}/users/{userID}/items",
            "?limit={by}",
            "&start={i}",
            ifelse(is.na(latestVersion), "", "&since={latestVersion}"),
            paste(glue("&itemType=-{excludedTypes}"), collapse=""),
            "&format=json&include=bibtex"
         ),
         add_headers("Zotero-API-Key"=key)
      )
      version <- unique(c(version, r$headers$`last-modified-version`))
      if(length(version)>1){
         warning("In between version update ==> restarting")
         i <- 0
         n <- 1
         bibTable <- c()
         version <- c()
      }else{
         if(i==0 && verbose){
            message(glue("{r$headers$'total-results'} records to get"))
         }
         bibTable <- bind_rows(
            bibTable,
            do.call(
               bind_rows,
               lapply(content(r), function(x) c(key=x$key, bib=x$bib))
            )
         )
         n <- length(content(r))
         by <- min(c(n, by))
         i <- i+by
      }
   }
   if("key" %in% colnames(bibTable)){
      bibTable <- bibTable %>%
         select(key, bib) %>%
         bind_cols(
            lapply(
               strsplit(bibTable$bib, split="\n"),
               function(x){
                  c(
                     type=grep("^[@]", x, value=TRUE) %>%
                        sub("[{].*$", "", .) %>%
                        sub("^[@]", "", .),
                     id=grep("^[@]", x, value=TRUE) %>%
                        sub("^@.*[{]", "", .) %>%
                        sub("[,]*", "", .),
                     title=grep("^\ttitle = [{]", x, value=TRUE) %>%
                        sub("^\ttitle = [{]", "", .) %>%
                        sub("[}],*$", "", .),
                     journal=grep("^\tjournal = [{]", x, value=TRUE) %>%
                        sub("^\tjournal = [{]", "", .) %>%
                        sub("[}],*$", "", .),
                     year=grep("^\tyear = [{]", x, value=TRUE) %>%
                        sub("^\tyear = [{]", "", .) %>%
                        sub("[}],*$", "", .),
                     authors=grep("^\tauthor = [{]", x, value=TRUE) %>%
                        sub("^\tauthor = [{]", "", .) %>%
                        sub("[}],*$", "", .),
                     pmid=grep("^\tpmid = [{]", x, value=TRUE) %>%
                        sub("^\tpmid = [{]", "", .) %>%
                        sub("[}],*$", "", .),
                     doi=grep("^\tdoi = [{]", x, value=TRUE) %>%
                        sub("^\tdoi = [{]", "", .) %>%
                        sub("[}],*$", "", .),
                     url=grep("^\turl = [{]", x, value=TRUE) %>%
                        sub("^\turl = [{]", "", .) %>%
                        sub("[}],*$", "", .)
                  )
               }
            ) %>%
               do.call(bind_rows, .)
         )
      attr(bibTable, "latestVersion") <- as.numeric(version)
      return(bibTable)
   }else{
      return(NULL)
   }
}

###############################################################################@
#' Check zoteroBib
#'
#' Internal function (not exported)
#'
#' @param x an object
#'
#' @return TRUE if x is a zoteroBib
#'
#' @importFrom dplyr is.tbl
#'
is.zoteroBib <- function(x){
   is.tbl(x) & !is.na(attr(x, "latestVersion")) &
      all(c("key", "bib", "type", "id") %in% colnames(x))
}

###############################################################################@
#' Merge 2 zoteroBib object
#'
#' Internal function (not exported)
#'
#' @param x a zoteroBib
#' @param y a zoteroBib
#'
#' @importFrom dplyr bind_rows filter
#'
mergeZoteroBib <- function(x, y){
   stopifnot(
      is.zoteroBib(x),
      is.zoteroBib(y)
   )
   if(attr(x, "latestVersion") <= attr(y, "latestVersion")){
      n <- y
      o <- x
   }else{
      n <- x
      o <- y
   }
   toRet <- n %>%
      bind_rows(
         o %>% filter(!o$key %in% n$key)
      )
   attr(toRet, "latestVersion") <- attr(n, "latestVersion")
   return(toRet)
}

###############################################################################@
#' Update Zotero bibliography
#'
#' Update the local copy of Zotero bibliography
#'
#' @param incremental a logical indicating if the update should be incremental
#' (Default: TRUE). If FALSE all the records are downloaded
#' @param verbose a logical indicating if additional messages should be
#' displayed.
#'
#' @importFrom glue glue
#' @export
#'
updateZoteroBib <- function(incremental=TRUE, verbose=TRUE){
   stopifnot(checkZoteroConn())
   bib <- zotapirEnv$bib
   if(incremental){
      latestVersion <- attr(bib, "latestVersion")
   }else{
      latestVersion <- NULL
   }
   if(is.null(latestVersion)){
      latestVersion <- NA
      if(verbose){
         message("Getting all the records")
      }
   }else{
      if(verbose){
         message(glue("Getting records from version {latestVersion}"))
      }
   }
   toAdd <- getZoteroBib(
      baseURL=zotapirEnv$baseURL,
      userID=zotapirEnv$userID,
      key=zotapirEnv$key,
      latestVersion=latestVersion,
      excludedTypes=zotapirEnv$excludedTypes,
      by=100,
      verbose=verbose
   )
   if(!incremental || is.null(bib)){
      bib <- toAdd
   }else{
      if(!is.null(toAdd)){
         bib <- mergeZoteroBib(bib, toAdd)
      }
   }
   saveRDS(bib, file=zotapirEnv$bibFile)
   assign("bib", bib, zotapirEnv)
}
