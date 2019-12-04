###############################################################################@
#' Check the connection with Zotero library
#'
#' @param verbose if TRUE print information about the Zotero connection
#' (default: FALSE).
#'
#' @return A logical:
#'  - TRUE if the connection can be established
#'  - Or FALSE if the connection cannot be established.
#'
#' @seealso [connectToZotero]
#'
#' @importFrom httr GET content
#' @importFrom glue glue glue_collapse
#'
#' @export
#'
checkZoteroConn <- function(verbose=FALSE){
   toRet <- TRUE
   if(
      is.null(zotapirEnv$baseURL) ||
      is.null(zotapirEnv$key) ||
      is.null(zotapirEnv$userID)
   ){
      warning("Not connected to Zotero")
      toRet <- FALSE
   }
   if(is.null(zotapirEnv$baseURL) || is.null(zotapirEnv$key)){
      rc <- list()
   }else{
      r <- GET(
         glue(
            "{zotapirEnv$baseURL}/keys/{zotapirEnv$key}"
         )
      )
      rc <- content(r)
      if(
         r$status_code!=200 ||
         length(rc$key)==0 ||
         rc$key!=zotapirEnv$key ||
         rc$userID!=zotapirEnv$userID
      ){
         toRet <- FALSE
      }
   }
   if(verbose){
      if(!toRet){
         warning("Not connected to Zotero: invalid connection")
         message(glue("baseURL: {zotapirEnv$baseURL}"))
         message(glue("Local key: {zotapirEnv$key}"))
         message(glue("Remote key: rc$key"))
         message(glue("Local userID: {zotapirEnv$userID}"))
         message(glue("Remote userID: rc$userID"))
      }
      message(glue("baseURL: {zotapirEnv$baseURL}"))
      message(glue("Key: {zotapirEnv$key}"))
      message(glue("userID: {zotapirEnv$userID}"))
      message(glue(
         "excludedTypes: ",
         "{glue_collapse(zotapirEnv$excludedTypes, sep=', ')}"
      ))
   }
   return(toRet)
}


###############################################################################@
#' Connect Zotero
#'
#' @param baseURL a character string (e.g. "https://api.zotero.org").
#' @param key a character string (see https://www.zotero.org/settings/keys)
#' @param connection the id of the connection already registered to use. By
#' default the first registered connection is used.
#' @param remember if TRUE the connection is registered. All the registered
#' connections can be listed with [lsZoteroConnections] and any of
#' them can be forgotten with [forgetZoteroConn].
#'
#' @return This function does not return any value. It prepares the zotapir
#' environment to allow transparent Zotero bibliography calls.
#'
#' @details Be carefull that you should reconnect to Zotero each time
#' the environment is reloaded.
#'
#'  @seealso [forgetZoteroConn], [checkZoteroConn], [lsZoteroConnections]
#'
#' @importFrom httr GET content
#' @importFrom glue glue glue_collapse
#' @export
#'
connectToZotero <- function(
   baseURL=NULL,
   key=NULL,
   excludedTypes="attachment",
   connection=1,
   remember=TRUE,
   importPath=NULL
){
   zoteroDir <- file.path(
      Sys.getenv("HOME"), "R", "zotapir"
   )
   dir.create(zoteroDir, showWarnings=FALSE, recursive=TRUE)
   conFile <- file.path(
      zoteroDir, "Zotero-Connections.rda"
   )
   connections <- list()
   if(file.exists(conFile)){
      load(conFile)
   }
   if(length(baseURL)==0 && length(key)==0){
      if(length(connections)==0){
         checkZoteroConn(verbose=TRUE)
         return(FALSE)
      }else{
         baseURL <- connections[[connection]][["baseURL"]]
         userID <- connections[[connection]][["userID"]]
         key <- connections[[connection]][["key"]]
         excludedTypes <- connections[[connection]][["excludedTypes"]]
      }
      connections <- c(connections[connection], connections[-connection])
   }else{
      if(length(baseURL)==0 || length(key)==0){
         stop("baseURL and key parameters are mandatory")
      }
      userID <-  GET(
         glue(
            "{baseURL}/keys/{key}"
         )
      ) %>% content() %>% `$`("userID")
      connections <- c(
         list(list(
            baseURL=baseURL, userID=userID, key=key,
            excludedTypes=excludedTypes
         )),
         connections
      )
   }
   ## The connection
   assign("baseURL", baseURL, zotapirEnv)
   assign("key", key, zotapirEnv)
   assign("userID", userID, zotapirEnv)
   assign("excludedTypes", excludedTypes, zotapirEnv)
   corrConn <- checkZoteroConn(verbose=TRUE)
   if(!corrConn){
      rm("baseURL", "key", "userID", "excludedTypes", envir=zotapirEnv)
      return(FALSE)
   }
   ##
   if(remember){
      connections <- connections[which(
         !duplicated(unlist(lapply(
            connections,
            function(x){
               x[c("baseURL", "key", "excludedTypes")]
            }
         )))
      )]
      save(connections, file=conFile)
   }
   ## Local data
   zotapirDir <- file.path(
      Sys.getenv("HOME"), "R",
      "zotapir",
      glue(
         sub(
            "[:]", "..",
            sub(
               "[/].*$", "",
               sub("^https{0,1}[:][/]{2}", "", baseURL)
            )
         ),
         "..",
         key,
         "..-",
         glue_collapse(excludedTypes, sep="-")
      )
   )
   dir.create(zotapirDir, showWarnings=FALSE, recursive=TRUE)
   bibFile <- file.path(zotapirDir, "zotapir-bib.rds")
   assign(
      "bibFile",
      bibFile,
      zotapirEnv
   )
   if(file.exists(bibFile)){
      bib <- readRDS(bibFile)
   }else{
      bib <- NULL
   }
   assign(
      "bib",
      bib,
      zotapirEnv
   )
   updateZoteroBib()
}

###############################################################################@
#' List all registered Zotero connection
#'
#' @seealso [forgetZoteroConn], [checkZoteroConn], [connectToZotero]
#'
#' @export
#'
lsZoteroConnections <- function(){
   conFile <- file.path(
      Sys.getenv("HOME"), "R", "zotapir", "Zotero-Connections.rda"
   )
   connections <- list()
   if(file.exists(conFile)){
      load(conFile)
   }
   return(connections)
}

###############################################################################@
#' Forget a Zotero connection
#'
#' @param connection the id of the connection to forget.
#'
#' @seealso [lsZoteroConnections], [checkZoteroConn], [connectToZotero]
#'
#' @export
#'
forgetZoteroConn <- function(connection){
   conFile <- file.path(
      Sys.getenv("HOME"), "R", "zotapir", "Zotero-Connections.rda"
   )
   connections <- list()
   if(file.exists(conFile)){
      load(conFile)
   }
   connections <- connections[-connection]
   save(connections, file=conFile)
}


###############################################################################@
###############################################################################@
zotapirEnv <- new.env(hash=TRUE, parent=emptyenv())
.onLoad <- function(libname, pkgname){
   connectToZotero()
}
