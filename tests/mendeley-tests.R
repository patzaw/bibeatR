library(httr)
library(dplyr)
library(glue)

r <- GET(
   "https://api.mendeley.com/documents?view=bib",
   add_headers(
      glue('Authorization'='Bearer {key}'),
      'Accept'='application/x-bibtex'
      
   )
)

getBrackPos <- function(x, op='[{]', cp='[}]'){
   oBrackPos <- setdiff(gregexpr(op, x)[[1]], -1)
   if(length(oBrackPos)==0){
      return(data.frame(s=1, e=1)[-1,])
   }
   cBrackPos <- setdiff(gregexpr(cp, x)[[1]], -1)
   if(length(cBrackPos)!=length(oBrackPos)){
      stop("Closing brackets not matching opening brackets")
   }
   posBrack <- tibble(
      pos=c(oBrackPos, cBrackPos),
      brack=c(rep("(", length(oBrackPos)), rep(")", length(cBrackPos)))
   ) %>%
      arrange(pos) %>%
      mutate(oc=ifelse(brack=="(", 1, -1)) %>%
      mutate(ib=cumsum(oc))
   e <- which(posBrack$ib==0)
   s <- c(1, e[-length(e)]+1)
   return(tibble(
      s=posBrack$pos[s],
      e=posBrack$pos[e]
   ))
}

splitBibSet <- function(x){
   stopifnot(length(x)==1)
   bp <- getBrackPos(x)
   bp$s <- c(1, bp$e[-nrow(bp)]+1)
   apply(bp, 1, function(y) substr(x, y[1], y[2]))
}

parseBibRec <- function(x){
   type <- sub("[{].*", "", sub("^[^@]*@", "", x))
   x <- sub("^[^@]*@[^{]*[{]", "", sub("[}][^}]*$", "", x))
   id <- sub(
      " *$", "",
      sub(
         "^ *", "",
            gsub(
            "[^[:graph:]^ ]", "",
            sub(",.*$", "", x)
         )
      )
   )
   x <- sub("^[^,]*,", "", x)
   bp <- getBrackPos(x)
   bp$s <- c(1, bp$e[-nrow(bp)]+1)
   strValues <- apply(bp, 1, function(y) substr(x, y[1], y[2]))
   values <- do.call(c, lapply(
      strValues,
      function(y){
         field <- sub(
            "^[^[:alnum:]]*", "",
            sub(
               "[[:blank:]]*=.*$", "",
               y
            )
         )
         value <- sub(
            "}[^}]*$", "",
            sub(
               "^[^{]*[{]", "",
               y
            )
         )
         names(value) <- field
         return(value)
      }
   ))
   return(c(
      id=id,
      type=type,
      values
   ))
}

parseBibSet <- function(x){
   do.call(bind_rows, lapply(x, parseBibRec))
}

