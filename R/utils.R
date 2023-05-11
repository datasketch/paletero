
do.call.namespace <- function(what, args, ...) {
  if(is.character(what)){
    fn <- strsplit(what, "::")[[1]]
    what <- if(length(fn)==1) {
      get(fn[[1]], envir=parent.frame(), mode="function")
    } else {
      get(fn[[2]], envir=asNamespace(fn[[1]]), mode="function")
    }
  }
  do.call(what, as.list(args), ...)
}

remove_transparency <- function(x){
  if(sum(nchar(x)) == 9 && grepl("FF$",x)){
    return(substring(x, 1, 7))
  }
  x
}

match_replace <- function(v,dic, force = TRUE){
  matches <- dic[[2]][match(v,dic[[1]])]
  out <- matches
  if(!force)
    out[is.na(matches)] <- v[is.na(matches)]
  out
}

`%||%` <- function (x, y)
{
  if (is.empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if (class(x) == "character" && sum(nchar(x), na.rm = T) == 0)
    return(y)
  else x
}

is.empty <- function (x)
{
  !as.logical(length(x))
}

