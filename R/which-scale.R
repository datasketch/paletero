
#' @export
#' @import homodatum
which_color_scale <- function(v, colorScale = NULL){

  if(is_Cat(v) || is_Bin(v) || is_Yea(v)) return("cat")
  if(is_Num(v) || is_Pct(c)) return("num")
  if(is_any_hdType(v)){
    return("cat")
    message("hdType found, defaulting to cat")
  }

  if(!is.null(colorScale)){
    if(colorScale == "num")
      v <- as.numeric(v)
    if(colorScale == "cat")
      v <- as.character(v)
    if(colorScale == "col"){
      colsIdx <- areColors(v)
      v[!colsIdx] <- NA
    }
  }
  if(is.numeric(v))
    return("num")
  if(is.factor(v))
    v <- as.character(v)
  if(is.character(v)){
    if(all(areColors(v))){
      return("col")
    }else{
      return("cat")
    }
  }
  NULL
}


areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

