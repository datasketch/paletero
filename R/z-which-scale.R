#'
#' #' @export
#' which_color_scale <- function(v, scale = NULL, palette = NULL){
#'
#'   if(is_Cat(v) || is_Bin(v) || is_Yea(v)) return("cat")
#'   if(is_Num(v) || is_Pct(c)) return("num")
#'   if(is_any_hdtype(v)){
#'     message("hdType found, defaulting to cat")
#'     return("cat")
#'   }
#'
#'   if(!is.null(scale)){
#'     if(scale == "num")
#'       v <- as.numeric(v)
#'     if(scale == "cat")
#'       v <- as.character(v)
#'     if(scale == "col"){
#'       colsIdx <- are_colors(v)
#'       v[!colsIdx] <- NA
#'     }
#'   }
#'   if(is.numeric(v))
#'     return("num")
#'   if(is.factor(v))
#'     v <- as.character(v)
#'   if(is.character(v)){
#'     if(all(are_colors(v))){
#'       return("col")
#'     }else{
#'       return("cat")
#'     }
#'   }
#'
#'
#'   NULL
#' }
#'
#'
#'
#'
