

#' @export
which_palette_type <- function(v, type = NULL){
  if(!is.null(type)){
    if(!type %in% palette_types()){
      stop("Palette type must be one of: ",
        dstools::collapse(palette_types()))
    } else {
      return(type)
    }
  }
  if(is.numeric(v)){
    return("sequential")
  }
  if(is.factor(v))
    v <- as.character(v)
  "categorical"
}

palette_types <- function(){
  c("categorical", "diverging", "sequential")
}


palette_mapping <- function(){
  c("continuous", "discrete")
}

