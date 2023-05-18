#' @export
map_colors <- function(data, color_by = NULL, palette = NULL,
                       colors_df = NULL,
                       preview = FALSE){

  if(is.null(palette) || class(palette) == "function") stop("Need a palette")
  f <- hdtable(data)
  if(is.empty(color_by)){
    palette <- palette[1]
    values <- rep(1,hdtable(f)$nrow)
  }else{
    values <- hdtable_column(f, color_by)
  }
  colors <- paletero(values, palette)
  if(preview){
    paletero:::preview_colors(colors)
  }
  colors
}
