map_colors <- function(data, color_by = NULL, palette = NULL,
                       colors_df = NULL,
                       preview = FALSE){

  if(is.null(palette)) stop("need a palette")
  f <- fringe(data)
  if(is.empty(color_by)){
    palette <- palette[1]
    values <- rep(1,getFringeStats(f)$nrow)
  }else{
    values <- pullFringeColumn(f, color_by)
  }
  colors <- paletero(values, palette)
  if(preview){
    paletero:::preview_colors(colors)
  }
  colors
}
