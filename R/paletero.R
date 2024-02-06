
#' @export
paletero <- function(v, var = NULL, colors = NULL, name = NULL,
                     type = "categorical",
                     color_dic = NULL,
                     color_var_name = "..colors"){

  if(is.null(colors) && is.null(name))
    stop("Need colors or a palette name")
  pal <- paleta(colors = colors, type = "categorical",
                name = name)

  if(is.vector(v)){
    colors <- pal$eval_categorical_pal(v, color_dic = color_dic)
    return(colors)
  }

  if(is.data.frame(v)){
    df <- v
    if(is.null(var)) var <- 1
    v <- df[[var]]
    colors <- pal$eval_categorical_pal(v, color_dic = color_dic)
    if(color_var_name %in% names(df))
      stop("color_var_name already in df" )
    df[[color_var_name]] <- colors
    return(df)
  }
  stop("not returning anything")

}



#' @export
paletero_old <- function(v = NULL, palette = NULL, as_fun = FALSE, ...){

  palette_type <-  which_palette_type(v, type = NULL)

  f_categorical <- function(v, palette = palette, ...){
    paletero_categorical(v, palette = palette, ...)
    # paletero_categorical(v, palette = palette, na.color = na.color,
    #              alpha = alpha, reverse = reverse,
    #              recycle = recycle)
  }
  f_sequential <- function(v, palette = palette, ...){
    # paletero_sequential(v, palette = palette, na.color = na.color,
    #              alpha = alpha, reverse = reverse)
  }
  f_diverging <- function(v, palette = palette, ...){
    # paletero_diverging(v, palette = palette, na.color = na.color,
    #              alpha = alpha, reverse = reverse)
  }
  f <- get(paste0("paletero_", palette_type))
  if(as_fun || is.null(v)) return(f)

  colors <- f(v, palette = palette, ...)
  colors
}





#' @export
paletero_categorical <- function(v, palette, na_color = "#808080", alpha = NULL,
                                 reverse = FALSE, recycle = "lighter"){
  if(!are_colors(palette) && !palette %in% available_palettes())
    stop("Palette not available")
  if(!is.null(alpha))
    na.color <- paste0(na.color, as.hexmode(alpha*255))
  #strtoi("0xFF")
  domain <- unique(as.character(v[!is.na(v)]))

  range <- paleta(palette, n = length(domain), alpha = alpha,
                  reverse = reverse, recycle = recycle, type = "qualitative")
  dic <- data.frame(domain, range, stringsAsFactors = FALSE)
  colors <- dstools::match_replace(v, dic)
  colors[is.na(v)] <- na_color
  #remove_transparency(colors)
  prismatic::color(colors)
}

#' @export
paletero_sequential <- function(v, palette, na_color = "#808080", alpha = NULL,
                                reverse = FALSE){
  if(!are_colors(palette) && !palette %in% available_palettes())
    stop("Palette not available")
  if(!is.null(alpha))
    na.color <- paste0(na.color, as.hexmode(alpha*255))

  domain <- scales::rescale(v, from = range(v, na.rm = TRUE))
  p <- paleta(palette, n = NULL, type = "sequential")
  ramp <- scales::colour_ramp(p)
  colors <- ramp(domain)
  colors[is.na(v)] <- na_color
  #remove_transparency(colors)
  prismatic::color(colors)
}


#' @export
paletero_diverging <- function(v, palette, na_color = "#808080", alpha = NULL,
                               reverse = FALSE){
  if(!are_colors(palette) && !palette %in% available_palettes())
    stop("Palette not available")
  if(!is.null(alpha))
    na.color <- paste0(na.color, as.hexmode(alpha*255))

  domain <- scales::rescale(v, from = range(v, na.rm = TRUE))
  p <- paleta(palette, n = NULL, type = "sequential") # TODO handle cases for divergente, sequencial, etc.
  ramp <- scales::colour_ramp(p)
  colors <- ramp(domain)
  colors[is.na(v)] <- na_color
  #remove_transparency(colors)
  prismatic::color(colors)
}






