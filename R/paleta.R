paleta <- function(colors = NULL, type = NULL,
                   n = NULL, name = NULL, alpha = TRUE,
                   reverse = FALSE, recycle = TRUE){
  pal <- paletaClass$new(colors = colors, type = type, n = n,
                  name = name, alpha = alpha,
                  reverse = reverse, recycle = recycle)
  pal

}









#' Generate Color Palettes
#'
#' This function generates color palettes of various types, including sequential,
#' categorical, or divergent, with a specified number of levels.
#'
#' @param name Character string or vector, specifying the name of the palette or
#'             directly providing the colors.
#' @param n Integer, number of colors to generate. If NULL, the default length of
#'          the palette is used.
#' @param type Character string, type of the palette ("sequential", "categorical",
#'             or "diverging"). If NULL, inferred from the name or defaults to
#'             "categorical".
#' @param alpha Numeric, value of alpha transparency (between 0 and 1). If NULL,
#'              no transparency is added.
#' @param reverse Logical, if TRUE, reverses the order of the colors.
#' @param recycle Logical, if TRUE and the palette is categorical, allows recycling
#'                of colors when n exceeds the number of available colors.
#'
#' @return A character vector of color values.
#'
#' @examples
#' paleta("viridis", n = 5)
#' paleta(c("red", "green", "blue"), n = 3, type = "categorical")
#'
#' @export
paleta_old <- function(name, n = NULL, type = NULL, alpha = NULL, reverse = FALSE,
                   recycle = TRUE){

  if(all(are_colors(name))){
    pal <- list(
      type = type %||% "categorical",
      length = length(name),
      colors = prismatic::color(name)
    )
  } else{
    if(!name %in% available_palettes())
      stop("Palette name not found. Check available_palettes()")
    pal <- get_palette(name)
  }

  if(is.null(n)){
    colors <- pal$colors
  }else{
    if(pal$type == "sequential"){
      colors <- palette_sequential(pal$colors, n = n)
    } else if(pal$type == "categorical"){
      colors <- palette_categorical(pal$colors, n = n, recycle = recycle)
    } else if(pal$type == "diverging"){
      colors <- palette_diverging(pal$colors, n = n)
    }
  }
  if(reverse) colors <- rev(colors)
  if(!is.null(alpha)) colors <- paste0(colors, as.hexmode(alpha*255))
  #colors <- remove_transparency(colors)
  colors

}




get_palette <- function(name){
  palettes <- paletero:::palettes
  palettes[[name]]
}

get_colors <- function(pal_name){
  palettes <- paletero:::palettes
  pals <- palettes[pal_name]
  if(length(pals) == 1) return(pals[[1]]$colors)
  purrr::map(pals, "colors")
}

#' @export
palette_search <- function(str, type = "categorical"){
  palettes <- paletero:::palettes_df
  in_type <- type
  palettes |>
    dplyr::filter(grepl(str, name)) |>
    dplyr::filter(type == in_type) |>
    dplyr::pull(name) |> unique()
}

#' @export
available_palettes <- function(){
  palettes <- paletero:::palettes
  names(palettes)
}






