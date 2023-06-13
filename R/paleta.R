

paleta <- function(name, n = NULL, type = NULL, alpha = NULL, reverse = FALSE,
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






