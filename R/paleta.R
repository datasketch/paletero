

paleta <- function(name, n = NULL, alpha = NULL, reverse = FALSE,
                   recycle = "lighter", type = NULL){

  if(all(areColors(name))){
    pal <- list(
      type = type,
      length = length(name),
      colors = name
    )
  } else{
    pal <- get_colors(pal_name = name)
  }

  if(is.null(n)){
    colors <- pal$colors
  }else{
    if(pal$type == "sequential"){
      colors <- pal$colors[round(seq(1,length(pal$colors), length.out = n))]
    } else if(pal$type == "qualitative"){
      colors <- pal$colors
      if(n > pal$length){
        colors <- pal$colors
        extra_colors <- colors
        while(n > length(colors)){
          extra_colors <- lighten(extra_colors)
          colors <- c(colors, extra_colors)
        }
        warning("Recycling with ", recycle, " colors.")
      }
      colors <- colors[1:n]
    }
  }
  if(reverse) colors <- rev(colors)
  if(!is.null(alpha)) colors <- paste0(colors, as.hexmode(alpha*255))
  remove_transparency(colors)

}

#' @export
palette_colors <- function(pal_name){
  get_colors(pal_name)$color
}



get_colors <- function(pal_name){
  palettes <- paletero:::palettes
  pals <- palettes %>% dplyr::filter(name == pal_name | palette_name == pal_name)
  if(nrow(pals) == 0) stop("Palette not found")
  if(length(unique(pals$palette_name)) > 1)
    stop("Found multiple palettes with that name: ",
         paste(unique(pals$palette_name), collapse = ", "))
  list(
    type = unique(pals$type),
    generator = unique(pals$generator),
    length = unique(pals$length),
    colors = pals$colors)
}


#' @export
availablePalettes <- function(){
  palettes <- paletero:::palettes
  c(unique(palettes$name), unique(palettes$palette_name))
}






