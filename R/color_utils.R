

are_colors <- function(x) {
  all(sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  }))
}


#' @export
color_dist <- function(colors, ref_colors){
  r <- decode_colour(colors)
  pals <- decode_colour(ref_colors)
  comp <- compare_colour(pals, r, 'rgb', method = 'cie2000')
  colnames(comp) <- colors
  comp <- comp |>  tibble::as_tibble()
  tmp <- cbind(tibble::tibble(ref_colors = ref_colors), comp)
  dist <- tidyr::pivot_longer(tmp, -ref_colors,
                       names_to = "colors",
                       values_to = "distance")
  dist <- dist |>
    dplyr::mutate(colors = prismatic::color(colors),
                  ref_colors = prismatic::color(ref_colors)) |>
    dplyr::select(colors, ref_colors, distance)
  dist
}



# lighten <- function(color, times){
#
#   prismatic::clr_lighten(color)
#
# }



color_lumuninance <- function(color){
  r <- decode_colour(color)
  xyz <- convert_colour(r, 'rgb', 'xyz')
  xyz[,'y']
}

#' @export
contrast_ratios <- function(in_colors,
                            light = "#ffffff",
                            dark = "#000000"){
  ld_luminance <- color_lumuninance(c(light,dark))
  lumi <- color_lumuninance(in_colors)

  # Ligh contrast
  light_ratio <- (ld_luminance[1]/100 + 0.05) / (lumi/100 + 0.05)
  dark_ratio <- (lumi/100 + 0.05) / (ld_luminance[2] + 0.05)
  tibble::tibble(in_colors, light = light_ratio, dark = dark_ratio)
}

#' @export
which_contrast <- function(in_colors,
                           light = "#ffffff",
                           dark = "#000000"){
  contrast <- contrast_ratios(in_colors, light = light, dark = dark) |>
    dplyr::mutate(more_contrast = ifelse(light > dark, "light", "dark")) |>
    dplyr::select(-light, -dark)
  if(nrow(contrast) == 1) return(unname(contrast$more_contrast[1]))
  contrast
}







