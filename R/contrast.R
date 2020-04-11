

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
  contrast <- contrast_ratios(in_colors, light = light, dark = dark) %>%
    dplyr::mutate(more_contrast = ifelse(light > dark, "light", "dark")) %>%
    dplyr::select(-light, -dark)
  if(nrow(contrast) == 1) return(unname(contrast$more_contrast[1]))
  contrast
}




