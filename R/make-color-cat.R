
#' @export
cat_palette <- function(color, n, mode = "iwanthue",
                        h_range = 100, c_range = 10, l_range = 10,
                        h_center = NULL, c_center = NULL, l_center = NULL
                        ){

  #color <- "#54d7f9"
  rgb <- decode_colour(color)
  hcl <- convert_colour(rgb, "rgb", "hcl")
  h_center <- h_center %||% hcl[1,'h']
  c_center <- c_center %||% hcl[1,'c']
  l_center <- l_center %||% hcl[1,'l']
  m <- 1000
  h_r <- runif(m, h_center - h_range/2, h_center + h_range/2)
  c_r <- runif(m, c_center - c_range/2, c_center + c_range/2)
  l_r <- runif(m, l_center - l_range/2, l_center + l_range/2)
  rand_hcl <- cbind(h_r,c_r,l_r)
  rand_lab <- convert_colour(rand_hcl, 'hcl','lab')
  k_means <- kmeans(rand_lab, n)
  centers <- tibble::tibble(centers = k_means$centers, sizes = k_means$size)
  centers <- centers %>% dplyr::arrange(desc(sizes))
  encode_colour(centers$centers, from = "lab")
}

#' @export
color_dist <- function(in_colors, ref_colors){
  r <- decode_colour(in_colors)
  pals <- decode_colour(ref_colors)
  comp <- compare_colour(pals, r, 'rgb', method = 'cie2000')
  colnames(comp) <- in_colors
  comp <- comp %>% tibble::as_tibble()
  tmp <- cbind(tibble::tibble(ref_colors = ref_colors), comp)
  dist <- tidyr::pivot_longer(tmp, -ref_colors,
                       names_to = "in_color",
                       values_to = "distance")
  dist
}

#' @export
which_palette <- function(in_colors, radius = 10,
                          n_pal_max = 5, n_colors_max = 20){
  # n_col_max, return palettes only with up to n_colors_max colors

  pals <- paletero::palettes %>%
    dplyr::group_by(palette_name) %>%
    dplyr::filter(dplyr::n() <= n_colors_max)
  dist <- color_dist(in_colors, pals$colors)
  top_colors <- dist %>%
    dplyr::group_by(in_color) %>%
    dplyr::arrange(distance) %>%
    dplyr::filter(distance < radius)
    # slice(1:100)
  top_pals <- top_colors %>%
    dplyr::left_join(pals, by = c("ref_colors" = "colors")) %>%
    dplyr::distinct(in_color, palette_name, .keep_all = TRUE)
  top_palettes_all_colors <- top_pals %>%
    dplyr::group_by(palette_name) %>%
    dplyr::filter(dplyr::n() >= length(in_colors)) %>%
    dplyr::summarise(total_distance = sum(distance)) %>%
    dplyr::arrange(total_distance) %>%
    dplyr::slice(1:n_pal_max)
  dplyr::pull(top_palettes_all_colors, palette_name)
}




