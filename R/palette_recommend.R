
#' @export
palette_recommend <- function(colors, type = NULL,
                              radius = NULL,
                          n_pal_max = 5, n_colors_max = NULL,
                          return = "colors"){
  # n_col_max, return palettes only with up to n_colors_max colors

  pals <- paletero:::palettes_df  |>
    dplyr::mutate(colors = prismatic::color(colors))
  if(!is.null(n_colors_max)){
    pals <- pals |> dplyr::filter(length <= n_colors_max)
  }
  if(!is.null(type)){
    in_type <- type
    pals <- pals |> dplyr::filter(type == in_type)
  }
  pals <- pals |>
    dplyr::select(name, colors)
  dist <- color_dist(colors, pals$colors)
  top_colors <- dist |>
    dplyr::group_by(colors) |>
    dplyr::arrange(distance) |>
    dplyr::distinct() |>
    dplyr::select(in_colors = colors, everything())
  if(!is.null(radius)){
    top_colors <- top_colors |> dplyr::filter(distance < radius)
  }
  m <- pals |>
    dplyr::left_join(top_colors, by = c("colors" = "ref_colors"))

  # TODO if categorical, take the first n_max
  # if sequential, take the n_max ramp
  pal_dist <- m |>
    dplyr::group_by(name, colors) |>
    dplyr::summarise(min_distance = min(distance)) |>
    dplyr::arrange(min_distance, .by_group = TRUE) |>
    dplyr::slice_min(min_distance, n = length(colors)) |>
    dplyr::ungroup()

  pal_dist_min <- pal_dist |>
    dplyr::group_by(name) |>
    #dplyr::filter(dplyr::n() >= length(colors)) |>
    dplyr::summarise(total_distance = mean(min_distance)) |>
    dplyr::arrange(total_distance) |>
    dplyr::slice(1:n_pal_max)

  pal_name <- dplyr::pull(pal_dist_min, name)
  if(return == "name"){
    return(pal_name)
  }
  get_colors(pal_name)


}





