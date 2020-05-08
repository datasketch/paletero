test_that("Test map colors", {

  library(homodatum)

  data <- data.frame(x = Cat(1:10))

  map_colors(data, color_by = 1, palette = paleta("magma"))

  data <- sample_data("Cat-Num")
  paletero::map_colors(data, color_by = NULL,
                       palette = paleta("Set1"), colors_df = NULL)

  f <- fringe(data)
  paletero::map_colors(f, color_by = NULL,
                       palette = paleta("Set1"), colors_df = NULL)
  paletero::map_colors(f, color_by = names(f$data)[1],
                       palette = paleta("Set1"), colors_df = NULL)



})
