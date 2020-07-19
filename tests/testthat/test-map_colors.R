test_that("Test map colors", {

  library(homodatum)

  data <- data.frame(x = Cat(1:10))

  map_colors(data, color_by = 1, palette = "magma")

  data <- sample_data("Cat-Num")
  paletero::map_colors(data, color_by = NULL,
                       palette = "Set1", colors_df = NULL)

  f <- fringe(data)
  paletero::map_colors(f, color_by = NULL,
                       palette = "Set1", colors_df = NULL)
  paletero::map_colors(f, color_by = names(f$data)[1],
                       palette = "Set1", colors_df = NULL)

  data <- sample_data("Cat-Yea-Num", addNA= FALSE)
  f <- homodatum::fringe(data)
  nms <- fringe_labels(f)
  d <- fringe_d(f)

  expect_error(paletero::map_colors(d, color_by = 'a', palette, colors_df = NULL),
  "Need a palette")

  paletero::map_colors(d, color_by = 'a', palette = "Set1", colors_df = NULL)

  pal <- c("#4578fa", "#9054d0")
  paletero::map_colors(d, color_by = 'a', palette = pal, colors_df = NULL)


})
