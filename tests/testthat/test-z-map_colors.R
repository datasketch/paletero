# test_that("Test map colors", {
#
#   #library(homodatum)
#
#   data <- data.frame(x = letters[1:10])
#
#   map_colors(data, palette = "magma")
#
#   map_colors(data, color_by = 1, palette = "magma")
#
#   map_colors(data, color_by = NULL,
#                        palette = "Set1", colors_df = NULL)
#
#   map_colors(data, color_by = NULL,
#                        palette = "Set1", colors_df = NULL)
#   map_colors(data, color_by = names(data)[1],
#                        palette = "Set1", colors_df = NULL)
#
#   data <- sample_data("Cat-Yea-Num", addNA= FALSE)
#
#
#
#   expect_error(paletero::map_colors(d, color_by = 'a', palette, colors_df = NULL),
#   "Need a palette")
#
#
#   # TODO: throw error when color_by is not a column
#
#   # d <- f$d()
#   # paletero::map_colors(d, color_by = 'adipisicing_cat', palette = "Set1", colors_df = NULL)
#   # pal <- c("#4578fa", "#9054d0")
#   # paletero::map_colors(d, color_by = 'adipisicing_cat', palette = pal, colors_df = NULL)
#
#
# })
