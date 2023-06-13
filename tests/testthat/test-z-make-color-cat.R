# test_that("color helpers", {
#
#   library(farver)
#
#   color <- "#54d7f9"
#
#   cat_pal <- cat_palette(color, n = 5)
#   preview_colors(cat_pal)
#
#   preview_colors(cat_palette(color, n = 9, h_range = 360, c_range = 100))
#   preview_colors(cat_palette(color, n = 3, h_range = 200, c_range = 1,
#                              l_range = 50))
#
#   color <- "#b4d749"
#   cat_pal <- cat_palette(color, n = 5)
#   preview_colors(cat_pal)
#
#   preview_colors(cat_palette(color, n = 9, h_range = 360, c_range = 100))
#   preview_colors(cat_palette(color, n = 3, h_range = 200, c_range = 1,
#                              l_range = 50))
#
#   ###
#
#   # test_palettes <- c('RColorBrewer::OrRd', 'wesanderson::Rushmore1',
#   #                    'viridis::magma','ghibli::TotoroLight')
#
#   in_colors <- c("#b4d749","#54d7f9")
#   preview_colors(in_colors)
#   my_pals <- which_palette(in_colors, n_pal_max = 9)
#   my_pals
#   preview_colors(palette_colors(my_pals[1]))
#   preview_colors(palette_colors(my_pals[2]))
#   preview_colors(palette_colors(my_pals[3]))
#   preview_colors(palette_colors(my_pals[4]))
#   preview_colors(palette_colors(my_pals[5]))
#   preview_colors(palette_colors(my_pals[6]))
#   preview_colors(palette_colors(my_pals[7]))
#   preview_colors(palette_colors(my_pals[8]))
#   preview_colors(palette_colors(my_pals[9]))
#
#   my_pals <- which_palette(in_colors, radius = 20,
#                            n_pal_max = 9)
#   my_pals
#   preview_colors(palette_colors(my_pals[1]))
#   preview_colors(palette_colors(my_pals[2]))
#   preview_colors(palette_colors(my_pals[3]))
#   preview_colors(palette_colors(my_pals[4]))
#   preview_colors(palette_colors(my_pals[5]))
#   preview_colors(palette_colors(my_pals[6]))
#   preview_colors(palette_colors(my_pals[7]))
#   preview_colors(palette_colors(my_pals[8]))
#   preview_colors(palette_colors(my_pals[9]))
#   preview_colors(darken(palette_colors(my_pals[9])))
#
#   my_pals <- which_palette(in_colors, radius = 40,
#                            n_colors_max = 6)
#   my_pals
#   preview_colors(palette_colors(my_pals[1]))
#   preview_colors(palette_colors(my_pals[2]))
#   preview_colors(palette_colors(my_pals[3]))
#   preview_colors(palette_colors(my_pals[4]))
#   preview_colors(palette_colors(my_pals[5]))
#
#
# })
