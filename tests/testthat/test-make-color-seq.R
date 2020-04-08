test_that("color helpers", {

  library(farver)

  color <- "#54d7f9"
  preview_colors(c(lighten(color), color, darken(color)))
  preview_colors(darker_scale(color, 10))
  preview_colors(lighter_scale(color, 5))
  preview_colors(seq_palette(color, n = 10))
  preview_colors(seq_palette(color, n = 11))

  color <- "#033434"
  preview_colors(c(lighten(color), color, darken(color)))
  preview_colors(darker_scale(color, 10))
  preview_colors(lighter_scale(color, 10))
  preview_colors(seq_palette(color, n = 10))
  preview_colors(seq_palette(color, n = 40))

  color <- "#FFC0CB"
  preview_colors(c(color, saturate(color, factor = 0.1)))
  color <- "#669999"
  preview_colors(c(color, saturate(color)))
  preview_colors(c(color, saturate(color, step = 3)))
  preview_colors(c(color, desaturate(color, step = 3)))
  preview_colors(c(
    desaturate(color, step = 2),
    desaturate(color, step = 1),
    color,
    saturate(color, step = 1),
    saturate(color, step = 2),
    saturate(color, step = 3)
    ))
  preview_colors(saturate(color, step = -1:4))

  color <- "#54d7f9"
  preview_colors(saturate(color, step = -2:3))
  preview_colors(saturate(color, step = -2:0))

  color <- "#459709"
  preview_colors(saturate(color, step = -2:3))




  # # Add value to channel
  # spectrum <- rainbow(10)
  # preview_colors(spectrum)
  #
  # preview_colors(add_to_channel(spectrum, 'l', 0.1*255, space = 'lab'))
  # preview_colors(add_to_channel(spectrum, 'l', -0.1*255, space = 'lab'))
  #
  # # Multiply a channel
  # preview_colors(multiply_channel(spectrum, 'l', 1.2, space = 'lab'))
  # preview_colors(multiply_channel(spectrum, 'l', 0.8, space = 'lab'))
  # preview_colors(multiply_channel(spectrum, 's', 0.8, space = 'hsl'))
  # preview_colors(multiply_channel(spectrum, 's', 0.2, space = 'hsl'))


})
