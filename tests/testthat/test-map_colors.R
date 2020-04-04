test_that("Test map colors", {

  library(homodatum)

  data <- data.frame(x = Cat(1:10))

  map_colors(data, color_by = 1, palette = paleta("magma"))


})
