test_that("Palettes", {

  colors <- "#896023"
  palette_recommend(colors)

  palette_recommend(colors, type = "categorical")
  palette_recommend(colors, type = "sequential")

  colors <- c("#F7DC05FF", "#3D98D3FF", "#EC0B88FF")
  palette_recommend(colors, type = "categorical")

  colors <- c( "#3D98D3FF","#F7DC05FF","#EC0B88FF")
  palette_recommend(colors)

  colors <- get_colors("awtools_ppalette")
  palette_recommend(colors)
  rec <- palette_recommend(colors, return = "name")
  rec
  expect_equal(rec[1], "awtools_ppalette")

  ## TODO
  # categorical: return the top matching colors in any order
  # sequential: return the top matching colors

})
