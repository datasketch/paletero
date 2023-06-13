test_that("Paletero 2", {

  v <- NULL
  expect_equal(which_palette_type(v, type = NULL), "categorical")

  v <- 1:4
  map1 <- paletero(1:4, palette = "viridis_magma")

  pal <- c("#000004", "#721F81", "#F1605D", "#FCFDBF")
  map2 <- paletero(1:4, palette = pal)

  expect_equal(map1, map2)

  cols <- paletero(c(NA, 1:4), palette = pal, na_color = "white")
  expect_equal(as.character(cols[1]), "#FFFFFFFF")

})



