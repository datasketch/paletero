test_that("palette diverging", {

  colors <- c('green', 'red')

  palette_diverging(colors, n = 5)
  palette_diverging(colors, n = 7)

  expect_error(palette_diverging(colors, n = 1))
  expect_error(palette_diverging(colors, n = 10))


})
