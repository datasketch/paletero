test_that("multiplication works", {


  colors <- c("blue")
  x <- palette_sequential(colors, n = 10)
  x


  colors <- c("blue", "orange")
  x <- palette_sequential(colors)
  x

  colors <- c("#0000FFFF", "#9731CDFF", "#C7599AFF", "#E77F6FFF", "#FFA500FF")
  x <- palette_sequential(colors, n = 3)
  x
  expect_equal(as.character(x),colors[c(1,3,5)])
  palette_sequential(colors, n = 4)



})
