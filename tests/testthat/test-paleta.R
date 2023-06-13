test_that("Paleta from name", {

  palettes <- available_palettes()
  palettes

  name <- "ggthemes_Traffic"
  get_colors(name)

  get_colors("ggthemes_Tableau_20")

  palette_search("excel")
  get_colors("miscpalettes_excel")

  s <- palette_search("economist")
  s
  get_colors(s)

  s <- palette_search("paletero")
  s
  get_colors(s[1])
  get_colors(s[2])

  s <- palette_search("paletero", type = "sequential")
  s
  get_colors(s[1])
  get_colors(s[2])
  get_colors(s[3])
  get_colors(s[4])

  s <- palette_search("paletero", type = "diverging")
  s
  get_colors(s[1])
  get_colors(s[2])
  get_colors(s[3])
  get_colors(s[4])


})

test_that("Paleta from name with n values", {

  # paleta from named pal
  expect_error(paleta("non_existing_palette"),
               "Palette name not found. Check available_palettes()")


  # #ith n colors

  ## Categorical
  name <- "miscpalettes_excel"
  colors <- paleta(name, n = 5) # n < palette lenght
  colors
  expect_equal(colors, get_colors(name)[1:5])
  colors <- paleta(name, n = 20) # n < palette lenght
  colors
  expect_equal(length(colors), 20)

  expect_error(paleta(name, n = 20, recycle = FALSE))

  ## Sequential

  name <- "viridis_viridis"
  get_palette(name)
  pal_cols <- get_colors(name)
  colors <- paleta(name, n = 5) # n < palette length
  expect_equal(pal_cols[1], colors[1])
  expect_equal(pal_cols[100], colors[5])

  ## Diverging
  name <- "colorBlindness_Green2Magenta16Steps"
  get_palette(name)
  pal_cols <- get_colors(name)
  colors <- paleta(name, n = 5) # n < palette lenght
  expect_equal(pal_cols[1], colors[1])
  expect_equal(pal_cols[length(pal_cols)], colors[5])


})

test_that("Paleta from list of colors", {


  # Tries to create a palette from the given color
  name <- "blue"
  paleta(name, n = 6) # n < palette lenght, default categorical

  paleta(name, n = 5, type = "sequential")
  paleta(name, n = 5, type = "diverging")




})




