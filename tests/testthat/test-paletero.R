context("paletero")

test_that("Paleta",{

  set1 <- paleta("Set1")
  paleta("Set1", n = 5)
  expect_equal(paleta("Set1", n = 12), c(set1, lighten(set1)[1:3]))

  paleta("RColorBrewer::Purples")
  paleta("magma")

  "Greys" %in% availablePalettes()

})



test_that("Palettes",{

  v <- c("A", "A", "B", "C", "D", "C", "B", "B")
  expect_equal(which_color_scale(v),"cat")
  f <- paletero(v, palette = "Set2", as_fun = TRUE)
  expect_equal(paletero(v, palette = "Set2"), f(v))


  preview_colors(paleta("magma"))

  preview_colors(paletero(v, palette = "magma"))

  f <- paletero(v, palette = "Set2", as_fun = TRUE)
  f(v)

  expect_true(all(paletero(v, palette = "Set2") %in% paleta("Set2")))



  v <- runif(10)
  expect_equal(which_color_scale(v),"num")
  colors <- paletero(v, palette = "Greys")
  f <- paletero(v, palette = "Greys", as_fun = TRUE)
  f(v)
  expect_equal(paletero(v, palette = "Greys"), f(v))




  v <- c("red","#002323", 1, "blue","xxx")
  expect_equal(which_color_scale(v),"cat")
  expect_equal(which_color_scale(v, scale = "col"),"col")
  v[!areColors(v)] <- NA
  expect_equal(v, paletero(v, palette = "Greys", scale = "col"))

  v <- iris$Species # when v is factor
  expect_equal(which_color_scale(v),"cat")

  colors <- paletero(iris, palette = "Greys", by = "Species")
  expect_equal(length(unique(iris$Species)), length(unique(colors)))


  v <- sample(LETTERS[1:5],10, replace = TRUE)
  v[sample(10,1)] <- NA
  palette <- "Set1"
  paletero_cat(v, palette)

  paleta("Greys")

  v <- 1:20
  v[sample(10,1)] <- NA
  paletero_num(v, "Greys")
  paletero_num(v, "magma")


  custom <- c("#95C11E",
                 "#FFED00",
                 "#E5007D",
                 "#009EE3",
                 "#F9B233",
                 "#EF8998",
                 "#16C5E0",
                 "#A839B0",
                 "#C92F2F",
                 "#A9A9A9",
                 "#9B71AF")
  paletero(1:10, custom)

  # estimar solo los colores nuevos, o repetir

  v <- letters[1:10]
  paletero(v, custom)
  paletero(letters[1:20], custom)

  preview_colors(c("#fdaffd","#dc3434","#3434dc"))
  preview_colors(1:5,"Set2")
  # preview_colors(1:100,"PuBu")
  preview_colors(LETTERS[1:3],"Set1")
  preview_colors(LETTERS[1:3],"viridis")
  preview_colors(1:100,"magma")
  preview_colors(1:100,"inferno")


})
