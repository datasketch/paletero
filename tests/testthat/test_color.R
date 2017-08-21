context("paletero")

test_that("Palettes",{

  v <- sample(letters[1:5], 10, replace = TRUE)
  expect_equal(whichColorScale(v),"cat")
  expect_true(all(paletero(v, palette = "Set2") %in% paletas("Set2")))

  v <- runif(10)
  expect_equal(whichColorScale(v),"num")
  colors <- paletero(v, palette = "Greys")

  v <- c("red","#002323", 1, "blue","xxx")
  expect_equal(whichColorScale(v),"cat")
  expect_equal(whichColorScale(v, colorScale = "col"),"col")
  v[!areColors(v)] <- NA
  expect_equal(v, paletero(v, palette = "Greys", colorScale = "col"))

  v <- iris$Species # when v is factor
  expect_equal(whichColorScale(v),"cat")

  colors <- paletero(iris, palette = "Greys", by = "Species")
  expect_equal(length(unique(iris$Species)), length(unique(colors)))

  brewer_pal("Set1",3)
  viridis_pal("magma",n = 10)

  v <- sample(LETTERS[1:5],10, replace = TRUE)
  v[sample(10,1)] <- NA
  palette <- "Set1"
  paletero_cat(v, palette)

  paletas("Greys")

  v <- 1:20
  v[sample(10,1)] <- NA
  paletero_num(v, "Greys")
  paletero_num(v, "magma")


  dsPalette <- c("#95C11E",
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
  custom_pal(dsPalette,4)
  paletero(1:10, dsPalette)
  paletero(letters[1:10], dsPalette)
  paletero(letters[1:20], dsPalette)

  expect_equal(
    paletero(letters[1:10], dsPalette),
    paletero(letters[1:10], "datasketch")
  )
  expect_equal(
    paletero(letters[1:20], dsPalette),
    paletero(letters[1:20], "datasketch")
  )



  previewColors("Set1",v/20)
  previewColors("Set2",1:5)
  previewColors("PuBu",1:100)
  previewColors("Set1",LETTERS[1:3])
  previewColors("viridis",LETTERS[1:3])
  previewColors("magma",1:100)
  previewColors("inferno",1:100)
  previewColors("datasketch",1:2)
  previewColors("amalia_light",1:2)

})
