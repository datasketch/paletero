context("paletero")

test_that("Palettes",{


  brewer_pal("Set1",3)
  viridis_pal("magma",n = 10)

  v <- sample(LETTERS[1:5],10, replace = TRUE)
  v[sample(10,1)] <- NA
  palette <- "Set1"
  paletero_cat(v, palette)

  paletero("Greys")

  v <- 1:20
  v[sample(10,1)] <- NA
  paletero_num(v, "Greys")
  paletero_num(v, "magma")

  previewColors("Set1",v/20)
  previewColors("Set1",1:5)
  previewColors("PuBu",1:100)
  previewColors("Set1",LETTERS[1:3])
  previewColors("viridis",LETTERS[1:3])
  previewColors("magma",1:100)
  previewColors("inferno",1:100)


})
