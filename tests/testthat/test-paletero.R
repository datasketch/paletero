test_that("Paletero 2", {

  v <- NULL
  expect_equal(which_palette_type(v, type = NULL), "categorical")

  v <- 1:4
  map1 <- paletero(1:4, name = "viridis_magma")
  map1

  pal <- c("#000004", "#721F81", "#F1605D", "#FCFDBF")
  map2 <- paletero(1:4, colors = pal)

  # expect_equal(map1, map2)

  # cols <- paletero(c(NA, 1:4), name = pal, na_color = "white")
  # expect_equal(as.character(cols[1]), "#FFFFFFFF")

  # cols <- paletero(c(NA, rev(1:4)), palette = pal, na_color = "white")


  colors <- c("#5476b0", "#b96739")
  df <- iris
  paletero(df, var = "Species", colors = colors)

  paletero(df, var = "Species", colors = c("#DADADA"))


})

test_that("Paletero with given colors", {


  cols <- c("red", "blue")


  # dados unos colores fijos, sacar una paleta con base en esos colores de N colores
  paleta(cols, n = 10)
  paleta(cols, n = 10, type = "sequential")

  # Cuando entra un vector categórico, mapear los colores a las categorías dadas
  # y que se mantenga cuando cambia la base3
  v <- letters[1:3]
  paletero(v, colors = c('red', 'blue'))

  # Entra un vector de colores



  pal <- c("#000004", "#721F81")
  paletero(1:100, colors = pal)


  paletero(letters[1:2], colors = pal)

  # Paletero debe retornar o el mapeo de color con el valor, o una función o el mínimo y máximo
  # param include alpha o no

  # que funcione con data.frame y uno pueda nombrar la columna de entrada y de salida de color

  # cuando queremos que todas sean de un solo color... la paleta tiene un único color

  # Cuando se quiere hacer un highlight... pasarle una condición como se hace en dplyr
  # la condición más simple... es un categoría de una columna para resaltar, o un índice
  # de todas las filas del data.frame para resaltar
  # y si cumple se queda con el color original y los demás en gris
  # y si cumple se le puede asignar un color difente





})



