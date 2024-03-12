test_that("paletero", {
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

})
