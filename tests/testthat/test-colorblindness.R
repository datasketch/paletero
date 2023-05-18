test_that("color blindness", {


  colors <- paleta("secuoya")
  test_cvd(colors)

  test_cvd(palette = c("Red", "Green", "blue"))

  test_cvd(palette = c("Red", "Green", "blue"), background = "#454567")


  # library(ggmagic)
  # #library(homodatum)
  #
  # data <- sample_data("Cat-Num")
  # gg <- gg_bar_CatNum(data, color_by = names(data)[1])
  # gg
  # test_cvd(gg = gg)



  # library(colorBlindness)
  # displayColors(c("Red", "Green", "blue"))
  # cvdPlot(replacePlotColor(displayColors(c("Red", "Green", "blue"))))
  #
  # displayAvailablePalette(color="green")
  #
  # colorBlindness::displayAllColors(c("Red", "Green", "blue"), color="white")
  #
  # replacePlotColor()
  #
  # colorBlindness::cvdSimulator("#459280", type = "protanope")
  # colorBlindness::cvdSimulator("#459280", type = "deuteranope")
  # colorBlindness::cvdSimulator("#459280", type = "desaturate")
  # colorBlindness::cvdSimulator("#459280", type = "safe")
  # colorBlindness::cvdSimulator("#459280", type = "none")
  #
  # colorBlindness::displayColors()



})
