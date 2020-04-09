test_that("Background", {



  # Magick
  library(magick)

  # Split an image by color
  img <- image_read("inst/imgs/bad-bunny-9.jpg")
  img <- image_scale(img, "200")
  img

  img_no_bg <- remove_background(img)
  img_no_bg

  img_foreground(img)
  img_background(img)


  img1 <- image_read("inst/imgs/bad-bunny-4.jpg") %>% image_scale("200")
  preview_colors(img_background_color(img1))
  preview_colors(img_background_color(img1, method = "mode"))
  preview_colors(img_palette(img1, n = 8))

  img2 <- "inst/imgs/bad-bunny-9.jpg"
  img3 <- image_read("inst/imgs/bad-bunny-9.jpg") %>% image_scale("200")
  preview_colors(img_background_color(img2))
  preview_colors(img_background_color(img2, method = "mode"))
  preview_colors(img_palette(img2, n = 6))


  ## TODO CHECK IF FUZZCMEANS CAN IMPROVE QUANTATIZATION
  # image_fuzzycmeans(img3, smoothing = 0.1) %>%
  #   image_quantize(5)

X

})
