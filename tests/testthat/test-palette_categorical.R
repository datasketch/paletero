test_that("Create categorical palette", {


  color <- "yellow"
  x <- palette_categorical(color)
  x

  colors <- c("green", "aliceblue")
  new_colors <- palette_categorical(colors)
  expect_equal(length(new_colors), 6)

})



test_that("Create palette", {

  # mode = "iwanthue",
  # h_range = 100, c_range = 10, l_range = 10,
  # h_center = NULL, c_center = NULL, l_center = NULL,

  # rgb <- decode_colour(color)
  # hcl <- convert_colour(rgb, "rgb", "hcl")
  # h_center <- h_center %||% hcl[1,'h']
  # c_center <- c_center %||% hcl[1,'c']
  # l_center <- l_center %||% hcl[1,'l']
  # m <- 1000
  # h_r <- runif(m, h_center - h_range/2, h_center + h_range/2)
  # c_r <- runif(m, c_center - c_range/2, c_center + c_range/2)
  # l_r <- runif(m, l_center - l_range/2, l_center + l_range/2)
  # rand_hcl <- cbind(h_r,c_r,l_r)
  # rand_lab <- convert_colour(rand_hcl, 'hcl','lab')
  # k_means <- kmeans(rand_lab, n)
  # centers <- tibble::tibble(centers = k_means$centers, sizes = k_means$size)
  # centers <- centers %>% dplyr::arrange(desc(sizes))
  # palette <- unname(encode_colour(centers$centers, from = "lab"))


})
