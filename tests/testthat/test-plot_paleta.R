test_that("multiplication works", {
  # Skip this test until fixed 
  skip("Needs to be fixed")
})

test_that("plot_paleta works with tibble input", {
  # Create sample color data
  colors_data <- tibble::tibble(
    color = c("#030412", "#25575B", "#DCA669", "#1A1D2B", 
              "#514A41", "#83785E", "#DBC38D", "#C3A472"),
    count = c(1084996, 683500, 499626, 498393, 
              307547, 169933, 160170, 102011),
    distribution = c(0.309, 0.195, 0.142, 0.142, 
                     0.0877, 0.0485, 0.0457, 0.0291)
  )
  
  # Test different visualization methods
  p1 <- plot_paleta(colors_data, method = "treemap")
  expect_s3_class(p1, "ggplot")
  
  p2 <- plot_paleta(colors_data, method = "tiles")
  expect_s3_class(p2, "ggplot")
  
  p3 <- plot_paleta(colors_data, method = "bars")
  expect_s3_class(p3, "ggplot")
  
  p4 <- plot_paleta(colors_data, method = "spectrum")
  expect_s3_class(p4, "ggplot")
  
  # Test sorting options
  p5 <- plot_paleta(colors_data, method = "tiles", sort_by = "hue")
  expect_s3_class(p5, "ggplot")
  
  p6 <- plot_paleta(colors_data, method = "treemap", sort_by = "distribution", reverse = TRUE)
  expect_s3_class(p6, "ggplot")
  
  # Test label toggle
  p7 <- plot_paleta(colors_data, method = "tiles", label_colors = FALSE)
  expect_s3_class(p7, "ggplot")
})

test_that("plot_paleta works with color vector input", {
  # Create sample color vector
  colors_vec <- c("#030412", "#25575B", "#DCA669", "#1A1D2B", 
                  "#514A41", "#83785E", "#DBC38D", "#C3A472")
  
  # Test different visualization methods with vector input
  p1 <- plot_paleta(colors_vec, method = "treemap")
  expect_s3_class(p1, "ggplot")
  
  p2 <- plot_paleta(colors_vec, method = "tiles")
  expect_s3_class(p2, "ggplot")
  
  p3 <- plot_paleta(colors_vec, method = "bars")
  expect_s3_class(p3, "ggplot")
  
  p4 <- plot_paleta(colors_vec, method = "spectrum", sort_by = "hue")
  expect_s3_class(p4, "ggplot")
  
  # Test with different options
  p5 <- plot_paleta(colors_vec, method = "tiles", sort_by = "hue", reverse = TRUE)
  expect_s3_class(p5, "ggplot")
  
  p6 <- plot_paleta(colors_vec, method = "tiles", label_colors = FALSE)
  expect_s3_class(p6, "ggplot")
  
  # Test with custom nrow
  p7 <- plot_paleta(colors_vec, method = "tiles", nrow = 2)
  expect_s3_class(p7, "ggplot")
})

test_that("plot_paleta handles edge cases", {
  # Single color
  single_color <- "#FF0000"
  p1 <- plot_paleta(single_color)
  expect_s3_class(p1, "ggplot")
  
  # Tibble with just color column
  minimal_data <- tibble::tibble(color = c("#FF0000", "#00FF00", "#0000FF"))
  p2 <- plot_paleta(minimal_data)
  expect_s3_class(p2, "ggplot")
  
  # Error cases
  expect_error(plot_paleta(NULL), "Input data must have a 'color' column")
  expect_error(plot_paleta(tibble::tibble(not_color = c("#FF0000"))), 
               "Input data must have a 'color' column")
})

test_that("plot_paleta works with all sorting options", {
  # Create sample color vector
  colors_vec <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", 
                 "#000000", "#FFFFFF", "#888888", "#FF8800")
  
  # Test all sorting methods
  sorting_methods <- c("distribution", "hue", "luminance", "saturation", 
                      "temperature", "harmony", "lab", "similarity", 
                      "category", "gradient")
  
  # Test distribution sorting with unequal distributions
  colors_with_dist <- tibble::tibble(
    color = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF"),
    distribution = c(0.4, 0.3, 0.15, 0.1, 0.05)
  )
  
  p_dist <- plot_paleta(colors_with_dist, method = "spectrum", sort_by = "distribution") 
  expect_s3_class(p_dist, "ggplot")
  p_dist_rev <- plot_paleta(colors_with_dist, method = "spectrum", sort_by = "distribution", reverse = TRUE)
  expect_s3_class(p_dist_rev, "ggplot")

  # Test hue sorting
  p_hue <- plot_paleta(colors_vec, method = "spectrum", sort_by = "hue")
  expect_s3_class(p_hue, "ggplot") 
  p_hue_rev <- plot_paleta(colors_vec, method = "spectrum", sort_by = "hue", reverse = TRUE)
  expect_s3_class(p_hue_rev, "ggplot")

  # Test luminance sorting
  p_lum <- plot_paleta(colors_vec, method = "spectrum", sort_by = "luminance")
  expect_s3_class(p_lum, "ggplot")
  p_lum_rev <- plot_paleta(colors_vec, method = "spectrum", sort_by = "luminance", reverse = TRUE) 
  expect_s3_class(p_lum_rev, "ggplot")

  # Test saturation sorting
  p_sat <- plot_paleta(colors_vec, method = "spectrum", sort_by = "saturation")
  expect_s3_class(p_sat, "ggplot")
  p_sat_rev <- plot_paleta(colors_vec, method = "spectrum", sort_by = "saturation", reverse = TRUE)
  expect_s3_class(p_sat_rev, "ggplot")

  # Test temperature sorting
  p_temp <- plot_paleta(colors_vec, method = "spectrum", sort_by = "temperature")
  expect_s3_class(p_temp, "ggplot")
  p_temp_rev <- plot_paleta(colors_vec, method = "spectrum", sort_by = "temperature", reverse = TRUE)
  expect_s3_class(p_temp_rev, "ggplot")

  # Test harmony sorting
  p_harm <- plot_paleta(colors_vec, method = "spectrum", sort_by = "harmony")
  expect_s3_class(p_harm, "ggplot")
  p_harm_rev <- plot_paleta(colors_vec, method = "spectrum", sort_by = "harmony", reverse = TRUE)
  expect_s3_class(p_harm_rev, "ggplot")

  # Test lab sorting
  p_lab <- plot_paleta(colors_vec, method = "spectrum", sort_by = "lab")
  expect_s3_class(p_lab, "ggplot")
  p_lab_rev <- plot_paleta(colors_vec, method = "spectrum", sort_by = "lab", reverse = TRUE)
  expect_s3_class(p_lab_rev, "ggplot")

  # Test similarity sorting
  p_sim <- plot_paleta(colors_vec, method = "spectrum", sort_by = "similarity")
  expect_s3_class(p_sim, "ggplot")
  p_sim_rev <- plot_paleta(colors_vec, method = "spectrum", sort_by = "similarity", reverse = TRUE)
  expect_s3_class(p_sim_rev, "ggplot")

  # Test category sorting
  p_cat <- plot_paleta(colors_vec, method = "spectrum", sort_by = "category")
  expect_s3_class(p_cat, "ggplot")
  p_cat_rev <- plot_paleta(colors_vec, method = "spectrum", sort_by = "category", reverse = TRUE)
  expect_s3_class(p_cat_rev, "ggplot")

  # Test gradient sorting
  p_grad <- plot_paleta(colors_vec, method = "spectrum", sort_by = "gradient")
  expect_s3_class(p_grad, "ggplot")
  p_grad_rev <- plot_paleta(colors_vec, method = "spectrum", sort_by = "gradient", reverse = TRUE)
  expect_s3_class(p_grad_rev, "ggplot")
  
  # Test similarity with custom reference color
  p_sim <- plot_paleta(colors_vec, method = "spectrum", sort_by = "similarity", 
                      reference_color = "#00FF00")
  expect_s3_class(p_sim, "ggplot")
})
