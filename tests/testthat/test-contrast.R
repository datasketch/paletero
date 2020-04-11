test_that("Contrast", {

  in_colors <- c("#b4d749","#54d7f9")
  preview_colors(in_colors)
  contrast_ratios(in_colors)
  contrast_ratios("#f95489")
  contrast_ratios("#f95489")$light

  preview_colors("#f95489")
  which_contrast("#f95489")

})
