test_that("Are colors", {

  expect_true(are_colors("transparent"))
  expect_true(are_colors(NA))

  expect_true(are_colors("#45d087"))
  expect_true(are_colors("#45d087AA"))

  expect_true(are_colors("yellow"))


})


test_that("Color distance", {

  colors <- "#45d087"
  ref_colors <- c("#b4d749","#54d7f9")
  color_dist(colors, ref_colors)

  colors <- c("#45D087FF", "#CD4587FF")
  ref_colors <- c("#D4D749FF","#54D7F9FF")
  color_dist(colors, ref_colors)

  })


test_that("Contrast",{

  in_colors <- c("#b4d749","#54d7f9")
  which_contrast("#f95489")
  contrast_ratios(in_colors)
  contrast_ratios("#f95489")
  contrast_ratios("#f95489")$light

})



test_that("Preview colors", {

  # preview_colors(in_colors)
  # preview_colors("#f95489")


})





