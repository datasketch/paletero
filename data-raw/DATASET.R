library(tidyverse)
library(paletteer)

p_cont <- palettes_c_names
p_disc <- palettes_d_names
p_dyna <- palettes_dynamic_names

pals_all <- bind_rows(
  list(
    paletteer_c = p_cont,
    paletteer_d = p_disc,
    paletteer_dynamic = p_dyna)
  ,
  .id = "fun") %>%
  mutate(length = ifelse(is.na(length), 20, length),
         palette_name = paste0(package,"::",palette)
         ) %>%
  filter(length < 100)

# pals_all <- sample_n(pals_all, 10)
pals <- transpose(pals_all)


l <- map(pals, function(x){
  fun <- x$fun
  params <- list()
  params$palette = x$palette_name
  params$n <- x$length
  tibble(color = do.call(fun, params))
}) %>% set_names(pals_all$palette_name)
palettes <- l %>%
  bind_rows(.id = "palette_name")

usethis::use_data(palettes)




