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
  mutate(length = ifelse(is.na(length), 100, length),
         palette_name = paste0(package,"::",palette)
  ) %>%
  filter(length <= 100)

# pals_all <- sample_n(pals_all, 10)
pals <- transpose(pals_all)

pals_tibble <- pals %>%
  set_names(pals_all$palette_name) %>%
  bind_rows(.id = "palette_name")

paletteer <- map(pals, function(x){
  fun <- x$fun
  params <- list()
  params$palette = x$palette_name
  params$n <- x$length
  list(
    name = x$palette,
    # type = ifelse(x$type == "qualitative", "categorical", x$type),
    type = x$type,
    generator = ifelse(fun == "paletteer_c", "continuous",
                       ifelse(fun == "paletteer_d", "discrete", "dynamic")),
    length = x$length,
    palette_name = x$palette_name,
    package = x$package,
    colors = as.character(do.call(fun, params))
  )}) %>%
  set_names(pals_all$palette_name) %>%
  bind_rows()

# Datasketch palette

dspal <- system.file("palettes","datasketch.yaml", package = "paletero")
dspal <- yaml::read_yaml(dspal)$palette
dspal <- unlist(dspal, recursive = FALSE)
dspal <- lapply(dspal, function(x){
  x$length <- length(x$colors)
  x
})

dspals <- dspal %>% bind_rows() %>%
  mutate(package = "paletero",
         palette_name = paste(package, name, sep = "::"),
         colors = paste0(colors, "FF")
  )



# Colorblind
library(colorBlindness)

avpals <- colorBlindness::availablePalette()
displayAvailablePalette()
avpals <- transpose(tribble(
  ~name,~type,
  "paletteMartin","qualitative",
  "Blue2DarkOrange12Steps","diverging",
  "Blue2DarkOrange18Steps","diverging",
  "Blue2DarkRed12Steps","diverging",
  "Blue2DarkRed18Steps","diverging",
  "Blue2Gray8Steps","diverging",
  "Blue2Green14Steps","diverging",
  "Blue2Orange10Steps","sequential",
  "Blue2Orange12Steps","sequential",
  "Blue2Orange8Steps","sequential",
  "Blue2OrangeRed14Steps","sequential",
  "Brown2Blue10Steps","sequential",
  "Brown2Blue12Steps","sequential",
  "Green2Magenta16Steps","diverging",
  "LightBlue2DarkBlue10Steps","sequential",
  "LightBlue2DarkBlue7Steps","sequential",
  "ModifiedSpectralScheme11Steps","sequential",
  "PairedColor12Steps","qualitative",
  "SteppedSequential5Steps","stepped"
))


colorblindPalettes <- lapply(avpals, function(x){
  x$colors = get(x$name)
  x$length = length(x$colors)
  x$package = "colorBlindness"
  x$palette_name = paste(x$package, x$name, sep = "::")
  x
}) %>% bind_rows()



palettes <- bind_rows(paletteer, dspals, colorblindPalettes)

usethis::use_data(palettes, internal = TRUE, overwrite = TRUE)




