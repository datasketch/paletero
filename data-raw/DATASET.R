library(tidyverse)
library(paletteer)

p_disc <- palettes_d_names
p_cont <- palettes_c_names
p_dyna <- palettes_dynamic_names

pals_all <- bind_rows(
  list(
    paletteer_d = p_disc,
    paletteer_c = p_cont,
    paletteer_dynamic = p_dyna)
  ,
  .id = "fun")

# Remove palettesForR
remove_palettesForR <- pals_all |>
  dplyr::filter(package == "palettesForR" & length > 102)
pals_all <- pals_all |>
  dplyr::anti_join(remove_palettesForR)


unique(pals_all$type)

pals_all <- pals_all |>
  mutate(length = ifelse(is.na(length), 100, length),
         palette_name = paste0(package,"_",palette),
         palette_fun = paste0(package,"::",palette)
  )

pals <- transpose(pals_all)

pals_tibble <- pals %>%
  set_names(pals_all$palette_name) %>%
  bind_rows(.id = "palette_name")




paletteer <- map(pals, function(x){
  #x <- pals[[114]]
  fun <- x$fun
  params <- list()
  params$palette = x$palette_fun
  params$n <- x$length

  colors <- tryCatch({do.call(fun, params)}, error = function(e)NA)
  colors <- as.character(colors)

  list(
    name = x$palette,
    # type = ifelse(x$type == "qualitative", "categorical", x$type),
    type = x$type,
    generator = ifelse(fun == "paletteer_c", "continuous",
                       ifelse(fun == "paletteer_d", "discrete", "dynamic")),
    length = length(colors),
    palette_name = x$palette_name,
    package = x$package,
    colors = colors
  )}) %>%
  set_names(pals_all$palette_name) %>%
  bind_rows()

paletteer <- paletteer |>
  filter(!is.na(colors))

# Datasketch palette

dspal <- system.file("palettes","datasketch.yaml", package = "paletero")
dspal <- yaml::read_yaml(dspal)$palette
dspal_light <- lapply(dspal$light, function(x){
  x$length <- length(x$colors)
  x$theme <- "light"
  x
}) |> bind_rows()
dspal_dark <- lapply(dspal$dark, function(x){
  x$length <- length(x$colors)
  x$theme <- "dark"
  x
})|> bind_rows()

dspal <- bind_rows(dspal_light, dspal_dark)

dspals <- dspal |>  bind_rows() |>
  mutate(package = "paletero",
         palette_name = paste(package, name, sep = "_"),
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
  x$palette_name = paste(x$package, x$name, sep = "_")
  x
}) %>% bind_rows()



palettes <- bind_rows(paletteer, dspals, colorblindPalettes)
names(palettes)
unique(palettes$type)

# x <- palettes |> filter(type == "stepped")
# prismatic::color(x$colors)
# x <- palettes |> filter(type == "diverging") |>
#   group_split(name)
# prismatic::color(x[[1]]$colors)
#
# x <- palettes |> filter(type == "sequantial") |>
#   group_split(name)
# length(x)
# prismatic::color(x[[3]]$colors)
#
# x <- palettes |> filter(type == "qualitative") |>
#   group_split(name)
# length(x)
# prismatic::color(x[[3]]$colors)


palettes_df <- palettes |>
  mutate(type = case_when(
    type %in% c("sequential", "sequantial") ~ "sequential",
    type %in% c("diverging", "divergent") ~ "diverging",
    type %in% "qualitative" ~ "categorical",
    .default = "stepped"
  )) |>
  mutate(theme = if_else(is.na(theme), "light", theme))

palettes_df <- palettes_df |>
  select(-name) |>
  select(name = palette_name, everything())


length(unique(palettes_df$name))

rep_names <- palettes_df |>
  group_by(name) |>
  summarise(rep = length(unique(name))) |>
  filter(rep > 1)

unique(palettes_df$type)
unique(palettes_df$theme)

palettes <- palettes_df |>
  group_split(name) |>
  map(function(pal){
    list(
      name = unique(pal$name),
      type = unique(pal$type),
      length = unique(pal$length),
      theme = unique(pal$theme),
      colors = prismatic::color(pal$colors)
    )
  })

palettes_nms <- palettes_df |>
  group_by(name) |>
  group_keys() |>
  pull(1)
names(palettes) <- palettes_nms



usethis::use_data(palettes, palettes_df, internal = TRUE, overwrite = TRUE)




