

paletero_cat <- function(v, palette, na.color = "#808080", alpha = NULL, reverse = FALSE){
  if(!palette %in% availablePalettes())
    stop("Palette not available")
  if(!is.null(alpha))
    na.color <- paste0(na.color, as.hexmode(alpha*255))
  #strtoi("0xFF")
  domain <- unique(as.character(v[!is.na(v)]))
  range <- paletero(palette, n = length(domain), alpha = alpha, reverse = reverse)
  colors <- match_replace(v, data.frame(domain, range, stringsAsFactors = FALSE))
  colors[is.na(v)] <- na.color
  colors
}

paletero_num <- function(v, palette, na.color = "#808080", alpha = NULL, reverse = FALSE){
  if(!palette %in% availablePalettes())
    stop("Palette not available")
  if(!is.null(alpha))
    na.color <- paste0(na.color, as.hexmode(alpha*255))
  #strtoi("0xFF")

  rng <- range(v, na.rm = TRUE)
  domain <- scales::rescale(v, from = rng)
  p <- paletero(palette, n = 2) # TODO handle cases for divergente, sequencial, etc.
  ramp <- colour_ramp(p)
  colors <- ramp(domain)
  colors[is.na(v)] <- na.color
  colors
}


availablePalettes <- function(){
  c(getViridisPalettes(), getBrewerPalettes())
}

catColor <- function(v,palette = NULL){
  palette <- palette %||% "RdYlBu"
  pal <- colorFactor(palette, levels = unique(v))
  pal(v)
}






# colorNumeric(palette, domain, na.color = "#808080", alpha = FALSE,
#              reverse = FALSE)
#
# colorBin(palette, domain, bins = 7, pretty = TRUE, na.color = "#808080",
#          alpha = FALSE, reverse = FALSE)
#
# colorQuantile(palette, domain, n = 4, probs = seq(0, 1, length.out = n + 1),
#               na.color = "#808080", alpha = FALSE, reverse = FALSE)
#
# colorFactor(palette, domain, levels = NULL, ordered = FALSE,
#             na.color = "#808080", alpha = FALSE, reverse = FALSE)
