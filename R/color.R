

#' @export
paletero <- function(v, palette, na.color = "#808080", alpha = NULL, reverse = FALSE,
                     by = NULL, colorScale = NULL, colorColName = "color"){
  if(all(areColors(palette))){
    colors <- palette
    palette <- "custom"
  }else{
    colors <- NULL
  }
  if("data.frame" %in% class(v)){
    d <- v
    v <- d[[by]]
  }
  scale <- colorScale %||% whichColorScale(v, colorScale = NULL)
  if(!scale %in% c("cat","num","col"))
    stop("vector is not categorical, numeric or a color.")
  if(scale == "col"){
    colsIdx <- areColors(v)
    v[!colsIdx] <- NA
    return(v)
  }
  colors <- do.call(paste0("paletero_",scale),
                    list(
                      v = v,
                      palette = palette, na.color = na.color,
                      alpha = alpha, reverse = reverse, colors = colors
                    ))
  if("data.frame" %in% class(v)){
    d[[colorColName]] <- colors
    return(d)
  }
  colors
}


#' @export
paletero_cat <- function(v, palette, na.color = "#808080", alpha = NULL,
                         reverse = FALSE, colors = NULL){
  if(!palette %in% availablePalettes())
    stop("Palette not available")
  if(!is.null(alpha))
    na.color <- paste0(na.color, as.hexmode(alpha*255))
  #strtoi("0xFF")
  domain <- unique(as.character(v[!is.na(v)]))
  range <- paleta(palette, n = length(domain), alpha = alpha,
                   reverse = reverse, colors = colors)
  colors <- match_replace(v, data.frame(domain, range, stringsAsFactors = FALSE))
  colors[is.na(v)] <- na.color
  colors
}

#' @export
paletero_num <- function(v, palette, na.color = "#808080", alpha = NULL,
                         reverse = FALSE, colors = NULL){
  if(!palette %in% availablePalettes())
    stop("Palette not available")
  if(!is.null(alpha))
    na.color <- paste0(na.color, as.hexmode(alpha*255))
  #strtoi("0xFF")

  rng <- range(v, na.rm = TRUE)
  domain <- scales::rescale(v, from = rng)
  p <- paleta(palette, n = 2, colors = colors) # TODO handle cases for divergente, sequencial, etc.
  ramp <- colour_ramp(p)
  colors <- ramp(domain)
  colors[is.na(v)] <- na.color
  colors
}

#' @export
availablePalettes <- function(){
  c(getViridisPalettes(), getBrewerPalettes(),getPaleteroPalettes(),"custom")
}

whichColorScale <- function(v, colorScale = NULL){
  if(!is.null(colorScale)){
    if(colorScale == "num")
      v <- as.numeric(v)
    if(colorScale == "cat")
      v <- as.character(v)
    if(colorScale == "col"){
      colsIdx <- areColors(v)
      v[!colsIdx] <- NA
    }
  }
  if(is.numeric(v))
    return("num")
  if(is.factor(v))
    v <- as.character(v)
  if(is.character(v)){
    if(all(areColors(v))){
      return("col")
    }else{
      return("cat")
    }
  }
  NULL
}


areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}





