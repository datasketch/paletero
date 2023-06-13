#'
#'
#' #' @export
#' paletero <- function(v, palette, scale = NULL, by = NULL,
#'                      na.color = "#808080", alpha = NULL,
#'                      reverse = FALSE, recycle = "lighter",
#'                      colorColName = "color",
#'                      as_fun = FALSE, print = FALSE){
#'
#'   if("data.frame" %in% class(v)){
#'     d <- v
#'     v <- d[[by]]
#'   }
#'
#'   scale <- scale %||% which_color_scale(v, scale = NULL)
#'   if(!is.null(scale)){
#'     if(!scale %in% c("cat","num","col"))
#'       stop("vector is not categorical, numeric or a color.")
#'     if(scale == "col"){
#'       colsIdx <- are_colors(v)
#'       v[!colsIdx] <- NA
#'       return(v)
#'     }
#'   }
#'
#'
#'   f_cat <- function(v, scale = "cat"){
#'     paletero_cat(v, palette = palette, na.color = na.color,
#'                  alpha = alpha, reverse = reverse,
#'                  recycle = recycle)
#'   }
#'   f_num <- function(v, scale = "num"){
#'     paletero_num(v, palette = palette, na.color = na.color,
#'                  alpha = alpha, reverse = reverse)
#'   }
#'
#'   if(scale == "cat") f <- f_cat
#'   if(scale == "num") f <- f_num
#'
#'   if(as_fun || is.null(v)) return(f)
#'
#'   colors <- f(v)
#'   if("data.frame" %in% class(v)){
#'     d[[colorColName]] <- colors
#'     return(d)
#'   }
#'   if(print) print_colors(colors)
#'   colors
#' }
#'
#'
#' #' @export
#' paletero_cat <- function(v, palette, na.color = "#808080", alpha = NULL,
#'                          reverse = FALSE, recycle = "lighter"){
#'   if(!are_colors(palette) && !palette %in% availablePalettes())
#'     stop("Palette not available")
#'   if(!is.null(alpha))
#'     na.color <- paste0(na.color, as.hexmode(alpha*255))
#'   #strtoi("0xFF")
#'   domain <- unique(as.character(v[!is.na(v)]))
#'
#'   range <- paleta(palette, n = length(domain), alpha = alpha,
#'                   reverse = reverse, recycle = recycle, type = "qualitative")
#'   colors <- match_replace(v, data.frame(domain, range, stringsAsFactors = FALSE))
#'   colors[is.na(v)] <- na.color
#'   remove_transparency(colors)
#' }
#'
#' #' @export
#' paletero_num <- function(v, palette, na.color = "#808080", alpha = NULL,
#'                          reverse = FALSE){
#'   if(!are_colors(palette) && !palette %in% availablePalettes())
#'     stop("Palette not available")
#'   if(!is.null(alpha))
#'     na.color <- paste0(na.color, as.hexmode(alpha*255))
#'
#'   domain <- scales::rescale(v, from = range(v, na.rm = TRUE))
#'   p <- paleta(palette, n = NULL, type = "sequential") # TODO handle cases for divergente, sequencial, etc.
#'   ramp <- scales::colour_ramp(p)
#'   colors <- ramp(domain)
#'   colors[is.na(v)] <- na.color
#'   remove_transparency(colors)
#' }
#'
#'
#'
#'
#'
