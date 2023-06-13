#'
#' #' @export
#' test_cvd <- function(palette = NULL, background = NULL, gg = NULL){
#'   if(!is.null(palette)){
#'     if(all(are_colors(palette))){
#'       pal <- list(
#'         length = length(palette),
#'         colors = palette
#'       )
#'     } else{
#'       pal <- get_colors(pal_name = palette)
#'     }
#'     colors <- pal$colors
#'     g <- colorBlindness::displayAllColors(colors)
#'   }
#'   theme <- list()
#'   if(!is.null(background)){
#'     text_color <- ifelse(which_contrast(background) == "light",
#'                          "#fafafa", "#202020")
#'     theme <- ggplot2::theme(
#'       panel.background = ggplot2::element_rect(
#'         fill = background,
#'         colour = background),
#'       plot.background = ggplot2::element_rect(
#'         colour = background,
#'         fill = background),
#'       line = ggplot2::element_line(
#'         size = 0),
#'       axis.text = ggplot2::element_text(
#'         colour = text_color)
#'     )
#'   }
#'   if(!is.null(gg)){
#'     g <- colorBlindness::cvdPlot(gg)
#'   }
#'   g <- g + theme
#'   g
#'
#' }
#'
#'
