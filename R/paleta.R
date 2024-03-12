#' @keywords internal
paleta <- function(colors = NULL, type = NULL,
                   n = NULL, name = NULL, alpha = TRUE){
  pal <- paletaClass$new(colors = colors, type = type, n = n,
                         name = name, alpha = alpha)
  pal

}
