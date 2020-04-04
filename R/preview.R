#' Color previewing utility
#'
#' @param pal A color mapping function, like those returned from \code{\link{colorNumeric}}, et al
#' @param values A set of values to preview colors for
#' @return An HTML-based list of the colors and values
#' @export
preview_colors <- function(values, palette = NULL) {
  if(!is.null(palette)){
    colors <- paletero(values, palette)
    scales::show_col(colors)
  } else{
    scales::show_col(values)
  }
}



#' Color previewing utility
#'
#' @param pal A color mapping function, like those returned from \code{\link{colorNumeric}}, et al
#' @param values A set of values to preview colors for
#' @return An HTML-based list of the colors and values
#' @export
previewColors <- function(palette, values) {

  values <- v
  names(values) <- paletero(v, palette)
  pal <- function(x) names(x)


  heading = htmltools::tags$code(deparse(substitute(pal)))
  subheading = htmltools::tags$code(deparse(substitute(values)))

  htmltools::browsable(
    with(htmltools::tags, htmltools::tagList(
      head(
        style(type = "text/css",
              "table { border-spacing: 1px; }",
              "body { font-family: Helvetica; font-size: 13px; color: #444; }",
              ".swatch { width: 24px; height: 18px; }",
              ".value { padding-left: 6px; }",
              "h3 code { font-weight: normal; }"
        )
      ),
      h3("Colors:", heading, br(), "Values:", class = "subhead", subheading),
      table(
        mapply(pal(values), values, FUN = function(color, x) {
          htmltools::tagList(tr(
            td(class = "swatch", style = paste0("background-color:", color)),
            td(class = "value", format(x, digits = 5))
          ))
        })
      )
    ))
  )
}
