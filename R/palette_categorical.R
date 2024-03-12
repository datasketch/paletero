#' @export
palette_categorical <- function(colors, n = 6,
                                mode = "rotate",
                                recycle = TRUE){

  if(!prismatic::is_color(colors))
    colors <- prismatic::color(colors)
  n_input <- length(colors)

  if(n_input < n){
    if(!recycle){
      stop("Length of palette (", pal$length,") less than length of requested colors")
    }
    recycle_times <- ceiling(n / n_input)
    recycle_fun1 <- function(clrs, n = 1){
      prismatic::clr_rotate(clrs, degrees = n * 30)
    }
    recycled <- purrr::accumulate(1:recycle_times,
                                  recycle_fun1, .init = colors)
    colors <- prismatic::color(unlist(recycled))
  }
  colors[1:n]
}
