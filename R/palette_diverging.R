#' @export
palette_diverging <- function(colors, n = 5, mid_color = "#F0FAF0"){

  if(n < 3){
    stop("n must be at least 3")
  }
  if(n %% 2 == 0){
    stop("n must be odd")
  }

  if(length(colors) == 1){
    colors <- c(colors[1], prismatic::clr_negate(colors))
  }
  if(length(colors) == 2){
    colors <- c(colors[1], mid_color, colors[2])
  }

  n_input <- length(colors)

  if(n_input < n){
    ramp <- scales::colour_ramp(colors)
    seq <- seq(0, 1, length.out = n)
    colors <- ramp(seq)
  }else{
    colors <- colors[round(seq(1,length(colors), length.out = n))]
    colors <- colors[1:n]
  }


  prismatic::color(colors)
}
