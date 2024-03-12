#' @export
palette_sequential <- function(colors, n = 5){

  if(!prismatic::is_color(colors))
    colors <- prismatic::color(colors)

  if(length(colors) == 1){
    colors <- c(colors, prismatic::clr_negate(colors))
  }

  n_input <- length(colors)

  ## TODO: Explore different interpolation spaces
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
