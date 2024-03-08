

remove_transparency <- function(x){
  if(sum(nchar(x)) == 9 && grepl("FF$",x)){
    return(substring(x, 1, 7))
  }
  x
}

is_gray <- function(colors) {
  rgb <- col2rgb(colors)
  return(rgb[1,] == rgb[2,] & rgb[2,] == rgb[3,])
}

shift_gray <- function(color){
  prismatic::clr_saturate(color, shift = 0.2)
}

recycle_categorical_colors <- function(colors, n_desired){
  if(n_desired < length(colors)) return(colors[1:n_desired])
  recycle_times <- ceiling(n_desired/length(colors))
  recycle_fun <- function(clrs, n = 1){
    if(any(is_gray(clrs))){
      clrs[is_gray(clrs)] <- shift_gray(clrs[is_gray(clrs)])
    }
    prismatic::clr_rotate(clrs, degrees = n * 360/(n_desired))
  }
  recycled <- purrr::accumulate(1:recycle_times,
                                recycle_fun, .init = colors)
  prismatic::color(unlist(recycled))[1:n_desired]
}



