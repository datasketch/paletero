#'
#'
#'
#' #' @export
#' #' @import farver
#' seq_palette <- function(color, n, step = 1, factor = 0.1){
#'   if(n %% 2 == 0){
#'     n_low <- n %/% 2 - 1
#'   }else{
#'     n_low <- n %/% 2
#'   }
#'   more_n <- 0
#'   lighter <- lighter_scale(color, n_low)
#'   if(length(lighter) < n_low) more_n <- n_low - length(lighter)
#'   n_high <- n %/% 2
#'   str(list(n_low = n_low, n_high = n_high, more_n = more_n))
#'   darker <- darker_scale(color, n_high + more_n)
#'   c( lighter, color, darker)
#' }
#'
#'
#' #' @export
#' darken <- function(color, step = 1, factor = 0.1, space = "lab"){
#'   if(space == "lab"){ chnl <- 'l' }
#'   if(space == "hsv"){ chnl <- 'v' }
#'   multiply_channel(color, chnl, step * (1-factor), space = space)
#' }
#'
#'
#' #' @export
#' darker_scale <- function(color, n, step = 1, factor = 0.1, space = "lab"){
#'   multpliers <- cumprod(rep(step * (1-factor),n + 1))
#'   if(space == "lab"){ chnl <- 'l' }
#'   if(space == "hsv"){ chnl <- 'v' }
#'   x <- multiply_channel(rep(color,n), chnl, multpliers, space = space)
#'   if(length(unique(x)) < length(x)){
#'     warning("Cannot darke  more, returning values: ",
#'             length(unique(x)))
#'     return(unique(x))
#'   }
#'   x
#' }
#'
#' #' #' @export
#' lighten <- function(color, step = 1, factor = 0.1, space = "lab"){
#'   rev(darken(color, step = step, factor = -factor, space = space))
#' }
#'
#' #' @export
#' lighter_scale <- function(color, n, step = 1, factor = 0.1, space = "lab"){
#'   multpliers <- rev(cumprod(rep(step * (1+factor),n + 1)))
#'   if(space == "lab"){ chnl <- 'l' }
#'   if(space == "hsv"){ chnl <- 'v' }
#'   x <- multiply_channel(rep(color,n), chnl, multpliers, space = space)
#'   if(length(unique(x)) < length(x)){
#'     warning("Cannot lighten more, returning values: ",
#'             length(unique(x)))
#'     return(unique(x))
#'   }
#'   x
#'   if(length(unique(x)) < length(x)){
#'     warning("Cannot lighten more, returning values: ",
#'             length(unique(x)))
#'     return(unique(x))
#'   }
#'   x
#' }
#'
#'
#' #' @export
#' saturate <- function(color, step = 1, factor = 0.1){
#'   multiplier <- step * (1+factor)
#'   multiplier[multiplier == 0] <- 1
#'   multiply_channel(rep(color, length(multiplier)), "s", multiplier, space = "hsl")
#' }
#'
#' #' @export
#' desaturate <- function(color, step = 1, factor = 0.1){
#'   multiplier <- -step * (1-factor)
#'   multiplier[multiplier == 0] <- 1
#'   multiply_channel(rep(color, length(multiplier)), "s", multiplier, space = "hsl")
#' }
#'
