#' @import magick
#' @export
img_palette <- function(img, n = 8,
                        n_quant = n * 2,
                        type = "cat",
                        include_bg = FALSE,
                        fuzz = 12 ){
  if(class(img) == "character"){
    img <- image_read(img) %>% image_scale("300")
  }
  if(!include_bg){
    img <- img_foreground(img, fuzz = fuzz)
  }
  img_quant <- image_quantize(img, n_quant)

  df <- img_to_df(img_quant) %>%
    dplyr::filter(R > 10, G > 10, B > 10)
  df$color <- farver::encode_colour(df[,c("R","G","B")])
  if(type %in% c("cat","seq")){
    df <- df %>%
      dplyr::group_by(color) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::arrange(desc(count)) %>%
      dplyr::slice(1:n)
    colors <- df$color
  }
  if(type == "num"){
    df <- df %>%
      dplyr::group_by(color) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::arrange(desc(count)) %>%
      dplyr::slice(1:2)
    colors <- df$color
  }
  colors
}

#' @export
img_background_color <- function(img, method = "mode", fuzz = 12){
  if(class(img) == "character"){
    img <- image_read(img) %>% image_scale("300")
  }
  backgr <- img_background(img, fuzz = fuzz)

  backgr_quant <- backgr %>% image_quantize(5)
  # backgr_quant <- image_quantize(backgr, 10)

  if(method == "mean"){
    df <- img_to_df(backgr) %>%
      dplyr::filter(R > 0.5, G > 0.5, B > 0.5) %>%
      dplyr::summarise(R = mean(R), G = mean(G), B = mean(B))
  }
  if(method == "mode"){
    df <- img_to_df(backgr_quant) %>%
      dplyr::filter(R > 0.5, G > 0.5, B > 0.5) %>%
      dplyr::summarise(R = Mode(R), G = Mode(G), B = Mode(B))
  }
  farver::encode_colour(df[,c("R","G","B")])
}


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' @export
img_background <- function(img, fuzz = 12){
  if(class(img) == "character"){
    img <- image_read(img) %>% image_scale("300")
  }
  back_mask <- background_mask(img, fuzz = fuzz)
  img_png <- image_convert(img, format = "png")
  image_composite(img_png, back_mask, operator = "CopyOpacity")
}

#' @export
img_foreground <- function(img, fuzz = 12){
  if(class(img) == "character"){
    img <- image_read(img) %>% image_scale("300")
  }
  fore_mask <- foreground_mask(img, fuzz = fuzz)
  img_png <- image_convert(img, format = "png")
  image_composite(img_png, fore_mask, operator = "CopyOpacity")
}


#' @export
remove_background <- function(img, fuzz = 12){
  #cmd <- 'convert picture.jpg -fill none -fuzz 12% -draw "matte 0,0 floodfill"
  # -flop  -draw "matte 0,0 floodfill"
  # -flip  -draw "matte 0,0 floodfill"
  # -flop  -draw "matte 0,0 floodfill"
  # -flip  result.png'
  img %>%
    image_fill("none", fuzz = fuzz, point = "+1+1") %>%
    image_flop() %>%
    image_fill("none", fuzz = fuzz, point = "+1+1") %>%
    image_flip() %>%
    image_fill("none", fuzz = fuzz, point = "+1+1") %>%
    image_flop() %>%
    image_fill("none", fuzz = fuzz, point = "+1+1") %>%
    image_flip()

}


background_mask <- function(img, fuzz = 12){
  img_no_bg <- remove_background(img, fuzz = fuzz)
  img_no_bg <- image_convert(img_no_bg, "png")
  image_channel(img_no_bg, channel = "alpha")
}

foreground_mask <- function(img, fuzz = 12){
  image_negate(background_mask(img, fuzz = fuzz))
}


img_to_df <- function(img){

  imgarr <- as.integer(img[[1]])
  imgDm <- dim(imgarr)

  img_df <- data.frame(
    x = rep(1:imgDm[2], each = imgDm[1]),
    y = rev(rep(imgDm[1]:1, imgDm[2]))
  )

  # TODO handle pngs
  if(imgDm[3] == 1){
    dims <- data.frame(I = as.vector(imgarr[,,1]))
  } else if(imgDm[3] == 3){
    dims <- data.frame(
      R = (as.vector(imgarr[,,1])),
      G = (as.vector(imgarr[,,2])),
      B = (as.vector(imgarr[,,3]))
    )
  } else if(imgDm[3] == 4){
    dims <- data.frame(
      R = (as.vector(imgarr[,,1])),
      G = (as.vector(imgarr[,,2])),
      B = (as.vector(imgarr[,,3])),
      alpha = (as.vector(imgarr[,,4]))
    )
  }
  cbind(img_df, dims)
}



