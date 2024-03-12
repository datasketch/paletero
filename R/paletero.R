#' @export
paletero <- function(v, var = NULL, colors = NULL, name = NULL,
                     type = "categorical",
                     color_dic = NULL,
                     color_var_name = "..colors"){

  print("IN PALETERO")
  if(is.null(colors) && is.null(name)) stop("Need colors or a palette name")
  print(colors)
  print(name)
  print("AHORA CALCUAL PALETA")
  pal <- paleta(colors = colors, type = "categorical",
                name = name)

print(class(v))
  if(is.vector(v)){
    colors <- pal$eval_categorical_pal(v, color_dic = color_dic)
    return(colors)
  }
  print(colors)
  print("EVAAL DATA")
  print(is.data.frame(v))
  if(is.data.frame(v)){
    print("in eval dataframe uno")
    df <- v
    if(is.null(var)) var <- 1
    v <- df[[var]]
    print(v)
    print("arg @@@@@@")
    print(pal$eval_categorical_pal)
    colors <- pal$eval_categorical_pal(v, color_dic = color_dic)

    print(colors)
    if(color_var_name %in% names(df))
      stop("color_var_name already in df" )
    df[[color_var_name]] <- colors
    print("in eval dataframe")
    print(df)
    return(df)
  }

  stop("not returning anything")

}
