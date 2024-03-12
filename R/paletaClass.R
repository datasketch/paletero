paletaClass <- R6::R6Class(
  "paletaClass",
  public = list(
    colors = NULL,
    input_colors = NULL,
    type = NULL,
    continuous = FALSE,
    n = NULL,
    min_color = NULL,
    max_color = NULL,
    mid_color = NULL,
    include_alpha = NULL,
    initialize = function(colors = NULL, type = "categorical", continuous = FALSE,
                          n = NULL, name = NULL, alpha = TRUE,
                          include_alpha = NULL) {
      self$input_colors <- prismatic::color(colors)
      self$type <- type %||% "categorical"
      self$continuous <- continuous
      self$min_color <- NULL
      self$max_color <- NULL
      self$mid_color <- NULL
      self$include_alpha <- NULL
      self$n <- n %||% length(colors)

      if(!is.null(name)){
        if(!name %in% available_palettes())
          stop("Palette name not found. Check available_palettes()")
        pal <- get_palette(name)
        colors <- pal$colors
        self$input_colors <- pal$colors
      }

      if(self$type == "categorical"){
        recycle <- TRUE
        colors <- palette_categorical(colors, n = self$n, recycle = recycle)
      } else if(self$type == "sequential"){
        colors <- palette_sequential(colors, n = self$n)
      } else if(self$type == "diverging"){
        colors <- palette_diverging(colors, n = n)
      } else {
        self$colors <- colors
      }
      if(alpha){
        self$colors <- paste0(self$colors, as.hexmode(alpha*255))
      }
      self$colors <- prismatic::color(colors)
      self$n <- n %||% length(colors)

    },
    generate_palette = function() {
      # Logic to generate palette based on the type, continuous, and n
      # This is a placeholder for actual palette generation logic
      message("Generating palette: ", self$type, " with ", self$n, " colors.")
      return(sample(colors(), self$n))
    },
    reverse = function(){
      self$colors <- rev(self$colors)
    },
    remove_transparency = function(){
      self$colors <- remove_transparency(colors)
    },
    recycle = function(n_input){
      if(self$type == "categorical"){
        colors <- recycle_categorical_colors(self$colors, n_input)
      }
      colors
    },
    eval_categorical_pal = function(v, na_color = "#CCCCCC",
                                    alpha = NULL,
                                    color_dic = NULL){
      if(!is.null(alpha))
        na_color <- paste0(na_color, as.hexmode(alpha*255))

      domain <- unique(as.character(v[!is.na(v)]))
      range <- self$recycle(length(domain))

      if(is.null(color_dic)){
        color_dic <- data.frame(domain, range, stringsAsFactors = FALSE)
      }else{
        # TODO validate color dic
      }
      colors <- dstools::match_replace(v, color_dic)
      colors[is.na(v)] <- na_color
      #remove_transparency(colors)
      colors
    },
    eval = function(v){
      palette_type <-  self$type
      f_categorical <- function(v, palette = palette, ...){
        paletero_categorical(v, palette = palette, ...)
      }
      f_sequential <- function(v, palette = palette, ...){
        # paletero_sequential(v, palette = palette, na.color = na.color,
        #              alpha = alpha, reverse = reverse)
      }
      f_diverging <- function(v, palette = palette, ...){
      }
      f <- get(paste0("paletero_", palette_type))
      if(as_fun || is.null(v)) return(f)

      colors <- f(v, palette = palette, ...)
      colors
    },
    print = function(...) {
      cat("Palette Type: ", self$type, "\n")
      print(self$colors)
      invisible(self)  # To prevent default printing
    }
  )
)
