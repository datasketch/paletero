#' Plot color palette from color distribution data
#'
#' @param color_data A tibble with at least 'color' column; optionally 'distribution' for size weighting.
#'                   Alternatively, a character vector of colors.
#' @param method Visualization method: "treemap", "tiles", "bars", or "spectrum"
#' @param sort_by How to sort colors: "distribution" (default), "hue", "luminance", "saturation", 
#'                "temperature", "harmony", "lab", "similarity", "category", "gradient", or "none"
#' @param reverse Logical. Whether to reverse the sort order (default: FALSE)
#' @param label_colors Logical. Whether to add color hex codes as labels (default: TRUE)
#' @param nrow Number of rows for "tiles" method (default: NULL for auto calculation)
#' @param reference_color Reference color for "similarity" sorting (default: "#FF5500")
#'
#' @return A ggplot object showing the color palette
#' @export
#' @examples
#' # With a tibble input
#' colors_data <- tibble::tibble(
#'   color = c("#FF0000", "#00FF00", "#0000FF"),
#'   distribution = c(0.5, 0.3, 0.2)
#' )
#' plot_paleta(colors_data, method = "treemap")
#'
#' # With a vector input
#' colors_vec <- c("#FF0000", "#00FF00", "#0000FF")
#' plot_paleta(colors_vec, method = "tiles")
#' 
#' # Sort by different methods
#' plot_paleta(colors_vec, method = "spectrum", sort_by = "luminance")
#' plot_paleta(colors_vec, method = "spectrum", sort_by = "temperature")
plot_paleta <- function(color_data,
                        method = c("treemap", "tiles", "bars", "spectrum"),
                        sort_by = c("distribution", "hue", "luminance", "saturation", 
                                   "temperature", "harmony", "lab", "similarity", 
                                   "category", "gradient", "none"),
                        reverse = FALSE,
                        label_colors = TRUE,
                        nrow = NULL,
                        reference_color = "#FF5500") {

  # Check required packages
  required_packages <- c("ggplot2", "tibble", "farver")
  if (method[1] == "treemap") required_packages <- c(required_packages, "treemapify")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required for this function"))
    }
  }

  # Convert vector input to tibble if needed
  if (is.character(color_data)) {
    color_data <- tibble::tibble(
      color = color_data,
      distribution = rep(1 / length(color_data), length(color_data))
    )
  }

  # Validate input data
  if (!("color" %in% colnames(color_data))) {
    stop("Input data must have a 'color' column")
  }

  # Add distribution column if missing (equal weights)
  if (!("distribution" %in% colnames(color_data))) {
    color_data$distribution <- 1 / nrow(color_data)
  }

  # Match arguments
  method <- match.arg(method[1], c("treemap", "tiles", "bars", "spectrum"))
  sort_by <- match.arg(sort_by[1], c("distribution", "hue", "luminance", "saturation", 
                                     "temperature", "harmony", "lab", "similarity", 
                                     "category", "gradient", "none"))

  # Sort colors
  if (sort_by != "none") {
    color_data <- sort_colors(color_data, sort_by, reverse, reference_color)
  }

  # Create plot based on method
  if (method == "treemap") {
    p <- plot_treemap(color_data, label_colors)
  } else if (method == "tiles") {
    p <- plot_tiles(color_data, label_colors, nrow)
  } else if (method == "bars") {
    p <- plot_bars(color_data, label_colors)
  } else if (method == "spectrum") {
    p <- plot_spectrum(color_data, label_colors)
  }

  return(p)
}

#' Sort colors by various methods
#'
#' @param color_data A tibble with 'color' and optionally 'distribution' columns
#' @param sort_by Method to sort by: "distribution", "hue", "luminance", "saturation", 
#'                "temperature", "harmony", "lab", "similarity", "category", or "gradient"
#' @param reverse Whether to reverse the sort order
#' @param reference_color Reference color for similarity sorting
#' @return A sorted tibble
#' @keywords internal
sort_colors <- function(color_data, sort_by = "distribution", reverse = FALSE, 
                        reference_color = "#FF5500") {
  # Get RGB values once for efficiency
  rgb_values <- farver::decode_colour(color_data$color, to = "rgb")
  
  if (sort_by == "distribution") {
    # Sort by distribution (descending by default)
    sorted_data <- color_data[order(color_data$distribution, decreasing = !reverse), ]
    
  } else if (sort_by == "hue") {
    # Convert to HSV and sort by hue
    hsv_values <- farver::decode_colour(color_data$color, to = "hsv")
    hues <- hsv_values[, 1]
    # Order by hue
    sorted_data <- color_data[order(hues, decreasing = reverse), ]
    
  } else if (sort_by == "luminance") {
    # Calculate luminance (perceived brightness)
    luminance <- 0.2126 * rgb_values[, 1] + 0.7152 * rgb_values[, 2] + 0.0722 * rgb_values[, 3]
    # Sort by luminance (darkest to lightest by default)
    sorted_data <- color_data[order(luminance, decreasing = reverse), ]
    
  } else if (sort_by == "saturation") {
    # Get HSV values and sort by saturation
    hsv_values <- farver::decode_colour(color_data$color, to = "hsv")
    saturation <- hsv_values[, 2]
    # Sort by saturation (most saturated first by default)
    sorted_data <- color_data[order(saturation, decreasing = !reverse), ]
    
  } else if (sort_by == "temperature") {
    # Calculate color temperature (red - blue)
    temperature <- rgb_values[, 1] - rgb_values[, 3]
    # Sort by temperature (warm to cool by default)
    sorted_data <- color_data[order(temperature, decreasing = !reverse), ]
    
  } else if (sort_by == "harmony") {
    # Group by hue segments for harmony
    hsv_values <- farver::decode_colour(color_data$color, to = "hsv")
    hue_angle <- hsv_values[, 1] * 360
    # Create harmony groups (30° segments)
    harmony_group <- floor(hue_angle / 30)
    # Sort by harmony groups
    sorted_data <- color_data[order(harmony_group, decreasing = reverse), ]
    
  } else if (sort_by == "lab") {
    # Sort using CIELAB perceptual space (by lightness)
    lab_values <- farver::decode_colour(color_data$color, to = "lab")
    # Sort by L* (lightness)
    sorted_data <- color_data[order(lab_values[, 1], decreasing = reverse), ]
    
  } else if (sort_by == "similarity") {
    # Sort by similarity to reference color
    ref_rgb <- farver::decode_colour(reference_color, to = "rgb")
    # Calculate Euclidean distance in RGB space
    color_distance <- sqrt(
      (rgb_values[, 1] - ref_rgb[1, 1])^2 +
      (rgb_values[, 2] - ref_rgb[1, 2])^2 +
      (rgb_values[, 3] - ref_rgb[1, 3])^2
    )
    # Sort by similarity (closest first by default)
    sorted_data <- color_data[order(color_distance, decreasing = reverse), ]
    
  } else if (sort_by == "category") {
    # Simple color categorization by primary hue
    hsv_values <- farver::decode_colour(color_data$color, to = "hsv")
    hue_angle <- hsv_values[, 1] * 360
    
    # Assign category by hue angle (0-360°)
    # This creates 12 basic color categories
    color_categories <- floor(hue_angle / 30)
    
    # Sort by color category
    sorted_data <- color_data[order(color_categories, hsv_values[, 2], 
                                   hsv_values[, 3], decreasing = reverse), ]
    
  } else if (sort_by == "gradient") {
    # Simple greedy algorithm for smooth gradient
    n_colors <- nrow(color_data)
    
    # Handle edge cases
    if (n_colors <= 1) {
      # Nothing to sort with 0 or 1 colors
      sorted_data <- color_data
    } else {
      # Start with first color
      order_idx <- rep(0, n_colors)
      order_idx[1] <- 1
      
      # Handle case with only 2 colors
      if (n_colors == 2) {
        order_idx[2] <- 2
      } else {
        # Normal case with 3+ colors
        remaining <- setdiff(1:n_colors, 1)  # More robust than 2:n_colors
        
        # For each position, find the closest remaining color to the last added
        for (i in 2:n_colors) {
          last_color <- rgb_values[order_idx[i-1], ]
          
          # Ensure we have remaining colors to process
          if (length(remaining) > 0) {
            # Calculate distances to all remaining colors
            if (length(remaining) == 1) {
              # Only one color left, calculate distance directly
              distance <- sqrt(sum((rgb_values[remaining, ] - last_color)^2))
              closest_idx <- 1
            } else {
              # Multiple colors left, use apply
              distances <- apply(rgb_values[remaining, , drop = FALSE], 1, function(color) {
                sqrt(sum((color - last_color)^2))
              })
              # Find the closest
              closest_idx <- which.min(distances)
            }
            
            order_idx[i] <- remaining[closest_idx]
            remaining <- remaining[-closest_idx]
          }
        }
      }
      
      # Apply the gradient ordering
      sorted_data <- color_data[order_idx, ]
      
      # Reverse if requested
      if (reverse) {
        sorted_data <- sorted_data[n_colors:1, ]
      }
    }
  }

  return(sorted_data)
}

#' Create a treemap visualization of colors
#'
#' @param color_data Tibble with 'color' and 'distribution' columns
#' @param label_colors Whether to add color labels
#' @return A ggplot object
#' @keywords internal
plot_treemap <- function(color_data, label_colors = TRUE) {
  p <- ggplot2::ggplot(color_data, ggplot2::aes(area = distribution, fill = color, label = color)) +
    treemapify::geom_treemap() +
    ggplot2::scale_fill_identity()

  if (label_colors) {
    p <- p +
      treemapify::geom_treemap_text(color = ifelse(
        grDevices::col2rgb(color_data$color)[1,] * 0.299 +
          grDevices::col2rgb(color_data$color)[2,] * 0.587 +
          grDevices::col2rgb(color_data$color)[3,] * 0.114 > 128,
        "black", "white"
      ))
  }

  p <- p + ggplot2::theme_void()
  return(p)
}

#' Create a tile visualization of colors
#'
#' @param color_data Tibble with 'color' and 'distribution' columns
#' @param label_colors Whether to add color labels
#' @param nrow Number of rows (NULL for auto)
#' @return A ggplot object
#' @keywords internal
plot_tiles <- function(color_data, label_colors = TRUE, nrow = NULL) {
  # Calculate optimal grid dimensions if nrow is NULL
  if (is.null(nrow)) {
    n_colors <- nrow(color_data)
    nrow <- round(sqrt(n_colors))
  }

  # Add position information
  color_data$id <- 1:nrow(color_data)
  color_data$y <- ceiling(color_data$id / nrow)
  color_data$x <- color_data$id - (color_data$y - 1) * nrow

  # Create plot
  p <- ggplot2::ggplot(color_data, ggplot2::aes(x = x, y = -y, fill = color)) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_equal() +
    ggplot2::theme_void()

  if (label_colors) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = color),
      color = ifelse(
        grDevices::col2rgb(color_data$color)[1,] * 0.299 +
          grDevices::col2rgb(color_data$color)[2,] * 0.587 +
          grDevices::col2rgb(color_data$color)[3,] * 0.114 > 128,
        "black", "white"
      )
    )
  }

  return(p)
}

#' Create a bar visualization of colors
#'
#' @param color_data Tibble with 'color' and 'distribution' columns
#' @param label_colors Whether to add color labels
#' @return A ggplot object
#' @keywords internal
plot_bars <- function(color_data, label_colors = TRUE) {
  # Ensure bars are wide enough for labels
  n_colors <- nrow(color_data)
  
  p <- ggplot2::ggplot(color_data, ggplot2::aes(x = reorder(color, -distribution),
                                                y = distribution,
                                                fill = color)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) # Add 20% more space on top

  if (label_colors) {
    if (n_colors > 10) {
      # If there are many colors, use vertical labels inside the bars
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = color, y = distribution / 2),
        angle = 90,
        hjust = 0.5,
        size = 3,
        color = ifelse(
          grDevices::col2rgb(color_data$color)[1,] * 0.299 +
            grDevices::col2rgb(color_data$color)[2,] * 0.587 +
            grDevices::col2rgb(color_data$color)[3,] * 0.114 > 128,
          "black", "white"
        )
      )
    } else {
      # For fewer colors, use normal horizontal labels
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = color, y = distribution / 2),
        color = ifelse(
          grDevices::col2rgb(color_data$color)[1,] * 0.299 +
            grDevices::col2rgb(color_data$color)[2,] * 0.587 +
            grDevices::col2rgb(color_data$color)[3,] * 0.114 > 128,
          "black", "white"
        )
      )
    }
  }

  p <- p + ggplot2::labs(x = NULL, y = "Distribution")
  return(p)
}

#' Create a color spectrum visualization
#'
#' @param color_data Tibble with 'color' column
#' @param label_colors Whether to add color labels
#' @return A ggplot object
#' @keywords internal
plot_spectrum <- function(color_data, label_colors = TRUE) {
  # Convert colors to HSV
  hsv_values <- t(grDevices::col2rgb(color_data$color)) / 255
  hsv_values <- grDevices::rgb2hsv(hsv_values[,1], hsv_values[,2], hsv_values[,3], maxColorValue = 1)
  color_data$hue <- hsv_values[1,]
  color_data$saturation <- hsv_values[2,]
  color_data$value <- hsv_values[3,]

  # Use the existing sort order from plot_paleta
  # No longer forcing sort by hue here
  color_data$x <- 1:nrow(color_data)

  # Create plot - making the tiles much taller
  p <- ggplot2::ggplot(color_data, ggplot2::aes(x = x, y = 1, fill = color)) +
    ggplot2::geom_tile(height = 5) + # Increased height from 1 to 5
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0))) # Remove padding

  if (label_colors) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = color),
      angle = 90,
      hjust = 0.5,
      color = ifelse(
        grDevices::col2rgb(color_data$color)[1,] * 0.299 +
          grDevices::col2rgb(color_data$color)[2,] * 0.587 +
          grDevices::col2rgb(color_data$color)[3,] * 0.114 > 128,
        "black", "white"
      )
    )
  }

  # Remove coord_fixed to allow the spectrum to expand vertically
  return(p)
}
