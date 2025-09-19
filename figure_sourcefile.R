highlight_cells <- function(df, skip_bottom_n = 3, small_better_rows = integer(0), color = "lightgreen") {
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  bg <- matrix("white", nrow = n_rows, ncol = n_cols)
  for (i in 1:n_rows) {
    if (i > (n_rows - skip_bottom_n)) next
    
    if (i %in% small_better_rows) {
      # find the minimum value in the row
      best_value <- min(df[i, ])
    } else {
      # find the maximum value in the row
      best_value <- max(df[i, ])
    }
    # find all columns with that best value and color them
    best_cols <- which(df[i, ] == best_value)
    bg[i, best_cols] <- color
  }
  return(bg)
}
border_colors <- c("black", "black", "black", "black", "red3", "darkgoldenrod3", "steelblue3")


