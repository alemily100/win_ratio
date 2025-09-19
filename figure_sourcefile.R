highlight_cells <- function(df, small_better_rows = NULL, color = "lightgreen", skip_bottom_n = 3) {
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  
  bg <- matrix("white", nrow = n_rows, ncol = n_cols)
  for (i in 1:n_rows) {
    if (i > (n_rows - skip_bottom_n)) next
    if (i %in% small_better_rows) {
      j <- which.min(df[i, ])
    } else {
      j <- which.max(df[i, ])
    }
    bg[i, j] <- color
  }
  return(bg)
}

border_colors <- c("black", "black", "black", "black", "red3", "darkgoldenrod3", "steelblue3")


