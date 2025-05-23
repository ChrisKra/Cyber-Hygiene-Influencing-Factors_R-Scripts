# Define columns and their corresponding max values
reverse_map <- c(
  "cch012_score" = 100,
  "cch021_cch021" = 5,
  "cch023_cch023" = 5,
  "cch031_cch031" = 5,
  "cch032_cch032" = 5,
  "big5_big13" = 5,
  "big5_big14" = 5,
  "big5_big23" = 5,
  "big5_big24" = 5,
  "big5_big33" = 5,
  "big5_big34" = 5,
  "big5_big43" = 5,
  "big5_big44" = 5,
  "big5_big52" = 5,
  "big5_big53" = 5,
  "big5_big54" = 5,
  "ra_ra1" = 7,
  "ra_ra2" = 7,
  "ra_ra3" = 7,
  "ra_ra5" = 7,
  "cchk_cch41" = 7,
  "cchk_cch51" = 7,
  "cchk_cch61" = 7,
  "ccha_cch12" = 7,
  "ccha_cch22" = 7,
  "ccha_cch42" = 7,
  "ccha_cch52" = 7,
  "ccha_cch62" = 7,
  "ccha_cch72" = 7,
  "ccha_cch82" = 7,
  "cchb_cch23" = 7,
  "cchb_cch33" = 7,
  "cchb_cch73" = 7
)

# Function to reverse scale and insert new columns
reverse_columns <- function(data, reverse_map) {
  for (col in names(reverse_map)) {
    if (col %in% names(data)) {
      col_index <- which(names(data) == col)
      max_val <- reverse_map[col]
      
      # Apply different reversal formula based on starting point
      reversed_col <- if (col == "cch012_score") {
        max_val - data[[col]]  # Starts at 0
      } else {
        (max_val + 1) - data[[col]]  # Starts at 1
      }
      
      new_col_name <- paste0(col, "_r")
      
      # Insert new column right after the original
      data <- cbind(data[, 1:col_index, drop = FALSE], 
                    setNames(data.frame(reversed_col), new_col_name), 
                    data[, (col_index + 1):ncol(data), drop = FALSE])
    }
  }
  return(data)
}

# Apply the function
survey <- reverse_columns(survey, reverse_map)

# Cleanup 
rm(reverse_map, reverse_columns)