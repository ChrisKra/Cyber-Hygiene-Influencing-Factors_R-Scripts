library(V8)

# Create a new V8 context
ct <- v8()

# Load the JavaScript files
## zxcvbn
ct$eval(paste(readLines("~/MA/02_scripts/05_utils/dependencies/zxcvbn.js", warn = FALSE), collapse = "\n"))

## password meter
ct$source("~/MA/02_scripts/05_utils/dependencies/password-list.js")
ct$eval(paste(readLines("~/MA/02_scripts/05_utils/dependencies/password-meter-list.js", warn = FALSE), collapse = "\n"))

# Function to evaluate password strength and insert columns immediately after the original ones
evaluate_passwords <- function(survey, columns) {
  total <- length(columns)
  progress_intervals <- seq(1, total, length.out = 10) # Generate 10 equal progress points
  
  for (i in seq_along(columns)) {
    col <- columns[i]
    
    # Compute scores
    zxcvbn_scores <- sapply(survey[[col]], function(pwd) {
      if (is.na(pwd) || pwd == "") return(NA)
      return(ct$call("zxcvbn", pwd)$score)
    }, USE.NAMES = FALSE)
    
    password_meter_scores <- sapply(survey[[col]], function(pwd) {
      if (is.na(pwd) || pwd == "") return(NA)
      return(ct$call("chkPass", pwd)$totalScore)
    }, USE.NAMES = FALSE)
    
    # Compute final score
    total_scores <- (zxcvbn_scores * 25 + password_meter_scores) / 2
    
    # Find column index
    col_index <- which(names(survey) == col)
    
    # Insert new columns right after the original column
    survey <- cbind(survey[, 1:col_index, drop = FALSE], 
                    setNames(data.frame(zxcvbn_scores, password_meter_scores, total_scores), 
                             c(paste0(col, "_zxcvbn"), 
                               paste0(col, "_password-meter"), 
                               paste0(col, "_score"))), 
                    survey[, (col_index + 1):ncol(survey), drop = FALSE])
    
    # Print progress update
    if (i %in% round(progress_intervals)) {
      percent_done <- round((i / total) * 100)
      cat(paste0(percent_done, "% done...\n"))
      flush.console()  # Ensure it prints immediately
    }
  }
  
  return(survey)
}

# Specify password columns
password_columns <- c("cch01", "cch012", "cch013")

# Evaluate passwords and insert results
survey <- evaluate_passwords(survey, password_columns)

# Cleanup: remove V8 context and temporary variables
rm(ct, password_columns, evaluate_passwords)
