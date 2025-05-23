# Use V8 as JavaScript runtime
install.packages("V8")
library(V8)

# Create a new context
ct <- v8()

# Load the zxcvbn.js file
ct$eval(paste(readLines("~/MA/initial_analysis/scripts/import/dependencies/zxcvbn.js", warn = FALSE), collapse = "\n"))

# Now you can use the zxcvbn function in the V8 context
# Example: Get the score of a password
print(ct$call("zxcvbn", "correcthorsebatterystaple")$score)

totalScore <- ct$call("zxcvbn", "Password1!")$guesses_log10 
totalScore <- totalScore * 10
if (totalScore > 100) {
  totalScore <- 100
} else {
  totalScore<- round(totalScore, digits = 0)
}
print(totalScore)
rm(totalScore)

print(ct$call("zxcvbn", "aaaaaIF)Eaaa!")$guesses)
print(ct$call("zxcvbn", "aaaaaIF)Eaaa!")$guesses_log10)