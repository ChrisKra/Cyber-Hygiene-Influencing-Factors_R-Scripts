# Use V8 as JavaScript runtime
install.packages("V8")
library(V8)

# Create a new context
ct <- v8()

# Load the zxcvbn.js file
ct$source("~/MA/initial_analysis/scripts/import/dependencies/password-list.js")
ct$eval(paste(readLines("~/MA/initial_analysis/scripts/import/dependencies/password-meter-list.js", warn = FALSE), collapse = "\n"))

# Now you can use the chkPass function in the V8 context
# Example: Evaluate the strength of a password
print(ct$call("chkPass", "lkjdagf32459862456"))

