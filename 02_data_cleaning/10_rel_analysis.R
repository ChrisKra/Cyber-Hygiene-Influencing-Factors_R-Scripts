library(semTools)

# Reliability Analysis
## Cronbach’s alpha

for (construct in names(constructs)[!names(constructs) %in% c("pe", "dem", "other")]) {
  alpha_score <- suppressWarnings(alpha(survey_min[, constructs[[construct]]])$total$raw_alpha)
  cat("Construct:", construct, "Alpha:", alpha_score, "\n")
  rm(alpha_score, construct)
}

### Password Exercise (low!)
alpha(survey_min[, constructs$ex_passw])

### Phishing Exercise (low!)
alpha(survey_min[, constructs$ex_phish])

### Website Exercise (low!)
alpha(survey_min[, constructs$ex_web])

### Big 5
alpha(survey_min[, constructs$big5_extra])
alpha(survey_min[, constructs$big5_agree])
alpha(survey_min[, constructs$big5_consc])
alpha(survey_min[, constructs$big5_neuro])
alpha(survey_min[, constructs$big5_imag])

### Risk Affinity
alpha(survey_min[, constructs$ra])

### FOMO
alpha(survey_min[, constructs$fomo_social])
alpha(survey_min[, constructs$fomo_socmed])
alpha(survey_min[, constructs$fomo_novel])

### Cyber Security Measures
alpha(survey_min[, constructs$csm])

### Perceived Vulnerability 
alpha(survey_min[, constructs$pv])

### Perceived Efficacy (low!)
alpha(survey_min[, constructs$pe])

### Technical efficacy
alpha(survey_min[, constructs$tue])

### Technology use (low!)
alpha(survey_min[, constructs$tud])

### Trust
alpha(survey_min[, constructs$tr])

### Comprehensive Cyber Hygiene
alpha(survey_min[, constructs$cchk])
alpha(survey_min[, constructs$ccha])
alpha(survey_min[, constructs$cchb])

### Social Influence
alpha(survey_min[, constructs$si])

### Cost
alpha(survey_min[, constructs$co])

### Online Resources
alpha(survey_min[, constructs$ori])
alpha(survey_min[, constructs$orh])

## Composite Reliability 
for (construct in names(constructs)[!names(constructs) %in% c("pe", "dem", "other")]) {
  # Define the model
  model <- paste(construct, paste(constructs[[construct]], collapse = "+"), sep = "=~")
  # print(model)
  
  # Calculate CR
  fit <- cfa(model, data = survey_min)
  sl <- standardizedSolution(fit)
  sl <- sl$est.std[sl$op == "=~"]
  re <- 1 - sl^2
  
  cat("Construct:", construct, "Composite reliability:", sum(sl)^2 / (sum(sl)^2 + sum(re)), " AVE: ", AVE(fit), "\n")
  
  # Clean up 
  rm(model, fit, sl, re, construct)
}

### Password Exercise
summary(cfa(paste("ex_passw", paste(constructs$ex_passw, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Phishing Exercise
summary(cfa(paste("ex_phish", paste(constructs$ex_phish, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Website Exercise
summary(cfa(paste("ex_web", paste(constructs$ex_web, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Big 5
summary(cfa(paste("big5_extra", paste(constructs$big5_extra, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)
summary(cfa(paste("big5_agree", paste(constructs$big5_agree, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)
summary(cfa(paste("big5_consc", paste(constructs$big5_consc, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)
summary(cfa(paste("big5_neuro", paste(constructs$big5_neuro, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)
summary(cfa(paste("big5_imag", paste(constructs$big5_imag, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Risk Affinity
summary(cfa(paste("ra", paste(constructs$ra, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### FOMO
summary(cfa(paste("fomo_social", paste(constructs$fomo_social, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)
summary(cfa(paste("fomo_socmed", paste(constructs$fomo_socmed, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)
summary(cfa(paste("fomo_novel", paste(constructs$fomo_novel, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Cyber Security Measures
summary(cfa(paste("csm", paste(constructs$csm, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Perceived Vulnerability 
summary(cfa(paste("pv", paste(constructs$pv, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Perceived Efficacy
summary(cfa(paste("pe", paste(constructs$pe, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Technical efficacy
summary(cfa(paste("tue", paste(constructs$tue, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Technology use
summary(cfa(paste("tud", paste(constructs$tud, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Trust
summary(cfa(paste("tr", paste(constructs$tr, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Comprehensive Cyber Hygiene
summary(cfa(paste("cchk", paste(constructs$cchk, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)
summary(cfa(paste("ccha", paste(constructs$ccha, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)
summary(cfa(paste("cchb", paste(constructs$cchb, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Social Influence
summary(cfa(paste("si", paste(constructs$si, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Cost
summary(cfa(paste("co", paste(constructs$co, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

### Online Resources
summary(cfa(paste("ori", paste(constructs$ori, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)
summary(cfa(paste("orh", paste(constructs$orh, collapse = "+"), sep = "=~"), data = survey_min), standardized = TRUE)

# Variance Inflation Factor
library(car)  # For vif()

# Function to compute VIF stats per construct
compute_vif_stats <- function(name, indicators, data) {
  # Ensure indicators exist
  valid_indicators <- indicators[indicators %in% names(data)]
  valid_data <- data[, valid_indicators, drop = FALSE]
  
  # Special handling: convert "Yes"/"No" to 1/0 for construct "pe"
  if (name == "pe") {
    valid_data[] <- lapply(valid_data, function(x) {
      if (is.character(x) || is.factor(x)) {
        return(as.numeric(tolower(x) == "yes"))
      } else {
        return(x)
      }
    })
  }
  
  # Filter to numeric columns only
  valid_data <- valid_data[sapply(valid_data, is.numeric)]
  
  # Drop rows with NA
  valid_data <- na.omit(valid_data)
  
  # Only run if there's enough data
  if (ncol(valid_data) >= 2 && nrow(valid_data) > 1) {
    model <- lm(rowMeans(valid_data) ~ ., data = valid_data)
    vif_values <- vif(model)
    return(data.frame(
      Construct = name,
      Mean_VIF = mean(vif_values),
      Min_VIF = min(vif_values),
      Max_VIF = max(vif_values)
    ))
  } else {
    return(data.frame(
      Construct = name,
      Mean_VIF = NA,
      Min_VIF = NA,
      Max_VIF = NA
    ))
  }
}

# Loop over constructs and bind results
vif_summary <- do.call(rbind, lapply(names(constructs), function(name) {
  compute_vif_stats(name, constructs[[name]], survey_min)
}))

# Show result
print(vif_summary)

# Print each result formatted as: Construct: Mean (Min, Max)
apply(vif_summary, 1, function(row) {
  cat(paste0(row["Construct"], ": ", round(as.numeric(row["Mean_VIF"]), 4), 
             " (", round(as.numeric(row["Min_VIF"]), 4), ", ", round(as.numeric(row["Max_VIF"]), 4), ")\n"))
})