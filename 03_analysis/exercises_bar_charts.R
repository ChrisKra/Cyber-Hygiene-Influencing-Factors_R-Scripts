library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(stringr)
library(scales)

# Phishing Rating Distribution
chart_data1 <- survey_min %>%
  select(cch021_cch021_r, cch022_cch022, cch023_cch023_r) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = str_replace(Variable, "^[^_]+_", ""))  # Remove text before the first underscore

plot1_data <- chart_data1 %>%
  count(Variable, Value) %>%
  group_by(Variable) %>%
  mutate(Percent = n / sum(n))

# Website Rating Distribution
chart_data2 <- survey_min %>%
  select(cch031_cch031_r, cch032_cch032_r, cch033_cch033) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = str_replace(Variable, "^[^_]+_", ""))  # Remove text before the first underscore

plot2_data <- chart_data2 %>%
  count(Variable, Value) %>%
  group_by(Variable) %>%
  mutate(Percent = n / sum(n))

# Password Score Distribution (Binned)
chart_data3 <- survey_min %>%
  select(cch01_score, cch012_score_r, cch013_score) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(
    Binned_Value = cut(Value, breaks = seq(floor(min(Value, na.rm = TRUE)),
                                           ceiling(max(Value, na.rm = TRUE)),
                                           by = 20), include.lowest = TRUE)
  )

plot3_data <- chart_data3 %>%
  count(Variable, Binned_Value) %>%
  group_by(Variable) %>%
  mutate(Percent = n / sum(n))

# Get the maximum Share across all three datasets
max_percent <- max(c(plot1_data$Percent, plot2_data$Percent, plot3_data$Percent), na.rm = TRUE)

# Create Plots
plot1 <- ggplot(plot1_data, aes(x = Value, y = Percent, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  theme_minimal() +
  labs(title = "Phishing Rating Distribution",
       x = "Rating",
       y = "Share of Respondents",
       fill = "Exercise") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max_percent)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

plot2 <- ggplot(plot2_data, aes(x = Value, y = Percent, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  theme_minimal() +
  labs(title = "Website Rating Distribution",
       x = "Rating",
       y = "Share of Respondents",
       fill = "Exercise") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max_percent)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

plot3 <- ggplot(plot3_data, aes(x = Binned_Value, y = Percent, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  theme_minimal() +
  labs(title = "Password Score Distribution (Discretized)",
       x = "Binned Score",
       y = "Share of Respondents",
       fill = "Exercise") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max_percent)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

# Combine the plots side by side
combined_plot <- plot3 + plot1 + plot2 + plot_layout(ncol = 3)

# Print the combined plot
print(combined_plot)

# Save the combined plot
# ggsave("variable_distribution_bar_chart.png", combined_plot, width = 15, height = 5, dpi = 300)

rm(chart_data1, chart_data2, chart_data3, plot1, plot2, plot3, combined_plot, plot1_data, plot2_data, plot3_data, max_percent)
