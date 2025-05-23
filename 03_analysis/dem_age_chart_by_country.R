ggplot(
  survey_min %>% filter(!is.na(dem1), !is.na(dem3)) %>% mutate(dem1 = factor(
    dem1,
    levels = c(
      "younger than 20 years old",
      "20 - 25 years old",
      "26 - 30 years old",
      "31 - 35 years old",
      "36 - 40 years old",
      "41 - 45 years old",
      "46 - 50 years old",
      "51 - 55 years old",
      "56 - 60 years old",
      "61 - 65 years old",
      "66 - 70 years old",
      "older than 70 years old"
    )
  )),
  aes(x = dem1, fill = dem3)
) +
  geom_bar(position = "dodge") +
  labs(x = "Age Group", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ))
