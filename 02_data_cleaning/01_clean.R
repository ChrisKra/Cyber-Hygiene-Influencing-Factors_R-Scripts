library(janitor)

# Clean names
survey <- clean_names(survey)

# Drop incomplete responses i.e. lastpage != 10 
survey <- survey %>% filter(lastpage == 10)

# Filter participants that failed at least one attention check (tue_imc1 or ori_imc2)
# Filter rows where at least one IMC question was failed
failed_responses <- survey %>%
  filter(is.na(tue_imc1) | tue_imc1 != "AO01" | is.na(ori_imc2) | ori_imc2 != "AO07") %>%
  mutate(
    failed_tue_imc1 = is.na(tue_imc1) | tue_imc1 != "AO01",
    failed_ori_imc2 = is.na(ori_imc2) | ori_imc2 != "AO07"
  ) %>%
  select(survey_name, id, g01q48, tue_imc1, ori_imc2, failed_tue_imc1, failed_ori_imc2)

# Print the result
failed_responses %>% print(n = nrow(.))

# Exclude responses with failed attention check
survey_cleaned <- survey %>%
  filter(tue_imc1 == "AO01" & ori_imc2 == "AO07")

# Print number of removed and remaining responses
cat("Number of removed responses (Failed attention check):", nrow(survey) - nrow(survey_cleaned), "\n")
cat("Number of remaining responses:", nrow(survey_cleaned), "\n")
survey <- survey_cleaned
rm(survey_cleaned, failed_responses)

# Remove attention check columns
survey <- survey[ , !(names(survey) %in% c("tue_imc1", "ori_imc2"))]

# Remove responses from respondents that have answered multiple times
survey <- survey %>%
  filter(!duplicated(g01q48) & !duplicated(g01q48, fromLast = FALSE))

# Drop responses (e.g. outliers) manually (see comment for drop reason)
survey <- survey %>% filter(!seed %in% c(
  "444891847" # low time spent on survey, responses could be considerd outliers
  ))

# Replace Likert codes with numbers
survey <- survey %>%
  mutate_all(~ gsub("^AO0", "", .))

# Convert Likert columns to numeric 
survey <- survey %>%
  mutate(across(c(cch021_cch021:orh_orh3), as.numeric))

# Convert time columns to numeric 
survey <- survey %>%
  mutate(across(c(interviewtime:group_time839), as.numeric))

# Combine group_time482 and group_time810 into group_time_exercise
survey <- survey %>%
  mutate(group_time_exercise = coalesce(group_time482, group_time810, group_time834, group_time844)) %>%
  relocate(group_time_exercise, .after = interviewtime) %>%
  select(-group_time482, -group_time810, -group_time834, -group_time844)

# Reorder new columns
survey <- survey %>%
  relocate(pe_pe7, .after = pe_pe6) %>% relocate(dem15_dem15, .after = dem2) %>% rename(dem15 = dem15_dem15)

# Replace codes for demographics with readable names
survey <- survey %>%
  mutate(dem1 = case_when(
    dem1 == "DM110" ~ "61 - 65 years old",
    dem1 == "DM111" ~ "66 - 70 years old",
    dem1 == "DM112" ~ "older than 70 years old",
    dem1 == "DEM11"  ~ "younger than 20 years old",
    dem1 == "DEM12"  ~ "20 - 25 years old",
    dem1 == "DEM13"  ~ "26 - 30 years old",
    dem1 == "DEM14"  ~ "31 - 35 years old",
    dem1 == "DEM15"  ~ "36 - 40 years old",
    dem1 == "DEM16"  ~ "41 - 45 years old",
    dem1 == "DEM17"  ~ "46 - 50 years old",
    dem1 == "DEM18"  ~ "51 - 55 years old",
    dem1 == "DEM19"  ~ "56 - 60 years old",
    TRUE ~ dem1
  ))

survey <- survey %>%
  mutate(dem2 = case_when(
    dem2 == "1" ~ "Male",
    dem2 == "2" ~ "Female",
    dem2 == "3" ~ "Non-binary",
    TRUE ~ dem2
  ))

survey <- survey %>%
  mutate(dem4 = case_when(
    dem4 == "DEM41" ~ "Inner city area (big city)",
    dem4 == "DEM42" ~ "Outskirts/Suburb (big city)",
    dem4 == "DEM43" ~ "Inner city area (medium-sized city)",
    dem4 == "DEM44" ~ "Outskirts/Suburb (medium-sized city)",
    dem4 == "DEM45" ~ "Small town/large municipality",
    dem4 == "DEM46" ~ "Countryside/small rural community",
    TRUE ~ dem4
  ))

survey <- survey %>%
  mutate(dem5 = case_when(
    dem5 == "DEM51" ~ "Pre-Secondary Education",
    dem5 == "DEM52" ~ "Secondary Education",
    dem5 == "DEM53" ~ "Bachelor",
    dem5 == "DEM54" ~ "Master",
    dem5 == "DEM55" ~ "PhD",
    TRUE ~ dem5
  ))

survey <- survey %>%
  mutate(dem7 = case_when(
    dem7 == "1" ~ "Employed - full time",
    dem7 == "2" ~ "Employed - part time",
    dem7 == "3" ~ "Self-employed",
    dem7 == "4" ~ "Student",
    dem7 == "5" ~ "Retired",
    dem7 == "6" ~ "Not employed",
    dem7 == "7" ~ "Other",
    TRUE ~ dem7
  ))

survey <- survey %>%
  mutate(dem8 = case_when(
    dem8 == "DEM71" ~ "Healthcare",
    dem8 == "DEM72" ~ "Business, Executive, Management",
    dem8 == "DEM73" ~ "Architecture and Engineering",
    dem8 == "DEM74" ~ "Education, Training and Library Occupations",
    dem8 == "DEM75" ~ "Office and Administrative Support",
    dem8 == "DEM76" ~ "Services Occupations",
    dem8 == "DEM77" ~ "Agriculture, Maintenance, Repair and Skilled Crafts",
    dem8 == "DEM78" ~ "Other",
    TRUE ~ dem8
  ))

survey <- survey %>%
  mutate(dem9 = case_when(
    dem9 == "DEM01" ~ "Living alone",
    dem9 == "DEM02" ~ "Living with a partner (no children)",
    dem9 == "DEM03" ~ "Living with a partner (and children)",
    dem9 == "DEM04" ~ "Single parent with children",
    dem9 == "DEM05" ~ "Living with roommates",
    dem9 == "DEM06" ~ "Living with parents or other relatives",
    dem9 == "DEM07" ~ "Multigenerational household",
    dem9 == "DEM08" ~ "Extended family household",
    TRUE ~ dem9
  ))

survey <- survey %>%
  mutate(dem10 = case_when(
    dem10 == "DEM81" ~ "1",
    dem10 == "DEM82" ~ "2",
    dem10 == "DEM83" ~ "3",
    dem10 == "DEM84" ~ "4",
    dem10 == "DEM85" ~ ">4",
    TRUE ~ dem10
  ))

survey <- survey %>%
  mutate(dem11 = case_when(
    dem11 == "DEM01" ~ "I myself",
    dem11 == "DEM02" ~ "My partner",
    dem11 == "DEM03" ~ "One of my siblings",
    dem11 == "DEM04" ~ "Friends/Roommates",
    dem11 == "DEM05" ~ "My parents",
    dem11 == "DEM06" ~ "My kids",
    dem11 == "DEM07" ~ "Other",
    TRUE ~ dem11
  ))

survey <- survey %>%
  mutate(dem12 = case_when(
    dem12 == "DEM91" ~ "Less than €15,000",
    dem12 == "DEM92" ~ "€15,000 - €24,999",
    dem12 == "DEM93" ~ "€25,000 - €34,999",
    dem12 == "DEM94" ~ "€35,000 - €49,999",
    dem12 == "DEM95" ~ "€50,000 - €74,999",
    dem12 == "DEM96" ~ "€75,000 - €99,999",
    dem12 == "DEM97" ~ "€100,000 - €150,000",
    dem12 == "DEM98" ~ "€150,000 and above",
    TRUE ~ dem12
  ))

survey <- survey %>%
  mutate(dem13 = case_when(
    dem13 == "DE131" ~ "Arab/Middle Eastern",
    dem13 == "DE132" ~ "Asian",
    dem13 == "DE133" ~ "Indigenous",
    dem13 == "DE134" ~ "Black/African/Afro-Caribbean",
    dem13 == "DE135" ~ "Hispanic/Latin American",
    dem13 == "DE136" ~ "White/European",
    dem13 == "DE137" ~ "Mixed background/Other",
    TRUE ~ dem13
  ))