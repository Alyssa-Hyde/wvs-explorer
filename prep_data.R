# ============================================================
# prep_data.R — Clean and prepare WVS Wave 6 data
# ============================================================

# Install packages 
#install.packages(c("tidyverse", "haven", "shiny", "shinydashboard",  "plotly", "DT"))

# Load libraries
library(tidyverse)

wvs <- WV6_Data_R_v20201117

# Select only the columns we need
wvs_clean <- wvs %>%
  select(
    V2,           # Country code
    
    # Attitudes to democracy
    V228A, V228B, V228C, V228D, V228E, V228F, V228G, V228H, V228I,
    
    # News consumption
    V217, V218, V219, V220, V221, V222, V223, V224,
    
    # Attitudes to science
    V192, V193, V194, V195, V196, V197
  )

# Recode negative values as NA (WVS uses -1, -2, -4, -5 for missing)
wvs_clean <- wvs_clean %>%
  mutate(across(everything(), ~ifelse(. < 0, NA, .)))

# Add readable country names using a lookup table
country_lookup <- c(
  "12"  = "Algeria",
  "31"  = "Azerbaijan",
  "32"  = "Argentina",
  "36"  = "Australia",
  "51"  = "Armenia",
  "76"  = "Brazil",
  "112" = "Belarus",
  "152" = "Chile",
  "156" = "China",
  "158" = "Taiwan",
  "170" = "Colombia",
  "196" = "Cyprus",
  "218" = "Ecuador",
  "233" = "Estonia",
  "268" = "Georgia",
  "275" = "Palestine",
  "276" = "Germany",
  "288" = "Ghana",
  "332" = "Haiti",
  "344" = "Hong Kong",
  "356" = "India",
  "368" = "Iraq",
  "392" = "Japan",
  "398" = "Kazakhstan",
  "400" = "Jordan",
  "410" = "South Korea",
  "414" = "Kuwait",
  "417" = "Kyrgyzstan",
  "422" = "Lebanon",
  "434" = "Libya",
  "458" = "Malaysia",
  "484" = "Mexico",
  "504" = "Morocco",
  "528" = "Netherlands",
  "554" = "New Zealand",
  "566" = "Nigeria",
  "586" = "Pakistan",
  "604" = "Peru",
  "608" = "Philippines",
  "616" = "Poland",
  "634" = "Qatar",
  "642" = "Romania",
  "643" = "Russia",
  "646" = "Rwanda",
  "702" = "Singapore",
  "705" = "Slovenia",
  "710" = "South Africa",
  "716" = "Zimbabwe",
  "724" = "Spain",
  "752" = "Sweden",
  "764" = "Thailand",
  "780" = "Trinidad and Tobago",
  "788" = "Tunisia",
  "792" = "Turkey",
  "804" = "Ukraine",
  "818" = "Egypt",
  "840" = "United States",
  "858" = "Uruguay",
  "860" = "Uzbekistan",
  "887" = "Yemen"
)

wvs_clean <- wvs_clean %>%
  mutate(country = country_lookup[as.character(V2)])

# Add readable question labels for each section

# Democracy labels (V228A-I are rated 1-4: Not at all democratic to Definitely democratic)
democracy_labels <- c(
  V228A = "Governments tax the rich and subsidize the poor",
  V228B = "Religious authorities interpret the laws",
  V228C = "People choose their leaders in free elections",
  V228D = "People receive state aid for unemployment",
  V228E = "The army takes over when government is incompetent",
  V228F = "Civil rights protect people from oppression",
  V228G = "The economy is prospering",
  V228H = "Criminals are severely punished",
  V228I = "People can change the laws through referendums"
)

# News consumption labels (V217-V224 are rated 1-5: Daily to Never)
news_labels <- c(
  V217 = "Daily newspaper",
  V218 = "TV news",
  V219 = "Radio news",
  V220 = "Mobile phone news",
  V221 = "Email news",
  V222 = "Internet news",
  V223 = "Talk with friends/family",
  V224 = "Social media"
)

# Science labels (V192-V197 are rated 1-10: Strongly disagree to Strongly agree)
science_labels <- c(
  V192 = "Science and technology make our lives healthier",
  V193 = "Science and technology create more opportunities",
  V194 = "Science makes our way of life change too fast",
  V195 = "We depend too much on science, not enough on faith",
  V196 = "Science breaks down people's ideas of right and wrong",
  V197 = "Science will solve our environmental problems"
)

# Pre-compute global averages (entire WVS sample) for each section

# Democracy: mean per question across all countries
global_democracy <- wvs_clean %>%
  summarise(across(V228A:V228I, ~mean(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "global_mean") %>%
  mutate(question = democracy_labels[variable])

# News: proportion in each category (1-5) per question across all countries
global_news <- wvs_clean %>%
  select(V217:V224) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n),
         question = news_labels[variable])

# Science: mean per question across all countries
global_science <- wvs_clean %>%
  summarise(across(V192:V197, ~mean(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "global_mean") %>%
  mutate(question = science_labels[variable])

# Get the final country list
country_list <- sort(unique(wvs_clean$country))
print(country_list)
cat("Total countries:", length(country_list), "\n")

# Save everything needed for the app
save(
  wvs_clean,
  country_list,
  democracy_labels,
  news_labels,
  science_labels,
  global_democracy,
  global_news,
  global_science,
  file = "data/wvs_prepared.RData"
)

cat("✅ Data preparation complete! wvs_prepared.RData saved.\n")
