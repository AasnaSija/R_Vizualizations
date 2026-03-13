# =============================================================================
# Data Visualizations: Remittances, Migration, and Gender
# Aasna Sijapati
# =============================================================================
# Topics covered:
#   - Remittance patterns by gender and marital status (Philippines)
#   - Remittance trends over time (Nepal, Chitwan Valley)
#   - Remittance distribution by education and gender (Philippines)
#   - Remittance trends by ethnicity (Nepal, Chitwan Valley)
#   - Household income composition including remittances (Nepal)
#   - Remittances and Female Human Development Index
#   - Remittances and Female Labor Force Participation
#   - Mean years of schooling in high-remittance countries
#
# Data sources:
#   - Survey of Overseas Filipino Workers (PSA, 2023)
#   - Chitwan Valley Family Study (1995–2019), Data Sharing for Demographic Research
#   - Custom dataset (DataStory1.xlsx)
# =============================================================================

# --- Packages -----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(haven)
library(scales)
library(stringr)
library(treemapify)

# ==============================================================================
# SECTION 1: PHILIPPINE OVERSEAS WORKERS — REMITTANCE PATTERNS
# Data: Survey of Overseas Filipino Workers, October 2023
# ==============================================================================

# ------------------------------------------------------------------------------
# Viz 1: Average Remittance by Marital Status and Gender
# ------------------------------------------------------------------------------

marstat_rem <- read_csv("marstat_rem.csv")

marstat_rem <- marstat_rem %>%
  filter(rq7_mstat != 6) %>%   # Remove "Other" category
  mutate(
    rq3_sex   = factor(rq3_sex,   levels = c(1, 2), labels = c("Male", "Female")),
    rq7_mstat = factor(rq7_mstat, levels = c(1, 2, 3, 4, 5),
                       labels = c("Single", "Married", "Widowed", "Separated", "Divorced"))
  )

annotation_text <- str_wrap(
  "Married men send the highest remittances, while divorced individuals remit the least",
  width = 35
)

ggplot(marstat_rem, aes(x = rq7_mstat, y = avg_remit, fill = rq3_sex)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = round(avg_remit)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  labs(
    title   = "Average Remittance by Marital Status and Gender",
    x       = "Marital Status",
    y       = "Average Remittance (PHP)",
    fill    = "Gender",
    caption = "Data source: Survey of Overseas Filipino Workers, October 2023"
  ) +
  scale_fill_manual(values = c("Male" = "dodgerblue3", "Female" = "firebrick2")) +
  annotate("text", x = 5, y = Inf, hjust = 0, label = annotation_text, size = 4) +
  coord_flip(clip = "off") +
  theme(
    panel.background    = element_blank(),
    plot.background     = element_blank(),
    panel.grid.major    = element_blank(),
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_line(linetype = "dotted", linewidth = 0.5, color = "gray"),
    axis.line           = element_blank(),
    axis.ticks          = element_blank(),
    plot.title          = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.text.x         = element_text(angle = 30, hjust = 1),
    legend.background   = element_blank(),
    legend.key          = element_blank()
  )

# ------------------------------------------------------------------------------
# Viz 2: Remittance Distribution by Education Level and Gender (Boxplot)
# ------------------------------------------------------------------------------

SOF.PUF.2023 <- read.csv("SOF PUF 2023.csv", header = TRUE)

SOF.PUF.2023 <- SOF.PUF.2023 %>%
  mutate(
    RQ3_SEX   = as.factor(RQ3_SEX),
    RQ8_HGRADE = as.factor(RQ8_HGRADE)
  ) %>%
  filter(!is.na(RQ8_HGRADE), RQ8_HGRADE != 0, RQ24_CASHAMT <= 500000) %>%
  mutate(
    Education_Group = case_when(
      RQ8_HGRADE %in% c("1", "2")                ~ "Primary Level",
      RQ8_HGRADE %in% c("3", "4", "5", "6")      ~ "Secondary Level",
      RQ8_HGRADE %in% c("7", "8")                ~ "Post-Secondary Level",
      RQ8_HGRADE %in% c("9", "10")               ~ "College Level",
      RQ8_HGRADE == "11"                          ~ "Postgraduate Level",
      TRUE                                        ~ NA_character_
    ),
    Education_Group = factor(Education_Group,
                             levels = c("Primary Level", "Secondary Level",
                                        "Post-Secondary Level", "College Level",
                                        "Postgraduate Level"))
  )

ggplot(SOF.PUF.2023, aes(x = Education_Group, y = RQ24_CASHAMT,
                          fill  = as.character(RQ3_SEX),
                          color = as.character(RQ3_SEX))) +
  geom_boxplot(alpha = 0.4, width = 0.8) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red"),
                    labels  = c("1" = "Male", "2" = "Female")) +
  scale_color_manual(values = c("1" = "blue", "2" = "red"),
                     labels  = c("1" = "Male", "2" = "Female")) +
  labs(
    title    = "Remittance Distribution by Education and Gender\nFor Filipino Migrants in 2023",
    subtitle = "Male Filipino migrants tend to send higher amounts of remittance across most education levels",
    caption  = "Data source: Philippine Statistics Authority (SOF 2023)",
    x        = "Highest Education Level Attained",
    y        = "Remittance Sent (Amount in PHP)"
  ) +
  theme(legend.title = element_blank())


# ==============================================================================
# SECTION 2: NEPAL — CHITWAN VALLEY FAMILY STUDY
# Data: Chitwan Valley Family Study (1995–2019)
# ==============================================================================

# ------------------------------------------------------------------------------
# Viz 3: Average Remittances Over Time in Chitwan, Nepal (Line Chart)
# ------------------------------------------------------------------------------

avgremyrs_chitwan <- read_excel("avgremyrs_chitwan.xlsx") %>%
  rename(year = Year, avg_rem = `Avg Rem`) %>%
  mutate(ad_year = year - 56)   # Convert Bikram Sambat (BS) to AD

annotation_text <- str_wrap(
  "Remittances decreased sharply in 2015, the year of the Nepal earthquake.",
  width = 35
)

ggplot(avgremyrs_chitwan) +
  annotate(geom = "rect",
           xmin = 2015, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "black") +
  geom_line(aes(x = ad_year, y = avg_rem), color = "dodgerblue4", linewidth = 0.8) +
  geom_point(data = avgremyrs_chitwan %>% filter(ad_year %in% range(ad_year)),
             aes(x = ad_year, y = avg_rem), color = "darkorange2", size = 2) +
  annotate("text", x = 2016, y = max(avgremyrs_chitwan$avg_rem, na.rm = TRUE) * 0.95,
           label = annotation_text, hjust = 0, size = 4, color = "black") +
  scale_x_continuous(breaks = avgremyrs_chitwan$ad_year, labels = avgremyrs_chitwan$ad_year) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = c(60000, NA)) +
  labs(
    title    = "Average Remittances Sent by Migrant Workers in Chitwan, Nepal",
    subtitle = "Remittance inflows have fluctuated over time, reflecting broader economic and migration patterns",
    x        = "Year (AD)",
    y        = "Average Remittance (in NPR)",
    caption  = "Data source: Chitwan Valley Family Study (1995–2019), Data Sharing for Demographic Research"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5, color = "gray30"),
    plot.caption    = element_text(size = 9, color = "gray40", hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", linewidth = 0.5, color = "gray"),
    axis.line.x     = element_line(linetype = "solid", color = "black"),
    plot.margin     = margin(10, 30, 10, 10)
  )

# ------------------------------------------------------------------------------
# Viz 4: Remittance Trends by Ethnic Group (Faceted Line Chart)
# ------------------------------------------------------------------------------

rem_data <- read_dta("36755-0001-Data.dta")

rem_long <- rem_data %>%
  select(ETHNICITY, starts_with("REM")) %>%
  pivot_longer(cols = starts_with("REM"), names_to = "year_bs", values_to = "remittance") %>%
  mutate(
    ETHNICITY  = as_factor(ETHNICITY),
    remittance = as.numeric(remittance),
    year_bs    = as.numeric(str_extract(year_bs, "\\d+")) + 2000,
    year_ad    = year_bs - 57
  ) %>%
  group_by(ETHNICITY, year_ad) %>%
  summarize(avg_remittance = mean(remittance, na.rm = TRUE), .groups = "drop") %>%
  filter(ETHNICITY != "Others")

ggplot(rem_long, aes(x = year_ad, y = avg_remittance, group = ETHNICITY)) +
  geom_line(linewidth = 1, color = "dodgerblue") +
  geom_point(size = 1.8, color = "dodgerblue") +
  facet_wrap(~ ETHNICITY, scales = "fixed") +
  scale_x_continuous(breaks = seq(2006, 2015, 1)) +
  scale_y_continuous(labels = comma) +
  expand_limits(y = 0) +
  labs(
    title    = "Trends in Average Remittances Sent by Ethnic Group, 2006–2015",
    subtitle = "Remittance levels rose across all major ethnic groups between 2006 and 2015,\nthough the pace and magnitude of increases differed by ethnic group.",
    y        = "Average Remittance (in Nepalese Rupees)",
    caption  = "Data source: Chitwan Valley Family Study (1995–2019), Data Sharing for Demographic Research"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed", linewidth = 0.4)
  )

# ------------------------------------------------------------------------------
# Viz 5: Household Income Composition Including Remittances (Treemap)
# ------------------------------------------------------------------------------

income_2015 <- rem_data %>%
  select(B11C_72, B12B_72, B13C_72, B14C_72, B15C_72, B16B_72,
         B17_1B_72, B17_2B_72, B17_3B_72, B17_4B_72, B17_5B_72, B17_7B_72,
         B17_8_72, D30B_72, D31B_72, REM72 = REM71) %>%
  mutate(across(everything(), as.numeric),
         across(everything(), ~ ifelse(. < 0 | is.na(.), NA, .)))

income_summary <- income_2015 %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "source", values_to = "avg_income") %>%
  mutate(
    avg_income = ifelse(is.na(avg_income) | is.nan(avg_income), 0, avg_income),
    crop_name = case_when(
      source == "B11C_72"    ~ "Rice",         source == "B12B_72"    ~ "Corn",
      source == "B13C_72"    ~ "Wheat",        source == "B14C_72"    ~ "Mustard",
      source == "B15C_72"    ~ "Red Lentil",   source == "B16B_72"    ~ "Vegetables",
      source == "B17_1B_72"  ~ "Buckwheat",    source == "B17_2B_72"  ~ "Finger Millet",
      source == "B17_3B_72"  ~ "Sesame",       source == "B17_4B_72"  ~ "Kidney Beans",
      source == "B17_5B_72"  ~ "Peas",         source == "B17_7B_72"  ~ "Other Crops",
      source == "B17_8_72"   ~ "Land Rent",    source == "D30B_72"    ~ "Poultry Farming",
      source == "D31B_72"    ~ "Other Farming",source == "REM72"      ~ "Remittances"
    ),
    category = case_when(
      source %in% c("B11C_72","B12B_72","B13C_72","B14C_72","B15C_72",
                    "B16B_72","B17_1B_72","B17_2B_72","B17_3B_72",
                    "B17_4B_72","B17_5B_72","B17_7B_72") ~ "Crop Production",
      source == "B17_8_72" ~ "Rental Income",
      source == "D30B_72"  ~ "Livestock & Poultry",
      source == "D31B_72"  ~ "Non-Crop Production",
      source == "REM72"    ~ "Remittance Income"
    )
  ) %>%
  filter(!(category == "Crop Production" & avg_income < 300)) %>%
  arrange(category, avg_income)

# Gradient colors for crop subcategories
n_crops       <- nrow(filter(income_summary, category == "Crop Production"))
crop_gradient <- colorRampPalette(c("#C8E6C9", "#1B5E20"))(n_crops)

income_summary <- income_summary %>%
  mutate(fill_color = case_when(
    category == "Rental Income"       ~ "navajowhite2",
    category == "Livestock & Poultry" ~ "lightskyblue3",
    category == "Non-Crop Production" ~ "olivedrab",
    category == "Remittance Income"   ~ "rosybrown",
    TRUE                              ~ NA_character_
  ))

income_summary$fill_color[income_summary$category == "Crop Production"] <-
  crop_gradient[rank(income_summary$avg_income[income_summary$category == "Crop Production"])]

ggplot(income_summary, aes(area = avg_income, fill = fill_color,
                            label = crop_name, subgroup = category)) +
  geom_treemap(color = "white") +
  geom_treemap_subgroup_border(color = "grey40", size = 1.2) +
  geom_treemap_subgroup_text(place = "topleft", grow = TRUE, alpha = 0.8,
                              colour = "grey20", fontface = "bold", size = 8) +
  geom_treemap_text(aes(label = crop_name), place = "center", reflow = TRUE,
                    colour = "white", fontface = "bold", min.size = 4) +
  scale_fill_identity() +
  labs(
    title    = "Composition of Average Household Income in 2015 (AD)",
    subtitle = "Including remittances, crop sales, livestock, land rental, and other farming activities.",
    caption  = "Data source: Chitwan Valley Family Study (1995–2019), Data Sharing for Demographic Research"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5, color = "grey30")
  )


# ==============================================================================
# SECTION 3: REMITTANCES AND GENDER DEVELOPMENT OUTCOMES (CROSS-COUNTRY)
# Data: DataStory1.xlsx
# ==============================================================================

df <- read_excel("DataStory1.xlsx") %>%
  mutate(
    IncomeLevel_clean = case_when(
      IncomeLevel %in% c("Lower middle income", "Lower-Middle", "Lower Middle") ~ "Lower Middle",
      IncomeLevel %in% c("Upper middle income", "Upper-Middle", "Upper Middle") ~ "Upper Middle",
      IncomeLevel %in% c("High income", "Upper", "High")                        ~ "High",
      TRUE ~ NA_character_
    ),
    IncomeLevel_clean = factor(IncomeLevel_clean, levels = c("Lower Middle", "Upper Middle", "High"))
  )

color_scale <- scale_color_manual(values = c(
  "Lower Middle" = "#3182bd",
  "Upper Middle" = "#cb181d",
  "High"         = "#31a354"
))

# Shared theme for Viz 6 and 7
dev_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4, linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------------------------
# Viz 6: Female HDI vs Remittance Dependence
# ------------------------------------------------------------------------------

ggplot(df, aes(x = `%GDP`, y = FHDI)) +
  geom_point(aes(color = IncomeLevel_clean), size = 3, alpha = 0.7) +
  geom_text(aes(label = CountryCode, color = IncomeLevel_clean),
            size = 3, vjust = -0.6, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  facet_wrap(~ IncomeLevel_clean, scales = "fixed") +
  color_scale +
  labs(
    title    = "HDI vs Remittance Dependence by Income Level",
    subtitle = "Linear trends shown separately for lower-middle, upper-middle, and high-income countries",
    x        = "Remittances (% of GDP)",
    y        = "Female Human Development Index (FHDI)",
    color    = "Income Level"
  ) +
  dev_theme

# ------------------------------------------------------------------------------
# Viz 7: Female Labor Force Participation vs Remittance Dependence
# ------------------------------------------------------------------------------

ggplot(df, aes(x = `%GDP`, y = FLP)) +
  geom_point(aes(color = IncomeLevel_clean), size = 3, alpha = 0.7) +
  geom_text(aes(label = CountryCode, color = IncomeLevel_clean),
            size = 3, vjust = -0.6, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.7, span = 0.75) +
  facet_wrap(~ IncomeLevel_clean, scales = "fixed") +
  color_scale +
  labs(
    title    = "Female Labor Force Participation vs Remittance Dependence",
    subtitle = "Linear trends shown separately for lower-middle, upper-middle, and high-income countries",
    x        = "Remittances (% of GDP)",
    y        = "Female Labor Force Participation (%)",
    color    = "Income Level"
  ) +
  dev_theme

# ------------------------------------------------------------------------------
# Viz 8: Mean Years of Schooling in High-Remittance Countries
# ------------------------------------------------------------------------------

df_long <- df %>%
  filter(`%GDP` >= 15) %>%
  select(Country, IncomeLevel, `%GDP`, FMYS, MMYS) %>%
  pivot_longer(cols = c(FMYS, MMYS), names_to = "Gender", values_to = "MeanYearsSchooling") %>%
  mutate(
    Gender = recode(Gender, FMYS = "Female", MMYS = "Male"),
    IncomeLevel_clean = case_when(
      IncomeLevel %in% c("Lower middle income", "Lower-Middle", "Lower Middle") ~ "Lower Middle",
      IncomeLevel %in% c("Upper middle income", "Upper-Middle", "Upper Middle") ~ "Upper Middle",
      IncomeLevel %in% c("High income", "Upper", "High")                        ~ "High",
      TRUE ~ NA_character_
    ),
    IncomeGender = paste(IncomeLevel_clean, Gender, sep = " – ")
  ) %>%
  group_by(Country) %>%
  mutate(remittance_gdp = first(`%GDP`)) %>%
  ungroup() %>%
  mutate(Country = reorder(Country, remittance_gdp))

fill_colors <- c(
  "Lower Middle – Female" = "#bdd7e7", "Lower Middle – Male"   = "#6baed6",
  "Upper Middle – Female" = "#fcae91", "Upper Middle – Male"   = "#cb181d",
  "High – Female"         = "#31a354", "High – Male"           = "#bae4b3"
)

ggplot(df_long, aes(x = Country, y = MeanYearsSchooling, fill = IncomeGender)) +
  geom_col(alpha = 0.95) +
  geom_text(aes(label = round(MeanYearsSchooling, 1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.3, fontface = "bold") +
  scale_fill_manual(values = fill_colors, name = "Income level × Gender") +
  labs(
    title    = "Mean Years of Schooling in High-Remittance Countries",
    subtitle = "Countries ordered by remittances as a share of GDP (≥15%)",
    x        = NULL,
    y        = "Mean Years of Schooling"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "black"),
    panel.border     = element_blank()
  )

# =============================================================================
# End of script
# =============================================================================
