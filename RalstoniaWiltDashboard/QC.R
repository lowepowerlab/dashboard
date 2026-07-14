library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(tibble)

#===========================================================
# 0. Extract unique locations (faster QC)
#===========================================================

unique_locs <- df_final %>%
  distinct(geocode_query, latitude, longitude, match_name,
           `Location (Country or Territory)`,
           `Location (continent)`)

#===========================================================
# 1. QC: GEOCODING FAILURES (NAs)
#===========================================================

qc_failures <- unique_locs %>%
  filter(is.na(latitude) | is.na(longitude))

#===========================================================
# 2. QC: IMPOSSIBLE COORDINATES
#===========================================================

qc_impossible <- unique_locs %>%
  filter(latitude < -90 | latitude > 90 |
           longitude < -180 | longitude > 180)

#===========================================================
# 3. QC: CONTINENT MISMATCHES
#===========================================================

unique_locs <- unique_locs %>%
  mutate(continent_guess = case_when(
    latitude > 0 & longitude < -30 & longitude > -170 ~ "North America",
    latitude < 0 & longitude < -30 & longitude > -170 ~ "South America",
    latitude > 0 & longitude > -30 & longitude < 60 ~ "Europe",
    latitude < 0 & longitude > -30 & longitude < 60 ~ "Africa",
    longitude > 60 & longitude < 180 ~ "Asia",
    TRUE ~ "Other"
  ))

qc_continent_mismatch <- unique_locs %>%
  filter(!is.na(`Location (continent)`)) %>%
  filter(tolower(continent_guess) != tolower(`Location (continent)`))

#===========================================================
# 4. QC: COUNTRY MISMATCHES
#===========================================================

unique_locs <- unique_locs %>%
  mutate(geo_country = word(match_name, -1))

qc_country_mismatch <- unique_locs %>%
  filter(!is.na(`Location (Country or Territory)`)) %>%
  filter(tolower(geo_country) != tolower(`Location (Country or Territory)`))

#===========================================================
# 5. QC: VAGUE QUERIES (continent-level or ambiguous)
#===========================================================

qc_vague <- unique_locs %>%
  filter(str_detect(geocode_query, regex("Africa|Asia|Europe|America|Oceania", ignore_case = TRUE)))

#===========================================================
# 6. QC: MALFORMED QUERIES (too many commas)
#===========================================================

qc_malformed <- unique_locs %>%
  filter(str_count(geocode_query, ",") > 1)

#===========================================================
# 7. QC: OUTLIERS (distance-based)
#===========================================================

# crude outlier detection: points far from cluster centers
qc_outliers <- unique_locs %>%
  mutate(outlier_flag = latitude > quantile(latitude, 0.99) |
           latitude < quantile(latitude, 0.01) |
           longitude > quantile(longitude, 0.99) |
           longitude < quantile(longitude, 0.01)) %>%
  filter(outlier_flag)

#===========================================================
# 8. QC: SUMMARY TABLE
#===========================================================

qc_summary <- tibble(
  QC_Category = c(
    "Geocoding failures (NA lat/long)",
    "Impossible coordinates",
    "Continent mismatches",
    "Country mismatches",
    "Vague queries",
    "Malformed queries"
   #"Outliers"
  ),
  Count = c(
    nrow(qc_failures),
    nrow(qc_impossible),
    nrow(qc_continent_mismatch),
    nrow(qc_country_mismatch),
    nrow(qc_vague),
    nrow(qc_malformed)
    #nrow(qc_outliers)
  )
)

#===========================================================
# 9. OUTPUT QC TABLES
#===========================================================

write_csv(qc_summary, "QC_summary.csv")
write_csv(qc_failures, "QC_failures.csv")
write_csv(qc_impossible, "QC_impossible.csv")
write_csv(qc_continent_mismatch, "QC_continent_mismatch.csv")
write_csv(qc_country_mismatch, "QC_country_mismatch.csv")
write_csv(qc_vague, "QC_vague_queries.csv")
write_csv(qc_malformed, "QC_malformed_queries.csv")
#write_csv(qc_outliers, "QC_outliers.csv")

#===========================================================
# 10. OPTIONAL: QUICK VISUAL QC
#===========================================================

ggplot(unique_locs, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.4, size = 1) +
  coord_fixed() +
  theme_minimal()
