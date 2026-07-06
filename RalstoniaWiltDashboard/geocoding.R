library(readxl)
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(stringr)
library(readr)
library(tibble)


#check and change token regularly
MAPBOX_TOKEN <- "TOKEN_GOES_HERE"

input_file  <- "RSSC_2026.xlsx"
output_file <- "RSSC_2026_Geocoded.csv"
lookup_file <- "location_lookup.csv"

#--------------------------------------------------
# CLEAN + BUILD QUERY
#--------------------------------------------------

clean_value <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x %in% c("", "NA", "N/A", "Unknown", "unknown", "Not reported", "missing", "?")] <- NA_character_
  x
}

build_query <- function(loc, country, continent) {
  loc      <- clean_value(loc)
  country  <- clean_value(country)
  continent <- clean_value(continent)
  
  pieces <- c()
  
  if (!is.na(loc))      pieces <- c(pieces, loc)
  if (!is.na(country))  pieces <- c(pieces, country)
  if (is.na(country) && !is.na(continent)) pieces <- c(pieces, continent)
  
  pieces <- unique(pieces)
  if (length(pieces) == 0) return(NA_character_)
  
  paste(pieces, collapse = ", ")
}

#--------------------------------------------------
# EMPTY RESULT (ALWAYS RETURNS A TIBBLE)
#--------------------------------------------------

empty_geo <- function(msg = NA_character_, status = NA_integer_) {
  tibble(
    longitude = NA_real_,
    latitude = NA_real_,
    match_name = NA_character_,
    status_code = status,
    error_message = msg
  )
}

#--------------------------------------------------
# GEOCODER (NO UNNEST, NO LIST-COLUMNS)
#--------------------------------------------------

geocode_mapbox <- function(query, token) {
  
  query <- clean_value(query)
  if (is.na(query)) return(empty_geo("Empty query"))
  
  encoded <- URLencode(query, reserved = TRUE)
  url <- paste0(
    "https://api.mapbox.com/geocoding/v5/mapbox.places/",
    encoded,
    ".json?limit=1&autocomplete=false&access_token=",
    token
  )
  
  resp <- try(GET(url), silent = TRUE)
  
  if (inherits(resp, "try-error")) {
    return(empty_geo("HTTP request failed"))
  }
  
  sc <- status_code(resp)
  txt <- content(resp, "text", encoding = "UTF-8")
  
  if (sc != 200) {
    return(empty_geo(txt, sc))
  }
  
  parsed <- try(fromJSON(txt), silent = TRUE)
  
  if (inherits(parsed, "try-error")) {
    return(empty_geo("JSON parse failed"))
  }
  
  if (is.null(parsed$features) || nrow(parsed$features) == 0) {
    return(empty_geo("No features returned", sc))
  }
  
  feat <- parsed$features[1, ]
  
  # center is a list-column → extract safely
  center <- feat$center[[1]]
  
  if (!is.numeric(center) || length(center) < 2) {
    return(empty_geo("Invalid center coordinates", sc))
  }
  
  tibble(
    longitude = center[1],
    latitude  = center[2],
    match_name = feat$place_name,
    status_code = sc,
    error_message = NA_character_
  )
}

#--------------------------------------------------
# READ EXCEL
#--------------------------------------------------

df <- read_excel(input_file, col_types = "text") %>%
  mutate(
    geocode_query = pmap_chr(
      list(
        `Location Isolated`,
        `Location (Country or Territory)`,
        `Location (continent)`
      ),
      build_query
    )
  )

#--------------------------------------------------
# LOAD CACHE
#--------------------------------------------------

if (file.exists(lookup_file)) {
  lookup <- read_csv(lookup_file, show_col_types = FALSE)
} else {
  lookup <- tibble(
    geocode_query = character(),
    longitude = numeric(),
    latitude = numeric(),
    match_name = character(),
    status_code = integer(),
    error_message = character()
  )
}

#--------------------------------------------------
# GEOCODE MISSING QUERIES (NO UNNEST)
#--------------------------------------------------

missing <- df %>%
  distinct(geocode_query) %>%
  filter(!is.na(geocode_query), geocode_query != "") %>%
  anti_join(lookup, by = "geocode_query")

if (nrow(missing) > 0) {
  
  new_results <- map_df(missing$geocode_query, function(q) {
    res <- geocode_mapbox(q, MAPBOX_TOKEN)
    res$geocode_query <- q
    res
  })
  
  lookup <- bind_rows(lookup, new_results) %>%
    distinct(geocode_query, .keep_all = TRUE)
  
  write_csv(lookup, lookup_file)
}

#--------------------------------------------------
# JOIN + SAVE
#--------------------------------------------------

df_final <- df %>%
  left_join(lookup, by = "geocode_query")

write_csv(df_final, output_file)

cat("Geocoding complete. Saved:", output_file, "\n")
