library(googlesheets4)
library(tidyverse)
library(tidytext)
library(sf)
library(ggmap)
library(geojsonio)

gs4_auth()

sheet_url <- "https://docs.google.com/spreadsheets/d/1PmbShY8hdUBKz3PJPfVD_OTJNzTdHUrmx_xfeWy0GOM/edit?gid=0#gid=0"

df <- read_sheet(sheet_url) |> 
  janitor::clean_names() |>  # standardizes column names (e.g., all lowercase, underscores)
  rename(
    account = account,
    address = address_as_shown_in_google,
    products = products_carried_currently,
    type = account_type
  ) |>
  select(account, address, products, type)

df_long <- df |>
  mutate(products = str_replace_all(products, ",\\s*", ",")) |>  # remove spaces after commas
  separate_rows(products, sep = ",") |>                          # one row per product
  filter(!is.na(products) & products != "") |>                  # drop empty rows
  mutate(present = 1)

df_wide <- df_long |>
  pivot_wider(
    names_from = products,
    values_from = present,
    values_fill = 0
  )

register_google(key = "API-KEY-HERE", write = TRUE)

df_to_geocode <- df_wide |> filter(!is.na(address))

geocoded <- geocode(df_to_geocode$address, output = "latlon", source = "google")

df_geo <- bind_cols(df_to_geocode, geocoded)

df_sf <- st_as_sf(df_geo, coords = c("lon", "lat"), crs = 4326)

df_sf <- df_sf |>
  select(account, address, type, starts_with("V"), starts_with("G"), starts_with("B"), starts_with("Rye"), starts_with("Lim"), geometry)

geojson_write(df_sf, file = "accounts.geojson")
