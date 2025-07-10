library(googlesheets4)
library(ggmap)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(tibble) 
library(sf)
library(geojsonio)

# the Google Sheets API must be enabled for this to work

# test the map by running: python3 -m http.server 8000

# to test, use:
# Sys.setenv(GSHEET_SERVICE_ACCOUNT_JSON = "/path/to/your/service-account.json")
# Sys.setenv(GOOGLE_MAPS_API_KEY = "your-google-maps-api-key")

# Authenticate with service account
gs4_auth(path = Sys.getenv("GSHEET_SERVICE_ACCOUNT_JSON"))

# Register Maps API Key
register_google(key = Sys.getenv("GOOGLE_MAPS_API_KEY"), write = TRUE)

# remap data to web map friendly termsx 
type_map <- tibble(type = c("On Premise",
                            "Off Premise"),
                   easy_type = c("Bar/Restaurant/Venue",
                                 "Liquor Store"))

product_map <- tibble(products = c("V", "B", "G", "Lim", "Rye", "Gen"),
                      easy_products = c("Vodka", "Bourbon", "Gin", "Limoncello", "Rye", "Genever"))

sheet_url <- "https://docs.google.com/spreadsheets/d/1PmbShY8hdUBKz3PJPfVD_OTJNzTdHUrmx_xfeWy0GOM"

df <- read_sheet(sheet_url) |> 
  janitor::clean_names() |>
  rename(
    account = account,
    google_url = google_maps_url,
    website_url = website,
    address = address_as_shown_in_google,
    products = products_carried_currently,
    type = account_type
  ) |>
  select(account, google_url, website_url, address, products, type)

# Transform to long-form and map products/types
df_long <- df |>
  mutate(products = str_replace_all(products, ",\\s*", ",")) |> 
  separate_rows(products, sep = ",") |>
  filter(!is.na(products) & products != "") |>
  mutate(products = str_remove_all(products, "750|1000") |> str_trim()) |>
  left_join(product_map, by = "products") |>
  left_join(type_map, by = "type")

# Collapse products per account
df_products <- df_long |>
  group_by(account) |>
  summarise(
    easy_products = paste(sort(unique(easy_products)), collapse = ", "),
    .groups = "drop"
  )

# Merge back into original
df_mapped <- df |>
  left_join(type_map, by = "type") |>
  left_join(df_products, by = "account")

# Add Mobb Mountain tasting room
mobb <- tibble(account = "Mobb Mountain Distillers",
               google_url = "https://maps.app.goo.gl/wMU7LySz7QERh8a86",
               website_url = "https://www.mobbmountain.com/",
               address = "400 Linden St, Fort Collins, CO 80524",
               easy_type = "Tasting Room",
               easy_products = "Vodka, Bourbon, Gin, Limoncello, Rye, Genever, Single Malt Whiskey (limited release)")

df_mapped <- bind_rows(df_mapped, mobb)

# Geocode
df_geocoded <- df_mapped |> filter(!is.na(address))
coords <- geocode(df_geocoded$address, output = "latlon", source = "google")
df_geo <- bind_cols(df_geocoded, coords) %>%
  drop_na(products, type, lon, lat)

# Convert to sf for GeoJSON export
df_sf <- st_as_sf(df_geo, coords = c("lon", "lat"), crs = 4326)

# Format popup field
df_sf <- df_sf |>
  mutate(
    account_link = if_else(
      !is.na(website_url) & website_url != "",
      paste0(
        "<a href='", website_url, "' target='_blank'>", account,
        "<img title='Open website' src='styles/up-right-from-square-solid.svg' style='width:12px;height:12px;margin-left:4px;vertical-align:middle;' />",
        "</a>"
      ),
      account
    ),
    maps_link = if_else(
      !is.na(google_url) & google_url != "",
      paste0(
        "<a href='", google_url, "' target='_blank'>View on Google Maps",
        "<img title='Open in Google Maps' src='styles/map-solid.svg' style='width:14px;height:14px;margin-left:4px;vertical-align:middle;' />",
        "</a>"
      ),
      ""
    ),
    type_icon = paste0(
      "<img src='styles/", 
      case_when(
        easy_type == "Tasting Room" ~ "crow.svg",
        easy_type == "Bar/Restaurant/Venue" ~ "glass-cocktail.svg",
        easy_type == "Liquor Store" ~ "bottle-wine.svg",
        TRUE ~ "bottle-wine.svg"
      ), 
      "' style='width:16px;height:16px;margin-left:8px;vertical-align:middle;' />"
    ),
    popup = paste0(
      "<strong>", account_link, "</strong><br>",
      "<em>", easy_type, "</em>", "<br>",
      "Products: ", easy_products, "<br>",
      maps_link
    )
  ) |>
  select(account, address, easy_type, easy_products, popup, geometry)

# Save to GeoJSON
geojson_write(df_sf, file = "accounts.geojson")