# Where to find Mobb Mountain products
Leaflet map showing locations to purchase Mobb products. Embedded in an iframe on [Where to Find Mobb](https://mobbmountain.com/where-to-find-mobb).

The master list of accounts is currently located [here](https://docs.google.com/spreadsheets/d/1rXT-PwitFTZMc9Q5oxVhd3z9Cv0tYltu4YKy70INvF0/edit#gid=0). After updating, the addresses should be geocoded using Google's API and it should be converted to a GeoJSON file for use in the web map. 

Steps:
1. Update the Google Sheet
2. Export to CSV
3. Install the MMQGIS plugin for QGIS
4. Batch geocode using Google (you need a free API key)
5. Export the layer to GeoJSON