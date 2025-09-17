# Clean Gas Flaring Data

# Load data --------------------------------------------------------------------
gf_df <- read_xlsx(file.path(gasflare_dir, "rawdata", 
                             "2012-2024-Flare-Volume-Estimates-by-individual-Flare-Location.xlsx"))

gf_sf <- gf_df %>%
  clean_names() %>%
  dplyr::select(country, latitude, longitude) %>%
  distinct() %>%
  dplyr::mutate(uid = 1:n()) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

saveRDS(gf_sf,      file.path(gasflare_dir, "finaldata", "gas_flare_locations.Rds"))


