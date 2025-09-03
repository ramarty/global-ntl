# Clean Gas Flaring Data

# Load data --------------------------------------------------------------------
clean_data <- function(x) x %>% clean_names()

df_2021 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "2021 Global Gas Flaring Volumes.xlsx"), 2) %>% clean_data()

df_2020_1 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "2020 Global Gas Flaring Volumes.xlsx"), 1) %>% clean_data()
df_2020_2 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "2020 Global Gas Flaring Volumes.xlsx"), 2) %>% clean_data()
df_2020_3 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "2020 Global Gas Flaring Volumes.xlsx"), 3) %>% clean_data()

df_2019 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "viirs_global_flaring_d.7_slope_0.029353_2019_web_v20201114-3.xlsx"), 1) %>% clean_data()

df_2018_4 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "viirs_global_flaring_d.7_slope_0.029353_2018_web.xlsx"), 4) %>% clean_data()
df_2018_5 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "viirs_global_flaring_d.7_slope_0.029353_2018_web.xlsx"), 5) %>% clean_data()
df_2018_6 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "viirs_global_flaring_d.7_slope_0.029353_2018_web.xlsx"), 6) %>% clean_data()

df_2017_1 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "viirs_global_flaring_d.7_slope_0.029353_2017_web_v1.xlsx"), 1) %>% clean_data()
df_2017_2 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "viirs_global_flaring_d.7_slope_0.029353_2017_web_v1.xlsx"), 2) %>% clean_data()
df_2017_3 <- read_xlsx(file.path(gas_flare_dir, "rawdata", "viirs_global_flaring_d.7_slope_0.029353_2017_web_v1.xlsx"), 3) %>% clean_data()

gs_df <- bind_rows(
  df_2021,
  df_2020_1,
  df_2020_2,
  df_2020_3,
  df_2019,
  df_2018_4,
  df_2018_5,
  df_2018_6,
  df_2017_1,
  df_2017_2,
  df_2017_3
)

gs_df <- gs_df %>%
  dplyr::select(latitude, longitude, iso_code) %>%
  distinct() %>%
  dplyr::mutate(uid = 1:n())

gs_sf <- st_as_sf(gs_df, coords = c("longitude", "latitude"), crs = 4326)
#gs_5km_sf  <- gs_sf %>% st_buffer(dist = 5000)
#gs_10km_sf <- gs_sf %>% st_buffer(dist = 10000)

saveRDS(gs_sf, file.path(gas_flare_dir, "finaldata", "gas_flare_locations.Rds"))
#saveRDS(gs_5km_sf, file.path(gas_flare_dir, "finaldata", "gas_flare_locations_5km.Rds"))
#saveRDS(gs_10km_sf, file.path(gas_flare_dir, "finaldata", "gas_flare_locations_10km.Rds"))


