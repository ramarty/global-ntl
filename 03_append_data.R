# Append Data

viirs_df <- file.path(db_dir, "data", "ntl", "aggregated_data") %>%
  list.files(pattern = ".Rds",
             recursive = T,
             full.names = T) %>%
  map_df(readRDS)

bm_df <- file.path(db_dir, "data", "ntl", "aggregated_data_bm") %>%
  list.files(pattern = ".Rds",
             recursive = T,
             full.names = T) %>%
  map_df(readRDS)

dmsp_df <- file.path(db_dir, "data", "ntl", "aggregated_data_dmsp") %>%
  list.files(pattern = ".Rds",
             recursive = T,
             full.names = T) %>%
  map_df(readRDS)

ntl_df <- viirs_df %>%
  full_join(dmsp_df, by = c("gid_0", "country", "year", "n_gasflare_locations")) %>%
  full_join(bm_df, by = c("gid_0", "country", "year", "n_gasflare_locations"))

## Add continent name
# ntl_df <- ntl_df %>%
#   dplyr::mutate(country = case_when(
#     country == "México" ~ "Mexico",
#     TRUE ~ country
#   ))

ntl_df$continent <- countrycode(sourcevar = ntl_df$gid_0,
                                origin = "iso3c",
                                destination = "continent")

saveRDS(ntl_df, file.path(db_dir, "data", "ntl", "global_annual_ntl.Rds"))
write_dta(ntl_df, file.path(db_dir, "data", "ntl", "global_annual_ntl.dta"))
