# Data Checks

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ADM 2 ========================================================================
adm_sf <- read_sf(file.path(wb_bound_dir, 
                            paste0("World Bank Official Boundaries - Admin ",2,".gpkg")))

# ADM2: Monthly -----------------------------------------------------------------
df <- read_parquet(file.path(agg_append_dir, paste0("ADM2", "_", "monthly", ".parquet")))

adm_sf[!(adm_sf$ADM2CD_c %in% df$ADM2CD_c),]
adm_sf[!(adm_sf$ADM2CD_c %in% df$ADM2CD_c),] %>%
  pull(ISO_A3) %>%
  table()

df %>%
  group_by(ISO_A3, NAM_0, NAM_1, ADM1CD_c, ADM2CD_c) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  dplyr::mutate(n_mode = get_mode(n)) %>%
  dplyr::filter(n != n_mode)

# ADM2: Annual -----------------------------------------------------------------
df <- read_parquet(file.path(agg_append_dir, paste0("ADM2", "_", "annual", ".parquet")))

adm_sf[!(adm_sf$ADM2CD_c %in% df$ADM2CD_c),]
adm_sf[!(adm_sf$ADM2CD_c %in% df$ADM2CD_c),] %>%
  pull(ISO_A3) %>%
  table()

df %>%
  group_by(ISO_A3, NAM_0, NAM_1, ADM1CD_c, ADM2CD_c) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  dplyr::mutate(n_mode = get_mode(n)) %>%
  dplyr::filter(n != n_mode)

# # ADM 0 ========================================================================
# adm_sf <- read_sf(file.path(wb_bound_dir, 
#                             paste0("World Bank Official Boundaries - Admin ",0,".gpkg")))
# 
# # ADM0: Monthly -----------------------------------------------------------------
# df <- read_parquet(file.path(agg_append_dir, paste0("ADM0", "_", "monthly", ".parquet")))
# 
# adm_sf[!(adm_sf$ISO_A3 %in% df$ISO_A3),]
# 
# df %>%
#   group_by(ISO_A3, NAM_0, GAUL_0) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   dplyr::mutate(n_mode = get_mode(n)) %>%
#   dplyr::filter(n != n_mode)
# 
# # ADM0: Annual -----------------------------------------------------------------
# df <- read_parquet(file.path(agg_append_dir, paste0("ADM0", "_", "annual", ".parquet")))
# 
# adm_sf[!(adm_sf$ISO_A3 %in% df$ISO_A3),]
# 
# df %>%
#   group_by(ISO_A3, NAM_0, GAUL_0) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   dplyr::mutate(n_mode = get_mode(n)) %>%
#   dplyr::filter(n != n_mode)
# 
# # ADM 1 ========================================================================
# adm_sf <- read_sf(file.path(wb_bound_dir, 
#                             paste0("World Bank Official Boundaries - Admin ",1,".gpkg")))
# 
# # ADM1: Monthly -----------------------------------------------------------------
# df <- read_parquet(file.path(agg_append_dir, paste0("ADM1", "_", "monthly", ".parquet")))
# 
# adm_sf[!(adm_sf$ADM1CD_c %in% df$ADM1CD_c),]
# adm_sf[!(adm_sf$ADM1CD_c %in% df$ADM1CD_c),] %>%
#   pull(ISO_A3) %>%
#   table()
# 
# df %>%
#   group_by(ISO_A3, NAM_0, NAM_1, ADM1CD_c) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   dplyr::mutate(n_mode = get_mode(n)) %>%
#   dplyr::filter(n != n_mode)
# 
# # ADM1: Annual -----------------------------------------------------------------
# df <- read_parquet(file.path(agg_append_dir, paste0("ADM1", "_", "annual", ".parquet")))
# 
# adm_sf[!(adm_sf$ADM1CD_c %in% df$ADM1CD_c),]
# adm_sf[!(adm_sf$ADM1CD_c %in% df$ADM1CD_c),] %>%
#   pull(ISO_A3) %>%
#   table()
# 
# df %>%
#   group_by(ISO_A3, NAM_0, NAM_1, ADM1CD_c) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   dplyr::mutate(n_mode = get_mode(n)) %>%
#   dplyr::filter(n != n_mode)
# 
# 
