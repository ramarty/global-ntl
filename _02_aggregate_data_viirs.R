# Aggregate Data

gadm_sf <- read_sf(file.path(db_dir, "data", "gadm", "gadm_410-levels.gpkg"),
                   "ADM_0")

gf_sf <- readRDS(file.path(gas_flare_dir, "finaldata", "gas_flare_locations.Rds"))

for(gid_i in gadm_sf$GID_0){
  dir.create(file.path(db_dir, "data", "ntl", "aggregated_data", gid_i))
  for(year_i in 2012:2024){
    
    OUT_PATH <- file.path(db_dir, "data", "ntl", "aggregated_data", gid_i,
                          paste0("ntl_", gid_i, "_", year_i, ".Rds"))
    
    if(!file.exists(OUT_PATH)){
      
      roi_sf <- gadm_sf[gadm_sf$GID_0 %in% gid_i,]
      roi_sf <- roi_sf %>% st_make_valid()
    
      if(year_i == 2024){
        r <- file.path(db_dir, "data", "ntl_global",
                       "VNL_npp_2024_global_vcmslcfg_v2_c202502261200.median.dat.tif") %>%
          rast()
      } else if(year_i == 2023){
        r <- file.path(db_dir, "data", "ntl_global",
                       "VNL_npp_2023_global_vcmslcfg_v2_c202402081600.median.dat.tif") %>%
          rast()
      } else if(year_i == 2022){
        r <- file.path(db_dir, "data", "ntl_global",
                       "VNL_v22_npp-j01_2022_global_vcmslcfg_c202303062300.median.dat.tif") %>%
          rast()
      } else if(year_i == 2012){
        r <- file.path(db_dir, "data", "ntl_global",
                       "VNL_v21_npp_201204-201303_global_vcmcfg_c202205302300.median.dat.tif") %>%
          rast()
      } else{
        r <- file.path(db_dir, "data", "ntl_global") %>%
          list.files(full.names = T) %>%
          str_subset(paste0("VNL_v21_npp_", year_i)) %>%
          rast()
      }
      
      r <- r %>%
        crop(roi_sf) %>%
        mask(roi_sf)
      
      inter_tf <- st_intersects(roi_sf, gf_sf, sparse = F) %>% as.vector()
      gf_sf_i <- gf_sf[inter_tf,]
      
      if(nrow(gf_sf_i) == 0){
        df_out <- data.frame(gid_0 = gid_i,
                             country = roi_sf$COUNTRY,
                             year = year_i,
                             n_gasflare_locations = nrow(gf_sf_i),
                             
                             ntl_viirs_sum    = r[] %>% sum(na.rm = T),
                             ntl_viirs_mean   = r[] %>% mean(na.rm = T),
                             ntl_viirs_median = r[] %>% median(na.rm = T),
                             ntl_viirs_max    = r[] %>% max(na.rm = T),
                             ntl_viirs_q95    = r[] %>% quantile(0.95, na.rm = T)) %>%
          dplyr::mutate(ntl_nogf_5km_viirs_sum    = ntl_viirs_sum,
                        ntl_nogf_5km_viirs_mean   = ntl_viirs_mean,
                        ntl_nogf_5km_viirs_median = ntl_viirs_median,
                        ntl_nogf_5km_viirs_max    = ntl_viirs_max,
                        ntl_nogf_5km_viirs_q95    = ntl_viirs_q95,
                        
                        ntl_nogf_10km_viirs_sum    = ntl_viirs_sum,
                        ntl_nogf_10km_viirs_mean   = ntl_viirs_mean,
                        ntl_nogf_10km_viirs_median = ntl_viirs_median,
                        ntl_nogf_10km_viirs_max    = ntl_viirs_max,
                        ntl_nogf_10km_viirs_q95    = ntl_viirs_q95)
      } else{
        
        ## Gas Flaring
        gf_sf_5km_i  <- gf_sf_i %>% st_buffer(dist = 5000)
        gf_sf_10km_i <- gf_sf_i %>% st_buffer(dist = 10000)
        
        ## Rasters
        r_gf_5km <- r %>% mask(gf_sf_5km_i)
        r_nogf_5km <- r %>% mask(gf_sf_5km_i, inverse = T)
        
        r_gf_10km <- r %>% mask(gf_sf_10km_i)
        r_nogf_10km <- r %>% mask(gf_sf_10km_i, inverse = T)
        
        df_out <- data.frame(gid_0 = gid_i,
                             country = roi_sf$COUNTRY,
                             year = year_i,
                             n_gasflare_locations = nrow(gf_sf_i),
                             
                             ntl_viirs_sum    = r[] %>% sum(na.rm = T),
                             ntl_viirs_mean   = r[] %>% mean(na.rm = T),
                             ntl_viirs_median = r[] %>% median(na.rm = T),
                             ntl_viirs_max    = r[] %>% max(na.rm = T),
                             ntl_viirs_q95    = r[] %>% quantile(0.95, na.rm = T),
                             
                             ntl_nogf_5km_viirs_sum    = r_nogf_5km[] %>% sum(na.rm = T),
                             ntl_nogf_5km_viirs_mean   = r_nogf_5km[] %>% mean(na.rm = T),
                             ntl_nogf_5km_viirs_median = r_nogf_5km[] %>% median(na.rm = T),
                             ntl_nogf_5km_viirs_max    = r_nogf_5km[] %>% max(na.rm = T),
                             ntl_nogf_5km_viirs_q95    = r_nogf_5km[] %>% quantile(0.95, na.rm = T),
                             
                             ntl_nogf_10km_viirs_sum    = r_nogf_10km[] %>% sum(na.rm = T),
                             ntl_nogf_10km_viirs_mean   = r_nogf_10km[] %>% mean(na.rm = T),
                             ntl_nogf_10km_viirs_median = r_nogf_10km[] %>% median(na.rm = T),
                             ntl_nogf_10km_viirs_max    = r_nogf_10km[] %>% max(na.rm = T),
                             ntl_nogf_10km_viirs_q95    = r_nogf_10km[] %>% quantile(0.95, na.rm = T),
                             
                             ntl_gf_5km_viirs_sum    = r_gf_5km[] %>% sum(na.rm = T),
                             ntl_gf_5km_viirs_mean   = r_gf_5km[] %>% mean(na.rm = T),
                             ntl_gf_5km_viirs_median = r_gf_5km[] %>% median(na.rm = T),
                             ntl_gf_5km_viirs_max    = r_gf_5km[] %>% max(na.rm = T),
                             ntl_gf_5km_viirs_q95    = r_gf_5km[] %>% quantile(0.95, na.rm = T),
                             
                             ntl_gf_10km_viirs_sum    = r_gf_10km[] %>% sum(na.rm = T),
                             ntl_gf_10km_viirs_mean   = r_gf_10km[] %>% mean(na.rm = T),
                             ntl_gf_10km_viirs_median = r_gf_10km[] %>% median(na.rm = T),
                             ntl_gf_10km_viirs_max    = r_gf_10km[] %>% max(na.rm = T),
                             ntl_gf_10km_viirs_q95    = r_gf_10km[] %>% quantile(0.95, na.rm = T))
        
      }
      
      saveRDS(df_out, OUT_PATH)
      
    }
  }
}