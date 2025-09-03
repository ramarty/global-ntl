# Aggregate Data

gadm_sf <- read_sf(file.path(db_dir, "data", "gadm", "gadm_410-levels.gpkg"),
                   "ADM_0")

gf_sf <- readRDS(file.path(gas_flare_dir, "finaldata", "gas_flare_locations.Rds"))

gid_all <- file.path(db_dir, "data", "ntl", "bm_ntl_rasters") %>% 
  list.files()

for(gid_i in gid_all){
  dir.create(file.path(db_dir, "data", "ntl", "aggregated_data_bm", gid_i))
  for(year_i in 2012:2023){
    
    OUT_PATH <- file.path(db_dir, "data", "ntl", "aggregated_data_bm", gid_i,
                          paste0("ntl_", gid_i, "_", year_i, ".Rds"))
    
    if(!file.exists(OUT_PATH)){
      
      roi_sf <- gadm_sf[gadm_sf$GID_0 %in% gid_i,]
      roi_sf <- roi_sf %>% st_make_valid()
      
      RASTER_PATH <- file.path(db_dir, "data", "ntl", "bm_ntl_rasters", gid_i, 
                               paste0("VNP46A4_NearNadir_Composite_Snow_Free_qflag_t",year_i,".tif"))
      
      if(file.exists(RASTER_PATH)){
        
        r <- rast(RASTER_PATH)
        r <- r %>% crop(roi_sf) %>% mask(roi_sf)
        
        inter_tf <- st_intersects(roi_sf, gf_sf, sparse = F) %>% as.vector()
        gf_sf_i <- gf_sf[inter_tf,]
        
        if(nrow(gf_sf_i) == 0){
          df_out <- data.frame(gid_0 = gid_i,
                               country = roi_sf$COUNTRY,
                               year = year_i,
                               n_gasflare_locations = nrow(gf_sf_i),
                               
                               ntl_bm_sum    = r[] %>% sum(na.rm = T),
                               ntl_bm_mean   = r[] %>% mean(na.rm = T),
                               ntl_bm_median = r[] %>% median(na.rm = T),
                               ntl_bm_max    = r[] %>% max(na.rm = T),
                               ntl_bm_q95    = r[] %>% quantile(0.95, na.rm = T)) %>%
            dplyr::mutate(ntl_nogf_5km_bm_sum    = ntl_bm_sum,
                          ntl_nogf_5km_bm_mean   = ntl_bm_mean,
                          ntl_nogf_5km_bm_median = ntl_bm_median,
                          ntl_nogf_5km_bm_max    = ntl_bm_max,
                          ntl_nogf_5km_bm_q95    = ntl_bm_q95,
                          
                          ntl_nogf_10km_bm_sum    = ntl_bm_sum,
                          ntl_nogf_10km_bm_mean   = ntl_bm_mean,
                          ntl_nogf_10km_bm_median = ntl_bm_median,
                          ntl_nogf_10km_bm_max    = ntl_bm_max,
                          ntl_nogf_10km_bm_q95    = ntl_bm_q95)
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
                               
                               ntl_bm_sum    = r[] %>% sum(na.rm = T),
                               ntl_bm_mean   = r[] %>% mean(na.rm = T),
                               ntl_bm_median = r[] %>% median(na.rm = T),
                               ntl_bm_max    = r[] %>% max(na.rm = T),
                               ntl_bm_q95    = r[] %>% quantile(0.95, na.rm = T),
                               
                               ntl_nogf_5km_bm_sum    = r_nogf_5km[] %>% sum(na.rm = T),
                               ntl_nogf_5km_bm_mean   = r_nogf_5km[] %>% mean(na.rm = T),
                               ntl_nogf_5km_bm_median = r_nogf_5km[] %>% median(na.rm = T),
                               ntl_nogf_5km_bm_max    = r_nogf_5km[] %>% max(na.rm = T),
                               ntl_nogf_5km_bm_q95    = r_nogf_5km[] %>% quantile(0.95, na.rm = T),
                               
                               ntl_nogf_10km_bm_sum    = r_nogf_10km[] %>% sum(na.rm = T),
                               ntl_nogf_10km_bm_mean   = r_nogf_10km[] %>% mean(na.rm = T),
                               ntl_nogf_10km_bm_median = r_nogf_10km[] %>% median(na.rm = T),
                               ntl_nogf_10km_bm_max    = r_nogf_10km[] %>% max(na.rm = T),
                               ntl_nogf_10km_bm_q95    = r_nogf_10km[] %>% quantile(0.95, na.rm = T),
                               
                               ntl_gf_5km_bm_sum    = r_gf_5km[] %>% sum(na.rm = T),
                               ntl_gf_5km_bm_mean   = r_gf_5km[] %>% mean(na.rm = T),
                               ntl_gf_5km_bm_median = r_gf_5km[] %>% median(na.rm = T),
                               ntl_gf_5km_bm_max    = r_gf_5km[] %>% max(na.rm = T),
                               ntl_gf_5km_bm_q95    = r_gf_5km[] %>% quantile(0.95, na.rm = T),
                               
                               ntl_gf_10km_bm_sum    = r_gf_10km[] %>% sum(na.rm = T),
                               ntl_gf_10km_bm_mean   = r_gf_10km[] %>% mean(na.rm = T),
                               ntl_gf_10km_bm_median = r_gf_10km[] %>% median(na.rm = T),
                               ntl_gf_10km_bm_max    = r_gf_10km[] %>% max(na.rm = T),
                               ntl_gf_10km_bm_q95    = r_gf_10km[] %>% quantile(0.95, na.rm = T))
          
        }
        
        saveRDS(df_out, OUT_PATH)
        
      }
    }
  }
}