# Aggregate Data

gf_sf <- readRDS(file.path(gasflare_dir, "finaldata", "gas_flare_locations.Rds"))

# Loop: ADM Level --------------------------------------------------------------
for(adm_level_i in 0:2){
  
  adm_sf <- read_sf(file.path(wb_bound_dir, 
                              paste0("World Bank Official Boundaries - Admin ",adm_level_i,".gpkg")))
  
  # Loop: Product --------------------------------------------------------------
  for(product_id_i in c("VNP46A3", "VNP46A4")){ 
    
    iso_all <- file.path(raster_ntl_root_dir, product_id_i) %>% 
      list.files()
    
    # Loop: ISO ----------------------------------------------------------------
    for(iso_i in iso_all){
      
      rasters_all <- file.path(raster_ntl_root_dir, product_id_i, iso_i) %>%
        list.files() 
      
      rasters_all <- rasters_all %>%
        sort() %>%
        head(120)
      
      roi_sf <- adm_sf[adm_sf$ISO_A3 %in% iso_i,]
      
      # Loop: Date -------------------------------------------------------------
      for(raster_i in rasters_all){
        
        # Prep date - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        if(product_id_i == "VNP46A3"){
          date_i <- str_extract(raster_i, "\\d{4}_\\d{2}.tif") %>%
            str_replace_all(".tif", "") %>%
            str_replace_all("_", "-") %>%
            paste0("-01")
        }
        
        if(product_id_i == "VNP46A4"){
          date_i <- str_extract(raster_i, "\\d{4}.tif") %>%
            str_replace_all(".tif", "") %>%
            as.numeric()
        }
        
        # Prep out file - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        adm_level_name_i <- paste0("ADM", adm_level_i)
        dir.create(file.path(agg_date_dir,  adm_level_name_i), showWarnings = F)
        dir.create(file.path(agg_date_dir,  adm_level_name_i, product_id_i), showWarnings = F)
        dir.create(file.path(agg_date_dir,  adm_level_name_i, product_id_i, iso_i), showWarnings = F)
        OUT_PATH <- file.path(agg_date_dir, adm_level_name_i, product_id_i, iso_i, paste0(date_i, ".Rds"))
        
        #### Check if file exists
        if(!file.exists(OUT_PATH)){
          
          message(paste0("Processing: ", adm_level_name_i, " - ", product_id_i, " - ", iso_i, " - ", date_i))
          
          # Prep raster - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          #### NTL
          r <- rast(file.path(raster_ntl_root_dir, product_id_i, iso_i, raster_i))
          
          #### Quality
          raster_qual_i <- raster_i %>%
            str_replace_all("Snow_Free", "Snow_Free_Quality")
          qual_r <- rast(file.path(raster_qual_root_dir, product_id_i, iso_i, raster_qual_i))
          
          #### Crop/Mask
          r      <- r      %>% crop(roi_sf) %>% mask(roi_sf)
          qual_r <- qual_r %>% crop(roi_sf) %>% mask(roi_sf)
          
          #### Prep Gas Flaring
          inter_tf <- st_intersects(roi_sf, 
                                    gf_sf, 
                                    sparse = F) %>% 
            apply(2, sum) %>%
            as.vector()
          gf_sf_i <- gf_sf[inter_tf > 0,]
          
          # Extract - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          METRICS <- c("sum", "mean", "median", "max", "quantile")
          QUANTILES <- c(0.05, 0.95)
          roi_df <- roi_sf %>% st_drop_geometry()
          
          ntl_df <- exact_extract(r, roi_sf, METRICS, quantiles = QUANTILES) %>%
            rename_with(~ paste0("ntl_", .x)) 
          
          # Extract Gas Flaring - - - - - - - - - - - - - - - - - - - - - - - - 
          if(nrow(gf_sf_i) > 0){
            
            ## Gas Flaring
            gf_sf_5km_i  <- gf_sf_i %>% st_buffer(dist = 5000)
            gf_sf_10km_i <- gf_sf_i %>% st_buffer(dist = 10000)
            
            ## Rasters
            r_gf_5km   <- r %>% mask(gf_sf_5km_i)
            r_nogf_5km <- r %>% mask(gf_sf_5km_i, inverse = T)
            
            r_gf_10km   <- r %>% mask(gf_sf_10km_i)
            r_nogf_10km <- r %>% mask(gf_sf_10km_i, inverse = T)
            
            ## Extract
            ntl_gf_5km_df <- exact_extract(r_gf_5km, roi_sf, METRICS, quantiles = QUANTILES) %>%
              rename_with(~ paste0("ntl_", .x)) 
            
            ntl_nogf_5km_df <- exact_extract(r_nogf_5km, roi_sf, METRICS, quantiles = QUANTILES) %>%
              rename_with(~ paste0("ntl_", .x)) 
            
            ntl_gf_10km_df <- exact_extract(r_gf_10km, roi_sf, METRICS, quantiles = QUANTILES) %>%
              rename_with(~ paste0("ntl_", .x)) 
            
            ntl_nogf_10km_df <- exact_extract(r_nogf_10km, roi_sf, METRICS, quantiles = QUANTILES) %>%
              rename_with(~ paste0("ntl_", .x)) 
            
          } else{
            
            #### No Gas Flares [Same as total NTL]
            ntl_nogf_5km_df  <- ntl_df
            ntl_nogf_10km_df <- ntl_df
            
            names(ntl_nogf_5km_df) <- names(ntl_nogf_5km_df) %>%
              str_replace_all("ntl_", "ntl_nogf_5km_")
            
            names(ntl_nogf_10km_df) <- names(ntl_nogf_10km_df) %>%
              str_replace_all("ntl_", "ntl_nogf_10km_")
            
            #### With Gas Flares
            ntl_gf_5km_df  <- ntl_df
            ntl_gf_10km_df <- ntl_df
            
            names(ntl_gf_5km_df) <- names(ntl_gf_5km_df) %>%
              str_replace_all("ntl_", "ntl_gf_5km_")
            
            names(ntl_gf_10km_df) <- names(ntl_gf_10km_df) %>%
              str_replace_all("ntl_", "ntl_gf_10km_")
            
            ntl_gf_5km_df[]  <- NA
            ntl_gf_10km_df[] <- NA
            
          }
          
          # Extract Quality - - - - - - - - - - - - - - - - - - - - - - - - - -
          ntl_quality_df <- exact_extract(qual_r, 
                                          roi_sf, 
                                          function(values, coverage_fraction) {
                                            # ntl_quality_prop_0_good <- sum( (values %in% 0) * coverage_fraction, na.rm = TRUE) /
                                            #   sum( !is.na(values) * coverage_fraction, na.rm = TRUE)
                                            # 
                                            # ntl_quality_prop_1_poor <- sum( (values %in% 1) * coverage_fraction, na.rm = TRUE) /
                                            #   sum( !is.na(values) * coverage_fraction, na.rm = TRUE)
                                            # 
                                            # ntl_quality_prop_2_gapfilled <- sum( (values %in% 2) * coverage_fraction, na.rm = TRUE) /
                                            #   sum( !is.na(values) * coverage_fraction, na.rm = TRUE)
                                            
                                            ntl_quality_prop_0_good <- sum( (values %in% 0) , na.rm = TRUE) /
                                              sum( length(values) , na.rm = TRUE)
                                            
                                            ntl_quality_prop_1_poor <- sum( (values %in% 1) , na.rm = TRUE) /
                                              sum( length(values) , na.rm = TRUE)
                                            
                                            ntl_quality_prop_2_gapfilled <- sum( (values %in% 2) , na.rm = TRUE) /
                                              sum( length(values) , na.rm = TRUE)
                                            
                                            data.frame(ntl_quality_prop_0_good, 
                                                       ntl_quality_prop_1_poor, 
                                                       ntl_quality_prop_2_gapfilled)
                                          })
          
          # Extract Proportion NA - - - - - - - - - - - - - - - - - - - - - - - 
          ntl_prop_na_df <- exact_extract(r, 
                                          roi_sf, 
                                          function(values, coverage_fraction) {
                                            # ntl_quality_prop_na <- sum( is.na(values) * coverage_fraction, na.rm = TRUE) /
                                            #   sum( !is.na(values) * coverage_fraction, na.rm = TRUE)
                                            
                                            ntl_quality_prop_na <- sum( is.na(values), na.rm = TRUE) /
                                              sum( length(values) , na.rm = TRUE)
                                            
                                            data.frame(ntl_quality_prop_na)
                                          })
          
          # Prep output - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          if(product_id_i == "VNP46A3"){
            date_i <- date_i %>% ymd()
          }
          
          roi_df$date <- date_i
          roi_df$n_gasflaring_locs <- nrow(gf_sf_i)
          
          roi_df <- bind_cols(roi_df, 
                              ntl_prop_na_df,
                              ntl_quality_df,
                              ntl_df,
                              ntl_gf_5km_df,
                              ntl_nogf_5km_df,
                              ntl_gf_10km_df,
                              ntl_nogf_10km_df)
          
          
          # Export - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          saveRDS(roi_df, OUT_PATH)
          
        }
      }
    }
  }
}


