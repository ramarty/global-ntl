
sf_use_s2(TRUE)

bearer <- read_csv("~/Dropbox/bearer_bm.csv") %>%
  pull(token)

# Aggregate Data
adm0_sf <- read_sf(file.path(wb_bound_dir, "World Bank Official Boundaries - Admin 0.gpkg"))

#### Loop: Country
for(uid_i in sort(unique(adm0_sf$ISO_A3))){
  
  #### Loop: Product
  for(product_id_i in c("VNP46A3", "VNP46A4")){ # "VNP46A3"
    
    if(product_id_i == "VNP46A3") date_vec <- seq.Date(from = ymd("2012-01-01"),
                                                       to = ymd("2025-07-01"),
                                                       by = "month") %>%
        as.character()
    if(product_id_i == "VNP46A4") date_vec <- 2012:2024
    
    #### Loop: Date
    for(date_i in date_vec){
      
      #### H5 Directory
      dir.create(file.path(h5_root_dir, product_id_i))
      dir.create(file.path(h5_root_dir, product_id_i, date_i))
      
      h5_out_dir <- file.path(h5_root_dir, product_id_i, date_i)
      
      #### Raster Directory
      dir.create(file.path(raster_root_dir, product_id_i))
      dir.create(file.path(raster_root_dir, product_id_i, uid_i))

      raster_out_dir <- file.path(raster_root_dir, product_id_i, uid_i)
      
      #### Query Data
      print(uid_i)
      
      roi_sf <- adm0_sf[adm0_sf$ISO_A3 %in% uid_i,]
      
      if(uid_i == "USA"){
        sf_use_s2(FALSE)
      } else{
        sf_use_s2(TRUE)
      }
      
      r_tmp <- bm_raster(roi_sf = roi_sf,
                         product_id = product_id_i,
                         date = date_i,
                         bearer = bearer,
                         output_location_type = "file",
                         file_dir = raster_out_dir,
                         h5_dir = h5_out_dir,
                         check_all_tiles_exist = F)
      
      # showConnections(all = TRUE)
      # closeAllConnections()
      
      ##
    }
  }
}
