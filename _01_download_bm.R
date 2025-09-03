
bearer <- read_csv("~/Dropbox/bearer_bm.csv") %>%
  pull(token)

# Aggregate Data
adm0_sf <- read_sf(file.path(wb_bound_dir, "World Bank Official Boundaries - Admin 0.gpkg"))

for(gid_i in gadm_sf$GID_0){
  
  if(gid_i %in% c("ATA", "CAN")) next
  
  print(gid_i)
  
  tryCatch({
    dir.create(file.path(db_dir, "data", "ntl", "bm_ntl_rasters", gid_i))
    
    n_files <- file.path(db_dir, "data", "ntl", "bm_ntl_rasters", gid_i) %>%
      list.files() %>%
      length()
    
    if(n_files < 12){
      
      roi_sf <- gadm_sf[gadm_sf$GID_0 %in% gid_i,]
      roi_sf <- roi_sf %>% st_make_valid()
      
      r_tmp <- bm_raster(roi_sf = roi_sf,
                         product_id = "VNP46A4",
                         date = 2012:2023,
                         bearer = bearer,
                         output_location_type = "file",
                         file_dir = file.path(db_dir, "data", "ntl", "bm_ntl_rasters", gid_i),
                         h5_dir = file.path(db_dir, "data", "ntl", "bm_ntl_h5"),
                         check_all_tiles_exist = F)
    }
  }, error = function(e) {
    message(paste("Error with GID:", gid_i, ":", e$message))
  }, finally = {
    message(paste("Finished processing GID:", gid_i))
  })
  
  
}


