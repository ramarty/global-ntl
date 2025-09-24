
sf_use_s2(TRUE)

bearer <- read_csv("~/Dropbox/bearer_bm.csv") %>%
  pull(token)

# Aggregate Data
adm0_sf <- read_sf(file.path(wb_bound_dir, "World Bank Official Boundaries - Admin 0.gpkg"))
world_union_sf <- read_sf(file.path(wb_bound_dir, "adm0_union", "World Bank Official Boundaries - Admin 0 - Union.gpkg"))

adm0_sf <- adm0_sf %>%
  dplyr::filter(!(ISO_A3 %in% c("TKL",
                                "TON",
                                "WLF",
                                "WSM")))



h5_dir <- file.path(data_dir, "blackmarble", "h5_files_temp")

#### Loop: Product
for(product_id_i in c("VNP46A3", "VNP46A4")){ 
  
  date_vec <- file.path(h5_root_dir, product_id_i) %>%
    list.files()
  
  #### Loop: Date
  for(date_i in date_vec){
    
    h5_temp_dir <- file.path(h5_dir, paste0(product_id_i, " - ", date_i))
    
    #### Loop: Country
    for(uid_i in sort(unique(adm0_sf$ISO_A3))){
      
      t1 <- Sys.time()
      
      #### H5 Directory
      # dir.create(file.path(h5_root_dir, product_id_i))
      # dir.create(file.path(h5_root_dir, product_id_i, date_i))
      
      h5_out_dir <- file.path(h5_root_dir, product_id_i, date_i)
      
      #### Raster Directory
      dir.create(file.path(raster_ntl_root_dir, product_id_i))
      dir.create(file.path(raster_ntl_root_dir, product_id_i, uid_i))
      raster_ntl_out_dir <- file.path(raster_ntl_root_dir, product_id_i, uid_i)
      
      dir.create(file.path(raster_qual_root_dir, product_id_i))
      dir.create(file.path(raster_qual_root_dir, product_id_i, uid_i))
      raster_qual_out_dir <- file.path(raster_qual_root_dir, product_id_i, uid_i)
      
      #### Check if file already exists; skip if exists
      if(product_id_i == "VNP46A3"){
        date_clean_i <- date_i %>% substring(1, 7) %>% str_replace_all("-", "_")
      }
      if(product_id_i == "VNP46A4"){
        date_clean_i <- date_i %>% as.character()
      }
      
      n_files <- raster_qual_out_dir %>%
        list.files() %>%
        str_subset(date_clean_i) %>%
        length()
      
      if(n_files == 0){
        
        # Download h5 files ----------------------------------------------------
        h5_files <- h5_temp_dir %>%
          list.files(full.names = T)
        
        raster_files <- file.path(raster_ntl_root_dir, product_id_i) %>%
          list.files(recursive = T) %>%
          str_subset(date_clean_i)
        
        if( (length(h5_files) < 340) & (length(raster_files) < length(unique(adm0_sf$ISO_A3)) ) ){
          
          dir.create(h5_temp_dir)
          download_h5_files(roi_sf = world_sf,
                            product_id = product_id_i,
                            date = date_i,
                            h5_dir = h5_temp_dir, 
                            bearer = bearer,
                            download_method = "httr")
        }
        
        # Query Data -----------------------------------------------------------
        message(paste0("Processing: ", product_id_i, " ", uid_i, " ", date_i))
        
        roi_sf <- adm0_sf[adm0_sf$ISO_A3 %in% uid_i,]
        
        if(uid_i == "USA"){
          sf_use_s2(FALSE)
        } else{
          sf_use_s2(TRUE)
        }
        
        #### Extract NTL
        r_tmp <- tryCatch(
          {
            bm_raster(
              roi_sf = roi_sf,
              product_id = product_id_i,
              date = date_i,
              bearer = bearer,
              output_location_type = "file",
              file_dir = raster_ntl_out_dir,
              h5_dir = h5_temp_dir,
              check_all_tiles_exist = FALSE,
              variable = "NearNadir_Composite_Snow_Free"
            )
          },
          error = function(e) {
            message("Error in bm_raster: ", conditionMessage(e))
            return(NULL)  # or NA, or some other placeholder
          })
        
        #### Extract Quality
        r_tmp <- tryCatch(
          {
            bm_raster(
              roi_sf = roi_sf,
              product_id = product_id_i,
              date = date_i,
              bearer = bearer,
              output_location_type = "file",
              file_dir = raster_qual_out_dir,
              h5_dir = h5_temp_dir,
              check_all_tiles_exist = FALSE,
              variable = "NearNadir_Composite_Snow_Free_Quality"
            )
          },
          error = function(e) {
            message("Error in bm_raster: ", conditionMessage(e))
            return(NULL)  # or NA, or some other placeholder
          })
        
        t2 <- Sys.time()
        
        print(t2 - t1)
        
        # showConnections(all = TRUE)
        # closeAllConnections()
        
        ##
      }
      
      
    } # loop: uid
    
    ## Delete h5 files if all raster files have been made
    raster_files <- file.path(raster_ntl_root_dir, product_id_i) %>%
      list.files(recursive = T) %>%
      str_subset(date_clean_i)
    
    ## h5 files
    h5_files <- h5_temp_dir %>%
      list.files(full.names = T)
    
    if(length(raster_files) == length(unique(adm0_sf$ISO_A3)) ){
      for(h5_files_i in h5_files) file.remove(h5_files_i)
      unlink(h5_temp_dir)
    }
    
  } # loop: date
} # loop: product
