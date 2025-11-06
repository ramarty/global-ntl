# Download NTL Rasters
# Download data for (1) each country and (2) each date

#### PARAMETERS
download_all_h5 <- F
use_other_r_session <- F

#### SETUP

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
counter <- 0
for(product_id_i in c("VNP46A3", "VNP46A4")){ 
  
  if(product_id_i == "VNP46A4"){
    date_vec <- 2012:2024
  }
  
  if(product_id_i == "VNP46A3"){
    date_vec <- seq.Date(from = ymd("2012-01-01"), # ymd("2012-01-01")
                         to = ymd("2025-09-01"),
                         by = "month") %>%
      as.character()
  }
  
  #### Loop: Date
  for(date_i in date_vec){
    message(paste0("Checking: ", date_i))
    
    #### h5 directory
    h5_temp_dir <- file.path(h5_dir, paste0(product_id_i, " - ", date_i))
    
    #### Clean date name
    if(product_id_i == "VNP46A3"){
      date_clean_i <- date_i %>% substring(1, 7) %>% str_replace_all("-", "_")
    }
    if(product_id_i == "VNP46A4"){
      date_clean_i <- date_i %>% as.character()
    }
    
    # Check to download h5 files -----------------------------------------------
    # Based on (1) whether rasters have been downloaded and (2) whether
    # h5 files have been downloaded
    
    if(download_all_h5){
      
      #### Files that already exist
      h5_temp_n <- h5_temp_dir %>%
        list.files() %>%
        length()
      
      tif_files_n <- file.path(raster_ntl_root_dir, product_id_i) %>%
        list.files(recursive = T,
                   pattern = date_str_i) %>%
        length()
      
      ## Check of download
      if( 
        (tif_files_n <= 200) & 
        (h5_temp_n <= 200) 
      ){
        
        # Download h5 files ----------------------------------------------------
        message("Downloading h5 files")
        dir.create(h5_temp_dir, showWarnings = FALSE)
        download_h5_files(roi_sf = world_union_sf,
                          product_id = product_id_i,
                          date = date_i,
                          h5_dir = h5_temp_dir,
                          bearer = bearer,
                          download_method = "httr")
      }
    }
    
    #### Loop: Country
    
    ## Determine which countries to extract data from
    ## Check if data has been extracted yet
    
    ## NTL
    ntl_files_all <- file.path(raster_ntl_root_dir, product_id_i) %>%
      list.files(recursive = T,
                 pattern = paste0(date_clean_i, ".tif"))
    
    ntl_uids_with_data <- ntl_files_all %>%
      str_replace_all("/.*", "")
    
    if(length(ntl_uids_with_data) > 0){
      ntl_uids_no_data <- adm0_sf$ISO_A3[!(adm0_sf$ISO_A3 %in% ntl_uids_with_data)] %>%
        unique()
    } else{
      ntl_uids_no_data <- adm0_sf$ISO_A3 %>% unique()
    }
    
    ## Quality
    qual_files_all <- file.path(raster_qual_root_dir, product_id_i) %>%
      list.files(recursive = T,
                 pattern = paste0(date_clean_i, ".tif"))
    
    qual_uids_with_data <- qual_files_all %>%
      str_replace_all("/.*", "")
    
    if(length(qual_uids_with_data) > 0){
      qual_uids_no_data <- adm0_sf$ISO_A3[!(adm0_sf$ISO_A3 %in% qual_uids_with_data)] %>%
        unique()
    } else{
      qual_uids_no_data <- adm0_sf$ISO_A3 %>% unique()
    }
    
    ## Combine
    uids_no_data <- unique(c(ntl_uids_no_data, qual_uids_no_data)) %>%
      sort()
    
    for(uid_i in uids_no_data){ # sort(unique(adm0_sf$ISO_A3))
      
      counter <- counter + 1
      # Periodic cleanup every 10 countries
      if(counter %% 20 == 0){
        gc(verbose = FALSE)
        closeAllConnections()
        Sys.sleep(1)  # Brief pause to allow cleanup
        terra::tmpFiles(remove = TRUE)
      }
      
      t1 <- Sys.time()
      
      #### Raster Directory
      dir.create(file.path(raster_ntl_root_dir, product_id_i), showWarnings = FALSE)
      dir.create(file.path(raster_ntl_root_dir, product_id_i, uid_i), showWarnings = FALSE)
      raster_ntl_out_dir <- file.path(raster_ntl_root_dir, product_id_i, uid_i)
      
      dir.create(file.path(raster_qual_root_dir, product_id_i), showWarnings = FALSE)
      dir.create(file.path(raster_qual_root_dir, product_id_i, uid_i), showWarnings = FALSE)
      raster_qual_out_dir <- file.path(raster_qual_root_dir, product_id_i, uid_i)
      
      #### Extract data if doesn't exist ---------------------------------------
      ntl_tif_n <- raster_ntl_out_dir %>%
        list.files() %>%
        str_subset(date_clean_i) %>%
        length()
      
      qual_tif_n <- raster_qual_out_dir %>%
        list.files() %>%
        str_subset(date_clean_i) %>%
        length()
      
      message(paste0("Checking: ", product_id_i, " ", uid_i, " ", date_i))
      
      if( (ntl_tif_n == 0) | (qual_tif_n == 0) ){
        
        # Query Data -----------------------------------------------------------
        message(paste0("Processing: ", product_id_i, " ", uid_i, " ", date_i))
        
        roi_sf <- adm0_sf[adm0_sf$ISO_A3 %in% uid_i,]
        
        if(uid_i == "USA"){
          sf_use_s2(FALSE)
        } else{
          if(!sf_use_s2()) sf_use_s2(TRUE)
        }
        
        # Extract NTL ----------------------------------------------------------
        if(use_other_r_session %in% F){
          
          if(download_all_h5){
            h5_temp_use_dir <- h5_temp_dir
          } else{
            h5_temp_use_dir <- NULL
          }
          
          bm_raster(
            roi_sf = roi_sf,
            product_id = product_id_i,
            date = date_i,
            bearer = bearer,
            output_location_type = "file",
            file_dir = raster_ntl_out_dir,
            h5_dir = h5_temp_use_dir,
            check_all_tiles_exist = FALSE,
            variable = "NearNadir_Composite_Snow_Free"
          )
          
          bm_raster(
            roi_sf = roi_sf,
            product_id = product_id_i,
            date = date_i,
            bearer = bearer,
            output_location_type = "file",
            file_dir = raster_qual_out_dir,
            h5_dir = h5_temp_use_dir,
            check_all_tiles_exist = FALSE,
            variable = "NearNadir_Composite_Snow_Free_Quality"
          )
          
        } else{
          tryCatch({
            
            r_tmp <- callr::r_safe(
              function(roi_sf, product_id_i, date_i, bearer, raster_ntl_out_dir, h5_temp_dir, download_all_h5) {
                tryCatch({
                  library(blackmarbler)
                  library(sf)
                  library(terra)
                  
                  if(download_all_h5){
                    h5_temp_use_dir <- h5_temp_dir
                  } else{
                    h5_temp_use_dir <- NULL
                  }
                  
                  bm_raster(
                    roi_sf = roi_sf,
                    product_id = product_id_i,
                    date = date_i,
                    bearer = bearer,
                    output_location_type = "file",
                    file_dir = raster_ntl_out_dir,
                    h5_dir = h5_temp_use_dir,
                    check_all_tiles_exist = FALSE,
                    variable = "NearNadir_Composite_Snow_Free"
                  )
                  
                  invisible(NULL)
                }, error = function(e) {
                  message("Error in bm_raster (NTL): ", conditionMessage(e))
                  closeAllConnections()
                  gc(verbose = FALSE)
                  terra::tmpFiles(remove = TRUE)
                  NULL
                })
              },
              args = list(
                roi_sf = roi_sf,
                product_id_i = product_id_i,
                date_i = date_i,
                bearer = bearer,
                raster_ntl_out_dir = raster_ntl_out_dir,
                h5_temp_dir = h5_temp_dir,
                download_all_h5 = download_all_h5
              )
            )
            
          }, error = function(e) {
            message("Error: ", conditionMessage(e))
          })
          
          # Extract Quality ------------------------------------------------------
          tryCatch({
            
            r_tmp <- callr::r_safe(
              function(roi_sf, product_id_i, date_i, bearer, raster_qual_out_dir, h5_temp_dir, download_all_h5) {
                tryCatch({
                  library(blackmarbler)
                  library(sf)
                  library(terra)
                  
                  if(download_all_h5){
                    h5_temp_use_dir <- h5_temp_dir
                  } else{
                    h5_temp_use_dir <- NULL
                  }
                  
                  bm_raster(
                    roi_sf = roi_sf,
                    product_id = product_id_i,
                    date = date_i,
                    bearer = bearer,
                    output_location_type = "file",
                    file_dir = raster_qual_out_dir,
                    h5_dir = h5_temp_use_dir,
                    check_all_tiles_exist = FALSE,
                    variable = "NearNadir_Composite_Snow_Free_Quality"
                  )
                  
                  invisible(NULL)
                }, error = function(e) {
                  message("Error in bm_raster (Quality): ", conditionMessage(e))
                  closeAllConnections()
                  gc(verbose = FALSE)
                  terra::tmpFiles(remove = TRUE)
                  NULL
                })
              },
              args = list(
                roi_sf = roi_sf,
                product_id_i = product_id_i,
                date_i = date_i,
                bearer = bearer,
                raster_qual_out_dir = raster_qual_out_dir,
                h5_temp_dir = h5_temp_dir,
                download_all_h5 = download_all_h5
              )
            )
            
          }, error = function(e) {
            message("Error: ", conditionMessage(e))
          })
          
        }
        
        # Cleanup --------------------------------------------------------------
        t2 <- Sys.time()
        
        print(t2 - t1)
        ##
        
      } # Extract if file doesn't exist
      
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
      unlink(h5_temp_dir, recursive = TRUE, force = TRUE)
    }
    
  } # loop: date
} # loop: product
