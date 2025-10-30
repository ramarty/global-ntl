# Make Rasters

# --------------------------------------------------------------------
# SETUP
# --------------------------------------------------------------------
sf_use_s2(TRUE)

bearer <- read_csv("~/Dropbox/bearer_bm.csv") %>%
  pull(token)

adm0_sf <- read_sf(file.path(wb_bound_dir, "World Bank Official Boundaries - Admin 0.gpkg")) %>%
  filter(!(ISO_A3 %in% c("TKL", "TON", "WLF", "WSM")))

world_union_sf <- read_sf(file.path(wb_bound_dir, "adm0_union",
                                    "World Bank Official Boundaries - Admin 0 - Union.gpkg"))

h5_dir <- file.path(data_dir, "blackmarble", "h5_files_temp")

# --------------------------------------------------------------------
# PARALLEL SETTINGS
# --------------------------------------------------------------------
plan(multisession, workers = 8)  # Adjust workers based on CPU cores
options(future.globals.maxSize = 6 * 1024^3)  # 5 GB limit per worker

# --------------------------------------------------------------------
# MAIN LOOP
# --------------------------------------------------------------------
counter <- 0
for (product_id_i in c("VNP46A3", "VNP46A4")) {
  
  if (product_id_i == "VNP46A4") {
    date_vec <- 2012:2024
  }
  
  if (product_id_i == "VNP46A3") {
    date_vec <- seq.Date(
      from = ymd("2015-09-01"),
      to = ymd("2025-07-01"),
      by = "month"
    ) %>% 
      as.character() %>%
      rev()
  }
  
  for (date_i in date_vec) {
    
    h5_temp_dir <- file.path(h5_dir, paste0(product_id_i, " - ", date_i))
    
    # Download H5 files once per date/product
    h5_files <- list.files(h5_temp_dir, full.names = TRUE)
    raster_files <- list.files(file.path(raster_ntl_root_dir, product_id_i),
                               recursive = TRUE) %>% str_subset(date_i)
    
    if ((length(h5_files) < 330) & (length(raster_files) < length(unique(adm0_sf$ISO_A3)))) {
      message("Downloading h5 files for ", product_id_i, " ", date_i)
      dir.create(h5_temp_dir, showWarnings = FALSE, recursive = TRUE)
      download_h5_files(
        roi_sf = world_union_sf,
        product_id = product_id_i,
        date = date_i,
        h5_dir = h5_temp_dir,
        bearer = bearer,
        download_method = "httr"
      )
    }
    
    # Parallel country processing ---------------------------------------
    uids <- sort(unique(adm0_sf$ISO_A3))
    
    future_lapply(uids, function(uid_i) {
      t1 <- Sys.time()
      
      raster_ntl_out_dir <- file.path(raster_ntl_root_dir, product_id_i, uid_i)
      raster_qual_out_dir <- file.path(raster_qual_root_dir, product_id_i, uid_i)
      dir.create(raster_ntl_out_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(raster_qual_out_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Date naming
      date_clean_i <- if (product_id_i == "VNP46A3") {
        substring(date_i, 1, 7) %>% str_replace_all("-", "_")
      } else {
        as.character(date_i)
      }
      
      n_files <- list.files(raster_qual_out_dir) %>%
        str_subset(date_clean_i) %>%
        length()
      
      if (n_files == 0) {
        message(paste0("Processing: ", product_id_i, " ", uid_i, " ", date_i))
        
        roi_sf <- adm0_sf[adm0_sf$ISO_A3 %in% uid_i, ]
        
        if (uid_i == "USA") sf_use_s2(FALSE) else sf_use_s2(TRUE)
        
        # --- NTL ---
        callr::r_safe(
          function(roi_sf, product_id_i, date_i, bearer, raster_ntl_out_dir, h5_temp_dir) {
            library(blackmarbler); library(sf); library(terra)
            tryCatch({
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
            }, error = function(e) {
              message("Error (NTL): ", e$message)
              closeAllConnections(); gc(FALSE); terra::tmpFiles(remove = TRUE)
              NULL
            })
          },
          args = list(
            roi_sf = roi_sf,
            product_id_i = product_id_i,
            date_i = date_i,
            bearer = bearer,
            raster_ntl_out_dir = raster_ntl_out_dir,
            h5_temp_dir = h5_temp_dir
          )
        )
        
        # --- QUALITY ---
        callr::r_safe(
          function(roi_sf, product_id_i, date_i, bearer, raster_qual_out_dir, h5_temp_dir) {
            library(blackmarbler); library(sf); library(terra)
            tryCatch({
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
            }, error = function(e) {
              message("Error (Quality): ", e$message)
              closeAllConnections(); gc(FALSE); terra::tmpFiles(remove = TRUE)
              NULL
            })
          },
          args = list(
            roi_sf = roi_sf,
            product_id_i = product_id_i,
            date_i = date_i,
            bearer = bearer,
            raster_qual_out_dir = raster_qual_out_dir,
            h5_temp_dir = h5_temp_dir
          )
        )
      }
      
      t2 <- Sys.time()
      message(paste0(uid_i, " completed in ", round(difftime(t2, t1, units = "mins"), 2), " min"))
      invisible(NULL)
    })
    
    # ----------------------------------------------------------------
    # Post-loop cleanup
    # ----------------------------------------------------------------
    raster_files <- list.files(file.path(raster_ntl_root_dir, product_id_i),
                               recursive = TRUE) %>% str_subset(date_i)
    h5_files <- list.files(h5_temp_dir, full.names = TRUE)
    
    if (length(raster_files) == length(unique(adm0_sf$ISO_A3))) {
      unlink(h5_temp_dir, recursive = TRUE, force = TRUE)
    }
    
    gc(FALSE); closeAllConnections(); terra::tmpFiles(remove = TRUE)
  }
}
