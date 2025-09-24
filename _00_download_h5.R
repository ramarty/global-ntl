
bearer <- read_csv("~/Dropbox/bearer_bm.csv") %>%
  pull(token)

world_sf <- read_sf(file.path(wb_bound_dir, "World Bank Official Boundaries - Admin 0.gpkg"))
world_sf <- world_sf %>% st_make_valid() %>% st_union() %>% st_make_valid() %>% st_as_sf()

# Only keep certain tiles ------------------------------------------------------
tiles_to_keep <- file.path(h5_root_dir, "VNP46A4", 2023) %>%
  list.files() %>%
  str_replace_all("VNP46A4.A2023001.", "") %>%
  str_replace_all("\\.002\\..*$", "") %>%
  paste(collapse = "|")

h5_files <- h5_root_dir %>%
  list.files(recursive = T,
             full.names = T,
             pattern = ".h5")

h5_keep_tf  <- h5_files %>% str_detect(tiles_to_keep)
h5_files_rm <- h5_files[!h5_keep_tf]

for(file_i in h5_files_rm){
  file.remove(file_i)
}

# Delete if file size is 0 -----------------------------------------------------
h5_files <- h5_root_dir %>%
  list.files(recursive = T,
             full.names = T,
             pattern = ".h5")

for(file_i in h5_files){
  
  if(file.size(file_i) == 0){
    file.remove(file_i)
  }
  
}

# Annual -----------------------------------------------------------------------
sf_use_s2(FALSE)

product_id <- "VNP46A4"

for(date_i in 2012:2024){
  
  dir.create(file.path(h5_root_dir, product_id, date_i))
  
  h5_dir <- file.path(h5_root_dir, product_id, date_i)
  
  download_h5_files(roi_sf = world_sf,
                    product_id = product_id,
                    date = date_i,
                    h5_dir = h5_dir, 
                    bearer = bearer,
                    download_method = "httr")
  
}

sf_use_s2(TRUE)

# Monthly ----------------------------------------------------------------------
sf_use_s2(FALSE)

product_id <- "VNP46A3"

months_vec <- seq.Date(from = ymd("2012-01-01"),
                       to = ymd("2025-07-01"),
                       by = "month") %>%
  as.character()

for(date_i in months_vec){
  message(date_i)
  
  dir.create(file.path(h5_root_dir, product_id, date_i))
  
  h5_dir <- file.path(h5_root_dir, product_id, date_i)
  
  # There are at most 351 files per date that intersect with world_sf. If all downloaded, then skip
  n_h5_date <- h5_dir %>%
    list.files(pattern = "*.h5") %>%
    length()
  
  if(n_h5_date < 351){
    download_h5_files(roi_sf = world_sf,
                      product_id = product_id,
                      date = date_i,
                      h5_dir = h5_dir, 
                      bearer = bearer,
                      download_method = "httr")
  }
  
}

sf_use_s2(TRUE)



