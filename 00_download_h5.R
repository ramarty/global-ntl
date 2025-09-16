
bearer <- read_csv("~/Dropbox/bearer_bm.csv") %>%
  pull(token)

# Monthly ----------------------------------------------------------------------
product_id <- "VNP46A3"

months_vec <- seq.Date(from = ymd("2012-01-01"),
                       to = ymd("2025-07-01"),
                       by = "month") %>%
  as.character()

for(date_i in months_vec){
  message(date_i)
  
  dir.create(file.path(h5_root_dir, product_id, date_i))
  
  h5_dir <- file.path(h5_root_dir, product_id, date_i)
  
  # There are at most 540 files per date. If all downloaded, then skip
  n_h5_date <- h5_dir %>%
    list.files(pattern = "*.h5") %>%
    length()
  
  if(n_h5_date < 540){
    download_h5_files(roi_sf = NULL,
                      product_id = product_id,
                      date = date_i,
                      h5_dir = h5_dir, 
                      bearer = bearer,
                      download_method = "httr")
  }
  
}

# Annual -----------------------------------------------------------------------
product_id <- "VNP46A4"
for(date_i in 2012:2024){
  
  dir.create(file.path(h5_root_dir, product_id, date_i))
  
  h5_dir <- file.path(h5_root_dir, product_id, date_i)
  
  download_h5_files(roi_sf = NULL,
                    product_id = product_id,
                    date = date_i,
                    h5_dir = h5_dir, 
                    bearer = bearer,
                    download_method = "httr")
  
}




