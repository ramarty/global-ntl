
bearer <- read_csv("~/Dropbox/bearer_bm.csv") %>%
  pull(token)

product_id_i <- "VNP46A4"
date_i <- 2024

dir.create(file.path(h5_root_dir, product_id_i))
dir.create(file.path(h5_root_dir, product_id_i, date_i))

h5_out_dir <- file.path(h5_root_dir, product_id_i, date_i)

wget_h5_files(roi_sf = NULL, 
              product_id = "VNP46A4", 
              date = 2024, 
              h5_dir = h5_out_dir, 
              bearer = bearer)
