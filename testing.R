
bearer <- read_csv("~/Dropbox/bearer_bm.csv") %>%
  pull(token)

# Aggregate Data
adm0_sf <- read_sf(file.path(wb_bound_dir, "World Bank Official Boundaries - Admin 1.gpkg"))
roi_sf <- adm0_sf[adm0_sf$ISO_A3 == "PAK",]

product_id_i <- "VNP46A4"
date_i <- "2012"
h5_out_dir <- file.path(h5_root_dir, product_id_i, date_i)

ntl_r <- bm_raster(
  roi_sf = roi_sf,
  product_id = product_id_i,
  date = date_i,
  bearer = bearer,
  #output_location_type = "file",
  #file_dir = raster_out_dir,
  h5_dir = h5_out_dir,
  check_all_tiles_exist = FALSE,
  variable = "NearNadir_Composite_Snow_Free"
)

quality_r <- bm_raster(
  roi_sf = roi_sf,
  product_id = product_id_i,
  date = date_i,
  bearer = bearer,
  #output_location_type = "file",
  #file_dir = raster_out_dir,
  h5_dir = h5_out_dir,
  check_all_tiles_exist = FALSE,
  variable = "NearNadir_Composite_Snow_Free_Quality"
)

ntl_prop_na_df <- exact_extract(ntl_r, 
                                roi_sf, 
                                function(values, coverage_fraction) {
                                  ntl_quality_prop_na <- sum( is.na(values) * coverage_fraction, na.rm = TRUE) /
                                    sum( !is.na(values) * coverage_fraction, na.rm = TRUE)
                                  
                                  data.frame(ntl_quality_prop_na)
                                })



exact_extract(quality_r, 
              roi_sf, 
              function(values, coverage_fraction) {
                prop_0 <- sum( (values==0) * coverage_fraction, na.rm = TRUE) /
                  sum( !is.na(values) * coverage_fraction, na.rm = TRUE)
                
                prop_1 <- sum( (values==1) * coverage_fraction, na.rm = TRUE) /
                  sum( !is.na(values) * coverage_fraction, na.rm = TRUE)
                
                prop_2 <- sum( (values==2) * coverage_fraction, na.rm = TRUE) /
                  sum( !is.na(values) * coverage_fraction, na.rm = TRUE)
                
                data.frame(prop_0, prop_1, prop_2)
              })

prop_1 <- exact_extract(
  quality_r,
  roi_sf,
  function(df, ...) {
    sum(df$coverage_fraction[df$value == 1], na.rm = TRUE) /
      sum(df$coverage_fraction, na.rm = TRUE)
  }
)




exact_extract(quality_r, roi_sf)

plot(ntl_r)
plot(quality_r)
table( is.na(r[]) )
