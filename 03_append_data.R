# Append Data

product_id_i <- "VNP46A4"
adm_level_name_i <- "ADM2"

for(product_id_i in c("VNP46A3", "VNP46A4")){ # , 
  for(adm_level_name_i in c("ADM2")){
    
    message(paste0("Appending: ", adm_level_name_i, " - ", product_id_i))
    
    #### Append data
    df <- file.path(agg_date_dir, adm_level_name_i, product_id_i) %>%
      list.files(full.names = T,
                 pattern = "*.Rds",
                 recursive = T) %>%
      map_df(readRDS)
    
    #### File name
    if(product_id_i == "VNP46A3") time_name <- "monthly"
    if(product_id_i == "VNP46A4") time_name <- "annual"
    
    adm_level_name_lw_i <- adm_level_name_i %>% tolower()
    
    file_name <- paste0(adm_level_name_lw_i,
                        "_",
                        time_name)
    
    #### Rename variables
    if(product_id_i == "VNP46A4"){
      df <- df %>%
        dplyr::rename(year = date)
    }
    
    #### Export
    write_csv(df,     file.path(agg_append_dir, paste0(file_name, ".csv")))
    write_parquet(df, file.path(agg_append_dir, paste0(file_name, ".parquet")))
    write_dta(df,     file.path(agg_append_dir, paste0(file_name, ".dta")))
    
  }
}

