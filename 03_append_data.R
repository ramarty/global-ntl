# Append Data

product_id_i <- "VNP46A3"
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
    
    #### Limit ADM variables
    df <- df %>%
      dplyr::select(-c(ISO_A2, WB_A3, WB_REGION, WB_STATUS, GEOM_SRCE)) %>%
      dplyr::select(ISO_A3, ADM1CD_c, ADM2CD_c, 
                    NAM_0, NAM_1, NAM_2,
                    everything())
    
    #### Add variable labels for dta
    df_dta <- df
    attr(df_dta$n_gasflaring_locs, "label")            <- "Number of gas flaring locations in the ADM unit"
    attr(df_dta$ntl_quality_prop_na, "label")          <- "Proportion of nighttime light pixels NA (missing)"
    attr(df_dta$ntl_quality_prop_0_good, "label")      <- "Proportion of nighttime light pixels with a quality value of 0 (“Good”)."
    attr(df_dta$ntl_quality_prop_1_poor, "label")      <- "Proportion of nighttime light pixels with a quality value of 1 (“Poor”)."
    attr(df_dta$ntl_quality_prop_2_gapfilled, "label") <- "Proportion of nighttime light pixels with a quality value of 2 (“Gap filled based on historic data”)."
    
    ntl_vars <- names(df_dta) %>%
      str_subset("^ntl_")
    
    ntl_vars <- ntl_vars[!(ntl_vars %>% str_detect("ntl_quality"))]
    
    for(var_i in ntl_vars){
      
      gf_text <- ""
      if(var_i %>% str_detect("_gf_5km"))  gf_text <- " only considering locations within 5km of a gas flare site"
      if(var_i %>% str_detect("_gf_10km")) gf_text <- " only considering locations within 10km of a gas flare site"
      if(var_i %>% str_detect("_nogf_5km"))  gf_text <- " excluding locations within 5km of a gas flaring site"
      if(var_i %>% str_detect("_nogf_10km")) gf_text <- " excluding locations within 10km of a gas flaring site"
      
      if(var_i %>% str_detect("_sum")) metrix_text <- "Sum"
      if(var_i %>% str_detect("_mean")) metrix_text <- "Mean"
      if(var_i %>% str_detect("_median")) metrix_text <- "Median"
      if(var_i %>% str_detect("_max")) metrix_text <- "Max"
      if(var_i %>% str_detect("_q05")) metrix_text <- "5th percentile"
      if(var_i %>% str_detect("_q95")) metrix_text <- "95th percentile"
      
      var_label <- paste0(metrix_text, " of nighttime lights", gf_text)
      
      attr(df_dta[[var_i]], "label") <- var_label
    }
    
    if(product_id_i == "VNP46A3"){
      attr(df_dta$date, "label") <- "Date/Month"
    }
    
    if(product_id_i == "VNP46A4"){
      attr(df_dta$year, "label") <- "Year"
    }
    
    #### Export
    if(product_id_i == "VNP46A4"){
      write_csv(df,     file.path(agg_append_dir, paste0("ntl_", file_name, ".csv")))
      #write_parquet(df, file.path(agg_append_dir, paste0("ntl_", file_name, ".parquet")))
      #write_dta(df_dta, file.path(agg_append_dir, paste0("ntl_", file_name, ".dta")))
    }
    
    if(product_id_i == "VNP46A3"){
      year_vec <- df$date %>% year()
      
      for(year_i in unique(year_vec)){
        write_csv(df[year_vec %in% year_i,], 
                  file.path(agg_append_dir, paste0("ntl_", file_name, "_", year_i,".csv")))
      }
      
      write_csv(df, 
                file.path(agg_append_dir, paste0("ntl_", file_name, "_", "all",".csv")))
      write_parquet(df, 
                    file.path(agg_append_dir, paste0("ntl_", file_name, "_", "all",".parquet")))
      
      #write_parquet(df, file.path(agg_append_dir, paste0("ntl_", file_name, ".parquet")))
      #write_dta(df_dta, file.path(agg_append_dir, paste0("ntl_", file_name, ".dta")))
    }
    
  }
}

# adm_df <- file.path(agg_date_dir, "ADM2", "VNP46A4") %>%
#   list.files(full.names = T,
#              pattern = "*.Rds",
#              recursive = T) %>%
#   map_df(readRDS) %>%
#   dplyr::select(!starts_with("ntl_")) %>%
#   dplyr::select(-c(n_gasflaring_locs, date)) %>%
#   distinct()
# 
# write_csv(adm_df, file.path(agg_append_dir, "adm2_additional_attributes.csv"))
# write_dta(adm_df, file.path(agg_append_dir, "adm2_additional_attributes.dta"))