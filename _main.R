# Global NTL

## Packages
library(tidyverse)
library(sf)
library(terra)
library(blackmarbler)
library(exactextractr)
library(dplyr)
library(readxl)
library(janitor)
library(haven)
library(ggpubr)
library(countrycode)
library(DescTools)
library(fixest)
library(leaflet)
library(arrow)
source("https://raw.githubusercontent.com/ramarty/fast-functions/refs/heads/master/R/functions_in_chunks.R")

## Paths
proj_dir <- "/Users/rmarty/Library/CloudStorage/OneDrive-WBG/Data Lab/Global NTL"
code_dir <- "/Users/rmarty/Library/CloudStorage/OneDrive-WBG/Documents/github/global-ntl"
ext_dir  <- "/Volumes/robmartyexternal/Global NTL"

data_dir    <- file.path(proj_dir, "data")
wb_bound_dir <- file.path(data_dir, "wb-official-boundaries")
gasflare_dir <- file.path(data_dir, "global-flaring-data")

h5_root_dir     <- file.path(ext_dir, "data", "blackmarble", "h5_files")
raster_ntl_root_dir <- file.path(proj_dir, "data", "blackmarble", "rasters_ntl")
raster_qual_root_dir <- file.path(proj_dir, "data", "blackmarble", "rasters_quality")
agg_date_dir    <- file.path(proj_dir, "data", "blackmarble", "aggregated_by_date")
agg_append_dir    <- file.path(proj_dir, "data", "blackmarble", "aggregated_appened")

# gas_flare_dir <- file.path(proj_dir, "data", "gas-flaring")
# figures_dir   <- file.path(proj_dir, "outputs", "figures")
