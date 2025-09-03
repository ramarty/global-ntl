# Global NTL

## Packages
library(tidyverse)
library(sf)
library(terra)
library(blackmarbler)
library(dplyr)
library(readxl)
library(janitor)
library(haven)
library(ggpubr)
library(countrycode)
library(DescTools)
library(fixest)
library(leaflet)

## Paths
proj_dir <- "/Users/rmarty/Library/CloudStorage/OneDrive-WBG/Data Lab/Global NTL"
code_dir <- "/Users/rmarty/Library/CloudStorage/OneDrive-WBG/Documents/github/global-ntl"
ext_dir  <- "/Volumes/robmartyexternal/Global NTL"

data_dir    <- file.path(proj_dir, "data")
wb_bound_dir <- file.path(data_dir, "wb_official_boundaries")

h5_root_dir     <- file.path(ext_dir, "data", "blackmarble", "h5_files")
raster_root_dir <- file.path(proj_dir, "data", "blackmarble", "rasters")

# gas_flare_dir <- file.path(proj_dir, "data", "gas-flaring")
# figures_dir   <- file.path(proj_dir, "outputs", "figures")
