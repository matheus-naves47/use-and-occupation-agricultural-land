library(tidyverse)
library(arrow)
library(ncdf4)
library(readxl)
library(geojsonio)
library(lubridate)
library(raster)
library(gridExtra)
library(leaflet)
library(exactextractr)

# Yearly Data ----------------------------------------------------------------------------------------------------------------------
ncdf_tmin <-
  'data/climate_grids/BR-DWGD/Tmin_1961_2020_yearmean.nc'
ncdf_tmax <-
  'data/climate_grids/BR-DWGD/Tmax_1961_2020_yearmean.nc'
ncdf_prec <-
  'data/climate_grids/BR-DWGD/pr_1961_2020_yearsum.nc'

tmin_brick <- brick(ncdf_tmin, varname = 'Tmin')
tmax_brick <- brick(ncdf_tmax, varname = 'Tmax')
prec_brick <- brick(ncdf_prec, varname = 'pr')

geo_loc_areas_origem <-
  geojson_read("data/pam/output/cafe/pam_cafe_geo_areas_origem.geojson",
               what = 'sp')

geo_loc_areas_origem_municipal <-
  geojson_read("data/pam/output/cafe/pam_cafe_geo_areas_origem_municipal.geojson",
               what = 'sp')

##  Regional Level ------------------------------------------------------------------------------------------------------------------------------------------------
### Extraction from raster ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tmin_df <-
  raster::extract(tmin_brick,
                  geo_loc_areas_origem,
                  df = TRUE,
                  fun = mean,
                  na.rm = TRUE) %>%
  gather(., key = 'DATE', value = 'TMIN',-ID)

tmax_df <-
  raster::extract(tmax_brick,
                  geo_loc_areas_origem,
                  df = TRUE,
                  fun = mean,
                  na.rm = TRUE) %>%
  gather(., key = 'DATE', value = 'TMAX',-ID)

prec_df <-
  raster::extract(prec_brick,
                  geo_loc_areas_origem,
                  df = TRUE,
                  fun = mean,
                  na.rm = TRUE) %>%
  gather(., key = 'DATE', value = 'PREC',-ID)

## Joins & Export ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

climatology_areas_origem_regional <-
  dplyr::left_join(tmin_df, tmax_df, by = c('DATE', 'ID')) %>%
  left_join(., prec_df, by = c('DATE', 'ID')) %>% mutate(
    DATE = str_replace(DATE, "X", ""),
    DATE = as_date(DATE),
    ID = as.character(ID))

geo_loc_areas_origem@data$ID <- rownames(geo_loc_areas_origem@data)

climatology_areas_origem_regional <- dplyr::left_join(climatology_areas_origem_regional, geo_loc_areas_origem@data, by="ID") %>% 
  dplyr::select(REGIAO_ORIGEM, UF, everything(), -ID)

write_csv(climatology_areas_origem_regional, "data/coffee-project/climatology_or_regional_yearly.csv")

rm(prec_df, tmin_df, tmax_df)

## Municipal Level ------------------------------------------------------------------------------------------------------------------------------------------------

### Extraction from raster -----------------------------------------------------------------------------------------------------------------------------------

tmin_df <- exact_extract(tmin_brick, geo_loc_areas_origem_municipal,
                           include_cols=TRUE) %>% do.call('rbind', .) %>%
  pivot_longer(cols=11:70, names_to = 'DATE', values_to = 'TMIN') %>%
  distinct(NOME, DATE, .keep_all = T) %>% dplyr::select(NOME, UF, REGIAO_ORIGEM, DATE, TMIN)

tmax_df <- exact_extract(tmax_brick, geo_loc_areas_origem_municipal,
                         include_cols=TRUE) %>% do.call('rbind', .) %>%
  pivot_longer(cols=11:70, names_to = 'DATE', values_to = 'TMAX') %>%
  distinct(NOME, DATE, .keep_all = T) %>% dplyr::select(NOME, UF, REGIAO_ORIGEM, DATE, TMAX)

prec_df <- exact_extract(prec_brick, geo_loc_areas_origem_municipal,
                         include_cols=TRUE) %>% do.call('rbind', .) %>%
  pivot_longer(cols=11:70, names_to = 'DATE', values_to = 'PREC') %>%
  distinct(NOME, DATE, .keep_all = T) %>% dplyr::select(NOME, UF, REGIAO_ORIGEM, DATE, PREC)

### Joins & Export -----------------------------------------------------------------------------------------------

climatology_areas_origem_municipal <-
  dplyr::left_join(tmin_df, tmax_df, by = c('NOME', 'UF', 'REGIAO_ORIGEM', 'DATE')) %>%
  left_join(., prec_df, by = c('NOME', 'UF', 'REGIAO_ORIGEM', 'DATE')) %>% mutate(
    DATE = str_replace(DATE, "X", ""),
    DATE = as_date(DATE)) %>% 
  relocate(UF, .after = REGIAO_ORIGEM) %>% rename(MUNICIPIO = NOME) %>% arrange(REGIAO_ORIGEM)

rm(tmin_df, tmax_df, prec_df)

write_csv(climatology_areas_origem_municipal, "data/coffee-project/climatology_or_municipal_yearly.csv")


# Daily Data ----------------------------------------------------------------------------------------------------------------------
ncdf_tmin <-
  'data/climate_grids/BR-DWGD/raw_data/Tmin_19610101_20200731_BR-DWGD.nc'
ncdf_tmax <-
  'data/climate_grids/BR-DWGD/raw_data/Tmax_19610101_20200731_BR-DWGD.nc'
ncdf_prec <-
  'data/climate_grids/BR-DWGD/raw_data/pr_19610101_20200731_BR-DWGD.nc'




unique(geo_loc_areas_origem@data$UF)

geo_loc_areas_origem <-
  geojson_read("data/pam/output/cafe/pam_cafe_geo_areas_origem.geojson",
               what = 'sp')

##  Regional Level ------------------------------------------------------------------------------------------------------------------------------------------------
### Extraction from raster ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tmin_brick <- brick(ncdf_tmin, varname = 'Tmin')

tmin_df <-
  raster::extract(tmin_brick,
                  geo_loc_areas_origem,
                  df = TRUE,
                  fun = mean,
                  verbose=TRUE) %>%
  gather(., key = 'DATE', value = 'TMIN',-ID)

rm(tmin_brick)

tmax_brick <- brick(ncdf_tmax, varname = 'Tmax')


tmax_df <-
  raster::extract(tmax_brick,
                  geo_loc_areas_origem,
                  df = TRUE,
                  fun = mean,
                  na.rm = TRUE) %>%
  gather(., key = 'DATE', value = 'TMAX',-ID)

rm(tmax_brick)

prec_brick <- brick(ncdf_prec, varname = 'pr')

prec_df <-
  raster::extract(prec_brick,
                  geo_loc_areas_origem,
                  df = TRUE,
                  fun = mean,
                  na.rm = TRUE) %>%
  gather(., key = 'DATE', value = 'PREC',-ID)

## Joins, manipulations and output ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

climatology_areas_origem_regional <-
  dplyr::left_join(tmin_df, tmax_df, by = c('DATE', 'ID')) %>%
  left_join(., prec_df, by = c('DATE', 'ID')) %>% mutate(
    DATE = str_replace(DATE, "X", ""),
    DATE = as_date(DATE),
    ID = as.character(ID))

rm(prec_df, tmin_df, tmax_df)

geo_loc_areas_origem@data$ID <- rownames(geo_loc_areas_origem@data)

climatology_areas_origem_regional <- dplyr::left_join(climatology_areas_origem_regional, geo_loc_areas_origem@data, by="ID") %>% 
  dplyr::select(REGIAO_ORIGEM, UF, everything(), -ID)

climatology_areas_origem_regional <- climatology_areas_origem_regional %>% mutate(TMIN = na_if(TMIN, NaN),
                                                     TMAX = na_if(TMAX, NaN),
                                                     PREC = na_if(PREC, NaN))

write_parquet(climatology_areas_origem_regional, "data/coffee-project/climatology_or_regional_daily.parquet")

