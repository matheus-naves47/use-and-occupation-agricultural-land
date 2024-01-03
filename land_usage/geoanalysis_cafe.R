library(tidyverse)
library(ncdf4)
library(raster)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(leafsync)

# Dados -------------------------------------------------------------------

pam_cafe_geo_sp <- geojson_read("data/pam/output/cafe/pam_geo_ufs/pam_cafe_geo_SP.geojson",
                                what='sp')

pam_cafe_diff_geo_sp <- geojson_read("data/pam/output/cafe/pam_cafe_diff_geo_sp.geojson",
                                     what='sp')

geo_loc_areas_origem <- geojson_read("data/pam/output/cafe/pam_cafe_geo_areas_origem_municipal.geojson",
                                     what='sp')

geo_loc_usinas_dest_sp <- geojson_read("data/conab/USINAS_CANA_BR_ATIVAS_2021.shp",
                                    what='sp') %>% subset(UF == 'SP')

geodata_sp <- geojson_read("data/gis-dataset-brasil/uf/shapefile/uf.shp",
                           what='sp') %>% subset(GEOCODIGO == 35)

# Correções ---------------------------------------------------------------

pam_cafe_geo_sp$AREA_PLANTADA_PERC <- as.numeric(pam_cafe_geo_sp$AREA_PLANTADA_PERC)

pam_cafe_diff_geo_sp$AREA_1990 <- as.numeric(pam_cafe_diff_geo_sp$AREA_1990) %>% replace_na(0)
pam_cafe_diff_geo_sp$AREA_2020 <- as.numeric(pam_cafe_diff_geo_sp$AREA_2020) %>% replace_na(0)

pam_cafe_diff_geo_sp@data <- pam_cafe_diff_geo_sp@data %>% mutate(DIFF_AREA = AREA_2020 - AREA_1990)

geo_loc_usinas_dest_sp$UNIDADE <-
  str_to_title(geo_loc_usinas_dest_sp$UNIDADE) %>%
  str_replace(., "S.a.", 'S.A.') %>% 
  str_trim(., 'both')

geo_loc_usinas_dest_sp$PERFIL <- str_to_title(geo_loc_usinas_dest_sp$PERFIL) %>%
  str_replace(., 'Info.', 'Informação')

# Área Plantada Percentual por Município x Ano - SP ----------------------------

anos_interesse <- seq(1990, 2020, 5)

pal <- colorNumeric(palette="RdPu", domain=pam_cafe_geo_sp$AREA_PLANTADA_PERC, na.color ='black')

for (ano in anos_interesse) {
  assign(paste0('pal_', ano),
         colorNumeric(palette="RdPu",
                      domain=(pam_cafe_geo_sp %>% subset(ANO == ano))$AREA_PLANTADA_PERC, na.color="black"))
  rm(ano)
}

mapa_cafe <- leaflet() %>% addScaleBar(position = 'bottomright') %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    group = '1990',
    data = (pam_cafe_geo_sp %>% subset(ANO == 1990)),
    weight = 0.7,
    fillColor = ~pal_1990((pam_cafe_geo_sp %>% subset(ANO == 1990))$AREA_PLANTADA_PERC),
    fillOpacity = 0.5,
    color = 'black',
    label = paste(
      "Município: ",
      (pam_cafe_geo_sp %>% subset(ANO == 1990))$MUNICIPIO,
      "<br/>",
      "Percentual da Área Total: ",
      (pam_cafe_geo_sp %>% subset(ANO == 1990))$AREA_PLANTADA_PERC,
      "%<br/>",
      "Área: ",
      (pam_cafe_geo_sp %>% subset(ANO == 1990))$AREA_PLANTADA,
      " Hectares<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>% 
  addPolygons(
    group = '1995',
    data = (pam_cafe_geo_sp %>% subset(ANO == 1995)),
    weight = 0.7,
    fillColor = ~pal_1995((pam_cafe_geo_sp %>% subset(ANO == 1995))$AREA_PLANTADA_PERC),
    fillOpacity = 0.5,
    color = 'black',
    label = paste(
      "Município: ",
      (pam_cafe_geo_sp %>% subset(ANO == 1995))$MUNICIPIO,
      "<br/>",
      "Percentual da Área Total: ",
      (pam_cafe_geo_sp %>% subset(ANO == 1995))$AREA_PLANTADA_PERC,
      "%<br/>",
      "Área: ",
      (pam_cafe_geo_sp %>% subset(ANO == 1995))$AREA_PLANTADA,
      " Hectares<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%  
  addPolygons(
    group = '2000',
    data = (pam_cafe_geo_sp %>% subset(ANO == 2000)),
    weight = 0.7,
    fillColor = ~pal_2000((pam_cafe_geo_sp %>% subset(ANO == 2000))$AREA_PLANTADA_PERC),
    fillOpacity = 0.5,
    color = 'black',
    label = paste(
      "Município: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2000))$MUNICIPIO,
      "<br/>",
      "Percentual da Área Total: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2000))$AREA_PLANTADA_PERC,
      "%<br/>",
      "Área: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2000))$AREA_PLANTADA,
      " Hectares<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>% 
  addPolygons(
    group = '2005',
    data = (pam_cafe_geo_sp %>% subset(ANO == 2005)),
    weight = 0.7,
    fillColor = ~pal_2005((pam_cafe_geo_sp %>% subset(ANO == 2005))$AREA_PLANTADA_PERC),
    fillOpacity = 0.5,
    color = 'black',
    label = paste(
      "Município: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2005))$MUNICIPIO,
      "<br/>",
      "Percentual da Área Total: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2005))$AREA_PLANTADA_PERC,
      "%<br/>",
      "Área: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2005))$AREA_PLANTADA,
      " Hectares<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>% 
  addPolygons(
    group = '2010',
    data = (pam_cafe_geo_sp %>% subset(ANO == 2010)),
    weight = 0.7,
    fillColor = ~pal_2010((pam_cafe_geo_sp %>% subset(ANO == 2010))$AREA_PLANTADA_PERC),
    fillOpacity = 0.5,
    color = 'black',
    label = paste(
      "Município: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2010))$MUNICIPIO,
      "<br/>",
      "Percentual da Área Total: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2010))$AREA_PLANTADA_PERC,
      "%<br/>",
      "Área: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2010))$AREA_PLANTADA,
      " Hectares<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>% 
  addPolygons(
    group = '2015',
    data = (pam_cafe_geo_sp %>% subset(ANO == 2015)),
    weight = 0.7,
    fillColor = ~pal_2015((pam_cafe_geo_sp %>% subset(ANO == 2015))$AREA_PLANTADA_PERC),
    fillOpacity = 0.5,
    color = 'black',
    label = paste(
      "Município: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2015))$MUNICIPIO,
      "<br/>",
      "Percentual da Área Total: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2015))$AREA_PLANTADA_PERC,
      "%<br/>",
      "Área: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2015))$AREA_PLANTADA,
      " Hectares<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>% 
  addPolygons(
    group = '2020',
    data = (pam_cafe_geo_sp %>% subset(ANO == 2020)),
    weight = 0.7,
    fillColor = ~pal_2020((pam_cafe_geo_sp %>% subset(ANO == 2020))$AREA_PLANTADA_PERC),
    fillOpacity = 0.5,
    color = 'black',
    label = paste(
      "Município: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2020))$MUNICIPIO,
      "<br/>",
      "Percentual da Área Total: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2020))$AREA_PLANTADA_PERC,
      "%<br/>",
      "Área: ",
      (pam_cafe_geo_sp %>% subset(ANO == 2020))$AREA_PLANTADA,
      " Hectares<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>% 
  addLegend(
    pal = pal,
    values = pam_cafe_geo_sp$AREA_PLANTADA_PERC,
    opacity = 0.9,
    title = "% de Área<br>Total",
    position = 'topright',
    na.label = 'N/A',
    labFormat = labelFormat(suffix = "%")
    )


# Diferença de Área Plantada Percentual por Município x Ano - SP, 1990-2020 --------------------------------------------

scale_range <- c(-75, 75)

pal_diff <- colorNumeric("BrBG", domain = scale_range)

mapa_cafe_diff <- leaflet() %>% addScaleBar(position='bottomright') %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = pam_cafe_diff_geo_sp,
    weight = 0.7,
    fillColor = ~pal_diff(pam_cafe_diff_geo_sp$DIFF_AREA),
    fillOpacity = 0.5,
    color = 'black',
    label = paste(
      "Município: ",
      pam_cafe_diff_geo_sp$MUNICIPIO,
      "<br/>",
      "Diferença de Percentual da Área Total: ",
      pam_cafe_diff_geo_sp$DIFF_AREA,
      "%<br/>",
      "Área em 1990: ",
      pam_cafe_diff_geo_sp$AREA_1990,
      " Hectares<br/>",
      "Área em 2020: ",
      pam_cafe_diff_geo_sp$AREA_2020,
      " Hectares<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>% 
  addLegend(
    pal = pal_diff,
    values = pam_cafe_diff_geo_sp$DIFF_AREA,
    opacity = 0.9,
    title = "Diferença de %<br>de Área Total",
    position = 'topright',
    na.label = 'N/A',
    labFormat = labelFormat(prefix = "(", suffix = "%)")
  ) %>%
  addPolygons(
    group = 'Alta Mogiana',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Alta Mogiana')),
    weight = 0.7,
    fillColor = 'green',
    fillOpacity = 0.4,
    color = 'green') %>%
  addPolygons(
    group = 'Marília e Garça',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Marília e Garça')),
    weight = 0.7,
    fillColor = 'blue',
    fillOpacity = 0.4,
    color = 'blue') %>%
  addPolygons(
    group = 'Média Mogiana',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Média Mogiana')),
    weight = 0.7,
    fillColor = 'red',
    fillOpacity = 0.4,
    color = 'black') %>%
  addPolygons(
    group = 'Ourinhos e Avaré',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Ourinhos e Avaré')),
    weight = 0.7,
    fillColor = 'purple',
    fillOpacity = 0.4,
    color = 'purple') %>%
  addPolygons(
    group = 'Pinhal',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Pinhal')),
    weight = 0.7,
    fillColor = 'grey',
    fillOpacity = 0.4,
    color = 'grey')



# Usinas e Destilarias em SP --------------------------------------------

icons <- iconList(
  "Destilarias" = makeIcon(
    iconUrl = "images/icons/destilarias.png",
    iconWidth = 25,
    iconHeight = 25
  ),
  "Usinas" = makeIcon(
    iconUrl = "images/icons/usinas.png",
    iconWidth = 25,
    iconHeight = 25
  ),
  "Usinas_Sem_Info" = makeIcon(
    iconUrl = "images/icons/usinas_sem_info.png",
    iconWidth = 25,
    iconHeight = 25
  )
)

mapa_cafe <- mapa_cafe %>% 
  addMarkers(
    group = 'Usinas Mistas',
    lat = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$LATITUDE,
    lng = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$LONGITUDE,
    icon = icons$Usinas,
    popup = paste(
      "Unidade: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$UNIDADE,
      "<br/>",
      "Perfil: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$PERFIL,
      "<br/>",
      "Latitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$LATITUDE,
      "<br/>",
      "Longitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$LONGITUDE,
      "<br/>",
      sep = ""
    )
  ) %>%
  addMarkers(
    group = 'Destilarias',
    lat = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$LATITUDE,
    lng = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$LONGITUDE,
    icon = icons$Destilarias,
    popup = paste(
      "Unidade: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$UNIDADE,
      "<br/>",
      "Perfil: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$PERFIL,
      "<br/>",
      "Latitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$LATITUDE,
      "<br/>",
      "Longitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$LONGITUDE,
      "<br/>",
      sep = ""
    )
  ) %>%
  addMarkers(
    group = 'Usinas s/ Info.',
    lat = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$LATITUDE,
    lng = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$LONGITUDE,
    icon = icons$Usinas_Sem_Info,
    popup = paste(
      "Unidade: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$UNIDADE,
      "<br/>",
      "Perfil: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$PERFIL,
      "<br/>",
      "Latitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$LATITUDE,
      "<br/>",
      "Longitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$LONGITUDE,
      "<br/>",
      sep = ""
    )
  ) %>%
  addPolygons(
    group = 'Alta Mogiana',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Alta Mogiana')),
              weight = 0.7,
              fillColor = 'green',
              fillOpacity = 0.4,
              color = 'green') %>%
  addPolygons(
    group = 'Marília e Garça',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Marília e Garça')),
              weight = 0.7,
              fillColor = 'blue',
              fillOpacity = 0.4,
              color = 'blue') %>%
  addPolygons(
    group = 'Média Mogiana',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Média Mogiana')),
              weight = 0.7,
              fillColor = 'red',
              fillOpacity = 0.4,
              color = 'black') %>%
  addPolygons(
    group = 'Ourinhos e Avaré',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Ourinhos e Avaré')),
              weight = 0.7,
              fillColor = 'purple',
              fillOpacity = 0.4,
              color = 'purple') %>%
  addPolygons(
    group = 'Pinhal',
    data = (geo_loc_areas_origem %>% subset(REGIAO_ORIGEM == 'Pinhal')),
    weight = 0.7,
    fillColor = 'grey',
    fillOpacity = 0.4,
    color = 'grey') %>% 
  addLayersControl(
    baseGroups = c(1990, 1995, 2000, 2005, 2010, 2015, 2020),
    overlayGroups = c('Usinas Mistas',
                      'Destilarias',
                      'Usinas s/ Info.',
                      'Alta Mogiana',
                      'Marília e Garça',
                      'Média Mogiana',
                      'Ourinhos e Avaré',
                      'Pinhal'),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomleft"
  ) %>% 
  hideGroup(c('Usinas Mistas', 'Destilarias', 'Usinas s/ Info.')) %>% 
  hideGroup(c('Alta Mogiana',
              'Marília e Garça',
              'Média Mogiana',
              'Ourinhos e Avaré',
              'Pinhal')) 

mapa_cafe_diff <- mapa_cafe_diff %>% 
  addMarkers(
    group = 'Usinas Mistas',
    lat = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$LATITUDE,
    lng = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$LONGITUDE,
    icon = icons$Usinas,
    popup = paste(
      "Unidade: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$UNIDADE,
      "<br/>",
      "Perfil: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$PERFIL,
      "<br/>",
      "Latitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$LATITUDE,
      "<br/>",
      "Longitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Usina Mista'))$LONGITUDE,
      "<br/>",
      sep = ""
    )
  ) %>%
  addMarkers(
    group = 'Destilarias',
    lat = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$LATITUDE,
    lng = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$LONGITUDE,
    icon = icons$Destilarias,
    popup = paste(
      "Unidade: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$UNIDADE,
      "<br/>",
      "Perfil: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$PERFIL,
      "<br/>",
      "Latitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$LATITUDE,
      "<br/>",
      "Longitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Destilaria'))$LONGITUDE,
      "<br/>",
      sep = ""
    )
  ) %>%
  addMarkers(
    group = 'Usinas s/ Info.',
    lat = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$LATITUDE,
    lng = (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$LONGITUDE,
    icon = icons$Usinas_Sem_Info,
    popup = paste(
      "Unidade: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$UNIDADE,
      "<br/>",
      "Perfil: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$PERFIL,
      "<br/>",
      "Latitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$LATITUDE,
      "<br/>",
      "Longitude: ",
      (geo_loc_usinas_dest_sp %>% subset(PERFIL == 'Sem Informação'))$LONGITUDE,
      "<br/>",
      sep = ""
    )
  ) %>% 
  addLayersControl(
    overlayGroups = c('Usinas Mistas',
                      'Destilarias',
                      'Usinas s/ Info.',
                      'Alta Mogiana',
                      'Marília e Garça',
                      'Média Mogiana',
                      'Ourinhos e Avaré',
                      'Pinhal'),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomleft"
  ) %>% 
  hideGroup(c('Usinas Mistas', 'Destilarias', 'Usinas s/ Info.')) %>% 
  hideGroup(c('Alta Mogiana',
              'Marília e Garça',
              'Média Mogiana',
              'Ourinhos e Avaré',
              'Pinhal'))

mapa_cafe
mapa_cafe_diff

