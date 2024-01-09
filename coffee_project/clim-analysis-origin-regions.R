library(tidyverse)

# Dados --------------------------------------------------------------------------------------------------------------------------------------

climatology_areas_origem_sp <- read_csv("data/coffee-project/climatology_or_regional_yearly.csv") %>% 
  filter(UF == 'SP')

# Análise ------------------------------------------------------------------------------------------------------------

# Temperatura Mínima----------------------------------------------------------------------------------------------------------------

plot_list <- list()

regions <- climatology_areas_origem_sp$REGIAO_ORIGEM %>% unique()

for (region in regions){
  p <- climatology_areas_origem_sp %>% subset(REGIAO_ORIGEM == region) %>%  ggplot(aes(x=DATE)) +
    geom_line(aes(y=TMIN), color='steelblue') + 
    scale_x_date("", date_labels = "%Y") +
    scale_y_continuous("Temperature (°C)", labels = scales::unit_format(unit = '°C')) +
    labs(title="Yearly Average Minimum Temperature", subtitle = paste0('Origin Region of ', region, " - São Paulo")) +
    theme_linedraw() + 
    theme(axis.title = element_text(hjust=0.5, size = 14),
          axis.text = element_text(hjust=0.5, size = 12),
          plot.title = element_text(hjust=0.5, size = 24),
          plot.subtitle = element_text(hjust=0.5, size = 18))
  
  plot_list[[region]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (region in regions) {
  file_name = paste("images/visualizations/coffee_project/tmin_", region, ".png", sep="")
  png(file_name, width = 1280, height=720)
  print(plot_list[[region]])
  dev.off()
}

# Temperatura Máxima ------------------------------------------------------------------------------------------------------------------

plot_list <- list()

regions <- climatology_areas_origem_sp$REGIAO_ORIGEM %>% unique()

for (region in regions){
  p <- climatology_areas_origem_sp %>% subset(REGIAO_ORIGEM == region) %>%  ggplot(aes(x=DATE)) +
    geom_line(aes(y=TMAX), color='steelblue') + 
    scale_x_date("", date_labels = "%Y") +
    scale_y_continuous("Temperature (°C)", labels = scales::unit_format(unit = '°C')) +
    labs(title="Yearly Average Maximum Temperature", subtitle = paste0('Origin Region of ', region, " - São Paulo")) +
    theme_linedraw() + 
    theme(axis.title = element_text(hjust=0.5, size = 14),
          axis.text = element_text(hjust=0.5, size = 12),
          plot.title = element_text(hjust=0.5, size = 24),
          plot.subtitle = element_text(hjust=0.5, size = 18))
  
  plot_list[[region]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (region in regions) {
  file_name = paste("images/visualizations/coffee_project/tmax_", region, ".png", sep="")
  png(file_name, width = 1280, height=720)
  print(plot_list[[region]])
  dev.off()
}

# Precipitação ------------------------------------------------------------------------------------------------------------------

plot_list <- list()

regions <- climatology_areas_origem_sp$REGIAO_ORIGEM %>% unique()

for (region in regions){
  p <- climatology_areas_origem_sp %>% subset(REGIAO_ORIGEM == region) %>%  ggplot(aes(x=DATE)) +
    geom_line(aes(y=PREC), color='steelblue') + 
    scale_x_date("", date_labels = "%Y") +
    scale_y_continuous("Precipitation (mm)", labels = scales::unit_format(unit = 'mm')) +
    labs(title="Yearly Average Precipitation", subtitle = paste0('Origin Region of ', region, " - São Paulo")) +
    theme_linedraw() + 
    theme(axis.title = element_text(hjust=0.5, size = 14),
          axis.text = element_text(hjust=0.5, size = 12),
          plot.title = element_text(hjust=0.5, size = 24),
          plot.subtitle = element_text(hjust=0.5, size = 18))
  
  plot_list[[region]] <- p
}

# Save plots to tiff. Makes a separate file for each plot.
for (region in regions) {
  file_name = paste("images/visualizations/coffee_project/prec_", region, ".png", sep="")
  png(file_name, width = 1280, height=720)
  print(plot_list[[region]])
  dev.off()
}