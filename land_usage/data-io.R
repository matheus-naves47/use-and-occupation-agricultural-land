# Data IO - Land Usage Subproject

library(tidyverse)
library(readxl)

# Café ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## PAM 1974-2020 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

df1 <-
  read_excel("data/pam/input/cafe/tabela_pam_74_99_cafe.xlsx",
             na = c("...", "-", ".."))
df2 <-
  read_excel("data/pam/input/cafe/tabela_pam_00_20_cafe.xlsx",
             na = c("...", "-", ".."))
tabela_ufs <- read_csv("data/tabela_unidades_federativas.csv")

col_names <-
  c(
    "CODIGO_MUNICIPIO",
    "MUNICIPIO",
    "ANO",
    "AREA_PLANTADA",
    "AREA_PLANTADA_PERC",
    "AREA_COLHIDA",
    "AREA_COLHIDA_PERC",
    "QUANTIDADE_PRODUZIDA",
    "RENDIMENTO_MED_PRODUCAO",
    "VALOR_PRODUCAO",
    "VALOR_PRODUCAO_PERC"
  )

colnames(df1) <- col_names
colnames(df2) <- col_names

df1 <- df1 %>% fill(CODIGO_MUNICIPIO, .direction = 'down') %>%
  fill(MUNICIPIO, .direction = 'down')

df2 <- df2 %>% fill(CODIGO_MUNICIPIO, .direction = 'down') %>%
  fill(MUNICIPIO, .direction = 'down')

df1 <- df1 %>%
  mutate(
    MOEDA_VIGENTE = str_extract(string = ANO, pattern = "(?<=\\().+?(?=\\))"),
    .after = ANO
  ) %>%
  mutate(ANO = str_extract(string = ANO, pattern = "[:digit:]+")) %>%
  slice(-144821)

df2 <-
  df2 %>% mutate(MOEDA_VIGENTE = 'Mil Reais', .after = ANO) %>%
  slice(-116971)

pam_cafe <- bind_rows(df1, df2) %>%
  arrange(CODIGO_MUNICIPIO) %>%
  mutate(CODIGO_UF = str_extract(CODIGO_MUNICIPIO, "\\b[:digit:]{2}"),
         .before = CODIGO_MUNICIPIO) %>%
  mutate(CODIGO_UF = as.numeric(CODIGO_UF)) %>%
  left_join(tabela_ufs, pam_cafe, by =
                                                                                                                                 'CODIGO_UF') %>% relocate(UF, .after = CODIGO_UF)

write_csv(pam_cafe, "data/pam/output/cafe/pam_cafe.csv")

## PAM - Diferença 1990-2020 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pam_cafe_diff <- pam_cafe %>% filter(ANO %in% c(1990, 2020)) %>%
  mutate(ANO = paste0('AREA_', ANO)) %>% select(1:5, 8) %>%
  pivot_wider(names_from = ANO, values_from = AREA_PLANTADA_PERC)

write_csv(pam_cafe_diff, 'data/pam/output/cafe/pam_cafe_diff.csv')

## PAM - Área Relativa por UF ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pam_cafe_uf <- read_xlsx('data/pam/input/cafe/tabela_area_relativa_pam_uf.xlsx',
                         n_max = 952,
                         col_types = c('text', 'text', 'numeric'))

colnames(pam_cafe_uf) <- c("UF", "ANO", "AREA_PLANTADA_PERC")

pam_cafe_uf <- pam_cafe_uf %>% fill(UF, .direction = 'down')

# Cana-de-Açúcar ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## PAM 1974-2020 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

df1 <-
  read_excel("data/pam/input/cana/pam_74_87_cana.xlsx",
             na = c("...", "-", ".."))
df2 <-
  read_excel("data/pam/input/cana/pam_88_20_cana.xlsx",
             na = c("...", "-", ".."))
tabela_ufs <- read_csv("data/tabela_unidades_federativas.csv")

col_names <-
  c(
    "CODIGO_MUNICIPIO",
    "MUNICIPIO",
    "ANO",
    "AREA_PLANTADA",
    "AREA_PLANTADA_PERC",
    "AREA_COLHIDA",
    "AREA_COLHIDA_PERC",
    "QUANTIDADE_PRODUZIDA",
    "RENDIMENTO_MED_PRODUCAO",
    "VALOR_PRODUCAO",
    "VALOR_PRODUCAO_PERC"
  )

colnames(df1) <- col_names
colnames(df2) <- col_names

df1 <- df1 %>% fill(CODIGO_MUNICIPIO, .direction = 'down') %>%
  fill(MUNICIPIO, .direction = 'down')

df2 <- df2 %>% fill(CODIGO_MUNICIPIO, .direction = 'down') %>%
  fill(MUNICIPIO, .direction = 'down')

df1 <- df1 %>%
  mutate(
    MOEDA_VIGENTE = str_extract(string = ANO, pattern = "(?<=\\().+?(?=\\))"),
    .after = ANO
  ) %>%
  mutate(ANO = str_extract(string = ANO, pattern = "[:digit:]+")) %>%
  slice(-77883)

df2 <-
  df2 %>% mutate(
    MOEDA_VIGENTE = str_extract(string = ANO, pattern = "(?<=\\().+?(?=\\))"),
    .after = ANO
  ) %>%
  mutate(ANO = str_extract(string = ANO, pattern = "[:digit:]+")) %>%
  slice(-183580)

pam_cana <- bind_rows(df1, df2) %>%
  arrange(CODIGO_MUNICIPIO) %>%
  mutate(CODIGO_UF = str_extract(CODIGO_MUNICIPIO, "\\b[:digit:]{2}"),
         .before = CODIGO_MUNICIPIO) %>%
  mutate(MUNICIPIO = str_replace(MUNICIPIO, " \\s*\\([^\\)]+\\)", "")) %>% 
  mutate(CODIGO_UF = as.numeric(CODIGO_UF)) %>%
  left_join(tabela_ufs, pam_cana, by =
              'CODIGO_UF') %>% relocate(UF, .after = CODIGO_UF)

write_csv(pam_cana, "data/pam/output/cana/pam_cana.csv")

## PAM - Diferença 1990-2020 -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


pam_cana

pam_cana_diff <- pam_cana %>% subset(ANO == 1990 | ANO == 2020) %>%
  mutate(ANO = paste0('AREA_', ANO)) %>% select(1:5, 8) %>%
  pivot_wider(names_from = ANO, values_from = AREA_PLANTADA_PERC)

write_csv(pam_cana_diff, 'data/pam/output/cana/pam_cana_diff.csv')