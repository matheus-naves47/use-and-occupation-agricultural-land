library(tidyverse)
library(trend)
library(lubridate)

# Data ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
yearly_climatology_or <- read_csv("data/coffee-project/climatology_or_regional_yearly.csv") %>%
  subset(year(DATE) != 2020)
yearly_climatology_or_mun <- read_csv("data/coffee-project/climatology_or_municipal_yearly.csv") %>% 
  subset(year(DATE) != 2020)

# 2020 doesn't have complete data

regioes <- yearly_climatology_or$REGIAO_ORIGEM %>% unique()
municipios <- yearly_climatology_or_mun$MUNICIPIO %>% unique()


# Regional level ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

df_results_tmin <- NULL
df_results_tmax <- NULL
df_results_prec <- NULL

## Min. Temperature ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (regiao in regioes) {
  df_aux <- yearly_climatology_or %>% subset(REGIAO_ORIGEM == regiao)
  res_mk <- mk.test(df_aux$TMIN)
  res_ptt <- pettitt.test(df_aux$TMIN)
  
  REGIAO_ORIGEM = regiao
  UF = df_aux$UF[1]
  MK_p = res_mk$p.value
  MK_z = res_mk$statistic
  MK_S = res_mk$estimates[1]
  MK_varS = res_mk$estimates[2]
  MK_tau = res_mk$estimates[3]
  PTT_cp = df_aux[[res_ptt$estimate[1], 3]]
  PTT_p = res_ptt$p.value
  PTT_U = res_ptt$statistic
  PTT_mu1 = mean(df_aux[1:res_ptt$estimate[1], ]$TMIN)
  PTT_mu2 = mean(df_aux[res_ptt$estimate[1]:nrow(df_aux), ]$TMIN)
  
  df_results_tmin <- rbind(
    df_results_tmin,
    data.frame(
      REGIAO_ORIGEM,
      UF,
      MK_p,
      MK_z,
      MK_S,
      MK_varS,
      MK_tau,
      PTT_cp,
      PTT_p,
      PTT_U,
      PTT_mu1,
      PTT_mu2
    )
  )
  
}

## Max. Temperature ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (regiao in regioes) {
  df_aux <- yearly_climatology_or %>% subset(REGIAO_ORIGEM == regiao)
  res_mk <- mk.test(df_aux$TMAX)
  res_ptt <- pettitt.test(df_aux$TMAX)
  
  REGIAO_ORIGEM = regiao
  UF = df_aux$UF[1]
  MK_p = res_mk$p.value
  MK_z = res_mk$statistic
  MK_S = res_mk$estimates[1]
  MK_varS = res_mk$estimates[2]
  MK_tau = res_mk$estimates[3]
  PTT_cp = df_aux[[res_ptt$estimate[1], 3]]
  PTT_p = res_ptt$p.value
  PTT_U = res_ptt$statistic
  PTT_mu1 = mean(df_aux[1:res_ptt$estimate[1], ]$TMAX)
  PTT_mu2 = mean(df_aux[res_ptt$estimate[1]:nrow(df_aux), ]$TMAX)
  
  df_results_tmax <- rbind(
    df_results_tmax,
    data.frame(
      REGIAO_ORIGEM,
      UF,
      MK_p,
      MK_z,
      MK_S,
      MK_varS,
      MK_tau,
      PTT_cp,
      PTT_p,
      PTT_U,
      PTT_mu1,
      PTT_mu2
    )
  )
  
}

## Precipitation ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (regiao in regioes) {
  df_aux <- yearly_climatology_or %>% subset(REGIAO_ORIGEM == regiao)
  res_mk <- mk.test(df_aux$PREC)
  res_ptt <- pettitt.test(df_aux$PREC)
  
  REGIAO_ORIGEM = regiao
  UF = df_aux$UF[1]
  MK_p = res_mk$p.value
  MK_z = res_mk$statistic
  MK_S = res_mk$estimates[1]
  MK_varS = res_mk$estimates[2]
  MK_tau = res_mk$estimates[3]
  PTT_cp = df_aux[[res_ptt$estimate[1], 3]]
  PTT_p = res_ptt$p.value
  PTT_U = res_ptt$statistic
  PTT_mu1 = mean(df_aux[1:res_ptt$estimate[1], ]$PREC)
  PTT_mu2 = mean(df_aux[res_ptt$estimate[1]:nrow(df_aux), ]$PREC)
  
  df_results_prec <- rbind(
    df_results_prec,
    data.frame(
      REGIAO_ORIGEM,
      UF,
      MK_p,
      MK_z,
      MK_S,
      MK_varS,
      MK_tau,
      PTT_cp,
      PTT_p,
      PTT_U,
      PTT_mu1,
      PTT_mu2
    )
  )
  
}

## Results Interpretation 

df_results_tmin <- df_results_tmin %>% mutate(
  MK_trend = case_when(
    MK_S > 0 & MK_p < 0.05 ~ 'increasing',
    MK_S > 0 & MK_p > 0.05 ~ 'no trend',
    MK_S == 0 ~ 'no trend',
    MK_S < 0 & MK_p > 0.05 ~ 'no trend',
    MK_S < 0 & MK_p < 0.05 ~ 'decreasing'
  )
) %>% relocate(MK_trend, .after = UF)

df_results_tmax <- df_results_tmax %>% mutate(
  MK_trend = case_when(
    MK_S > 0 & MK_p < 0.05 ~ 'increasing',
    MK_S > 0 & MK_p > 0.05 ~ 'no trend',
    MK_S == 0 ~ 'no trend',
    MK_S < 0 & MK_p > 0.05 ~ 'no trend',
    MK_S < 0 & MK_p < 0.05 ~ 'decreasing'
  )
) %>% relocate(MK_trend, .after = UF)

df_results_prec <- df_results_prec %>% mutate(
  MK_trend = case_when(
    MK_S > 0 & MK_p < 0.05 ~ 'increasing',
    MK_S > 0 & MK_p > 0.05 ~ 'no trend',
    MK_S == 0 ~ 'no trend',
    MK_S < 0 & MK_p > 0.05 ~ 'no trend',
    MK_S < 0 & MK_p < 0.05 ~ 'decreasing'
  )
) %>% relocate(MK_trend, .after = UF)

## Export ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

write_csv(df_results_tmin, "data/coffee-project/trend_tests/cp_trends_tmin_yearly.csv")
write_csv(df_results_tmax, "data/coffee-project/trend_tests/cp_trends_tmax_yearly.csv")
write_csv(df_results_prec, "data/coffee-project/trend_tests/cp_trends_prec_yearly.csv")


# Municipal level ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

df_results_tmin <- NULL
df_results_tmax <- NULL
df_results_prec <- NULL

## Min. Temperature ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (municipio in municipios) {
  df_aux <- yearly_climatology_or_mun %>% subset(MUNICIPIO == municipio) %>% replace(is.na(.), 0)
  res_mk <- mk.test(df_aux$TMIN)
  res_ptt <- pettitt.test(df_aux$TMIN)
  
  MUNICIPIO = municipio
  REGIAO_ORIGEM = df_aux$REGIAO_ORIGEM[1]
  UF = df_aux$UF[1]
  MK_p = res_mk$p.value
  MK_z = res_mk$statistic
  MK_S = res_mk$estimates[1]
  MK_varS = res_mk$estimates[2]
  MK_tau = res_mk$estimates[3]
  PTT_cp = df_aux[[res_ptt$estimate[1], 3]]
  PTT_p = res_ptt$p.value
  PTT_U = res_ptt$statistic
  PTT_mu1 = mean(df_aux[1:res_ptt$estimate[1], ]$TMIN)
  PTT_mu2 = mean(df_aux[res_ptt$estimate[1]:nrow(df_aux), ]$TMIN)
  
  df_results_tmin <- rbind(
    df_results_tmin,
    data.frame(
      MUNICIPIO,
      REGIAO_ORIGEM,
      UF,
      MK_p,
      MK_z,
      MK_S,
      MK_varS,
      MK_tau,
      PTT_cp,
      PTT_p,
      PTT_U,
      PTT_mu1,
      PTT_mu2
    )
  )
  
}

## Max. Temperature ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (municipio in municipios) {
  df_aux <- yearly_climatology_or_mun %>% subset(MUNICIPIO == municipio) %>% replace(is.na(.), 0)
  res_mk <- mk.test(df_aux$TMAX)
  res_ptt <- pettitt.test(df_aux$TMAX)
  
  MUNICIPIO = municipio
  REGIAO_ORIGEM = df_aux$REGIAO_ORIGEM[1]
  UF = df_aux$UF[1]
  MK_p = res_mk$p.value
  MK_z = res_mk$statistic
  MK_S = res_mk$estimates[1]
  MK_varS = res_mk$estimates[2]
  MK_tau = res_mk$estimates[3]
  PTT_cp = df_aux[[res_ptt$estimate[1], 3]]
  PTT_p = res_ptt$p.value
  PTT_U = res_ptt$statistic
  PTT_mu1 = mean(df_aux[1:res_ptt$estimate[1], ]$TMAX)
  PTT_mu2 = mean(df_aux[res_ptt$estimate[1]:nrow(df_aux), ]$TMAX)
  
  df_results_tmax <- rbind(
    df_results_tmax,
    data.frame(
      MUNICIPIO,
      REGIAO_ORIGEM,
      UF,
      MK_p,
      MK_z,
      MK_S,
      MK_varS,
      MK_tau,
      PTT_cp,
      PTT_p,
      PTT_U,
      PTT_mu1,
      PTT_mu2
    )
  )
  
}

## Precipitation ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (municipio in municipios) {
  df_aux <- yearly_climatology_or_mun %>% subset(MUNICIPIO == municipio) %>% replace(is.na(.), 0)
  res_mk <- mk.test(df_aux$PREC)
  res_ptt <- pettitt.test(df_aux$PREC)
  
  MUNICIPIO = municipio
  REGIAO_ORIGEM = df_aux$REGIAO_ORIGEM[1]
  UF = df_aux$UF[1]
  MK_p = res_mk$p.value
  MK_z = res_mk$statistic
  MK_S = res_mk$estimates[1]
  MK_varS = res_mk$estimates[2]
  MK_tau = res_mk$estimates[3]
  PTT_cp = df_aux[[res_ptt$estimate[1], 3]]
  PTT_p = res_ptt$p.value
  PTT_U = res_ptt$statistic
  PTT_mu1 = mean(df_aux[1:res_ptt$estimate[1], ]$PREC)
  PTT_mu2 = mean(df_aux[res_ptt$estimate[1]:nrow(df_aux), ]$PREC)
  
  df_results_prec <- rbind(
    df_results_prec,
    data.frame(
      MUNICIPIO,
      REGIAO_ORIGEM,
      UF,
      MK_p,
      MK_z,
      MK_S,
      MK_varS,
      MK_tau,
      PTT_cp,
      PTT_p,
      PTT_U,
      PTT_mu1,
      PTT_mu2
    )
  )
  
}

## Results Interpretation

df_results_tmin <- df_results_tmin %>% mutate(
  MK_trend = case_when(
    MK_S > 0 & MK_p < 0.05 ~ 'increasing',
    MK_S > 0 & MK_p > 0.05 ~ 'no trend',
    MK_S == 0 ~ 'no trend',
    MK_S < 0 & MK_p > 0.05 ~ 'no trend',
    MK_S < 0 & MK_p < 0.05 ~ 'decreasing'
  )
) %>% relocate(MK_trend, .after = UF)

df_results_tmax <- df_results_tmax %>% mutate(
  MK_trend = case_when(
    MK_S > 0 & MK_p < 0.05 ~ 'increasing',
    MK_S > 0 & MK_p > 0.05 ~ 'no trend',
    MK_S == 0 ~ 'no trend',
    MK_S < 0 & MK_p > 0.05 ~ 'no trend',
    MK_S < 0 & MK_p < 0.05 ~ 'decreasing'
  )
) %>% relocate(MK_trend, .after = UF)

df_results_prec <- df_results_prec %>% mutate(
  MK_trend = case_when(
    MK_S > 0 & MK_p < 0.05 ~ 'increasing',
    MK_S > 0 & MK_p > 0.05 ~ 'no trend',
    MK_S == 0 ~ 'no trend',
    MK_S < 0 & MK_p > 0.05 ~ 'no trend',
    MK_S < 0 & MK_p < 0.05 ~ 'decreasing'
  )
) %>% relocate(MK_trend, .after = UF)

## Export ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

write_csv(df_results_tmin, 'data/coffee-project/trend_tests/cp_trends_tmin_mun_yearly.csv')
write_csv(df_results_tmax, "data/coffee-project/trend_tests/cp_trends_tmax_mun_yearly.csv")
write_csv(df_results_prec, "data/coffee-project/trend_tests/cp_trends_prec_mun_yearly.csv")
