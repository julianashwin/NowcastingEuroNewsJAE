"
This file adds in benchmarks based macro and financial variables
Note that this file will not run from the replication package, as some of the underlying data are sensitive/proprietary
However, for transparency, we include this file to show how the data are cleaned
"
setwd("/Users/julianashwin/Documents/GitHub/NowcastingEuroNewsJAE/")
rm(list=ls())

library(tidyverse)
library(lubridate)
library(gplots)
library(stargazer)
library(readxl)
library(zoo)


## Function to standardise data so that it is mean zero and s.d. 1
standardise<- function(series){
  new_series <- (series - mean(series,na.rm=TRUE))/sd(series, na.rm = TRUE)
  return(new_series)
}


"
Daily financial data
"
financial_df <- tibble(read.csv("data/macrofin_bench/financial_daily.csv")) 
financial_df <- financial_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))


"
Dynamic Factor Model output
"
dates_df <- tibble(date = seq.Date(as.Date("2005-01-07"), as.Date("2020-12-31"), by = "day"))
dfm_df <- tibble(read.csv("data/macrofin_bench/ForecastsDFM.csv")) %>%
  mutate(quarter = as.Date(str_c(year, "-", (month-2), "-1"))) %>%
  mutate(next_quarter = quarter + months(3)) %>%
  pivot_longer(cols = `X2005.01.07`:`X2021.01.13`, names_to = "date", values_to = "DFM_fcst") %>%
  mutate(date = as.Date(str_remove(date, "X"), format = "%Y.%m.%d")) %>%
  filter(date < next_quarter & !is.na(DFM_fcst)) %>%
  mutate(temp_date = case_when(date <= quarter ~ quarter, TRUE ~ NA_Date_)) %>%
  mutate(special_case = as.numeric(is.na(temp_date))) %>%
  group_by(quarter, special_case) %>%
  filter(special_case == 1 | date == max(date)) %>%
  ungroup() %>%
  mutate(date = case_when(!is.na(temp_date) ~ temp_date, TRUE ~ date)) %>%
  filter(date >= quarter & date < next_quarter) %>%
  right_join(dates_df) %>%
  arrange(date) %>%
  fill(quarter, GDPfl, GDPls, DFM_fcst, .direction = "down") %>%
  select(date, GDPfl, GDPls, DFM_fcst)
  
dfm_df %>%
  ggplot(aes(x = date)) + theme_bw() +
  geom_line(aes(y = GDPfl, color = "Flash")) +
  geom_line(aes(y = GDPls, color = "Latest")) +
  geom_line(aes(y = DFM_fcst, color = "DFM"))


dfm_short_df <- tibble(read.csv("data/macrofin_bench/ForecastsDFM_short.csv")) %>%
  mutate(quarter = as.Date(str_c(year, "-", (month-2), "-1"))) %>%
  mutate(next_quarter = quarter + months(3)) %>%
  pivot_longer(cols = `X07.01.2005`:`X18.12.2020`, names_to = "date", values_to = "DFM_short_fcst") %>%
  mutate(date = as.Date(str_remove(date, "X"), format = "%d.%m.%Y")) %>%
  filter(date < next_quarter & !is.na(DFM_short_fcst)) %>%
  mutate(temp_date = case_when(date <= quarter ~ quarter, TRUE ~ NA_Date_)) %>%
  mutate(special_case = as.numeric(is.na(temp_date))) %>%
  group_by(quarter, special_case) %>%
  filter(special_case == 1 | date == max(date)) %>%
  ungroup() %>%
  mutate(date = case_when(!is.na(temp_date) ~ temp_date, TRUE ~ date)) %>%
  filter(date >= quarter & date < next_quarter) %>%
  right_join(dates_df) %>%
  arrange(date) %>%
  fill(quarter, GDPfl, GDPls, DFM_short_fcst, .direction = "down") %>%
  select(date, GDPfl, GDPls, DFM_short_fcst)

dfm_df %>%
  inner_join(select(dfm_short_df, date, DFM_short_fcst)) %>%
  ggplot(aes(x = date)) + theme_bw() +
  geom_line(aes(y = GDPfl, color = "Flash")) +
  geom_line(aes(y = GDPls, color = "Latest")) +
  geom_line(aes(y = DFM_fcst, color = "DFM")) + 
  geom_line(aes(y = DFM_short_fcst, color = "DFM short"))


"
Macro factor model
"
factor_df <- tibble(read.csv("data/macrofin_bench/Factor.csv")) %>%
  mutate(month = as.Date(str_c(year, "-", month,"-01"))) %>%
  filter(month >= "1998-07-01") %>%
  pivot_longer(cols = "X.2005.01.07.":"X.2021.01.13.", names_to = "vintage", 
               values_to = "macro_factor") %>%
  mutate(vintage = as.Date(str_sub(vintage, 3, 12), format = "%Y.%m.%d")) %>%
  filter(!is.na(macro_factor)) %>%
  arrange(vintage) %>%
  mutate(quarter = floor_date(month, unit = "quarters")) %>%
  mutate(moq = 1 + month(month) - month(quarter)) %>%
  arrange(vintage, month, moq) %>%
  pivot_wider(id_cols = c(quarter, vintage), names_from = moq, names_prefix = str_c("macro_factor_m"),
              values_from = macro_factor)

factor_short_df <- tibble(read.csv("data/macrofin_bench/Factor_short.csv")) %>%
  mutate(month = as.Date(str_c(year, "-", month,"-01"))) %>%
  filter(month >= "1998-07-01") %>%
  pivot_longer(cols = "X.2005.01.07.":"X.2020.12.18.", names_to = "vintage", 
               values_to = "macro_factor_short") %>%
  mutate(vintage = as.Date(str_sub(vintage, 3, 12), format = "%Y.%m.%d")) %>%
  filter(!is.na(macro_factor_short)) %>%
  arrange(vintage) %>%
  mutate(quarter = floor_date(month, unit = "quarters")) %>%
  mutate(moq = 1 + month(month) - month(quarter)) %>%
  arrange(vintage, month, moq) %>%
  pivot_wider(id_cols = c(quarter, vintage), names_from = moq, names_prefix = str_c("macro_factor_short_m"),
              values_from = macro_factor_short)


factor_comp_df <- factor_df %>%
  inner_join(factor_short_df)

cor.test(factor_comp_df$macro_factor_m1, factor_comp_df$macro_factor_short_m1)

factor_df %>%
  ggplot(aes(x = quarter)) + theme_bw() +
  geom_line(aes(y = (macro_factor_m1 + macro_factor_m2 + macro_factor_m3)/3, color = vintage, group = vintage))






factor_comp_df %>%
  write.csv("data/clean_data/macrofactor.csv", row.names = F)




"
MIDAS forecasts
"
midas_daily_df_old <- read_csv("data/macrofin_bench/MIDAS_results.csv") %>%
  filter(!is.na(Vintage)) %>%
  mutate(Target_Q = as.Date(str_remove_all(Target_Q, "'"), format = "%d-%b-%Y"),
         Vintage = as.Date(str_remove_all(Vintage, "'"), format = "%d-%b-%Y"),
         quarter = floor_date(Target_Q, unit = "quarters")) %>%
  rename(date = Vintage) %>%
  relocate(quarter, .after = Target_Q) %>%
  full_join(mutate(dates_df, quarter = floor_date(date, unit = "quarters"))) %>% 
  arrange(quarter, date) %>% 
  fill(Target_Q:`pmi_daily_90_econlex_daily_90`, .direction = "down") %>%
  filter(date >= quarter, date <= Target_Q) %>%
  filter(!is.na(Final)) %>%
  distinct() %>% dplyr::select(-Target_Q)


midas_daily_df <- read_csv("data/macrofin_bench/MIDAS_results_190723.csv") %>%
  filter(!is.na(Vintage)) %>%
  mutate(Target_Q = as.Date(str_remove_all(Target_Q, "'"), format = "%d-%b-%Y"),
         Vintage = as.Date(str_remove_all(Vintage, "'"), format = "%d-%b-%Y"),
         quarter = floor_date(Target_Q, unit = "quarters")) %>%
  rename(date = Vintage) %>%
  relocate(quarter, .after = Target_Q) %>%
  full_join(mutate(dates_df, quarter = floor_date(date, unit = "quarters"))) %>% 
  arrange(quarter, date) %>% 
  fill(Target_Q:`econlex_cum90, legendre2`, .direction = "down") %>%
  filter(date >= quarter, date <= Target_Q) %>%
  filter(!is.na(Final)) %>%
  distinct() %>% dplyr::select(-Target_Q, -number)

midas_monthly_df <- read_xlsx("data/macrofin_bench/Midas_monthly_final.xlsx", sheet = "both") %>%
  filter(!is.na(Vintage)) %>%
  mutate(Target_Q = as.Date(str_remove_all(Target_Q, "'"), format = "%d-%b-%Y"),
         Vintage = as.Date(Vintage), 
         quarter = floor_date(Target_Q, unit = "quarters")) %>%
  rename(date = Vintage) %>%
  relocate(quarter, .after = Target_Q) %>%
  full_join(mutate(dates_df, quarter = floor_date(date, unit = "quarters"))) %>% 
  arrange(quarter, date) %>% 
  fill(Target_Q:`econlex_rolling,almon`, .direction = "down") %>%
  filter(date >= quarter & date <= Target_Q & quarter >= "2006-04-01") %>%
  filter(!is.na(Final)) %>%
  distinct() %>% dplyr::select(-Target_Q)




midas_preds_df <- midas_daily_df %>%
  dplyr::select(-Flash, -Final) %>%
  full_join(midas_daily_df_old) %>%
  dplyr::select(-Flash, -Final) %>%
  full_join(midas_monthly_df) %>%
  pivot_longer(cols = c(-date, -quarter, -Flash, -Final)) %>%
  mutate(bench = case_when(str_detect(name, "vader") ~ "preds_text",
                           str_detect(name, "stability") ~ "preds_text",
                           str_detect(name, "econlex") ~ "preds_text",
                           TRUE ~ "preds_bench"))  %>%
  #filter(str_detect(name, "pmi")) %>%
  pivot_wider(id_cols = c(date, quarter, Flash, Final, name), names_from = bench, values_from = value) %>%
  mutate(metric_name = case_when(str_detect(name, "vader") ~ "vader",
                                 str_detect(name, "stability") ~ "stability",
                                 str_detect(name, "econlex") ~ "econlex",
                                 TRUE ~ NA_character_)) %>%
  filter(!(metric_name %in% c("econlex"))) %>%
  mutate(bench_vars = case_when(str_detect(name, "pmi_monthly_6") ~ "pmi_monthly_6",
                                str_detect(name, "pmi_monthly6") ~ "pmi_monthly_6",
                                str_detect(name, "pmi6") ~ "pmi_monthly_6",
                                str_detect(name, "pmi_daily_180") ~ "pmi_daily_180",
                                str_detect(name, "pmi_daily_90") ~ "pmi_daily_90",
                                str_detect(name, "pmi_monthly_180") ~ "pmi_daily_180",
                                str_detect(name, "pmi_monthly_90") ~ "pmi_daily_90",
                                TRUE ~ NA_character_)) %>%
  mutate(text_vars = case_when(str_detect(name, "vader_daily_180") ~ "metric_daily_180",
                               str_detect(name, "vader_daily_90") ~ "metric_daily_90",
                               str_detect(name, "vader_daily90") ~ "metric_daily_90",
                               str_detect(name, "vader_month_roll90") ~ "metric_rolling_90",
                               str_detect(name, "vader_cum90") ~ "metric_cum_90",
                               str_detect(name, "vader_cum") ~ "metric_cum",
                               str_detect(name, "vader_rolling") ~ "metric_rolling",
                               str_detect(name, "stability_daily_180") ~ "metric_daily_180",
                               str_detect(name, "stability_daily_90") ~ "metric_daily_90",
                               str_detect(name, "stability_daily90") ~ "metric_daily_90",
                               str_detect(name, "stability_month_roll90") ~ "metric_rolling_90",
                               str_detect(name, "stability_cum90") ~ "metric_cum_90",
                               str_detect(name, "stability_cum") ~ "metric_cum",
                               str_detect(name, "stability_rolling") ~ "metric_rolling",
                               str_detect(name, "econlex_daily_180") ~ "metric_daily_180",
                               str_detect(name, "econlex_daily_90") ~ "metric_daily_90")) %>%
  mutate(text_vars = case_when(!str_detect(name, "pmi") ~ str_c(text_vars, ", no pmi"),
                               TRUE ~ text_vars)) %>%
  mutate(bench_vars = case_when(str_detect(text_vars, ", no pmi") ~ "pmi_monthly_6",
                               TRUE ~ bench_vars))  %>%
  mutate(model = case_when(str_detect(name, "legendre" ) ~ "MIDAS (legendre)",
                           str_detect(name, "almon" ) ~ "MIDAS (almon)",
                           TRUE ~ "MIDAS (legendre)")) %>%
  group_by(date, quarter, Flash, Final, model, bench_vars) %>%
  mutate(preds_bench = max(preds_bench, na.rm = T)) %>%
  filter(!is.na(text_vars)) 

midas_preds_df %>%
  write_csv("data/clean_data/daily/midas_preds.csv")

midas_preds_df %>%
  arrange(date, text_vars, bench_vars, model)

  

"
Combine with the original daily data
"
euro_df <- read_csv("data/clean_data/daily/euro_data_daily_orig.csv") %>%
  mutate(date = as.Date(date), quarter = as.Date(quarter)) %>%
  left_join(financial_df) %>%
  arrange(date) %>%
  fill(corp_spr, eurostoxx325, .direction = "down") %>%
  left_join(dfm_df) %>%
  relocate(growth_final, GDPfl, ECB_proj_latest, pmi_daily, DFM_fcst, corp_spr, eurostoxx325, .after = quarter) %>%
  left_join(select(dfm_short_df, date, DFM_short_fcst)) %>%
  arrange(date) %>% # Remove the daily PMI and cumulative metrics as they're already saved elsewhere
  select(-pmi_daily, -pmi_releases, -pmi_m1_latest, -pmi_m2_latest, -pmi_m3_latest, -pmi_m3_prevq_latest,
         -vader_cum, -stability_cum)

euro_df %>% filter(date < "2021-04-01") %>%
  ggplot() + theme_bw() + 
  geom_line(aes(x = date, y = (growth_final), color = "Growth")) +
  geom_line(aes(x = date, y = (GDPls), color = "Flash")) +
  geom_line(aes(x = date, y = DFM_fcst, color = "DFM")) +
  geom_line(aes(x = date, y = standardise(eurostoxx325), color = "Stoxx"))



write.csv(euro_df,"data/clean_data/daily/euro_data_daily_new.csv", row.names = FALSE)

## Merge in sentiment metrics and export
euro_sent_df <- as_tibble(read.csv(str_c(import_dir, "daily/euro_daily_sentiments.csv"))) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))

euro_df <- as_tibble(read.csv(str_c(import_dir, "daily/euro_data_daily_new.csv"))) %>%
  mutate(date = as.Date(date), month = as.Date(month), quarter = as.Date(quarter)) %>%
  left_join(euro_sent_df) %>%
  filter(date > "2002-01-01")

## The file saved here is available in the replication package
euro_df %>%
  write_csv("data/euro_daily_export.csv")

