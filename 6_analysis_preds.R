"
This file produces the figures based on the nowcasting analysis for the paper
"
setwd("/Users/julianashwin/Documents/GitHub/NowcastingEuroNewsJAE/")
rm(list=ls())

library(stringr)
library(lubridate)
library(ggplot2)
library(gplots)
library(ggpubr)
library(ggh4x)
library(stargazer)
library(reshape2)
library(glmnet)
library(randomForest)
library(tidyverse)
library(tree)
library(zoo)
library(tictoc)
library(beepr)
library(janitor)


## Import the data for Euro Area
euro_df <- read_csv("data/euro_daily_export.csv")

## Get the first vintage of growth for each quarter
growth_first_df <- euro_df %>%
  filter(!is.na(gdp_1lag_latest) & !is.na(gdp_1lag_latest)) %>%
  group_by(quarter, growth_final) %>% arrange(date) %>%
  summarise(gdp_1lag_first = first(gdp_1lag_latest),
            gdp_2lag_first = first(gdp_2lag_latest)) %>%
  ungroup() %>% arrange(quarter) %>%
  mutate(growth_1lag_first = 100*(gdp_1lag_first/gdp_2lag_first -1)) %>%
  mutate(growth_first = lead(growth_1lag_first, n = 1)) %>% 
  mutate(growth_first = case_when(is.na(growth_first) ~ growth_final,
                                  TRUE ~ growth_first)) %>%
  select(quarter, growth_first)
euro_df <- euro_df %>%
  left_join(growth_first_df, by = "quarter") 
rm(growth_first_df)

## Function to standardise data so that it is mean zero and s.d. 1
standardise<- function(series){
  new_series <- (series - mean(series,na.rm=TRUE))/sd(series, na.rm = TRUE)
  return(new_series)
}


"
Import the results from a few different sources
"
ols_preds_df <- read_csv("data/results/ols_preds.csv") %>%
  left_join(select(euro_df, date, month, quarter, year, doq, moq, dom, growth_final, growth_first, ECB_proj_latest, 
                  DFM_fcst, DFM_short_fcst))

all_vars_df <- ols_preds_df %>%
  select(date, month, quarter, year, doq, moq, dom, growth_final, growth_first, ECB_proj_latest, DFM_fcst, DFM_short_fcst) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date)

## Import MIDAS forecasts 
midas_preds_df <- read_csv("data/results/midas_preds.csv") %>%
  mutate(target = "growth_latest") %>%
  left_join(euro_df) %>%
  select(date, month, quarter, year, doq, moq, dom, growth_final, growth_first, ECB_proj_latest, DFM_fcst, DFM_short_fcst, metric_name, model, 
         text_vars, bench_vars, target, preds_bench, preds_text)

midas_preds_df %>%
  filter(model == "MIDAS (almon)")

midas_preds_df %>%
  select(date, model, bench_vars, preds_bench) %>%
  distinct()

midas_preds_df %>%
  filter(date <= "2019-12-31") %>%
  mutate(nopmi = str_detect(text_vars, "no pmi")) %>%
  left_join(select(euro_df, date, doq, growth_final, growth_first)) %>%
  group_by(metric_name, bench_vars, text_vars, model, nopmi, doq) %>%
  summarise(mse_text_final = mean((preds_text - growth_final)^2), 
            mse_bench_final = mean((preds_bench - growth_final)^2),
            mse_text_first = mean((preds_text - growth_first)^2), 
            mse_bench_first = mean((preds_bench - growth_first)^2)) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~bench_vars+metric_name) + 
  geom_line(aes(y = mse_bench_final, group = model), color = "grey") +
  geom_point(aes(y = mse_text_final, color = text_vars, shape = nopmi))



# Import the ML and OLS predictions from Eleni's code
original_preds_df <- read_csv("data/results/ML_preds_Eleni.csv")
original_text <- original_preds_df %>%
  select(-quarter, -doq, - growth, -ECB_proj) %>% # will merge these in from all_vars_df anyway
  select(-contains("bench")) %>%
  pivot_longer(cols = c(-date, -contains("bench")), values_to = "preds_text") %>% 
  mutate(model = case_when(str_detect(name, "OLS") ~ "lm_orig",
                           str_detect(name, "Ridge") ~ "ridge",
                           str_detect(name, "Boosting") ~ "boosting",
                           str_detect(name, "Forest") ~ "rf",
                           str_detect(name, "NN") ~ "NN",
                           str_detect(name, "AvML") ~ "AvML")) %>%
  mutate(metric_name = case_when(str_detect(name, "stab") ~ "stability",
                                 str_detect(name, "econlex") ~ "econlex",
                                 str_detect(name, "vader") ~ "vader")) %>%
  mutate(text_vars = case_when(str_detect(name, "diff") ~ "metric_cum, metric_diff, pmi_daily, pmi_diff",
                               TRUE ~ "metric_cum, pmi_daily"), 
         bench_vars = case_when(str_detect(name, "diff") ~ "pmi_daily, pmi_diff",
                                TRUE ~ "pmi_daily")) %>%
  mutate(target = case_when(str_detect(name, "noproj") ~ "growth_latest",
                            str_detect(name, "proj") ~ "proj_fe", 
                            TRUE ~ "growth_latest")) %>%
  select(-name) %>%
  left_join(all_vars_df)
original_bench <- original_preds_df %>%
  select(-quarter, -doq, - growth, -ECB_proj) %>% # will merge these in from all_vars_df anyway
  select(date, contains("bench")) %>%
  pivot_longer(cols = c(-date), values_to = "preds_bench") %>% 
  mutate(model = case_when(str_detect(name, "OLS") ~ "lm_orig",
                           str_detect(name, "Ridge") ~ "ridge",
                           str_detect(name, "Boosting") ~ "boosting",
                           str_detect(name, "Forest") ~ "rf",
                           str_detect(name, "NN") ~ "NN",
                           str_detect(name, "AvML") ~ "AvML")) %>%
  mutate(text_vars = case_when(str_detect(name, "diff") ~ "metric_cum, metric_diff, pmi_daily, pmi_diff",
                               TRUE ~ "metric_cum, pmi_daily"), 
         bench_vars = case_when(str_detect(name, "diff") ~ "pmi_daily, pmi_diff",
                                TRUE ~ "pmi_daily")) %>%
  mutate(target = case_when(str_detect(name, "noproj") ~ "growth_latest",
                            str_detect(name, "proj") ~ "proj_fe", 
                            TRUE ~ "growth_latest")) %>%
  select(-name) %>%
  left_join(all_vars_df)
orig_preds_df <- original_text %>%
  inner_join(original_bench) %>%
  dplyr::select(date, month, quarter, year, doq, moq, dom, growth_final, growth_first, ECB_proj_latest, DFM_fcst, DFM_short_fcst, metric_name, model, 
         text_vars, bench_vars, target, preds_bench, preds_text)
# Bind all predictions together and rename for pretty plots
all_preds_df <- rbind(orig_preds_df, ols_preds_df, midas_preds_df) %>%
  mutate(text_vars_only = str_remove(str_remove(text_vars, bench_vars), ",$")) %>%
  mutate(metric_name = case_when(metric_name == "stability" ~ "CGLM",
                                 metric_name == "vader" ~ "VADER",
                                 metric_name == "econlex" ~ "ECONLEX")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, "metric_mly_m3_1lag, metric_mly_m1, metric_mly_m2, metric_mly_m3", "Monthly Text (-1:3)")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, "metric_mly_m3_1lag, metric_cum", "Daily Cumulative Text + lag")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, "metric_cum, metric_diff", "Daily Cumulative Text + diff")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, "metric_cum", "Daily Cumulative Text")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, "metric_daily", "Daily Text")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, "metric_diff", "Text First Diff")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, "metric_rolling", "30-day Rolling Average Text")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, "_90", " (90 lags)")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, "_180", " (180 lags)")) %>%
  mutate(text_vars_only = str_replace_all(text_vars_only, ", no pmi", ", no PMI")) %>%
  mutate(text_vars_only = str_remove(str_squish(text_vars_only), ",$")) %>%
  mutate(bench_vars = str_replace_all(bench_vars, "pmi_m0_latest, pmi_m1_latest, pmi_m2_latest, pmi_m3_latest", "Monthly PMI (-1:3)")) %>%
  mutate(bench_vars = str_replace_all(bench_vars, "pmi_monthly_6", "Monthly PMI (6 lags)")) %>%
  mutate(bench_vars = str_replace_all(bench_vars, "pmi_daily_180", "Daily PMI (180 lags)")) %>%
  mutate(bench_vars = str_replace_all(bench_vars, "pmi_daily_90", "Daily PMI (90 lags)")) %>%
  mutate(bench_vars = str_replace_all(bench_vars, "pmi_daily", "Quasi-daily PMI")) %>%
  mutate(bench_vars = str_replace_all(bench_vars, "pmi_diff", "PMI First Diff")) %>%
  mutate(bench_vars = str_replace_all(
    bench_vars, "macro_factor_m1, macro_factor_m2, macro_factor_m3, macro_factor_m1_1lag, macro_factor_m2_1lag, macro_factor_m3_1lag", "Monthly Factors (-3:3)")) %>%
  mutate(bench_vars = str_replace_all(bench_vars, "macro_factor_m1, macro_factor_m2, macro_factor_m3", "Monthly Factors (1:3)")) %>%
  mutate(bench_vars = str_replace_all(bench_vars, "macro_factor_qly", "Quarterly Factor")) %>%
  mutate(bench_vars = str_replace_all(bench_vars, "corp_spr, eurostoxx325", "Daily Financial")) %>%
  mutate(model = str_replace_all(model, "lm", "OLS")) %>%
  mutate(model = str_replace_all(model, "aOLSon", "almon")) %>%
  mutate(model = str_replace_all(model, "AvML", "ML Average")) %>%
  mutate(model = str_replace_all(model, "NN", "Neural Network")) %>%
  mutate(model = str_replace_all(model, "ridge", "Ridge Regression")) %>%
  mutate(model = str_replace_all(model, "rf", "Random Forest")) %>%
  mutate(model = str_replace_all(model, "boosting", "Gradient Boosting"))



"
Define colors etc for use in plots later 
"
text_colors <- c("Monthly Text (-1:3)" = "blue3", "Daily Cumulative Text" = "firebrick2", "30-day Rolling Average Text" = "darkmagenta", 
                 "Daily Cumulative Text + lag" = "darkolivegreen", "Daily Cumulative Text + diff" = "chartreuse",
                 "OLS: Monthly Text (-1:3)" = "blue3", "OLS: Daily Cumulative Text" = "firebrick2", "OLS: 30-day Rolling Average Text" = "darkmagenta", 
                 "OLS: Daily Cumulative Text + lag" = "darkolivegreen", "OLS: Daily Cumulative Text + diff" = "chartreuse",
                 "MIDAS: Daily Text (90 lags)" = "aquamarine", "MIDAS: Daily Cumulative Text (90 lags)" = "firebrick2", 
                 "MIDAS (almon): Daily Text (90 lags)" = "aquamarine", "MIDAS (almon): Daily Cumulative Text (90 lags)" = "firebrick2",
                 "MIDAS (almon): Daily Text" = "aquamarine", "MIDAS (almon): Daily Cumulative Text" = "firebrick2",
                 "MIDAS (legendre): Daily Text (90 lags)" = "aquamarine", "MIDAS (legendre): Daily Cumulative Text (90 lags)" = "firebrick2",
                 "MIDAS (legendre): Daily Text" = "aquamarine", "MIDAS (legendre): Daily Cumulative Text" = "firebrick2")


pmi_shapes <- c("MIDAS PMI" = 8, "Quasi-daily PMI" = 16, "Monthly PMI (-1:3)" = 1,
                "MIDAS: Monthly PMI (6 lags)" = 8, "OLS: Quasi-daily PMI" = 16, "OLS: Monthly PMI (-1:3)" = 1,
                "MIDAS (almon): Monthly PMI (6 lags)" = 8, "MIDAS (legendre): Monthly PMI (6 lags)" = 8)

model_colors <- c("OLS" = "black", "ML Average" = "aquamarine", "Neural Network" = "bisque3", "MIDAS" = "darkolivegreen",
                  "MIDAS (almon)" = "darkolivegreen1", "MIDAS (legendre)" = "darkolivegreen3",
                  "Ridge Regression" = "chartreuse", "Random Forest" = "chocolate1", "Gradient Boosting" = "darkorchid1")

bench_colors <- c("ECB projection" = "darkgoldenrod2", "Dynamic Factor Model" = "blue3", "OLS: Daily Financial" = "darkmagenta",
                  "OLS: Monthly Factors (1:3)" = "firebrick1",  "OLS: Quarterly Factor" = "deeppink3", 
                  "OLS: Quasi-daily PMI" = "darkolivegreen", "OLS: Monthly PMI (-1:3)" = "forestgreen", 
                  "MIDAS: Monthly PMI (6 lags)" = "chartreuse", 
                  "MIDAS (almon): Monthly PMI (6 lags)" = "chartreuse3", "MIDAS (legendre): Monthly PMI (6 lags)" = "chartreuse4", 
                  "Ridge: Quasi-daily PMI" = "aquamarine")

scale_illus_plots <- list(
  scale_color_manual("Models", values = c("Text model MSE" = "forestgreen", "Text Ridge model MSE" = "forestgreen", 
                                          "PMI model MSE" = "firebrick2", "PMI Ridge model MSE" = "firebrick2", 
                                          "PMI OLS model MSE" =  "blue", 
                                          "ECB projection MSE" = "darkgoldenrod2")),
  scale_fill_manual("Text vs PMI", values = adjustcolor(c("PMI advantage" = "firebrick2",
                                                          "Text advantage" = "forestgreen"), alpha.f = 0.5)))

scale_nowcasts_plt <- list(
  scale_color_manual("Legend", 
                     values = c("GDP growth" = "black", "ECB projection" = "darkgoldenrod2", "DFM nowcast" = "blue3",
                                "Text Ridge model" =  "forestgreen", "PMI Ridge model" = "firebrick2",
                                "Text ML Average model" =  "forestgreen", "PMI ML Average model" = "firebrick2",
                                "Text model" =  "forestgreen", "Factors model" =  "firebrick2", 
                                "Average ML with Projection and diff" = "purple")))


bench_shapes <- c("Monthly Factors" = 0, "Quasi-daily PMI, Monthly PMI (-1:3),\n Daily Financial, Monthly Factors" = 3, 
                  "Daily Financial" = 6, 
                  "MIDAS PMI" = 11, "Quasi-daily PMI" = 16, "Monthly PMI (-1:3)" = 4)

vars_shapes <- c("Text" = 16, "PMI" = 3)

tabyl(all_preds_df, bench_vars)


"
Calculate mse for all models and periods
"
pre20_mse_df <- all_preds_df %>%
  filter(doq <= 90) %>%
  filter(date < "2020-01-01") %>%
  mutate(period = "pre-2020") %>%
  group_by(period, model, metric_name, text_vars, text_vars_only, bench_vars, target, doq) %>%
  summarise(mse_text_final = mean((preds_text - growth_final)^2), 
            mse_bench_final = mean((preds_bench - growth_final)^2),
            mse_text_first = mean((preds_text - growth_first)^2), 
            mse_bench_first = mean((preds_bench - growth_first)^2), 
            mse_ECB_final = mean((ECB_proj_latest - growth_final)^2),
            mse_ECB_first = mean((ECB_proj_latest - growth_first)^2),
            mse_DFM_final = mean((DFM_fcst - growth_final)^2),
            mse_DFM_first = mean((DFM_fcst - growth_first)^2),
            mse_DFM_short_final = mean((DFM_short_fcst - growth_final)^2),
            mse_DFM_short_first = mean((DFM_short_fcst - growth_first)^2))
GR_mse_df <- all_preds_df %>%
  filter(doq <= 90) %>%
  filter(year >= 2006 & year <= 2009) %>%
  mutate(period = "2006-2009") %>%
  group_by(period, model, metric_name, text_vars, text_vars_only, bench_vars, target, doq) %>%
  summarise(mse_text_final = mean((preds_text - growth_final)^2), 
            mse_bench_final = mean((preds_bench - growth_final)^2),
            mse_text_first = mean((preds_text - growth_first)^2), 
            mse_bench_first = mean((preds_bench - growth_first)^2), 
            mse_ECB_final = mean((ECB_proj_latest - growth_final)^2),
            mse_ECB_first = mean((ECB_proj_latest - growth_first)^2),
            mse_DFM_final = mean((DFM_fcst - growth_final)^2),
            mse_DFM_first = mean((DFM_fcst - growth_first)^2),
            mse_DFM_short_final = mean((DFM_short_fcst - growth_final)^2),
            mse_DFM_short_first = mean((DFM_short_fcst - growth_first)^2))
GL_mse_df <- all_preds_df %>%
  filter(doq <= 90) %>%
  filter(year == 2020) %>%
  mutate(period = "2020") %>%
  group_by(period, model, metric_name, text_vars, text_vars_only, bench_vars, target, doq) %>%
  summarise(mse_text_final = mean((preds_text - growth_final)^2), 
            mse_bench_final = mean((preds_bench - growth_final)^2),
            mse_text_first = mean((preds_text - growth_first)^2), 
            mse_bench_first = mean((preds_bench - growth_first)^2), 
            mse_ECB_final = mean((ECB_proj_latest - growth_final)^2),
            mse_ECB_first = mean((ECB_proj_latest - growth_first)^2),
            mse_DFM_final = mean((DFM_fcst - growth_final)^2),
            mse_DFM_first = mean((DFM_fcst - growth_first)^2),
            mse_DFM_short_final = mean((DFM_short_fcst - growth_final)^2),
            mse_DFM_short_first = mean((DFM_short_fcst - growth_first)^2))
mse_df <- rbind(pre20_mse_df, GR_mse_df, GL_mse_df)


"
Correlation with errors
"

cor_se <- function(x){
  unname(sqrt((1 - x$estimate^2)/x$parameter))
}

euro_df %>%
  mutate(ECB_error = growth_final - ECB_proj_latest) %>%
  mutate(DFM_error = growth_final - DFM_fcst) %>%
  mutate(DFM_short_error = growth_final - DFM_short_fcst) %>%
  pivot_longer(cols = c(ECB_error, DFM_error, DFM_short_error), names_to = "benchmark", values_to = "error") %>%
  filter(date <= "2019-12-31") %>%
  ggplot(aes(x = date)) + theme_bw() + facet_wrap(~benchmark) + 
  geom_line(aes(y = standardise(growth_final))) + 
  #geom_line(aes(y = ECB_proj_latest, color = "ECB")) + 
  #geom_line(aes(y = DFM_fcst, color = "DFM")) + 
  geom_line(aes(y = standardise(error), color = "error")) + 
  geom_line(aes(y = standardise(vader_cum), color = "VADER")) + 
  geom_line(aes(y = standardise(stability_cum), color = "CGLM"))

euro_df %>%
  mutate(ECB_error = growth_final - ECB_proj_latest) %>%
  mutate(DFM_error = growth_final - DFM_fcst) %>%
  mutate(DFM_short_error = growth_final - DFM_short_fcst) %>%
  mutate(stability_rolling = rollmean(stability_sum/nwords, k = 30, align = "right", fill = NA)) %>%
  mutate(vader_rolling = rollmean(vader_sum/nsent, k = 30, align = "right", fill = NA)) %>%
  pivot_longer(cols = c(ECB_error, DFM_error), names_to = "benchmark", values_to = "error") %>%
  filter(date <= "2019-12-31") %>%
  filter(date <= "2009-12-31") %>%
  filter(!is.na(error)) %>%
  group_by(benchmark,doq) %>%
  filter(doq <= 90) %>%
  summarise(vader_cor = cor(vader_cum, error),
            vader_cor_se = cor_se(cor.test(vader_cum, error)),
            stability_cor = cor(stability_cum, error),
            stability_cor_se = cor_se(cor.test(vader_cum, error))) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~benchmark) + 
  geom_line(aes(y = vader_cor, color = "VADER")) +
  geom_ribbon(aes(ymin = vader_cor - 1.96*vader_cor_se, ymax = vader_cor + 1.96*vader_cor_se, fill = "VADER"), alpha = 0.1) +
  geom_ribbon(aes(ymin = vader_cor - 1.645*vader_cor_se, ymax = vader_cor + 1.645*vader_cor_se, fill = "VADER"), alpha = 0.3) +
  geom_line(aes(y = stability_cor, color = "CGLM")) + 
  geom_ribbon(aes(ymin = stability_cor - 1.96*stability_cor_se, ymax = stability_cor + 1.96*stability_cor_se, fill = "CGLM"), alpha = 0.1) +
  geom_ribbon(aes(ymin = stability_cor - 1.645*stability_cor_se, ymax = stability_cor + 1.645*stability_cor_se, fill = "CGLM"), alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  labs(x = "Day of Quarter", y = "Correlation", color = "Text metric")


euro_df %>%
  mutate(DFM_error = growth_final - DFM_fcst) %>%
  filter(date <= "2019-12-31") %>%
  #filter(date <= "2009-12-31") %>%
  filter(!is.na(DFM_error)) %>%
  dplyr::select(date, doq, DFM_error, vader_cum, stability_cum) %>%
  pivot_longer(cols = c(vader_cum, stability_cum), names_to = "metric") %>%
  mutate(metric = case_when(metric == "stability_cum" ~ "CGLM",
                            metric == "vader_cum" ~ "VADER")) %>%
  group_by(metric, doq) %>%
  filter(doq <= 90) %>%
  summarise(cor = cor(value, DFM_error),
            cor_se = cor_se(cor.test(value, DFM_error))) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric) + 
  geom_line(aes(y = cor)) +
  geom_ribbon(aes(ymin = cor - 1.96*cor_se, ymax = cor + 1.96*cor_se), alpha = 0.1) +
  geom_ribbon(aes(ymin = cor - 1.645*cor_se, ymax = cor + 1.645*cor_se), alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  labs(x = "Day of Quarter", y = "Correlation with DFM error")
ggsave("figures/DFM_error_corr_pre20.pdf", width = 9, height = 3)

euro_df$DFM_error <- euro_df$growth_final - euro_df$DFM_fcst
cor.test(euro_df$stability_cum[which(euro_df$year<2020)], euro_df$DFM_error[which(euro_df$year<2020)]) 
cor.test(euro_df$vader_cum[which(euro_df$year<2020)], euro_df$DFM_error[which(euro_df$year<2020)])
euro_df$ECB_error <- euro_df$growth_final - euro_df$ECB_proj_latest

euro_df %>%
  mutate(ECB_error = growth_final - ECB_proj_latest) %>%
  filter(date >= "2019-12-31") %>%
  #filter(date <= "2009-12-31") %>%
  filter(!is.na(ECB_error)) %>%
  dplyr::select(date, doq, ECB_error, vader_cum, stability_cum) %>%
  pivot_longer(cols = c(vader_cum, stability_cum), names_to = "metric") %>%
  mutate(metric = case_when(metric == "stability_cum" ~ "CGLM",
                            metric == "vader_cum" ~ "VADER")) %>%
  group_by(metric, doq) %>%
  filter(doq <= 90) %>%
  summarise(cor = cor(value, ECB_error),
            cor_se = cor_se(cor.test(value, ECB_error))) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric) + 
  geom_line(aes(y = cor)) +
  geom_ribbon(aes(ymin = cor - 1.96*cor_se, ymax = cor + 1.96*cor_se), alpha = 0.1) +
  geom_ribbon(aes(ymin = cor - 1.645*cor_se, ymax = cor + 1.645*cor_se), alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  labs(x = "Day of Quarter", y = "Correlation with ECB error")
#ggsave("figures/illus_example/ECB_error_corr_all.pdf", width = 9, height = 3)

"
Illustrative linear example
"
p1 <- pre20_mse_df %>%
  filter(model == "OLS" & text_vars == "metric_cum, pmi_daily" & target == "growth_latest" &
           metric_name %in% c("CGLM", "VADER")) %>%
  mutate(text_up = case_when(mse_text_final >= mse_bench_final ~ mse_text_final, TRUE ~ NA_real_),
         text_down = case_when(mse_text_final < mse_bench_final ~ mse_text_final, TRUE ~ NA_real_)) %>%
  ggplot(aes(x= doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_illus_plots + 
  geom_line(aes(y=mse_bench_final, color = "PMI model MSE")) +
  geom_line(aes(y=mse_text_final, color = paste0("Text model MSE"))) +
  geom_ribbon(aes(ymin = text_up, ymax = mse_bench_final, fill= "PMI advantage"), show.legend = FALSE) + 
  geom_ribbon(aes(ymin = text_down, ymax = mse_bench_final, fill="Text advantage")) +
  #geom_line(aes(y=ols_mse, color = "PMI OLS model MSE")) +
  geom_line(aes(y=mse_ECB_final, color = "ECB projection MSE")) +
  labs(x = "", y = "MSE (pre-2020)", title = "") 
p2 <- pre20_mse_df %>%
  filter(model == "OLS" & text_vars == "metric_cum, pmi_daily" & target == "growth_latest" &
           metric_name %in% c("CGLM", "VADER")) %>%
  mutate(text_up = case_when(mse_text_final >= mse_bench_final ~ "PMI advantage", TRUE ~ "Text advantage"),
         bench_decr = 100*(mse_bench_final - mse_text_final)/mse_bench_final) %>%
  ggplot(aes(x= doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_illus_plots + 
  geom_col(aes(y= bench_decr, fill = text_up)) +
  geom_smooth(aes(y =bench_decr), method = "loess", se = FALSE, color = "black") +
  labs(x = "Day of quarter", y = "Decrease (%)")
ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")
ggsave("figures/text_v_pmi_illus_pre20.pdf", width = 9, height = 5)

p1 <- GR_mse_df %>%
  filter(model == "OLS" & text_vars == "metric_cum, pmi_daily" & target == "growth_latest" &
           metric_name %in% c("CGLM", "VADER")) %>%
  mutate(text_up = case_when(mse_text_final >= mse_bench_final ~ mse_text_final, TRUE ~ NA_real_),
         text_down = case_when(mse_text_final < mse_bench_final ~ mse_text_final, TRUE ~ NA_real_)) %>%
  ggplot(aes(x= doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_illus_plots + 
  geom_line(aes(y=mse_bench_final, color = "PMI model MSE")) +
  geom_line(aes(y=mse_text_final, color = paste0("Text model MSE"))) +
  geom_ribbon(aes(ymin = text_up, ymax = mse_bench_final, fill= "PMI advantage"), show.legend = FALSE) + 
  geom_ribbon(aes(ymin = text_down, ymax = mse_bench_final, fill="Text advantage")) +
  #geom_line(aes(y=ols_mse, color = "PMI OLS model MSE")) +
  geom_line(aes(y=mse_ECB_final, color = "ECB projection MSE")) +
  labs(x = "", y = "MSE (2006-2009)", title = "") 
p2 <- GR_mse_df %>%
  filter(model == "OLS" & text_vars == "metric_cum, pmi_daily" & target == "growth_latest" &
           metric_name %in% c("CGLM", "VADER")) %>%
  mutate(text_up = case_when(mse_text_final >= mse_bench_final ~ "PMI advantage", TRUE ~ "Text advantage"),
         bench_decr = 100*(mse_bench_final - mse_text_final)/mse_bench_final) %>%
  ggplot(aes(x= doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_illus_plots + 
  geom_col(aes(y= bench_decr, fill = text_up)) +
  geom_smooth(aes(y =bench_decr), method = "loess", se = FALSE, color = "black") +
  labs(x = "Day of quarter", y = "Decrease (%)")
ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")
ggsave("figures/illus_example/text_v_pmi_illus_GR.pdf", width = 9, height = 5)




"
Compare the different aggregation of text metrics
"
## Pre-2020
p1 <- pre20_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "MIDAS (legendre)") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_line(aes(y = mse_ECB_final), color = "darkgoldenrod2") + 
  geom_point(aes(y = mse_text_final, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  coord_cartesian(ylim = c(0.1, 0.385)) +
  labs(x = "Day of Quarter", y = "MSE (pre-2020)", color = "Text aggregation", shape = "Model")
p1
ggsave("figures/text_v_pmi_mse_pre20.pdf", width = 12, height = 4)
pre20_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "MIDAS (legendre)") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  filter(!str_detect(text_vars_only, "no PMI") & !str_detect(text_vars_only, "Average Text \\(90 lags\\)")) %>%
  mutate(mse_decr = 100*(mse_bench_final - mse_text_final)/mse_bench_final) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  geom_smooth(aes(y = mse_decr), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(-10, 20)) +
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Text aggregation", shape = "Model")
ggsave("text_v_pmi/text_v_pmi_gain_pre20.pdf", width = 12, height = 4)
p2 <- pre20_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  mutate(mse_decr = 100*(mse_bench_final - mse_text_final)/mse_bench_final) %>%
  mutate(mse_decr_nomidas = case_when(model == "OLS" ~ mse_decr, TRUE ~ NA_real_)) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), 
             size = 0.85, alpha = 0.5) + 
  geom_point(aes(y = mse_decr_nomidas, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), 
             size = 0.85) + 
  geom_smooth(aes(y = mse_decr_nomidas), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(-10, 20)) +
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Text aggregation", shape = "Model")
ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")
ggsave("figures/text_v_pmi_pre20.pdf", width = 12, height = 7)



## Great Recession
p1 <- GR_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "MIDAS (legendre)") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_line(aes(y = mse_ECB_final), color = "darkgoldenrod2") + 
  geom_point(aes(y = mse_text_final, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  coord_cartesian(ylim = c(0.25, 1.1)) +
  labs(x = "Day of Quarter", y = "MSE (2006-2009)", color = "Text aggregation", shape = "PMI aggregation")
p1
ggsave("figures/text_v_pmi_mse_GR.pdf", width = 12, height = 4)
GR_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "MIDAS (legendre)") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  mutate(mse_decr = 100*(mse_bench_final - mse_text_final)/mse_bench_final) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  geom_smooth(aes(y = mse_decr), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(-15, 22)) +
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Text aggregation", shape = "PMI aggregation")
ggsave("figures/text_v_pmi_gain_GR.pdf", width = 12, height = 4)
p2 <- GR_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  mutate(mse_decr = 100*(mse_bench_final - mse_text_final)/mse_bench_final) %>%
  mutate(mse_decr_nomidas = case_when(model == "OLS" ~ mse_decr, TRUE ~ NA_real_)) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  geom_smooth(aes(y = mse_decr_nomidas), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(-15, 22)) +
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Text aggregation", shape = "PMI aggregation")
ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")
ggsave("figures/text_v_pmi/text_v_pmi_GR.pdf", width = 12, height = 7)



"
Compare the different aggregation of text metrics for first revision
"
## Pre-2020
p1 <- pre20_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "MIDAS (legendre)") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_line(aes(y = mse_ECB_first), color = "darkgoldenrod2") + 
  geom_point(aes(y = mse_text_first, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  coord_cartesian(ylim = c(0.1, 0.33)) +
  labs(x = "Day of Quarter", y = "MSE v flash estimate (pre-2020)", color = "Text aggregation", shape = "Model")
p1
ggsave("figures/text_v_pmi_mse_pre20_flash.pdf", width = 12, height = 4)
pre20_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "MIDAS (legendre)") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  mutate(mse_decr = 100*(mse_bench_first - mse_text_first)/mse_bench_first) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  geom_smooth(aes(y = mse_decr), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(-10, 20)) +
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Text aggregation", shape = "Model")
ggsave("figures/text_v_pmi_gain_pre20_flash.pdf", width = 12, height = 4)
p2 <- pre20_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  mutate(mse_decr = 100*(mse_bench_first - mse_text_first)/mse_bench_first) %>%
  mutate(mse_decr_nomidas = case_when(model == "OLS" ~ mse_decr, TRUE ~ NA_real_)) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), 
             size = 0.85, alpha = 0.5) + 
  geom_point(aes(y = mse_decr_nomidas, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), 
             size = 0.85) + 
  geom_smooth(aes(y = mse_decr_nomidas), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(-10, 20)) +
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Text aggregation", shape = "Model")
ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")
ggsave("figures/text_v_pmi_pre20_flash.pdf", width = 12, height = 7)



## Great Recession
p1 <- GR_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "MIDAS (legendre)") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_line(aes(y = mse_ECB_first), color = "darkgoldenrod2") + 
  geom_point(aes(y = mse_text_first, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  coord_cartesian(ylim = c(0.25, 1.1)) +
  labs(x = "Day of Quarter", y = "MSE v flash estimate (2006-2009)", color = "Text aggregation", shape = "PMI aggregation")
p1
ggsave("figures/text_v_pmi_mse_GR_flash.pdf", width = 12, height = 4)
GR_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "MIDAS (legendre)") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  mutate(mse_decr = 100*(mse_bench_first - mse_text_first)/mse_bench_first) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  geom_smooth(aes(y = mse_decr), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(-15, 22)) +
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Text aggregation", shape = "PMI aggregation")
ggsave("figures/text_v_pmi_gain_GR_flash.pdf", width = 12, height = 4)
p2 <- GR_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Monthly PMI (6 lags)")) %>%
  filter(!str_detect(text_vars_only, "no PMI") & 
           (!str_detect(model, "MIDAS") | str_detect(text_vars_only, "Daily Text \\(90 lags\\)"))) %>%
  mutate(mse_decr = 100*(mse_bench_first - mse_text_first)/mse_bench_first) %>%
  mutate(mse_decr_nomidas = case_when(model == "OLS" ~ mse_decr, TRUE ~ NA_real_)) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = str_c(model, ": ", text_vars_only), shape = str_c(model, ": ", bench_vars)), size = 0.85) + 
  geom_smooth(aes(y = mse_decr_nomidas), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(-15, 22)) +
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Text aggregation", shape = "PMI aggregation")
ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")
ggsave("figures/text_v_pmi_GR_flash.pdf", width = 12, height = 7)



"
ECONLEX results
"
mse_df %>%
  filter(metric_name %in% c("ECONLEX") & period %in% c("2006-2009", "pre-2020")) %>%
  mutate(period = factor(period, ordered = T, levels = c("2006-2009", "pre-2020"))) %>%
  mutate(main_bench = case_when(text_vars == "metric_cum, pmi_daily" ~ 1, 
                                text_vars == "metric_cum, pmi_m0_latest, pmi_m1_latest, pmi_m2_latest, pmi_m3_latest" ~ 1, TRUE ~ NA_real_)) %>%
  filter(model == "OLS" & target == "growth_latest") %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI")) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~period+metric_name, scales = "free_y") + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_line(aes(y = mse_ECB_final), color = "darkgoldenrod2") + 
  geom_point(aes(y = mse_text_final, color = text_vars_only, shape = bench_vars)) + 
  labs(title = "ECONLEX results", x = "Day of Quarter", y = "MSE", color = "Text aggregation", shape = "PMI aggregation")
ggsave("figures/text_v_pmi_mse_econlex.pdf", width = 12, height = 4)
mse_df %>%
  filter(metric_name %in% c("ECONLEX") & period %in% c("2006-2009", "pre-2020")) %>%
  mutate(period = factor(period, ordered = T, levels = c("2006-2009", "pre-2020"))) %>%
  mutate(mse_decr = 100*(mse_bench_final - mse_text_final)/mse_bench_final) %>%
  filter(model == "OLS" & target == "growth_latest") %>%
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI")) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~period+metric_name, scales = "free_y") + 
  scale_color_manual(values = text_colors) + scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = text_vars_only, shape = bench_vars)) + 
  geom_smooth(aes(y = mse_decr), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  coord_cartesian(ylim = c(-15, 20)) +
  labs(title = "ECONLEX results", x = "Day of Quarter", y = "Decrease in MSE (%)", color = "Text aggregation", shape = "PMI aggregation")
ggsave("figures/text_v_pmi_gain_econlex.pdf", width = 12, height = 4)



"
MIDAS models
"
## PMI models
mse_df %>%
  mutate(period = factor(period, ordered = T, levels = c("pre-2020", "2006-2009", "2020"))) %>%
  filter((model %in% c("MIDAS (legendre)", "MIDAS (almon)", "MIDAS") | 
            (model == "OLS" & bench_vars == "Quasi-daily PMI" & text_vars_only == "Daily Cumulative Text")) & 
            target == "growth_latest") %>% # & target != "growth_latest") %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~period, scales = "free_y") +
  #scale_color_manual(values = bench_colors) + 
  geom_line(aes(y = mse_ECB_final), color = "darkgoldenrod2") + 
  #geom_line(aes(y = mse_DFM_final, color = "DFM nowcast")) + 
  geom_line(aes(y = mse_bench_final, color = str_c(model, ": ", bench_vars))) + 
  labs(x = "Day of Quarter", y = "MSE", color = "Benchmark Model")

mse_df %>%
  mutate(period = factor(period, ordered = T, levels = c("pre-2020", "2006-2009", "2020"))) %>%
  filter(model %in% c("MIDAS (legendre)", "MIDAS (almon)") & target == "growth_latest") %>% # & target != "growth_latest") %>%
  
  filter(bench_vars %in% c("Monthly PMI (-1:3)", "Quasi-daily PMI", "Daily PMI (90 lags)", "Monthly PMI (6 lags)")) %>%
  filter(text_vars_only %in% c("Daily Cumulative Text", "Daily Text (90 lags)",
                               "Daily Cumulative Text (90 lags)")) %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  mutate(text_vars_only = case_when(model ==  "MIDAS (almon)" ~ "Daily Text (90 lags)", 
                                    TRUE ~ text_vars_only)) %>%
  filter(text_vars_only %in% c("Daily Text (90 lags)", "Daily Cumulative Text (90 lags)")) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~ metric_name + period, scales = "free_y") +
  scale_color_manual(values = c("ECB projection" = "darkgoldenrod2", 
                                "MIDAS (almon): Monthly PMI (6 lags)" = "blue3",
                                "MIDAS (legendre): Monthly PMI (6 lags)" = "forestgreen",
                                "MIDAS (legendre): Daily PMI (90 lags)" = "firebrick")) + 
  geom_line(aes(y = mse_ECB_final, color = "ECB projection")) + 
  geom_line(aes(y = mse_bench_final, color = str_c(model, ": ", bench_vars))) + 
  geom_point(aes(y = mse_text_final, color = str_c(model, ": ", bench_vars), shape = text_vars_only), size = 0.85) + 
  labs(x = "Day of Quarter", y = "MSE", color = "Benchmark model", shape = "Text variable")
ggsave("figures/comparing_midas_benchmarks.pdf", width = 12, height = 6.5)



"
ML models pre-2020
"
## Pre-2020
pre20_mse_df %>%
  filter(model %in% c("Neural Network", "Random Forest", "Gradient Boosting",
                      "Ridge Regression", "Neural Network", "OLS")) %>%
  filter(text_vars =="metric_cum, pmi_daily" & target == "growth_latest") %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = model_colors) + scale_shape_manual(values = vars_shapes) + 
  geom_point(aes(y = sqrt(mse_text_final), color = model, shape = "Text")) + 
  geom_point(aes(y = sqrt(mse_bench_final), color = model, shape = "PMI")) + 
  geom_line(aes(y = sqrt(mse_ECB_final)), color = "darkgoldenrod2") + 
  coord_cartesian(ylim = c(0.3, 1.1)) +
  labs(title = "pre-2020", x = "Day of Quarter", y = "RMSE", color = "Model", shape = "Variables")
ggsave("figures/text_v_pmi_ML_mse_pre20.pdf", width = 12, height = 4)
## Great Recession
GR_mse_df %>%
  filter(model %in% c("Neural Network", "Random Forest", "Gradient Boosting",
                      "Ridge Regression", "Neural Network", "OLS")) %>%
  filter(text_vars == "metric_cum, pmi_daily" & target == "growth_latest") %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = model_colors) + scale_shape_manual(values = vars_shapes) + 
  geom_point(aes(y = sqrt(mse_text_final), color = model, shape = "Text")) + 
  geom_point(aes(y = sqrt(mse_bench_final), color = model, shape = "PMI")) + 
  geom_line(aes(y = sqrt(mse_ECB_final)), color = "darkgoldenrod2") + 
  #coord_cartesian(ylim = c(0.3, 1.1)) +
  labs(title = "2006-2009", x = "Day of Quarter", y = "RMSE", color = "Model", shape = "Variables")
ggsave("figures/text_v_pmi_ML_mse_GR.pdf", width = 12, height = 4)


"
GR ridge results
"
p1 <- GR_mse_df %>%
  filter(model %in% c("Ridge Regression", "OLS")) %>%
  filter(text_vars == "metric_cum, pmi_daily" & target == "growth_latest" &
           metric_name %in% c("CGLM", "VADER")) %>%
  group_by(metric_name, doq) %>%
  mutate(mse_OLSbench_final = max((model == "OLS")*mse_bench_final)) %>%
  filter(model == "Ridge Regression") %>% ungroup() %>%
  mutate(text_up = case_when(mse_text_final >= mse_bench_final ~ mse_text_final, TRUE ~ NA_real_),
         text_down = case_when(mse_text_final < mse_bench_final ~ mse_text_final, TRUE ~ NA_real_)) %>%
  ggplot(aes(x= doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_illus_plots + 
  geom_line(aes(y=mse_bench_final, color = "PMI Ridge model MSE")) +
  geom_line(aes(y=mse_text_final, color = paste0("Text Ridge model MSE"))) +
  geom_ribbon(aes(ymin = text_up, ymax = mse_bench_final, fill= "PMI advantage"), show.legend = FALSE) + 
  geom_ribbon(aes(ymin = text_down, ymax = mse_bench_final, fill="Text advantage")) +
  geom_line(aes(y=mse_OLSbench_final, color = "PMI OLS model MSE")) +
  geom_line(aes(y=mse_ECB_final, color = "ECB projection MSE")) +
  labs(x = "", y = "MSE (2006-2009)", title = "") 
p2 <- GR_mse_df %>%
  filter(model %in% c("Ridge Regression")) %>%
  filter(text_vars == "metric_cum, pmi_daily" & target == "growth_latest" &
           metric_name %in% c("CGLM", "VADER")) %>%
  mutate(text_up = case_when(mse_text_final >= mse_bench_final ~ "PMI advantage", TRUE ~ "Text advantage"),
         bench_decr = 100*(mse_bench_final - mse_text_final)/mse_bench_final) %>%
  ggplot(aes(x= doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_illus_plots + 
  geom_col(aes(y= bench_decr, fill = text_up)) +
  geom_smooth(aes(y =bench_decr), method = "loess", se = FALSE, color = "black") +
  coord_cartesian(ylim = c(-30,50)) +
  labs(x = "Day of quarter", y = "Decrease (%)")
ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")
ggsave("figures/text_v_pmi_ridge_GR.pdf", width = 9, height = 5)



all_preds_df %>%
  filter(date >= "2006-04-01" & date <= "2009-12-31") %>%
  filter(model == "Ridge Regression") %>%
  filter(text_vars == "metric_cum, pmi_daily" & target == "growth_latest") %>%
  ggplot(aes(x = date)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_nowcasts_plt + 
  geom_line(aes(y=growth_final, color = "GDP growth")) +
  geom_line(aes(y=ECB_proj_latest, color = "ECB projection")) +
  geom_line(aes(y=preds_text, color = "Text Ridge model")) +
  geom_line(aes(y=preds_bench, color = "PMI Ridge model")) +
  labs(x = "Date", y = "Growth rate")
ggsave("figures/ridge_nowcasts_GR.pdf", width = 9, height = 3.5)



"
Macro and financial variabless
"
tabyl(mse_df, bench_vars)
## What's the best benchmark?
mse_df %>%
  mutate(period = factor(period, ordered = T, levels = c("pre-2020", "2006-2009", "2020"))) %>%
  filter(model %in% c("OLS", "MIDAS (legendre)", "Ridge Regression") & target == "growth_latest") %>% # & target != "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  mutate(model = str_remove(model, " Regression")) %>%
  filter(bench_vars %in% c("Daily Financial", "Common Factor", "Quasi-daily PMI", "Monthly PMI (6 lags)",
    "Monthly Factors (1:3)", "Monthly PMI (-1:3)", "Quarterly Factor")) %>% 
  filter(text_vars_only %in% c("Daily Cumulative Text", "Daily Text (180 lags)", "Daily Text (90 lags)")) %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  mutate(benchmark = str_c(model, ": ", bench_vars)) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~period, scales = "free_y") +
  scale_color_manual(values = bench_colors) + 
  geom_line(aes(y = mse_ECB_final, color = "ECB projection")) + 
  geom_line(aes(y = mse_DFM_final, color = "Dynamic Factor Model")) + 
  geom_line(aes(y = mse_bench_final, color = str_c(model, ": ", bench_vars))) + 
  labs(x = "Day of Quarter", y = "MSE", color = "Benchmark Model")
ggsave("figures/best_benchmarks.pdf", width = 12, height = 4)

## Just the daily cumulative 
mse_df %>%
  filter(period %in% c("2006-2009", "pre-2020")) %>%
  mutate(period = factor(period, ordered = T, levels = c("pre-2020", "2006-2009", "2020"))) %>%
  mutate(mse_decr = 100*(mse_bench_final - mse_text_final)/mse_bench_final) %>%
  filter(model %in% c("OLS") & target == "growth_latest") %>%
  filter(text_vars_only %in% c("Daily Cumulative Text")) %>%
  filter(bench_vars %in% c(
    "Monthly Factors (1:3)",
    "Quarterly Factor")) %>% 
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  mutate(bench_vars = case_when(target == "proj_fe" ~ str_c(bench_vars, ", ECB projection"), TRUE ~ bench_vars)) %>%
  arrange(text_vars_only) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~period + metric_name, scales = "free_y") + 
  scale_color_manual(values = text_colors) + 
  geom_point(aes(y = mse_decr, color = text_vars_only, shape = bench_vars)) + 
  geom_smooth(aes(y = mse_decr), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  #coord_cartesian(ylim = c(-18, 10)) +
  labs(title = "", x = "Day of Quarter", y = "Decrease in MSE (%)", color = "Text aggregation", shape = "Benchmark variables")
ggsave("figures/text_v_macro_gain_cum.pdf", width = 12, height = 5)



"
2020 results
"
## All models perform worse than ECB projections in 2020 if they just target growth
p1 <- GL_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "Gradient Boosting", "Neural Network", 
                      "Random Forest", "Ridge Regression", "ML Average") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(text_vars_only %in% c("Daily Cumulative Text", "Daily Cumulative Text + diff")) %>%
  filter(bench_vars %in% c("Quasi-daily PMI", "Quasi-daily PMI, PMI First Diff")) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = model_colors) + #scale_shape_manual(values = pmi_shapes) + 
  geom_line(aes(y = mse_ECB_final), color = "darkgoldenrod2") + 
  geom_point(aes(y = mse_text_final, color = model, shape = text_vars_only), size = 0.85) + 
  labs(x = "Day of Quarter", y = "MSE (2020)", color = "Model", shape = "Text aggregation")
p2 <- GR_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "Gradient Boosting", "Neural Network", 
                      "Random Forest", "Ridge Regression", "ML Average") & target == "growth_latest") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(text_vars_only %in% c("Daily Cumulative Text", "Daily Cumulative Text + diff")) %>%
  filter(bench_vars %in% c("Quasi-daily PMI", "Quasi-daily PMI, PMI First Diff")) %>%
  mutate(mse_ECB_bench_final = case_when(mse_bench_final <= mse_ECB_final ~ mse_bench_final, 
                                         mse_bench_final > mse_ECB_final ~ mse_ECB_final)) %>%
  mutate(mse_decr = 100*(mse_ECB_bench_final - mse_text_final)/mse_ECB_bench_final) %>%  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = model_colors) + #scale_shape_manual(values = pmi_shapes) + 
  ylim(c(-50, 100)) +
  geom_point(aes(y = mse_decr, color = model, shape = str_c(text_vars_only)), size = 0.85) + 
  geom_smooth(aes(y = mse_decr), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Model", shape = "Text aggregation")
ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")
ggsave("figures/2020_growth_target_GL.pdf", width = 12, height = 7)


## When targeting ECB projection error in 2020 we get better performance
p1 <- GL_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "Gradient Boosting", "Neural Network", 
                      "Random Forest", "Ridge Regression", "ML Average") & target == "proj_fe") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(text_vars_only %in% c("Daily Cumulative Text", "Daily Cumulative Text + diff")) %>%
  filter(bench_vars %in% c("Quasi-daily PMI", "Quasi-daily PMI, PMI First Diff")) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = model_colors) + #scale_shape_manual(values = pmi_shapes) + 
  geom_line(aes(y = mse_ECB_final), color = "darkgoldenrod2") + 
  geom_point(aes(y = mse_text_final, color = model, shape = text_vars_only), size = 0.85) + 
  labs(x = "Day of Quarter", y = "MSE (2020)", color = "Model", shape = "Text aggregation")
p2 <- GL_mse_df %>%
  filter(metric_name %in% c("CGLM", "VADER")) %>%
  filter(model %in% c("OLS", "Gradient Boosting", "Neural Network", 
                      "Random Forest", "Ridge Regression", "ML Average") & target == "proj_fe") %>%
  mutate(model = str_replace(model, " \\(legendre\\)", "")) %>%
  filter(text_vars_only %in% c("Daily Cumulative Text", "Daily Cumulative Text + diff")) %>%
  filter(bench_vars %in% c("Quasi-daily PMI", "Quasi-daily PMI, PMI First Diff")) %>%
  mutate(mse_ECB_bench_final = case_when(mse_bench_final <= mse_ECB_final ~ mse_bench_final, 
                                         mse_bench_final > mse_ECB_final ~ mse_ECB_final)) %>%
  mutate(mse_decr = 100*(mse_ECB_bench_final - mse_text_final)/mse_ECB_bench_final) %>%
  ggplot(aes(x = doq)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_color_manual(values = model_colors) + #scale_shape_manual(values = pmi_shapes) + 
  geom_point(aes(y = mse_decr, color = model, shape = str_c(text_vars_only)), size = 0.85) + 
  geom_smooth(aes(y = mse_decr), color = "black", se = F) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  ylim(c(-100, 100)) + 
  labs(x = "Day of Quarter", y = "Decrease (%)", color = "Model", shape = "Text aggregation")
ggarrange(p1, p2, nrow = 2, common.legend = T, legend = "right")
ggsave("figures/2020_proj_fe_GL.pdf", width = 12, height = 7)



all_preds_df %>%
  filter(date >= "2020-01-01" & date <= "2020-12-31") %>%
  filter(model %in% c("Ridge Regression")) %>%
  filter(text_vars ==  "metric_cum, metric_diff, pmi_daily, pmi_diff"  & target == "proj_fe") %>%
  ggplot(aes(x = date)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_nowcasts_plt + 
  geom_line(aes(y=growth_final, color = "GDP growth")) +
  geom_line(aes(y=ECB_proj_latest, color = "ECB projection")) +
  geom_line(aes(y=preds_text, color = "Text Ridge model")) +
  geom_line(aes(y=preds_bench, color = "PMI Ridge model")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = "Date", y = "Growth rate")
ggsave("figures/2020_ridge_nowcasts.pdf", width = 7, height = 3.5)

all_preds_df %>%
  filter(date >= "2020-01-01" & date <= "2020-12-31") %>%
  filter(model %in% c("ML Average")) %>%
  filter(text_vars ==  "metric_cum, metric_diff, pmi_daily, pmi_diff"  & target == "proj_fe") %>%
  ggplot(aes(x = date)) + theme_bw() + facet_wrap(~metric_name) + 
  scale_nowcasts_plt + 
  geom_line(aes(y=growth_final, color = "GDP growth")) +
  geom_line(aes(y=ECB_proj_latest, color = "ECB projection")) +
  geom_line(aes(y=preds_text, color = "Text ML Average model")) +
  geom_line(aes(y=preds_bench, color = "PMI ML Average model")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = "Date", y = "Growth rate")
ggsave("figures/2020_AvML_nowcasts.pdf", width = 7, height = 3.5)




"
Why are the CGLM and LM dictionaries bad in 2020?
"
## These files are the orginal news articles, so not available in the replication package. 
files_dir <- "/Users/julianashwin/Documents/Research/Clean_Data/ECB_articles/2020_translations/"
files <- dir(files_dir)
articles_df <- tibble()
for (file in files){
  temp <- read_csv(str_c(files_dir, file)) %>%
    select(date, text, source_code) %>%
    filter(date >= "2020-01-01")
  articles_df <- rbind(articles_df, temp)
}

# Dictionaries
library(readxl)
cglm_dict <- read_xlsx("data/dictionaries/financial_stability.xlsx") %>%
  rename(word = Word) %>%
  mutate(polarity = case_when(Positive == 1 ~ "Positive", Negative == 1 ~ "Negative")) %>%
  select(word, polarity)
econlex_dict <- read_csv("data/dictionaries/Economic_Lexicon.csv") %>%
  rename(word = token) %>%
  mutate(polarity = case_when(polarity == 1 ~ "Positive", polarity == -1 ~ "Negative")) %>%
  filter(!is.na(polarity)) %>%
  select(word, sentiment, polarity)
vader_dict <- read_table("data/dictionaries/vader_lexicon.txt",
                         col_names = FALSE) %>%
  rename(word = X1, sentiment = X2) %>%
  mutate(sentiment = as.numeric(sentiment)) %>%
  mutate(polarity = case_when(sentiment >= 0 ~ "Positive", sentiment < 0 ~ "Negative")) %>%
  filter(!is.na(polarity)) %>%
  select(word, sentiment, polarity) %>%
  mutate(sentiment = sentiment/5)

# Which words are selected from each dictionary?
cglm_terms_df <- articles_df %>%
  mutate(text = tolower(str_squish(str_replace_all(text, regex("\\W+"), " ")))) %>%
  unnest_tokens(word, text) %>% 
  filter(word %in% cglm_dict$word) %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  left_join(cglm_dict)
econlex_terms_df <- articles_df %>%
  mutate(text = tolower(str_squish(str_replace_all(text, regex("\\W+"), " ")))) %>%
  unnest_tokens(word, text) %>% 
  filter(word %in% econlex_dict$word) %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  left_join(econlex_dict)
vader_terms_df <- articles_df %>%
  mutate(text = tolower(str_squish(str_replace_all(text, regex("\\W+"), " ")))) %>%
  unnest_tokens(word, text) %>% 
  filter(word %in% vader_dict$word) %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  left_join(vader_dict)



econlex_terms_df %>%
  mutate(dictionary = "ECONLEX") %>%
  rbind(mutate(vader_terms_df, dictionary = "VADER")) %>%
  mutate(n = n*abs(sentiment)) %>%
  select(-sentiment) %>%
  rbind(mutate(cglm_terms_df, dictionary = "CGLM")) %>%
  group_by(dictionary, polarity) %>%
  slice_max(order_by = n, n = 12) %>% 
  mutate(order = row_number()) %>%
  filter(order <= 25) %>%
  ggplot() + theme_bw() +  facet_wrap(~polarity + dictionary, nrow = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(aes(x = 1, y = -order, label = word, size = log(n), color = polarity)) +
  scale_color_manual(values = c("Positive" = "forestgreen", "Negative" = "firebrick")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  theme(legend.position="none", axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x = "", y = "")
ggsave("figures/lexicon_comp.pdf", width = 9, height = 3.5)


"
End of script
"