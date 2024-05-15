"
This file runs the daily nowcasting exercises for the EA
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

source("plotting_fns.R")

library(foreach)
library(doParallel)
registerDoParallel(4)  # use multicore, set to the number of our cores

## Import the data for Euro Area
euro_df <- read_csv("data/euro_daily_export.csv")

## Import all vintages of the macro factor
factor_df <- read_csv("data/macrofactor.csv") %>%
  mutate(vintage = as.Date(vintage), quarter = as.Date(quarter))

factor_df <- factor_df %>%
  mutate(macro_factor_qly =  macro_factor_m1 + macro_factor_m2 + macro_factor_m3,
         macro_factor_short_qly = macro_factor_short_m1 + macro_factor_short_m2 + macro_factor_short_m3)
factor_df <- factor_df %>%
  arrange(vintage, quarter) %>%
  group_by(vintage) %>%
  mutate(macro_factor_qly_1lag = lag(macro_factor_qly, n = 1),
         macro_factor_short_qly_1lag = lag(macro_factor_short_qly, n = 1),
         macro_factor_m1_1lag = lag(macro_factor_m1, n = 1),
         macro_factor_m2_1lag = lag(macro_factor_m2, n = 1),
         macro_factor_m3_1lag = lag(macro_factor_m3, n = 1),
         macro_factor_short_m1_1lag = lag(macro_factor_short_m1, n = 1),
         macro_factor_short_m2_1lag = lag(macro_factor_short_m2, n = 1),
         macro_factor_short_m3_1lag = lag(macro_factor_short_m3, n = 1))


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
  dplyr::select(quarter, growth_first)
euro_df <- euro_df %>%
  left_join(growth_first_df, by = "quarter") 
rm(growth_first_df)

## Function to standardise data so that it is mean zero and s.d. 1
standardise<- function(series){
  new_series <- (series - mean(series,na.rm=TRUE))/sd(series, na.rm = TRUE)
  return(new_series)
}

### Function that computes historic growth rates using latest available data
create_training_data <- function(country_df, today){
  
  # today's dates for lags, and identify this quarter
  today_df <- country_df[which(country_df$date == today),]
  quart <- floor_date(as.Date(today), unit = "quarters")
  # Only use data up to previous quarter anyway
  country_tr <- filter(country_df, quarter < quart)
  country_tr$growth_latest <- NA
  country_tr$growth_1lag_latest <- NA
  # Use real time data for previous 5 quarters
  for(ii in 1:4){
    qt <- quart %m-% months(3*ii)
    obs <- which(country_tr$quarter==qt)
    country_tr$growth_latest[obs] <- (today_df[,paste0("gdp_",ii,"lag_latest")]/
                                        today_df[,paste0("gdp_",(ii+1),"lag_latest")] - 1)*100
    country_tr$growth_1lag_latest[obs] <- (today_df[,paste0("gdp_",(ii+1),"lag_latest")]/
                                             today_df[,paste0("gdp_",(ii+2),"lag_latest")] - 1)*100
    if (ii == 4){
      qt <- quart %m-% months(15)
      obs <- which(country_tr$quarter==qt)
      country_tr$growth_latest[obs] <- (today_df[,paste0("gdp_",(ii+1),"lag_latest")]/
                                          today_df[,paste0("gdp_",(ii+2),"lag_latest")] - 1)*100
    }
    # Use final for lags further back
    qt <- quart %m-% months(15)
    obs <- which(country_tr$quarter <= qt)
    country_tr$growth_1lag_latest[obs] <- country_tr$growth_1lag_final[obs]
    # Use final for growth further back
    qt <- quart %m-% months(18)
    obs <- which(country_tr$quarter <= qt)
    country_tr$growth_latest[obs] <- country_tr$growth_final[obs]
    ### end of Q-on-Q growth part
  }
  
  # Make sure growth series are vectors
  country_tr$growth_latest <- unlist(country_tr$growth_latest)
  country_tr$growth_1lag_latest <- unlist(country_tr$growth_1lag_latest)
  
  return(country_tr)
}


##### Out of sample OLS #####
country_df <- euro_df[euro_df$date > "2002-01-01",]
start_date <- as.Date("2006-04-01")
end_date <- as.Date("2020-12-31")
metric <- "econlex"
model <- "lm"
text_vars <- c("metric_cum", "pmi_daily") 
bench_vars <- c("pmi_daily") 
target <- "growth_latest" 
same_pmi_rls <- TRUE
same_data_rls <- TRUE

daily_oos <- function(country_df, factor_df, start_date, end_date, metric= "vader", model = "lm", 
                      text_vars = c("metric_cum", "pmi_daily"), bench_vars = c("pmi_daily"),
                      target = "growth_latest", same_pmi_rls = FALSE, same_data_rls = FALSE){
  # Create some variables
  country_df <- as.data.frame(country_df)
  country_df$metric_name <- metric
  country_df$model <- model
  country_df$text_vars <- paste(text_vars, collapse = ", ")
  country_df$bench_vars <- paste(bench_vars, collapse = ", ")
  country_df$target <- target
  
  country_df[,c("preds_bench", "preds_text")] <- NA
  country_df$pmi_1lag <- rowMeans(country_df[,c("pmi_m1_prevq_latest","pmi_m2_prevq_latest",
                                                "pmi_m3_prevq_latest")],na.rm = TRUE)
  country_df$pmi_m0_latest <- country_df$pmi_m3_prevq_latest
  # Placeholders for latest values of growth
  country_df$growth_latest <- country_df$growth_final
  country_df$growth_1lag_latest <- NA
  # Define metrics and placeholders for transformation
  if (metric == "vader"){
    country_df$nwords <- country_df$nsent
  }
  country_df <- country_df %>%
    rename_with(~str_replace_all(.x,metric, "metric"))
  country_df$metric <- country_df$metric_sum/country_df$nwords
  # Get quarterly values
  country_qly <- country_df %>%
    group_by(quarter) %>%
    summarise(metric_qly = sum(metric_sum)/sum(nwords)) %>%
    mutate(metric_qly_1lag = lag(metric_qly))
  # Get monthly values
  country_mly <- country_df %>%
    group_by(quarter, month, moq) %>%
    summarise(metric_mly = sum(metric_sum)/sum(nwords)) %>%
    pivot_wider(id_cols = quarter, names_from = moq, names_prefix = "metric_mly_m", 
                values_from = metric_mly) %>%
    ungroup() %>% arrange(quarter) %>%
    mutate(metric_mly_m1_1lag = lag(metric_mly_m1), metric_mly_m2_1lag = lag(metric_mly_m2),
           metric_mly_m3_1lag = lag(metric_mly_m3))
  # Merge in together
  country_df <- country_df %>%
    left_join(country_qly, by = "quarter") %>%
    group_by(month) %>%
    mutate(metric_mly = sum(metric_sum)/sum(nwords)) %>%
    ungroup() %>%
    left_join(country_mly, by = "quarter") %>%
    mutate(metric_mly_m1 = case_when(moq >1 ~ metric_mly_m1, TRUE ~ NA_real_)) %>%
    mutate(metric_mly_m2 = case_when(moq >2 ~ metric_mly_m2, TRUE ~ NA_real_)) %>%
    mutate(metric_mly_m3 = case_when(quarter != lead(quarter, n = 1) ~ metric_mly_m3, TRUE ~ NA_real_)) %>%
    mutate(metric_diff = metric_cum - metric_qly_1lag) %>%
    mutate(pmi_diff = pmi_daily - pmi_1lag)
  country_df <- as.data.frame(country_df)
  # Getting rolling window
  country_df <- country_df %>%
    mutate(metric_rolling = rollmean(metric_sum/nwords, k = 30, align = "right", fill = NA))
  
  # Starting observation
  start_obs <- which(country_df$date == start_date)
  end_obs <- which(country_df$date == end_date)
  
  pretrain_df <- country_df[1:(start_obs-1),]
  
  print(paste("Running exercise for", country_df$country[1] ))
  tic()
  preds_df <- foreach(tt = start_obs:end_obs, .combine=rbind) %dopar% {
  #pb = txtProgressBar(min = start_obs, max = (nrow(country_df)-1), initial = start_obs) 
  #for (tt in start_obs:end_obs){
  #  setTxtProgressBar(pb,tt)
    
    row <- country_df[tt,]
    today <- row$date
    pmi_rls <- row$pmi_releases
    data_rls <- row$data_releases
    
    ## Add in macro factor
    # Get the most recent vintage of the macro factor
    vintages <- unique(factor_df$vintage)
    relevant_vintage <- max(vintages[which(vintages <= today)]) 
    rel_factor_df <- factor_df %>%
      filter(vintage == relevant_vintage) %>% dplyr::select(-vintage)
    country_df_vint <- left_join(country_df, rel_factor_df, by = "quarter") %>% arrange(date)
    # Standardise metric on in sample period only (removed for now)
    
    ### Create training data
    # Use latest estimates of growth rates going back a year, and after that use gdp_growth
    country_tr <- create_training_data(country_df_vint, today)
    if (same_pmi_rls){
      country_tr <- filter(country_tr, pmi_releases == pmi_rls)
    } 
    if (same_data_rls){
      country_tr <- filter(country_tr, data_releases == data_rls)
    }
    # Forecast error relative to the latest ECB projection
    country_tr$proj_fe <- country_tr$growth_latest - country_tr$ECB_proj_latest
    country_tr$DFM_fe <- country_tr$growth_latest - country_tr$DFM_fcst
    # Check which of these variables are actually available in today's data. 
    text_vars_today <- text_vars
    bench_vars_today <- bench_vars
    if ("macro_factor" %in% text_vars){
      text_vars_today <- str_replace(text_vars_today, "macro_factor", str_c("macro_factor_m", row$moq))
      bench_vars_today <- str_replace(bench_vars_today, "macro_factor", str_c("macro_factor_m", row$moq))
    }
    text_vars_today <- text_vars_today[which(!is.na(country_df_vint[(tt),text_vars_today]))]
    bench_vars_today <- bench_vars_today[which(!is.na(country_df_vint[(tt),bench_vars_today]))]
    # Define the formula
    text_fm <- formula(paste(target, "~", paste(text_vars_today, collapse = " + ")))   
    bench_fm <- formula(paste(target, "~", paste(bench_vars_today, collapse = " + ")))   
    # Keep only complete observations with today's variables
    all_vars_today <- unique(c(target, text_vars_today, bench_vars_today))
    country_tr <- country_tr %>%
      dplyr::select(all_of(c("quarter", all_vars_today))) %>%
      drop_na(any_of(all_vars_today))
      
    
    ### Estimate the models
    if (model == "lm"){
      # linear regression
      model_text <- lm(text_fm, country_tr)
      model_bench <- lm(bench_fm, country_tr)
    } else if (model == "ridge"){
      N <- nrow(country_tr)
      # Ridge regression inputs
      text_xs <- cbind(rep(1, N), apply(data.matrix(country_tr[, text_vars_today]), 2, standardise))
      bench_xs <- cbind(rep(1, N),apply(data.matrix(country_tr[, bench_vars_today]), 2, standardise))
      ys <- country_tr[, target]
      # cross fold splits
      quarters <- unique(country_tr$quarter)
      quarters1 <- quarters[1:ceiling(length(quarters)/3)]
      quarters2 <- quarters[(ceiling(length(quarters)/3)+1):ceiling(2*length(quarters)/3)]
      quarters3 <- quarters[(ceiling(2*length(quarters)/3) +1):length(quarters)]
      country_tr <- mutate(country_tr, foldid = case_when(quarter %in% quarters1 ~ 1, quarter %in% quarters2 ~ 2, quarter %in% quarters3 ~ 3))
      # text model
      text_cv <- cv.glmnet(text_xs, ys, alpha = 0, nfolds = 3, foldid = country_tr$foldid)
      lambda_text <- text_cv$lambda.min
      model_text <- glmnet(text_xs, ys, alpha = 0, lambda = lambda_text)
      # bench model
      bench_cv <- cv.glmnet(bench_xs, ys, alpha = 0, nfolds = 3, foldid = country_tr$foldid)
      lambda_bench <- bench_cv$lambda.min
      model_bench <- glmnet(bench_xs, ys, alpha = 0, lambda = lambda_bench)
    } else if (model == "tree"){
      # Tree
      model_text <- tree(text_fm, country_tr)
      model_bench <- tree(bench_fm, country_tr)
    } else if (model == "rf"){
      # Random Forest 
      model_text <- randomForest(text_fm, data = country_tr, mtry = length(text_vars_today), ntrees = 50)
      model_bench <- randomForest(bench_fm, data = country_tr, mtry = length(bench_vars_today), ntrees = 50)
    } else {
      "Need to specify a valid model"
    }
    
    # Predict on today's data
    country_ts <- country_df_vint[(tt),]
    # Store predictions
    if (model == "ridge"){
      # Benchmark
      newx_bench <- as.matrix((cbind(1, country_ts[,bench_vars_today]) - c(0,colMeans(cbind(country_tr[,bench_vars_today]))))/
                                c(1, apply(cbind(country_tr[,bench_vars_today]), 2, sd)))
      row$preds_bench <- predict(model_bench, newx_bench) + 
        (target == "proj_fe")*country_ts$ECB_proj_latest + (target == "DFM_fe")*country_ts$DFM_fcst
      # Text
      newx_text <- as.matrix((cbind(1, country_ts[,text_vars_today]) - c(0,colMeans(cbind(country_tr[,text_vars_today]))))/
                               c(1, apply(cbind(country_tr[,text_vars_today]), 2, sd)))
      row$preds_text <-  predict(model_text, newx_text) + 
        (target == "proj_fe")*country_ts$ECB_proj_latest + (target == "DFM_fe")*country_ts$DFM_fcst
    } else {
      row$preds_bench <- 
        predict(model_bench, country_ts) + (target == "proj_fe")*country_ts$ECB_proj_latest + (target == "DFM_fe")*country_ts$DFM_fcst
      row$preds_text <- 
        predict(model_text, country_ts) + (target == "proj_fe")*country_ts$ECB_proj_latest + (target == "DFM_fe")*country_ts$DFM_fcst
    }
    
    row
  }
  toc()
  
  preds_df <- rbind(pretrain_df, preds_df)
  
  return(preds_df)
}





"
Run the nowcasting exercises
"
# Initialise tibble to store results
all_preds_df <- tibble()

#### Compare PMI with text in OLS models
model_type <- "lm"
bench_vars_opts <- list(c("pmi_daily"), c("pmi_m0_latest", "pmi_m1_latest", "pmi_m2_latest", "pmi_m3_latest"))
text_vars_opts <- list(c("metric_cum"), c("metric_rolling"), c("metric_mly_m3_1lag", "metric_cum"), c("metric_cum", "metric_diff"),
                       c("metric_mly_m3_1lag", "metric_mly_m1", "metric_mly_m2", "metric_mly_m3"))
# Benchmark variables specification
for (bench_vars in bench_vars_opts){
  writeLines("\n");print(bench_vars);writeLines("\n")
  # Text variable specification
  for (text_vars_only in text_vars_opts){
    text_vars <- c(text_vars_only, bench_vars)
    writeLines("\n");print(text_vars);writeLines("\n")
    # Text metric name
    for (metric in c("econlex", "vader", "stability")){
      # Target variable 
      for (target_var in c("growth_latest", "proj_fe")){
        print(str_c("Using ", metric, " to predict ", target_var, ", with spec ", paste(text_vars_only, collapse = ", "),
                    ", and with benchmark ", paste(bench_vars, collapse = ", ")))
        # If there is monthly data, no need to retrict to smae pmi release
        if (any(str_detect(text_vars, "m[0-9]+$"))){
          same_pmi_rls <- FALSE
        } else {
          same_pmi_rls <- TRUE
        }
        # Calculate the predictions
        pred_df <- daily_oos(euro_df, factor_df, start_date, end_date, metric=metric, target = target_var,
                             model = model_type, text_vars = text_vars, bench_vars = bench_vars, same_pmi_rls = same_pmi_rls) %>%
          dplyr::select(date, month, quarter, year, doq, moq, dom, growth_final, growth_first, ECB_proj_latest, DFM_fcst,
                 metric_name, model, text_vars, bench_vars, target, preds_bench, preds_text)
        # Append to all_preds data
        all_preds_df <- rbind(all_preds_df, as_tibble(pred_df))
      } # end of target variable loop
    } # end of text metric loop
  } # end of text vars loop
} # end of benchmark vars loop


#### Compare Macro factor and financial indicators with text in OLS models
model_type <- "lm"
bench_vars_opts <- list(c("corp_spr", "eurostoxx325"),
                        c("macro_factor_qly"),
                        c("macro_factor_qly", "macro_factor_qly_1lag"),
                        c("macro_factor_m1", "macro_factor_m2", "macro_factor_m3")
                        )
text_vars_opts <- list(c("metric_cum"))
start_date <- as.Date("2006-04-01")
end_date <- as.Date("2020-12-31")
# Benchmark variables specification
for (bench_vars in bench_vars_opts){
  writeLines("\n");print(bench_vars);writeLines("\n")
  # Text variable specification
  for (text_vars_only in text_vars_opts){
    text_vars <- c(text_vars_only, bench_vars)
    writeLines("\n");print(text_vars);writeLines("\n")
    # Text metric name
    for (metric in c("vader", "stability")){
      # Target variable 
      for (target_var in c("growth_latest")){ #, "proj_fe")){
        print(str_c("Using ", metric, " to predict ", target_var, ", with spec ", paste(text_vars_only, collapse = ", "),
                    ", and with benchmark ", paste(bench_vars, collapse = ", ")))
        # If there is monthly data, no need to retrict to smae pmi release
        if (any(str_detect(text_vars, "m[0-9]+$"))){
          same_pmi_rls <- FALSE
        } else {
          same_pmi_rls <- TRUE
        }
        # Calculate the predictions
        pred_df <- daily_oos(euro_df, factor_df, start_date, end_date, metric=metric, target = target_var,
                             model = model_type, text_vars = text_vars, bench_vars = bench_vars, same_pmi_rls = same_pmi_rls) %>%
          dplyr::select(date, month, quarter, year, doq, moq, dom, growth_final, growth_first, ECB_proj_latest, DFM_fcst,
                 metric_name, model, text_vars, bench_vars, target, preds_bench, preds_text)
        # Append to all_preds data
        all_preds_df <- rbind(all_preds_df, as_tibble(pred_df))
      } # end of target variable loop
    } # end of text metric loop
  } # end of text vars loop
} # end of benchmark vars loop


all_preds_df %>%
  write_csv("data/results/ols_preds.csv", row.names = F)





### End of script ###