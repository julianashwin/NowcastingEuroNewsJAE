# This file merges and cleans macro and sentiment data
setwd("/Users/julianashwin/Documents/GitHub/NowcastingEuroGDPNews/")
rm(list=ls())

library(stringr)
library(lubridate)
library(ggplot2)
library(stargazer)
library(tidyverse)

import_dir <- "/Users/julianashwin/Documents/DPhil/Clean_Data/ECB_articles/econlex/"
export_dir <- "data/econlex/"

standardise<- function(series){
  new_series <- (series - mean(series,na.rm=TRUE))/sd(series, na.rm = TRUE)
  return(new_series)
}

sent_files <- dir(import_dir)


"
Import sources and combine into one 
"
import_sources <- function(import_dir, sources){
  df <- read.csv(paste0(import_dir, sources[1], "_econlex.csv"),stringsAsFactors = FALSE)
  if (length(sources) > 1){
    for (source in sources[2:length(sources)]){
      df_in <- read.csv(paste0(import_dir, source, "_econlex.csv"),stringsAsFactors = FALSE)
      df <- rbind(df, df_in)
    }
  }
  # Order by date and cut off pre-1997
  df$date <- as.Date(df$date)
  df <- df[order(df$date),]
  df <- df[which(df$date>="1997-01-01"),]
  return(df)
}


"
Aggregate article level sentiment
"
aggregate_senti <- function(df){
  # Include "empty" days
  dates_df <- data.frame(date = seq(as.Date("1997-01-01"), as.Date("2021-01-29"), by = "day"))
  dates_df$quarter <- floor_date(dates_df$date, unit = "quarters")
  # Aggregate by day
  df$date <- as.Date(df$date)
  df_agg <- as_tibble(df) %>%
    mutate(ndocs = 1) %>%
    group_by(date) %>%
    summarise(econlex_sum = sum(econlex_sum, na.rm = T), nwords = sum(nwords, na.rm = T),
              ndocs = sum(ndocs, na.rm = T)) %>%
    full_join(dates_df) %>%
    mutate(econlex_sum = replace_na(econlex_sum, 0), nwords = replace_na(nwords, 0), 
           ndocs = replace_na(ndocs, 0)) %>%
    arrange(date) %>%
    group_by(quarter) %>%
    mutate(econlex = econlex_sum/nwords,
           econlex_cum = cumsum(econlex_sum)/cumsum(nwords)) %>%
    mutate(econlex = replace_na(econlex, 0), econlex_cum = replace_na(econlex_cum, 0))
  
  return(df_agg)
}




# Group sources by country
french_sources <- c("ECHOS",  "FIGARO", "LEMOND")
german_sources <- c("DWELT", "SDDZ", "GERCOL", "TAGSS")
italian_sources <- c("CORDES", "LAREP", "SOLE", "STMA")
spanish_sources <- c("EXPNSI", "MUNDO", "PAISN", "VNGDIA")



## France
french_articles <- import_sources(import_dir, french_sources)
french_daily <- aggregate_senti(french_articles)
write.csv(french_daily, paste0(export_dir, "french_econlex.csv"),row.names = FALSE)

french_daily %>%
  mutate(month = floor_date(date, unit = "months")) %>%
  group_by(month) %>%
  summarise(econlex_sum = sum(econlex_sum), nwords = sum(nwords)) %>%
  mutate(econlex = econlex_sum/nwords) %>%
  ggplot(aes(x = month, y = econlex)) + geom_line()


## German
german_articles <- import_sources(import_dir, german_sources)
german_daily <- aggregate_senti(german_articles)
write.csv(german_daily, paste0(export_dir, "german_econlex.csv"),row.names = FALSE)

german_daily %>%
  mutate(month = floor_date(date, unit = "months")) %>%
  group_by(month) %>%
  summarise(econlex_sum = sum(econlex_sum), nwords = sum(nwords)) %>%
  mutate(econlex = econlex_sum/nwords) %>%
  ggplot(aes(x = month, y = econlex)) + geom_line()

## Italian
italian_articles <- import_sources(import_dir, italian_sources)
italian_daily <- aggregate_senti(italian_articles)
write.csv(italian_daily, paste0(export_dir, "italian_econlex.csv"),row.names = FALSE)

italian_daily %>%
  mutate(month = floor_date(date, unit = "months")) %>%
  group_by(month) %>%
  summarise(econlex_sum = sum(econlex_sum), nwords = sum(nwords)) %>%
  mutate(econlex = econlex_sum/nwords) %>%
  ggplot(aes(x = month, y = econlex)) + geom_line()

## Spanish
spanish_articles <- import_sources(import_dir, spanish_sources)
spanish_daily <- aggregate_senti(spanish_articles)
write.csv(spanish_daily, paste0(export_dir, "spanish_econlex.csv"),row.names = FALSE)

spanish_daily %>%
  mutate(month = floor_date(date, unit = "months")) %>%
  group_by(month) %>%
  summarise(econlex_sum = sum(econlex_sum), nwords = sum(nwords)) %>%
  mutate(econlex = econlex_sum/nwords) %>%
  ggplot(aes(x = month, y = econlex)) + geom_line()








