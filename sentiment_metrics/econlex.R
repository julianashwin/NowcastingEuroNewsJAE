"
Calculate Economic Lexicon sentiment scores
"
setwd("/Users/julianashwin/Documents/GitHub/NowcastingEuroGDPNews/")
rm(list = ls())

library(tidyverse)
library(progress)
library(tictoc)
library(tidytext)
library(lubridate)

econlex_df <- as_tibble(read.csv("data/dictionaries/Economic_Lexicon.csv")) %>%
  rename(word = token)

# Identify the files
import_dir_old <- "/Users/julianashwin/Documents/DPhil/Raw_Data/ECB_articles/translations_nodup/"
import_dir_new <- "/Users/julianashwin/Documents/DPhil/Raw_Data/ECB_articles/new_translations/"
export_dir <- "/Users/julianashwin/Documents/DPhil/Clean_Data/ECB_articles/econlex/"


files_old <- dir(import_dir_old)
files_new <- dir(import_dir_new)


sources <- c("ECHOS",  "FIGARO", "LEMOND", "DWELT", "SDDZ", "GERCOL", "TAGSS", 
           "CORDES", "SOLE", "LAREP", "STMA", "EXPNSI", "MUNDO", "PAISN", "VNGDIA")

source <- "VNGDIA"
#str_c(sources[12],"_trans.csv") [1] %in% files_new

# Import source
if (str_c(source,"_trans.csv") %in% files_new){
  raw_text_df_new <- as_tibble(read.csv(str_c(import_dir_new, source, "_trans.csv")))
  raw_text_df_new$an <- raw_text_df_new$ID
  raw_text_df_new <- raw_text_df_new[,c("an","date", "text")]
  raw_text_df_new <- raw_text_df_new %>%
    mutate(an = replace_na(an, ""), date = replace_na(date, ""), text = replace_na(text, ""))
  
  if (str_c(source,"_trans.csv") %in% files_old){
    raw_text_df_old <- as_tibble(read.csv(str_c(import_dir_old, source, "_trans.csv")))
    raw_text_df_old <- raw_text_df_old %>%
      mutate(an = replace_na(an, ""), date = replace_na(date, ""), title = replace_na(title, ""),
             snippet = replace_na(snippet, ""), body = replace_na(body, ""))
    raw_text_df_old <- raw_text_df_old %>%
      mutate(text = paste(title, snippet, body))
    raw_text_df_old = raw_text_df_old[,c("an","date", "text")]
    raw_text_df = rbind(raw_text_df_old,raw_text_df_new)
  } else{
    raw_text_df = raw_text_df_new
  }
} else {
  raw_text_df_old = as_tibble(read.csv(str_c(import_dir_old, source, "_trans.csv")))
  raw_text_df_old <- raw_text_df_old %>%
    mutate(an = replace_na(an, ""), date = replace_na(date, ""), title = replace_na(title, ""),
           snippet = replace_na(snippet, ""), body = replace_na(body, ""))
  raw_text_df_old <- raw_text_df_old %>%
    mutate(text = paste(title, snippet, body))
  raw_text_df_old <- raw_text_df_old[,c("an","date", "text")]
  raw_text_df <- raw_text_df_old
  
}

# Clean up text 
raw_text_df$text <- tolower(raw_text_df$text)
raw_text_df$text <- str_replace_all(tolower(raw_text_df$text), "[^[:alpha:]]", " ")
raw_text_df$text <- str_squish(raw_text_df$text)
raw_text_df$nwords <- str_count(raw_text_df$text, " ") +1

tic()
sent_df <- raw_text_df %>%
  select(an, date, nwords, text) %>%
  unnest_tokens(word, text) %>%
  inner_join(econlex_df) %>%
  group_by(an, date, nwords) %>%
  summarise(econlex_sum = sum(sentiment), 
            econlex_polsum = sum(polarity))
toc()
sent_df <- sent_df %>%
  arrange(date, an)

write.csv(sent_df, str_c(export_dir, source, "_econlex.csv"),row.names=FALSE)


mly_df <- sent_df %>%
  mutate(month = floor_date(as.Date(date), unit = "months")) %>%
  group_by(month) %>%
  mutate(econlex = sum(econlex_sum)/sum(nwords))
mly_df %>%
  ggplot() + theme_bw() + 
  geom_line(aes(x = month, y = econlex))
