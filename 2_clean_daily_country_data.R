"
This file creates daily sentiment, PMI and GDP data for each country
Note that this file will not run from the replication package, as some of the underlying data are sensitive/proprietary
However, for transparency, we include this file to show how the data are cleaned
"
setwd("/Users/julianashwin/Documents/GitHub/NowcastingEuroNewsJAE/")
rm(list=ls())
require(tidyverse)
require(lubridate)
require(gplots)
require(stargazer)



## Function to standardise data so that it is mean zero and s.d. 1
standardise<- function(series){
  new_series <- (series - mean(series,na.rm=TRUE))/sd(series, na.rm = TRUE)
  return(new_series)
}

#df <- french_sentiment
#series <- "afinn"
## Function that takes last value from previous quarter as lag
quarter_lag<- function(df,series){
  series_lag <- rep(NA, nrow(df))
  for (ii in 2:nrow(df)){
    if (df$quarter[ii] == df$quarter[ii-1]){
      series_lag[ii] <- series_lag[ii-1]
    } else {
      series_lag[ii] <- df[ii-1,series]
    }
  }
  return(series_lag)
}


# Identify all the sentiment csv files
import_en <- "data/metrics/"
en_files <- dir(import_en)
en_files <- en_files[which(str_detect(en_files, ".csv"))]

# The sentiment metrics to keep 
sent_metrics <- c("loughran", "stability", "afinn","vader", "econlex")


### Import the sentiment series from translated articles
## France
french_sentiment <- as_tibble(read.csv(paste0(import_en,"french_metrics.csv"))) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  mutate(quarter = floor_date(date, unit = "quarters"),
         month = floor_date(date, unit = "months")) %>%
  relocate(month, quarter, .after = date) %>%
  select(date, month, quarter, nwords, loughran_sum, stability_sum, afinn_sum) %>%
  mutate(loughran_sum = replace_na(loughran_sum, 0), stability_sum = replace_na(stability_sum, 0),
         afinn_sum = replace_na(afinn_sum, 0), nwords = replace_na(nwords, 0))
## Add vader series
french_vader <- as_tibble(read.csv("data/vader/french_vader.csv", stringsAsFactors = FALSE)) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  rename(nsent = vader_nwords) %>%
  select(date, vader_sum, nsent)
french_sentiment <- french_sentiment %>%
  left_join(french_vader)
## Add econlex series
french_econlex <- as_tibble(read.csv("data/econlex/french_econlex.csv", stringsAsFactors = FALSE)) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  select(date, econlex_sum, ndocs)
french_sentiment <- french_sentiment %>%
  left_join(french_econlex) %>%
  select(date, month, quarter, nwords, ndocs, nsent, loughran_sum, stability_sum, afinn_sum, vader_sum, econlex_sum)

cor.test(french_sentiment$vader_sum, french_sentiment$afinn_sum)
cor.test(french_sentiment$vader_sum, french_sentiment$econlex_sum)

## Germany
german_sentiment <- as_tibble(read.csv(paste0(import_en,"german_metrics.csv"))) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  mutate(quarter = floor_date(date, unit = "quarters"),
         month = floor_date(date, unit = "months")) %>%
  relocate(month, quarter, .after = date) %>%
  select(date, month, quarter, nwords, loughran_sum, stability_sum, afinn_sum) %>%
  mutate(loughran_sum = replace_na(loughran_sum, 0), stability_sum = replace_na(stability_sum, 0),
         afinn_sum = replace_na(afinn_sum, 0), nwords = replace_na(nwords, 0))
## Add vader series
german_vader <- as_tibble(read.csv("data/vader/german_vader.csv", stringsAsFactors = FALSE)) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  rename(nsent = vader_nwords) %>%
  select(date, vader_sum, nsent)
german_sentiment <- german_sentiment %>%
  left_join(german_vader)
## Add econlex series
german_econlex <- as_tibble(read.csv("data/econlex/german_econlex.csv", stringsAsFactors = FALSE)) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  select(date, econlex_sum, ndocs)
german_sentiment <- german_sentiment %>%
  left_join(german_econlex) %>%
  select(date, month, quarter, nwords, ndocs, nsent, loughran_sum, stability_sum, afinn_sum, vader_sum, econlex_sum)

cor.test(german_sentiment$vader_sum, german_sentiment$econlex_sum)



## Italian
italian_sentiment <- as_tibble(read.csv(paste0(import_en,"italian_metrics.csv"))) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  mutate(quarter = floor_date(date, unit = "quarters"),
         month = floor_date(date, unit = "months")) %>%
  relocate(month, quarter, .after = date) %>%
  select(date, month, quarter, nwords, loughran_sum, stability_sum, afinn_sum) %>%
  mutate(loughran_sum = replace_na(loughran_sum, 0), stability_sum = replace_na(stability_sum, 0),
         afinn_sum = replace_na(afinn_sum, 0), nwords = replace_na(nwords, 0))
## Add vader series
italian_vader <- as_tibble(read.csv("data/vader/italian_vader.csv", stringsAsFactors = FALSE)) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  rename(nsent = vader_nwords) %>%
  select(date, vader_sum, nsent)
italian_sentiment <- italian_sentiment %>%
  left_join(italian_vader)
## Add econlex series
italian_econlex <- as_tibble(read.csv("data/econlex/italian_econlex.csv", stringsAsFactors = FALSE)) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  select(date, econlex_sum, ndocs)
italian_sentiment <- italian_sentiment %>%
  left_join(italian_econlex) %>%
  select(date, month, quarter, nwords, ndocs, nsent, loughran_sum, stability_sum, afinn_sum, vader_sum, econlex_sum)

cor.test(italian_sentiment$vader_sum, italian_sentiment$econlex_sum)



## Spanish
spanish_sentiment <- as_tibble(read.csv(paste0(import_en, "spanish_metrics.csv"))) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  mutate(quarter = floor_date(date, unit = "quarters"),
         month = floor_date(date, unit = "months")) %>%
  relocate(month, quarter, .after = date) %>%
  select(date, month, quarter, nwords, loughran_sum, stability_sum, afinn_sum) %>%
  mutate(loughran_sum = replace_na(loughran_sum, 0), stability_sum = replace_na(stability_sum, 0),
         afinn_sum = replace_na(afinn_sum, 0), nwords = replace_na(nwords, 0))
## Add vader series
spanish_vader <- as_tibble(read.csv("data/vader/spanish_vader.csv", stringsAsFactors = FALSE)) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  rename(nsent = vader_nwords) %>%
  select(date, vader_sum, nsent)
spanish_sentiment <- spanish_sentiment %>%
  left_join(spanish_vader)
## Add econlex series
spanish_econlex <- as_tibble(read.csv("data/econlex/spanish_econlex.csv", stringsAsFactors = FALSE)) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "1997-10-01" & date < "2021-01-01") %>%
  select(date, econlex_sum, ndocs)
spanish_sentiment <- spanish_sentiment %>%
  left_join(spanish_econlex) %>%
  select(date, month, quarter, nwords, ndocs, nsent, loughran_sum, stability_sum, afinn_sum, vader_sum, econlex_sum)

cor.test(spanish_sentiment$vader_sum, spanish_sentiment$econlex_sum)


## Import the daily real-time indicators for all countries
# Euro Area
euro_dailyind <- as_tibble(read.csv("data/euro_dailyind.csv")) %>%
  mutate(date = as.Date(date), month = as.Date(month), quarter = as.Date(quarter))

## Combine into euro area level
gdp_weights <- as_tibble(read.csv("data/GDP_weights.csv")) %>%
  select(year, FR, DE, IT, ES) %>%
  mutate(year = year + 1)
gdp_weights[nrow(gdp_weights)+1,"year"]<- 1997 
gdp_weights[nrow(gdp_weights),2:5] <- gdp_weights[1,2:5]
gdp_weights <- arrange(gdp_weights, year)
gdp_weights[,2:5] <- gdp_weights[,2:5]/rowSums(gdp_weights[,2:5])
colnames(gdp_weights)[2:5] <- paste0(colnames(gdp_weights)[2:5],"_weight")
# Merge in the weights and the number of words and sentences from each country
euro_df <- euro_dailyind %>%
  mutate(year = year(date)) %>%
  left_join(gdp_weights,by = "year") %>%
  relocate(year, FR_weight, DE_weight, IT_weight, ES_weight, .after = quarter) %>%
  left_join(select(french_sentiment, date, nwords, nsent), by ="date") %>%
  rename(nwords_fr = nwords, nsent_fr = nsent) %>%
  left_join(select(german_sentiment, date, nwords, nsent), by ="date") %>%
  rename(nwords_ge = nwords, nsent_ge = nsent) %>%
  left_join(select(italian_sentiment, date, nwords, nsent), by ="date") %>%
  rename(nwords_it = nwords, nsent_it = nsent) %>%
  left_join(select(spanish_sentiment, date, nwords, nsent), by ="date") %>%
  rename(nwords_sp = nwords, nsent_sp = nsent) %>%
  mutate(nwords = FR_weight*nwords_fr + DE_weight*nwords_ge + IT_weight*nwords_it + ES_weight*nwords_sp) %>%
  mutate(nsent = FR_weight*nsent_fr + DE_weight*nsent_ge + IT_weight*nsent_it + ES_weight*nsent_sp) %>% 
  relocate(nwords, nsent, DE_weight, IT_weight, ES_weight, .after = ES_weight)

# Add the sum for each sent_metric
french_sentiment[,str_c(sent_metrics, "_sum_fr")] <- french_sentiment[,str_c(sent_metrics, "_sum")]
german_sentiment[,str_c(sent_metrics, "_sum_ge")] <- german_sentiment[,str_c(sent_metrics, "_sum")]
italian_sentiment[,str_c(sent_metrics, "_sum_it")] <- italian_sentiment[,str_c(sent_metrics, "_sum")]
spanish_sentiment[,str_c(sent_metrics, "_sum_sp")] <- spanish_sentiment[,str_c(sent_metrics, "_sum")]

for (met in c(sent_metrics)){
  print(met)
  # Add french series
  euro_df <- euro_df %>%
    left_join(french_sentiment[,c("date", str_c(met, "_sum_fr"))], by = "date") %>%
    left_join(german_sentiment[,c("date", str_c(met, "_sum_ge"))], by = "date") %>%
    left_join(italian_sentiment[,c("date", str_c(met, "_sum_it"))], by = "date") %>%
    left_join(spanish_sentiment[,c("date", str_c(met, "_sum_sp"))], by = "date") 
  
  # Euro Area metric is weighted sum of country-level ones
  euro_df[,str_c(met, "_sum")] <- (euro_df$FR_weight*euro_df[,paste0(met, "_sum_fr")]) + 
    (euro_df$DE_weight*euro_df[,paste0(met, "_sum_ge")]) + 
    (euro_df$IT_weight*euro_df[,paste0(met, "_sum_it")]) + 
    (euro_df$ES_weight*euro_df[,paste0(met, "_sum_sp")])
  
}


euro_df %>%
  filter(month <= "2019-01-01") %>%
  group_by(month, growth_final) %>%
  summarise(vader_sum = sum(vader_sum), econlex_sum = sum(econlex_sum), stability_sum = sum(stability_sum), 
            nwords = sum(nwords), nsent = sum(nsent)) %>%
  ggplot(aes(x = month)) + theme_bw() + 
  geom_line(aes(y= growth_final, color = "GDP")) + 
  geom_line(aes(y= standardise(vader_sum/nsent), color = "Vader")) + 
  geom_line(aes(y= standardise(econlex_sum/nwords), color = "EconLex")) +
  geom_line(aes(y= standardise(stability_sum/nwords), color = "CGLM")) 

euro_df %>%
  filter(month > "2019-01-01") %>%
  group_by(month, growth_final) %>%
  summarise(vader_sum = sum(vader_sum), econlex_sum = sum(econlex_sum), stability_sum = sum(stability_sum), 
            nwords = sum(nwords), nsent = sum(nsent)) %>%
  ggplot(aes(x = month)) + theme_bw() + 
  geom_line(aes(y= growth_final, color = "GDP")) + 
  geom_line(aes(y= standardise(vader_sum/nsent), color = "Vader")) + 
  geom_line(aes(y= standardise(econlex_sum/nwords), color = "EconLex")) +
  geom_line(aes(y= standardise(stability_sum/nwords), color = "CGLM")) 
  

# Country
euro_df$country <- "Euro Area"

## Adjust PMI so that it's centered roughly around zero
euro_df[,which(str_detect(colnames(euro_df), "pmi"))] <- 
  euro_df[,which(str_detect(colnames(euro_df), "pmi"))] - 50
euro_df$pmi_releases <- euro_df$pmi_releases + 50

euro_df %>%
  filter(month < "2019-01-01") %>%
  ggplot(aes(x = date)) + theme_bw() + 
  geom_line(aes(y= growth_final, color = "GDP")) + 
  geom_line(aes(y= pmi_daily, color = "PMI"))


# Remove days for which we don't have the sentiment index
euro_df <- euro_df[which(!is.na(euro_df$stability_sum)),]



## Principle components on full sample
# Euro Area
if (FALSE){
  input_data <- euro_df[,c("stability", "loughran", "afinn")] 
  new_pc <- prcomp(input_data, scale = TRUE)
  flipsign <- cor(new_pc$x[,1], euro_df$stability)
  euro_df$PC1 <- sign(flipsign)*new_pc$x[,1]
  ggplot(euro_df, aes(x=date)) + geom_line(aes(y=standardise(stability), color = "S")) +
    geom_line(aes(y=standardise(loughran), color = "L")) + 
    geom_line(aes(y=standardise(afinn), color = "A")) +
    geom_line(aes(y=standardise(PC1), color = "PC1")) +
    geom_line(aes(y=standardise(gdp_final/gdp_1lag_final-1), color = "GDP"))
  input_data <- euro_df[,c("stability_un", "loughran_un", "afinn_un")] 
  new_pc <- prcomp(input_data, scale = TRUE)
  flipsign <- cor(new_pc$x[,1], euro_df$stability_un)
  euro_df$PC1_un <- sign(flipsign)*new_pc$x[,1]
  ggplot(euro_df, aes(x=date)) + geom_line(aes(y=PC1_un, color = "PC1_un")) +
    geom_line(aes(y=standardise(PC1), color = "PC1"))
}

# Check that the daily_pmi looks okay
ggplot(euro_df, aes(x=date, y=pmi_daily)) + geom_line()

## Add data release (including the projection release)
data_rls <- function(country_df){
  country_df$data_releases <- country_df$pmi_releases
  obs <- which(country_df$moq == 3)
  country_df$data_releases[obs] <- country_df$data_releases[obs] + 1
  obs <- which(country_df$moq == 2 & country_df$dom >= 15)
  country_df$data_releases[obs] <- country_df$data_releases[obs] + 1
  return(country_df)
}
euro_df <- data_rls(euro_df)



euro_df <- euro_df %>%
  arrange(date) %>%
  group_by(quarter) %>%
  mutate(vader_cum = cumsum(vader_sum)/cumsum(nsent),
         stability_cum = cumsum(stability_sum)/cumsum(nwords),
         econlex_cum = cumsum(econlex_sum)/cumsum(nwords))

## Export data
write.csv(euro_df,"data/clean_data/daily/euro_data_daily_orig.csv", row.names = FALSE)


### Plot examples of the daily series
ggplot(euro_df[which((euro_df$date > "2009-01-01") & (euro_df$date < "2009-06-30")),], aes(x=date)) +
  scale_color_manual("Variable", values = c("CGLM metric" = "blue", "VADER metric" = "red")) +
  geom_line(aes(y=standardise(vader_cum), color = "VADER metric")) +
  #geom_line(aes(y=standardise(stability_cum), color = "CGLM metric")) +
  ggtitle("Daily sentiment metric (Euro Area, 2009)") + theme_bw() + 
  xlab("Date") + ylab("Std. units")
ggsave(paste0(export_dir, "dailysent_example",".pdf"), width = 10, height = 3)
# PMI
ggplot(euro_df[which((euro_df$date > "2009-01-01") & (euro_df$date < "2009-06-30")),], aes(x=date)) +
  scale_color_manual("Variable", values = c("ECB projection" = "blue", "Quasi-daily PMI" = "red")) +
  geom_line(aes(y=standardise(pmi_daily), color = "Quasi-daily PMI")) +
  geom_line(aes(y=standardise(ECB_proj_latest), color = "ECB projection")) +
  ggtitle("Quasi-daily PMI metric and ECB projection (Euro Area 2009)") + theme_bw() + 
  xlab("Date") + ylab("Std. units")
ggsave(paste0(export_dir, "dailypmi_example",".pdf"), width = 10, height = 3)





############################### End of script ############################### 

