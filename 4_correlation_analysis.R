"
This file compares the different translation methodologies
"
setwd("/Users/julianashwin/Documents/GitHub/NowcastingEuroNewsJAE/")
rm(list=ls())

require(stringr)
require(lubridate)
require(ggplot2)
require(gplots)
require(stargazer)
require(reshape2)
require(ggpubr)

daily_data <- TRUE

## Function to standardise data so that it is mean zero and s.d. 1
standardise<- function(series, cutoff = NA){
  if (!is.na(cutoff)){
    new_series <- (series - mean(series[1:cutoff],na.rm=TRUE))/sd(series[1:cutoff], na.rm = TRUE)
  } else{
    new_series <- (series - mean(series,na.rm=TRUE))/sd(series, na.rm = TRUE)
  }
  
  return(new_series)
}

lastdayquarter <- function(df){
  df$quarter_lead <- NA
  df$quarter_lead[1:(nrow(df)-1)] <- df$quarter[2:nrow(df)]
  df <- df[which(df$quarter != df$quarter_lead),]
  df <- subset(df, select = -c(quarter_lead))
}

lastdaymonth <- function(df){
  df$month_lead <- NA
  df$month_lead[1:(nrow(df)-1)] <- df$month[2:nrow(df)]
  df <- df[which(df$month != df$month_lead),]
  df <- subset(df, select = -c(month_lead))
}

## Add in the econlext data
econlex_df <- read_csv("data/euro_daily_export.csv") %>%
  group_by(quarter) %>%
  mutate(econlex_cum_fr = cumsum(econlex_sum_fr)/cumsum(nwords_fr),
         econlex_cum_ge = cumsum(econlex_sum_ge)/cumsum(nwords_ge),
         econlex_cum_it = cumsum(econlex_sum_it)/cumsum(nwords_it),
         econlex_cum_sp = cumsum(econlex_sum_sp)/cumsum(nwords_sp)) %>%
  ungroup() %>%
  dplyr::select(date, econlex_cum, econlex_cum_fr, econlex_cum_ge, econlex_cum_it, econlex_cum_sp)
  
  


## Import the data for each country
french_df <- read_csv("data/country_data/french_data_daily.csv")
french_df <- french_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"),
         month = as.Date(month, format = "%d/%m/%Y"),
         quarter = as.Date(quarter, format = "%d/%m/%Y")) %>%
  left_join(select(econlex_df, date, econlex_cum_fr)) %>%
  rename(econlex = econlex_cum_fr)
french_df_mly <- lastdaymonth(french_df)
french_df_mly <- french_df_mly[which(french_df_mly$date < "2021-01-01"),]
french_df_qly <- lastdayquarter(french_df)
german_df <- read_csv("data/country_data/german_data_daily.csv")
german_df <- german_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"),
         month = as.Date(month, format = "%d/%m/%Y"),
         quarter = as.Date(quarter, format = "%d/%m/%Y")) %>%
  left_join(select(econlex_df, date, econlex_cum_ge)) %>%
  rename(econlex = econlex_cum_ge)
german_df_mly <- lastdaymonth(german_df)
german_df_mly <- german_df_mly[which(german_df_mly$date < "2021-01-01"),]
german_df_qly <- lastdayquarter(german_df)
italian_df <- read_csv("data/country_data/italian_data_daily.csv")
italian_df <- italian_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"),
         month = as.Date(month, format = "%d/%m/%Y"),
         quarter = as.Date(quarter, format = "%d/%m/%Y")) %>%
  left_join(select(econlex_df, date, econlex_cum_it)) %>%
  rename(econlex = econlex_cum_it)
italian_df_mly <- lastdaymonth(italian_df)
italian_df_mly <- italian_df_mly[which(italian_df_mly$date < "2021-01-01"),]
italian_df_qly <- lastdayquarter(italian_df)
spanish_df <- read_csv("data/country_data/spanish_data_daily.csv")
spanish_df <- spanish_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"),
         month = as.Date(month, format = "%d/%m/%Y"),
         quarter = as.Date(quarter, format = "%d/%m/%Y")) %>%
  left_join(select(econlex_df, date, econlex_cum_sp)) %>%
  rename(econlex = econlex_cum_sp)
spanish_df_mly <- lastdaymonth(spanish_df)
spanish_df_mly <- spanish_df_mly[which(spanish_df_mly$date < "2021-01-01"),]
spanish_df_qly <- lastdayquarter(spanish_df)
euro_df <- read_csv("data/country_data/euro_data_daily.csv")
euro_df <- euro_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"),
         month = as.Date(month, format = "%d/%m/%Y"),
         quarter = as.Date(quarter, format = "%d/%m/%Y")) %>%
  left_join(select(econlex_df, date, econlex_cum)) %>%
  rename(econlex = econlex_cum)
euro_df_mly <- lastdaymonth(euro_df)
euro_df_mly <- euro_df_mly[which(euro_df_mly$date < "2021-01-01"),]
euro_df_qly <- lastdayquarter(euro_df)


french_df %>% 
  filter(date >= "2000-01-01" & date < "2001-01-01") %>%
  mutate(date = as.Date(date)) %>%
  ggplot() + theme_bw() + 
  geom_line(aes(x = date, y = vader)) + 
  geom_line(aes(x = date, y = econlex))


## Define the sentiment metrics to examine
sent_metrics <- c("stability", "loughran", "afinn", "harvard", "nyman","opinion")
sent_metrics <- c(sent_metrics, paste0(sent_metrics, "_un"),"vader","econlex")


### Compare translated and untranslated metrics
sent_metrics1 <- sent_metrics[1:6]
sent_metrics2 <- sent_metrics[7:12]
transcorrs <- function(df, sent_metrics1, sent_metrics2, siglevel = FALSE){
  vec <- rep("", length(sent_metrics1))
  for (ii in 1:length(sent_metrics1)){
    ctst <- cor.test(unlist(as.vector(df[,sent_metrics1[ii]])), unlist(as.vector(df[,sent_metrics2[ii]])))
    est <- round(ctst$estimate,3)
    if (siglevel){
      pval <- ctst$p.value
      stars <- "$^{}"
      if (pval < 0.1){stars <- "$^{*}$"}
      if (pval < 0.05){stars <- "$^{**}$"}
      if (pval < 0.01){stars <- "$^{***}$"}
      field <- paste0(est, stars)
    } else {
      field <- est
    }
    
    vec[ii] <- field
  }
  return(vec)
}

## Correlation table
corr_vec <- function(series, df, sent_metrics, siglevel = FALSE){
  vec <- rep("", length(sent_metrics))
  ii <-1
  for (met in sent_metrics){
    print(met)
    ctst <- cor.test(unlist(as.vector(df[,series])), unlist(as.vector(df[,met]) )) 
    est <- round(ctst$estimate,3)
    if (siglevel){
      pval <- ctst$p.value
      stars <- "$^{}"
      if (pval < 0.1){stars <- "$^{*}$"}
      if (pval < 0.05){stars <- "$^{**}$"}
      if (pval < 0.01){stars <- "$^{***}$"}
      field <- paste0(est, stars)
    } else {
      field <- est
    }
    vec[ii] <- field
    ii <- ii + 1
  }
  return(vec)
}


### Compare metrics from two different translation methodologies
transcorr_df_dly<- data.frame(sent_metrics1)
transcorr_df_dly$France <- transcorrs(french_df, sent_metrics1, sent_metrics2, siglevel = FALSE)
transcorr_df_dly$Germany <- transcorrs(german_df, sent_metrics1, sent_metrics2, siglevel = FALSE)
transcorr_df_dly$Italy <- transcorrs(italian_df, sent_metrics1, sent_metrics2, siglevel = FALSE)
transcorr_df_dly$Spain <- transcorrs(spanish_df, sent_metrics1, sent_metrics2, siglevel = FALSE)
transcorr_df_dly$EuroArea <- transcorrs(euro_df, sent_metrics1, sent_metrics2, siglevel = FALSE)

stargazer(as.matrix(transcorr_df_dly))


### Compare metrics from two different translation methodologies
transcorr_df_mly<- data.frame(sent_metrics1)
transcorr_df_mly$France <- transcorrs(french_df_mly, sent_metrics1, sent_metrics2, siglevel = FALSE)
transcorr_df_mly$Germany <- transcorrs(german_df_mly, sent_metrics1, sent_metrics2, siglevel = FALSE)
transcorr_df_mly$Italy <- transcorrs(italian_df_mly, sent_metrics1, sent_metrics2, siglevel = FALSE)
transcorr_df_mly$Spain <- transcorrs(spanish_df_mly, sent_metrics1, sent_metrics2, siglevel = FALSE)
transcorr_df_mly$EuroArea <- transcorrs(euro_df_mly, sent_metrics1, sent_metrics2, siglevel = FALSE)

stargazer(as.matrix(transcorr_df_mly))




french_df_qly <- french_df_qly[which(french_df_qly$date>= "2000-01-01" & 
                                       french_df_qly$date< "2020-01-01"),]
german_df_qly <- german_df_qly[which(german_df_qly$date>= "2000-01-01" & 
                                       german_df_qly$date< "2020-01-01"),]
italian_df_qly <- italian_df_qly[which(italian_df_qly$date>= "2000-01-01" & 
                                         italian_df_qly$date< "2020-01-01"),]
spanish_df_qly <- spanish_df_qly[which(spanish_df_qly$date>= "2000-01-01" & 
                                         spanish_df_qly$date< "2020-01-01"),]
euro_df_qly <- euro_df_qly[which(euro_df_qly$date>= "2000-01-01" & 
                                   euro_df_qly$date< "2020-01-01"),]

corr_df<- data.frame(c(sent_metrics, "bpw"))
corr_df$french_gdp <- c(corr_vec("growth_final", french_df_qly, sent_metrics),NA)
#corr_df$french_pmi <- c(corr_vec("pmicomp", french_df_mly, sent_metrics),NA)
corr_df$german_gdp <- corr_vec("growth_final", german_df_qly, c(sent_metrics, "bpw"))
#corr_df$german_pmi <- corr_vec("pmicomp", german_df_mly, c(sent_metrics, "bpw"))
corr_df$italian_gdp <- c(corr_vec("growth_final", italian_df_qly, sent_metrics),NA)
#corr_df$italian_pmi <- c(corr_vec("pmicomp", italian_df_mly, sent_metrics),NA)
corr_df$spanish_gdp <- c(corr_vec("growth_final", spanish_df_qly, sent_metrics),NA)
#corr_df$spanish_pmi <- c(corr_vec("pmicomp", spanish_df_mly, sent_metrics),NA)
corr_df$euro_gdp <- c(corr_vec("growth_final", euro_df_qly, sent_metrics),NA)
#corr_df$euro_pmi <- c(corr_vec("pmicomp", euro_df_mly, sent_metrics),NA)

stargazer(as.matrix(corr_df))

#corr_df$av_gdp <- round(rowMeans(corr_df[,c(2,4,6,8)], na.rm =T),3)
#corr_df$av_pmi <- round(rowMeans(corr_df[,c(3,5,7,9)], na.rm =T),3)
trans_av <- mean(c(as.numeric(as.matrix(corr_df[1:6,2:6]))))
untrans_av <- mean(c(as.numeric(as.matrix(corr_df[7:12,2:6]))))




## Plots
# Euro Area plot
euro_df_mly$date <- as.Date(euro_df_mly$date)
std_cutoff <- max(which(euro_df_mly$date < "2020-01-01"))
euro_df_mly$loughran_std <- standardise(euro_df_mly$loughran, cutoff = std_cutoff)
euro_df_mly$stability_std <- standardise(euro_df_mly$stability, cutoff = std_cutoff)
euro_df_mly$afinn_std <- standardise(euro_df_mly$afinn, cutoff = std_cutoff)
euro_df_mly$vader_std <- standardise(euro_df_mly$vader, cutoff = std_cutoff)
euro_df_mly$econlex_std <- standardise(euro_df_mly$econlex, cutoff = std_cutoff)
euro_df_mly$growth_no20 <- euro_df_mly$growth_final
euro_df_mly$growth_no20[which(euro_df_mly$date >= "2020-01-01")] <-NA
growth_mean <- mean(euro_df_mly$growth_no20, na.rm = TRUE)
growth_sd <- sd(euro_df_mly$growth_no20, na.rm = TRUE)
min_val <- min(c((euro_df_mly$growth_no20-growth_mean)/growth_sd, euro_df_mly$vader_std),na.rm=TRUE)

euro_plt <- ggplot(euro_df_mly[which(euro_df_mly$date <"2021-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Legend", values = c("GDP growth" = "black", "Y-on-Y" = "green",
                                          "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_no20-growth_mean)/growth_sd, color = "GDP growth")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits =c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  ggtitle("Euro Area GDP and news sentiment")
euro_plt

min_val <- min(c((euro_df_mly$growth_final-growth_mean)/growth_sd, 
                 -(euro_df_mly$growth_final-growth_mean)/growth_sd, 
                 euro_df_mly$vader_std),na.rm=TRUE)
euro_plt20 <- ggplot(euro_df_mly[which(euro_df_mly$date >="2019-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Source", values = c("GDP" = "black", "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_final-growth_mean)/growth_sd, color = "GDP")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits =c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  scale_x_continuous(breaks = as.Date(c("2019-01-01", "2020-01-01")), 
                     labels = paste0(c("2019", "2020"))) + 
  ggtitle("(2019-2020)")
euro_plt20
ggarrange(euro_plt, euro_plt20, ncol = 2, common.legend = TRUE,widths = c(4, 1))
ggsave("figures/Euro_gdp_sent.pdf", width = 10, height = 3)



# France plot
french_df_mly$date <- as.Date(french_df_mly$date)
std_cutoff <- max(which(french_df_mly$date < "2020-01-01"))
french_df_mly$loughran_std <- standardise(french_df_mly$loughran, cutoff = std_cutoff)
french_df_mly$stability_std <- standardise(french_df_mly$stability, cutoff = std_cutoff)
french_df_mly$afinn_std <- standardise(french_df_mly$afinn, cutoff = std_cutoff)
french_df_mly$vader_std <- standardise(french_df_mly$vader, cutoff = std_cutoff)
french_df_mly$econlex_std <- standardise(french_df_mly$econlex, cutoff = std_cutoff)
french_df_mly$growth_no20 <- french_df_mly$growth_final
french_df_mly$growth_no20[which(french_df_mly$date >= "2020-01-01")] <-NA
growth_mean <- mean(french_df_mly$growth_no20, na.rm = TRUE)
growth_sd <- sd(french_df_mly$growth_no20, na.rm = TRUE)
min_val <- min(c((french_df_mly$growth_no20-growth_mean)/growth_sd, french_df_mly$vader_std),na.rm=TRUE)

french_plt <- ggplot(french_df_mly[which(french_df_mly$date <"2021-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Legend", values = c("GDP growth" = "black", "Y-on-Y" = "green",
                                          "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_no20-growth_mean)/growth_sd, color = "GDP growth")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits = c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  ggtitle("French GDP and news sentiment")
french_plt

min_val <- min(c((french_df_mly$growth_final-growth_mean)/growth_sd, 
                 -(french_df_mly$growth_final-growth_mean)/growth_sd, 
                 french_df_mly$vader_std),na.rm=TRUE)
french_plt20 <- ggplot(french_df_mly[which(french_df_mly$date >="2019-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Source", values = c("GDP" = "black", "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_final-growth_mean)/growth_sd, color = "GDP")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits = c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  scale_x_continuous(breaks = as.Date(c("2019-01-01", "2020-01-01")), 
                     labels = paste0(c("2019", "2020"))) + 
  ggtitle("(2019-2020)")
french_plt20
ggarrange(french_plt, french_plt20, ncol = 2, common.legend = TRUE,widths = c(4, 1))


# Germany plot
german_df_mly$date <- as.Date(german_df_mly$date)
std_cutoff <- max(which(german_df_mly$date < "2020-01-01"))
german_df_mly$loughran_std <- standardise(german_df_mly$loughran, cutoff = std_cutoff)
german_df_mly$stability_std <- standardise(german_df_mly$stability, cutoff = std_cutoff)
german_df_mly$afinn_std <- standardise(german_df_mly$afinn, cutoff = std_cutoff)
german_df_mly$vader_std <- standardise(german_df_mly$vader, cutoff = std_cutoff)
german_df_mly$econlex_std <- standardise(german_df_mly$econlex, cutoff = std_cutoff)
german_df_mly$growth_no20 <- german_df_mly$growth_final
german_df_mly$growth_no20[which(german_df_mly$date >= "2020-01-01")] <-NA
growth_mean <- mean(german_df_mly$growth_no20, na.rm = TRUE)
growth_sd <- sd(german_df_mly$growth_no20, na.rm = TRUE)
min_val <- min(c((german_df_mly$growth_no20-growth_mean)/growth_sd, german_df_mly$vader_std),na.rm=TRUE)

german_plt <- ggplot(german_df_mly[which(german_df_mly$date <"2021-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Legend", values = c("GDP growth" = "black", "Y-on-Y" = "green",
                                          "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_no20-growth_mean)/growth_sd, color = "GDP growth")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits = c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  ggtitle("German GDP and news sentiment")
german_plt

min_val <- min(c((german_df_mly$growth_final-growth_mean)/growth_sd, 
                 -(german_df_mly$growth_final-growth_mean)/growth_sd, 
                 german_df_mly$vader_std),na.rm=TRUE)
german_plt20 <- ggplot(german_df_mly[which(german_df_mly$date >="2019-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Source", values = c("GDP" = "black", "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_final-growth_mean)/growth_sd, color = "GDP")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits = c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  scale_x_continuous(breaks = as.Date(c("2019-01-01", "2020-01-01")), 
                     labels = paste0(c("2019", "2020"))) + 
  ggtitle("(2019-2020)")
german_plt20
ggarrange(german_plt, german_plt20, ncol = 2, common.legend = TRUE,widths = c(4, 1))


# Italy plot
italian_df_mly$date <- as.Date(italian_df_mly$date)
std_cutoff <- max(which(italian_df_mly$date < "2020-01-01"))
italian_df_mly$loughran_std <- standardise(italian_df_mly$loughran, cutoff = std_cutoff)
italian_df_mly$stability_std <- standardise(italian_df_mly$stability, cutoff = std_cutoff)
italian_df_mly$afinn_std <- standardise(italian_df_mly$afinn, cutoff = std_cutoff)
italian_df_mly$vader_std <- standardise(italian_df_mly$vader, cutoff = std_cutoff)
italian_df_mly$econlex_std <- standardise(italian_df_mly$econlex, cutoff = std_cutoff)
italian_df_mly$growth_no20 <- italian_df_mly$growth_final
italian_df_mly$growth_no20[which(italian_df_mly$date >= "2020-01-01")] <-NA
growth_mean <- mean(italian_df_mly$growth_no20, na.rm = TRUE)
growth_sd <- sd(italian_df_mly$growth_no20, na.rm = TRUE)
min_val <- min(c((italian_df_mly$growth_no20-growth_mean)/growth_sd, italian_df_mly$vader_std),na.rm=TRUE)

italian_plt <- ggplot(italian_df_mly[which(italian_df_mly$date <"2021-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Legend", values = c("GDP growth" = "black", "Y-on-Y" = "green",
                                          "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_no20-growth_mean)/growth_sd, color = "GDP growth")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits = c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  ggtitle("Italian GDP and news sentiment")
italian_plt

min_val <- min(c((italian_df_mly$growth_final-growth_mean)/growth_sd, 
                 -(italian_df_mly$growth_final-growth_mean)/growth_sd, 
                 italian_df_mly$vader_std),na.rm=TRUE)
italian_plt20 <- ggplot(italian_df_mly[which(italian_df_mly$date >="2019-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Source", values = c("GDP" = "black", "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_final-growth_mean)/growth_sd, color = "GDP")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits = c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  scale_x_continuous(breaks = as.Date(c("2019-01-01", "2020-01-01")), 
                     labels = paste0(c("2019", "2020"))) + 
  ggtitle("(2019-2020)")
italian_plt20
ggarrange(italian_plt, italian_plt20, ncol = 2, common.legend = TRUE,widths = c(4, 1))


# Spain plot
spanish_df_mly$date <- as.Date(spanish_df_mly$date)
std_cutoff <- max(which(spanish_df_mly$date < "2020-01-01"))
spanish_df_mly$loughran_std <- standardise(spanish_df_mly$loughran, cutoff = std_cutoff)
spanish_df_mly$stability_std <- standardise(spanish_df_mly$stability, cutoff = std_cutoff)
spanish_df_mly$afinn_std <- standardise(spanish_df_mly$afinn, cutoff = std_cutoff)
spanish_df_mly$vader_std <- standardise(spanish_df_mly$vader, cutoff = std_cutoff)
spanish_df_mly$econlex_std <- standardise(spanish_df_mly$econlex, cutoff = std_cutoff)
spanish_df_mly$growth_no20 <- spanish_df_mly$growth_final
spanish_df_mly$growth_no20[which(spanish_df_mly$date >= "2020-01-01")] <-NA
growth_mean <- mean(spanish_df_mly$growth_no20, na.rm = TRUE)
growth_sd <- sd(spanish_df_mly$growth_no20, na.rm = TRUE)
min_val <- min(c((spanish_df_mly$growth_no20-growth_mean)/growth_sd, spanish_df_mly$vader_std),na.rm=TRUE)

spanish_plt <- ggplot(spanish_df_mly[which(spanish_df_mly$date <"2021-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Legend", values = c("GDP growth" = "black", "Y-on-Y" = "green",
                                          "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_no20-growth_mean)/growth_sd, color = "GDP growth")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits = c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  ggtitle("Spanish GDP and news sentiment")
spanish_plt

min_val <- min(c((spanish_df_mly$growth_final-growth_mean)/growth_sd, 
                 -(spanish_df_mly$growth_final-growth_mean)/growth_sd, 
                 spanish_df_mly$vader_std),na.rm=TRUE)
spanish_plt20 <- ggplot(spanish_df_mly[which(spanish_df_mly$date >="2019-01-01"),]) + aes(x = date) + xlab("Date") + 
  scale_color_manual("Source", values = c("GDP" = "black", "LM metric" = "steelblue", 
                                          "CGLM metric" =  "seagreen4", 
                                          "AFINN metric" = "firebrick",
                                          "VADER metric" = "purple", 
                                          "ECONLEX metric" = "darkorange")) + theme_bw() + 
  geom_line(aes(y=(growth_final-growth_mean)/growth_sd, color = "GDP")) + 
  geom_line(aes(y=loughran_std, color = "LM metric"), linetype = "dashed") + 
  geom_line(aes(y=afinn_std, color = "AFINN metric"), linetype = "dashed") + 
  geom_line(aes(y=vader_std, color = "VADER metric"), linetype = "dashed") + 
  geom_line(aes(y=stability_std, color = "CGLM metric"), linetype = "dashed") + 
  geom_line(aes(y=econlex_std, color = "ECONLEX metric"), linetype = "dashed") + 
  scale_y_continuous("Std.units", limits = c(min_val-0.3,-min_val+0.3), 
                     sec.axis = sec_axis(~ (. + growth_mean)*growth_sd, name = "Growth rate")) + 
  scale_x_continuous(breaks = as.Date(c("2019-01-01", "2020-01-01")), 
                     labels = paste0(c("2019", "2020"))) + 
  ggtitle("(2019-2020)")
spanish_plt20
ggarrange(spanish_plt, spanish_plt20, ncol = 2, common.legend = TRUE,widths = c(4, 1))


ggarrange(french_plt, french_plt20, german_plt, german_plt20, 
          italian_plt, italian_plt20, spanish_plt, spanish_plt20,
          nrow = 4, ncol = 2, common.legend = TRUE,widths = c(4, 1))
ggsave("figures/countries_sent.pdf", width = 10, height = 12)







### End of script ###