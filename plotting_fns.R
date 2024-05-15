mse_byperiod_init <- function(){
  mse_byday <- data.frame(doq = 1:90)
  mse_byperiod<-list(mse_06_09 = mse_byday, mse_10_13 = mse_byday, mse_14_17 = mse_byday, 
                     mse_18_19 = mse_byday, mse_20 = mse_byday, mse_pre20 = mse_byday,
                     mse_neg = mse_byday, full = mse_byday)
  return(mse_byperiod)
}


"
Populate MSE every day of quarter
"
populate_byday <- function(mse_byday, pred_df, pred_var, target = "growth_final"){
  # Create variable
  mse_byday[, paste0(pred_var,"_mse")] <- NA
  # MSE for each day
  for (ii in 1:90){
    obs <- which(pred_df$doq == ii)
    if (length(obs) > 1){
      # Compute MSE
      errors <- as.vector(unlist(pred_df[obs,target] - pred_df[obs,pred_var]))
      mse_ii <- round(mean((errors)^2, na.rm = TRUE),3)
      mse_byday[ii, paste0(pred_var,"_mse")] <- mse_ii
    }
  }
  return(mse_byday)
}

"
Populate MSE every day of quarter  for each subperiod
"
populate_byperiod <- function(mse_byperiod, pred_df, pred_var, target = "growth_final"){
  
  # 06-09
  obs <- which(pred_df$date >="2006-01-01" & pred_df$date < "2010-01-01")
  mse_byperiod$mse_06_09 <- populate_byday(mse_byperiod$mse_06_09, pred_df[obs,], pred_var, target = target)
  # 10-13
  obs <-which(pred_df$date >="2010-01-01" & pred_df$date < "2014-01-01")
  mse_byperiod$mse_10_13 <- populate_byday(mse_byperiod$mse_10_13, pred_df[obs,], pred_var, target = target)
  # 14-17
  obs <- which(pred_df$date >="2014-01-01" & pred_df$date < "2018-01-01")
  mse_byperiod$mse_14_17 <- populate_byday(mse_byperiod$mse_14_17, pred_df[obs,], pred_var, target = target)
  # 18-19
  obs <- which(pred_df$date >="2018-01-01" & pred_df$date < "2019-01-01")
  mse_byperiod$mse_18_19 <- populate_byday(mse_byperiod$mse_18_19, pred_df[obs,], pred_var, target = target)
  # 20
  obs <- which(pred_df$date >="2020-01-01" & pred_df$date < "2021-01-01")
  mse_byperiod$mse_20 <- populate_byday(mse_byperiod$mse_20, pred_df[obs,], pred_var, target = target)
  # No 2020
  obs <- which(pred_df$date < "2020-01-01")
  mse_byperiod$mse_pre20 <- populate_byday(mse_byperiod$mse_pre20, pred_df[obs,], pred_var, target = target)
  # Only negative growth quarters
  obs <- which(pred_df$growth_final < 0)
  mse_byperiod$mse_neg <- populate_byday(mse_byperiod$mse_neg, pred_df[obs,], pred_var, target = target)
  # Full 
  mse_byperiod$full <- populate_byday(mse_byperiod$full, pred_df, pred_var, target = target)
  
  return(mse_byperiod)
}



"
Compute MSE for a prediction with a rolling window
"
populate_rollingwindow <- function(mse_rolling, pred_df, pred_var, days){
  
  mse_rolling$end_date <- as.Date(mse_rolling$end_date)
  pred_df$date <- as.Date(pred_df$date)
  mse_rolling[, paste0(pred_var,"_mse")] <- NA
  # MSE for each day
  for (ii in 1:nrow(mse_rolling)){
    start_date <- mse_rolling$start_date[ii]
    end_date <- mse_rolling$end_date[ii]
    
    obs <- which(pred_df$doq %in% c(days) & pred_df$date >= start_date & pred_df$date < end_date)
    if (length(obs) > 1){
      mse_ii <- round(mean((pred_df$growth[obs] - pred_df[obs,pred_var])^2, na.rm = TRUE),3)
      mse_rolling[ii, paste0(pred_var,"_mse")] <- mse_ii
    }
  }
  return(mse_rolling)
}








# Plot the out of sample predictions
plot_msebyday <- function(mse_byday, text_var, bench_var, title = "", isOLS = FALSE){
  
  # Identify the text and benchmark models
  mse_byday$text_mse <- mse_byday[, str_c(text_var, "_mse")]
  mse_byday$bench_mse <- mse_byday[, str_c(bench_var, "_mse")]
  mse_byday$proj_mse <- mse_byday[, "ECB_proj_latest_mse"]
  
  mse_byday[,c("text_up","text_down")] <- NA
  mse_byday$text_up[which(mse_byday$text_mse >= mse_byday$bench_mse)] <- 
    mse_byday$text_mse[which(mse_byday$text_mse >= mse_byday$bench_mse)] 
  mse_byday$text_down[which(mse_byday$text_mse < mse_byday$bench_mse)] <- 
    mse_byday$text_mse[which(mse_byday$text_mse < mse_byday$bench_mse)] 
  
  # Define color scales
  scale_custom <- list(
    scale_color_manual("Models", values = c("Text model MSE" = "forestgreen", 
      "PMI model MSE" = "firebrick", "PMI OLS model MSE" =  "blue", 
      "ECB projection MSE" = "darkgoldenrod2")),
    scale_fill_manual("Text vs PMI", values = adjustcolor(c("PMI advantage" = "firebrick",
      "Text advantage" = "forestgreen"), alpha.f = 0.5)))
  # Plot MSE over cycle
  mse_plt <- ggplot(mse_byday, aes(x= doq)) + scale_custom + theme_bw() +
    geom_line(aes(y=bench_mse, color = "PMI model MSE")) +
    geom_line(aes(y=text_mse, color = "Text model MSE")) +
    xlab("Day of quarter") + ylab("MSE") +
    geom_ribbon(aes(ymin = text_up, ymax = bench_mse, fill= "PMI advantage"), show.legend = FALSE) + 
    geom_ribbon(aes(ymin = text_down, ymax = bench_mse, fill="Text advantage")) +
    #geom_line(aes(y=ols_mse, color = "PMI OLS model MSE")) +
    geom_line(aes(y=proj_mse, color = "ECB projection MSE")) +
    ggtitle(title) 
  
  
  return(mse_plt)
}

plot_decr <- function(mse_byday, metric, model = "lm", title = ""){
  
  if (metric == "stab"){
    met_name <- "CGLM"
  } else if (metric == "vader"){
    met_name <- "VADER"
  }
  # Identify the text and benchmark models
  text_var <- str_c(model, "_pred_text_mse")
  bench_var <- str_c(model, "_pred_mse")
  proj_mse <- "ECB_proj_latest_mse"
  
  mse_byday$text_mse <- mse_byday[, text_var]
  mse_byday$ols_mse <- mse_byday[, ols_var]
  mse_byday$proj_mse <- mse_byday[, proj_mse]
  
  mse_byday$bench_decr <- (1 - mse_byday$text_mse/mse_byday$bench_mse)*100
  
  mse_byday$text_up <- "PMI advantage"
  mse_byday$text_up[which(mse_byday$text_mse < mse_byday$bench_mse)] <- "Text advantage"
  
  # Define color scales
  scale_custom <- list(
    scale_color_manual("Models", values = c("Text model MSE" = "forestgreen", 
      "PMI model MSE" = "firebrick", "PMI OLS model MSE" =  "blue", 
      "ECB projection MSE" = "darkgoldenrod2")),
    scale_fill_manual("Text vs PMI", values = adjustcolor(c("PMI advantage" = "firebrick",
      "Text advantage" = "forestgreen"), alpha.f = 0.5)))
  
  # Plot MSE improvement over cycle
  decr_plt <- ggplot(mse_byday, aes(x= doq)) + scale_custom + theme_bw() + 
    geom_col(aes(y= bench_decr, fill = text_up)) +
    geom_smooth(aes(y =bench_decr), method = "loess", se = FALSE, color = "black") +
    xlab("Day of quarter") + ylab("Decrease (%)") + 
    ggtitle(title)
  
  return(decr_plt)
}







