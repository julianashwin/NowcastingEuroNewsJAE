"
This file creates real time PMI, GDP and projections data
Note that this file will not run from the replication package, as some of the underlying data are sensitive/proprietary
However, for transparency, we include this file to show how the data are cleaned
"
setwd("/Users/julianashwin/Documents/GitHub/NowcastingEuroNewsJAE/")

require(stringr)
require(lubridate)
library(zoo)
require(readxl)

all_xls <- dir("data/realtime_raw/")

### Some functions to deal with dates
dayofquarter <- function(df){
  df$date <- as.Date(df$date)
  df$quarter <- as.Date(df$quarter)
  df$doq <- NA
  for (dt in 1:nrow(df)){
    df$doq[dt] <- as.numeric(df$date[dt] - df$quarter[dt]) + 1
  }
  return(df)
}
monthofquarter <- function(df){
  df$moq <- month(df$month)
  df$moq[which(df$moq %in% c(1,4,7,10))] <- 1
  df$moq[which(df$moq %in% c(2,5,8,11))] <- 2
  df$moq[which(df$moq %in% c(3,6,9,12))] <- 3
  return(df)
}
dayofmonth <- function(df){
  df$dom <- day(df$date)
  return(df)
}

# filename <- "data/realtime_raw/RealTimeVintages_EAfinal.xls"
# sheetname <- "pmicomp"
# Function to import realtime vintage data
import_realtime <- function(filename, sheetname){
  # Import data and throw out empty rows
  raw_df <- as.data.frame(read_xls(filename, sheet = sheetname, col_names = FALSE))
  release_dates <- as.Date(openxlsx::convertToDate(raw_df[1,], origin = "1899-12-30"), format = "%Y-%m-%d")
  starting_point <- which(!is.na(raw_df[,3]))[2]
  clean_df <- raw_df[starting_point:nrow(raw_df),]
  # Rename columns
  colnames(clean_df) <- paste("release",as.character(release_dates), sep = "-")
  colnames(clean_df)[1] <- "year"
  colnames(clean_df)[2] <- "month"
  release_dates <- release_dates[which(!is.na(release_dates))]
  # Convert the month column into an R date and create quarter
  clean_df$month <- as.character(clean_df$month)
  clean_df$month[which(nchar(clean_df$month)==1)] <- paste0("0", clean_df$month[which(nchar(clean_df$month)==1)])
  clean_df$month <- as.Date(paste(clean_df$year,clean_df$month, "01", sep = "-"))
  clean_df$quarter <- floor_date(clean_df$month, unit = "quarters")
  # Identify where in the quarter we are
  clean_df <- monthofquarter(clean_df)
  
  # Re-order columns
  clean_df <- clean_df[,c("year", "quarter", "month", "moq", paste("release",as.character(release_dates), sep = "-"))]
  rownames(clean_df) <- NULL
  
  return(clean_df)
}



#filename <- "data/Vintages_projections_2005_20.xlsx" 
#sheetname <- "EA"
import_projections <- function(filename, sheetname){
  
  # Import early Euro Area projections where we don't have the proper ones
  early_df <- read.csv("data/projections/early_ECB_projections.csv", stringsAsFactors = FALSE)
  early_df$release_date <- as.Date(early_df$release_date, format = "%d/%m/%Y")
  
  # Import data and throw out empty rows
  raw_df <- as.data.frame(read_xlsx(filename, sheet = sheetname, col_names = FALSE))
  raw_df <- raw_df[,-1]
  raw_df[3,1] <- "ref_date"
  # Tidy up the projections data
  release_dates <- openxlsx::convertToDate(raw_df[3,2:ncol(raw_df)])
  col_names <- c(raw_df[3,1], paste0("release_",release_dates))
  colnames(raw_df) <- col_names
  raw_df<- raw_df[-c(1:3),]
  rownames(raw_df)<-NULL
  raw_df$ref_date <- as.Date(as.yearqtr(raw_df$ref_date, format = "%YQ%q"))
  
  
  # Create dates for real time measures
  proj_df <- data.frame(date = seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by = "days"))
  proj_df$quarter <- floor_date(proj_df$date, unit = "quarters")
  # Include latest SPF forecast for this year
  proj_df$ECB_proj_latest <- NA
  start_row <- which(proj_df$date == min(release_dates))
  for (ii in start_row:nrow(proj_df)){
    this_quarter <- proj_df$quarter[ii]
    latest_proj <- release_dates[max(which(release_dates <= proj_df$date[ii]))]
    
    quarter_projs <- raw_df[which(raw_df$ref_date == this_quarter),]
    proj_df$ECB_proj_latest[ii] <- quarter_projs[1,paste0("release_", latest_proj)]
  }
  
  # Use the annual projections for early period for now
  start_row2 <- which(proj_df$date == min(early_df$release_date))
  for (ii in start_row2:(start_row-1)){
    this_year <- year(proj_df$date[ii])
    latest_proj <- max(which(early_df$release_date <= proj_df$date[ii]))
    proj_df$ECB_proj_latest[ii] <- early_df[latest_proj,paste0("ref_", this_year)]/4
  }
  
  return(proj_df)
}


import_spf <- function(filename){
  # Import data 
  raw_df <- read.csv(filename)
  # Throw out the irrelevant columns and rows
  average_cols <- which(str_detect(raw_df[2,],"Average"))
  raw_df <-raw_df[,c(1,average_cols)]
  raw_df<- raw_df[-c(1,3,4),]
  rownames(raw_df)<-NULL
  # Create column names
  raw_df[1,1] <- "spf_round"
  raw_df[1,2:ncol(raw_df)] <- str_remove(raw_df[1,2:ncol(raw_df)], ", Average of forecasts")
  raw_df[1,2:ncol(raw_df)] <- paste0("ref_",raw_df[1,2:ncol(raw_df)])
  colnames(raw_df) <-raw_df[1,]
  raw_df <- raw_df[-1,]
  raw_df <- raw_df[order(raw_df$spf_round),]
  rownames(raw_df)<-NULL
  # Publication dates for SPF rounds
  round_dates <- read.csv("data/projections/SPF_rounds_dates.csv",stringsAsFactors = FALSE)
  round_dates$results_published <- as.Date(round_dates$results_published, format = "%d/%m/%Y")
  round_dates <- round_dates[,c("spf_round", "results_published")]
  round_dates$results_published[1:3] <- as.Date(c("1999-03-09","1999-06-15","1999-09-21"))
  # Merge spf with publication dates
  raw_df <- merge(round_dates, raw_df, by = "spf_round", all.y = TRUE)
  # Manually add publication dates where they aren't included (from ECB website)
  raw_df$results_published[which(raw_df$spf_round == "2020Q4")] <- as.Date("2020-10-30")
  raw_df$results_published[which(raw_df$spf_round == "2021Q1")] <- as.Date("2021-01-22")
  
  # Create dates for real time measures
  spf_df <- data.frame(date = seq(as.Date("1999-01-01"), as.Date("2020-12-31"), by = "days"))
  spf_df$quarter <- floor_date(spf_df$date, unit = "quarters")
  spf_df$year <- year(spf_df$date)
  # Include latest SPF forecast for this year
  spf_df$SPF_latest <- NA
  start_row <- which(spf_df$date == min(raw_df$results_published))
  for (ii in start_row:nrow(spf_df)){
    this_year <- spf_df$year[ii]
    latest_spf <- max(which(raw_df$results_published <= spf_df$date[ii]))
    spf_df$SPF_latest[ii] <- as.numeric(raw_df[latest_spf,paste0("ref_",this_year)])
  }

  return(spf_df)
}








# Keep these here for de-bugging
#pmi_final_df <- euro_pmi_df
#pmi_flash_df <- euro_pmi_flash_df
#gdp_final_df <- euro_gdp_df
#gdp_flash_df <- euro_gdp_flash_df

create_realtime <- function(pmi_final_df, pmi_flash_df, gdp_final_df, gdp_flash_df){
  
  # Create df and fill in the dates
  all_dates <- seq(min(pmi_final_df$month), as.Date("2020-12-31"), by = "days")
  realtime_df <- data.frame(date = all_dates)
  realtime_df$month <- floor_date(realtime_df$date, unit = "months")
  realtime_df$quarter <- floor_date(realtime_df$date, unit = "quarters")
  realtime_df <- dayofquarter(realtime_df)
  realtime_df <- monthofquarter(realtime_df)
  realtime_df$dom <- day(realtime_df$date)
  realtime_df$dow <- weekdays(realtime_df$date)
  # Columns for the PMI
  realtime_df[,c("pmi_daily", "pmi_final", "pmi_releases",
                 "pmi_m1_latest", "pmi_m2_latest", "pmi_m3_latest",
                 "pmi_m1_prevq_latest", "pmi_m2_prevq_latest", "pmi_m3_prevq_latest",
                 "pmi_m1_final", "pmi_m2_final", "pmi_m3_final",
                 "pmi_m1_prevq_final", "pmi_m2_prevq_final", "pmi_m3_prevq_final")] <- NA
  # Columns for GDP
  realtime_df[,c("gdp_latest", "gdp_1lag_latest", "gdp_2lag_latest",  "gdp_3lag_latest", 
                 "gdp_4lag_latest",  "gdp_5lag_latest",  "gdp_6lag_latest",
                 "gdp_final", "gdp_1lag_final", "gdp_2lag_final", "gdp_3lag_final",
                 "gdp_4lag_final", "gdp_5lag_final", "gdp_6lag_final",
                 "growth_final", "growth_1lag_final")] <- NA
  
  ## Extract release dates from colnames
  # PMI
  pmi_release_final <- colnames(pmi_final_df)[which(str_detect(colnames(pmi_final_df),"release"))]
  pmi_release_final <- as.Date(str_remove(pmi_release_final, "release-"))
  pmi_final_last <- paste0("release-",max(pmi_release_final))
  pmi_final_first <- paste0("release-",min(pmi_release_final))
  pmi_release_flash <- colnames(pmi_flash_df)[which(str_detect(colnames(pmi_flash_df),"release"))]
  pmi_release_flash <- as.Date(str_remove(pmi_release_flash, "release-"))
  # GDP
  gdp_release_final <- colnames(gdp_final_df)[which(str_detect(colnames(gdp_final_df),"release"))]
  gdp_release_final <- as.Date(str_remove(gdp_release_final, "release-"))
  gdp_final_last <- paste0("release-",max(gdp_release_final))
  gdp_final_first <- paste0("release-",min(gdp_release_final))
  gdp_release_flash <- colnames(gdp_flash_df)[which(str_detect(colnames(gdp_flash_df),"release"))]
  gdp_release_flash <- as.Date(str_remove(gdp_release_flash, "release-"))
  # All
  all_release <- c(pmi_release_final, pmi_release_flash, gdp_release_final, gdp_release_flash)
  first_vintage <- min(all_release)
  
  
  ### Loop through every date in realtime_df
  pb = txtProgressBar(min = 1, max = nrow(realtime_df), initial = 1) 
  for (dt in 1:nrow(realtime_df)){
    setTxtProgressBar(pb,dt)
    today <- realtime_df$date[dt]
    month <- realtime_df$month[dt]
    quarter <- realtime_df$quarter[dt]
    
    # Identify the row of each df we are interested in for this day
    pmi_final_row <- which(pmi_final_df$month == month)
    pmi_flash_row <- which(pmi_flash_df$month == month)
    gdp_final_row <- which(gdp_final_df$quarter == quarter)
    gdp_flash_row <- which(gdp_flash_df$quarter == quarter)
    # Identify all PMI months from this and previous quarters (for MIDAS)
    pmi_quarter_final_df <- pmi_final_df[which(pmi_final_df$quarter == quarter),]
    pmi_quarterlag_final_df <- pmi_final_df[which(pmi_final_df$quarter == (quarter %m-% months (3))),]
    pmi_quarter_flash_df <- pmi_flash_df[which(pmi_flash_df$quarter == quarter),]
    pmi_quarterlag_flash_df <- pmi_flash_df[which(pmi_flash_df$quarter == (quarter %m-% months (3))),]
    
    ## For the "final" estimate, use the last available vintage
    # GDP
    realtime_df[dt, "gdp_final"] <- gdp_final_df[gdp_final_row,gdp_final_last]
    realtime_df[dt, "gdp_1lag_final"] <- gdp_final_df[(gdp_final_row-1),gdp_final_last]
    realtime_df[dt, "gdp_2lag_final"] <- gdp_final_df[(gdp_final_row-2),gdp_final_last]
    realtime_df[dt, "gdp_3lag_final"] <- gdp_final_df[(gdp_final_row-3),gdp_final_last]
    realtime_df[dt, "gdp_4lag_final"] <- gdp_final_df[(gdp_final_row-4),gdp_final_last]
    realtime_df[dt, "gdp_5lag_final"] <- gdp_final_df[(gdp_final_row-5),gdp_final_last]
    realtime_df[dt, "gdp_6lag_final"] <- gdp_final_df[(gdp_final_row-6),gdp_final_last]
    # Growth 
    if (!is.na(realtime_df$gdp_final[dt])){
      realtime_df[dt, "growth_final"] <- (realtime_df$gdp_final[dt]/realtime_df$gdp_1lag_final[dt] -1)*100
    } 
    if (!is.na(realtime_df$gdp_1lag_final[dt])){
      realtime_df[dt, "growth_1lag_final"] <- (realtime_df$gdp_1lag_final[dt]/realtime_df$gdp_2lag_final[dt] -1)*100
    }
    # PMI
    realtime_df[dt, "pmi_final"] <- pmi_final_df[pmi_final_row,pmi_final_last]
    if (nrow(pmi_quarter_final_df) ==3){
      realtime_df[dt, "pmi_m1_final"] <- pmi_quarter_final_df[
        which(pmi_quarter_final_df$moq == 1),pmi_final_last]
    }
    realtime_df[dt, "pmi_m2_final"] <- pmi_quarter_final_df[
      which(pmi_quarter_final_df$moq == 2),pmi_final_last]
    realtime_df[dt, "pmi_m3_final"] <- pmi_quarter_final_df[
      which(pmi_quarter_final_df$moq == 3),pmi_final_last]
    if (nrow(pmi_quarterlag_final_df)>0){
      if (nrow(pmi_quarterlag_final_df) ==3){
        realtime_df[dt, "pmi_m1_prevq_final"] <- pmi_quarterlag_final_df[
          which(pmi_quarterlag_final_df$moq == 1),pmi_final_last]
      }
      realtime_df[dt, "pmi_m2_prevq_final"] <- pmi_quarterlag_final_df[
        which(pmi_quarterlag_final_df$moq == 2),pmi_final_last]
      realtime_df[dt, "pmi_m3_prevq_final"] <- pmi_quarterlag_final_df[
        which(pmi_quarterlag_final_df$moq == 3),pmi_final_last]
    } else{
      realtime_df[dt, "pmi_m1_prevq_final"] <- NA
      realtime_df[dt, "pmi_m2_prevq_final"] <- NA
      realtime_df[dt, "pmi_m3_prevq_final"] <- NA
    }
    
    
    if (month <= floor_date(first_vintage, unit = "month")){
      ## For dates before the vintages are available, just use the first real-time estimate
      # GDP
      realtime_df[dt, "gdp_latest"] <- NA
      if (realtime_df$doq[dt] >= 45){
        realtime_df[dt, "gdp_1lag_latest"] <- gdp_final_df[(gdp_final_row-1),gdp_final_first]
      }
      realtime_df[dt, "gdp_2lag_latest"] <- gdp_final_df[(gdp_final_row-2),gdp_final_first]
      realtime_df[dt, "gdp_3lag_latest"] <- gdp_final_df[(gdp_final_row-3),gdp_final_first]
      realtime_df[dt, "gdp_4lag_latest"] <- gdp_final_df[(gdp_final_row-4),gdp_final_first]
      realtime_df[dt, "gdp_5lag_latest"] <- gdp_final_df[(gdp_final_row-5),gdp_final_first]
      realtime_df[dt, "gdp_6lag_latest"] <- gdp_final_df[(gdp_final_row-6),gdp_final_first]
      
      # PMI
      if ((realtime_df$moq[dt] == 1 & realtime_df$dom[dt]>=24) | realtime_df$moq[dt] > 1){
        if(nrow(pmi_quarter_final_df)==2){
          realtime_df[dt,"pmi_m1_latest"] <- NA
        } else{
          realtime_df[dt,"pmi_m1_latest"] <- pmi_quarter_final_df[
            which(pmi_quarter_final_df$moq == 1), pmi_final_first]
        }
      } 
      if ((realtime_df$moq[dt] == 2 & realtime_df$dom[dt]>=24)| realtime_df$moq[dt] > 2){
        realtime_df[dt,"pmi_m2_latest"] <- pmi_quarter_final_df[
          which(pmi_quarter_final_df$moq == 2),pmi_final_first]
      } 
      if (realtime_df$moq[dt] == 3 & realtime_df$dom[dt] >= 24){
        realtime_df[dt,"pmi_m3_latest"] <- pmi_quarter_final_df[
          which(pmi_quarter_final_df$moq == 3),pmi_final_first]
      }
      if (nrow(pmi_quarterlag_final_df)>0){
        if(nrow(pmi_quarterlag_final_df)==2){
          realtime_df[dt, "pmi_m1_prevq_latest"] <- NA
        } else{
          realtime_df[dt, "pmi_m1_prevq_latest"] <- pmi_quarterlag_final_df[
            which(pmi_quarterlag_final_df$moq == 1),pmi_final_first]
        }
        realtime_df[dt, "pmi_m2_prevq_latest"] <- pmi_quarterlag_final_df[
          which(pmi_quarterlag_final_df$moq == 2),pmi_final_first]
        realtime_df[dt, "pmi_m3_prevq_latest"] <- pmi_quarterlag_final_df[
          which(pmi_quarterlag_final_df$moq == 3),pmi_final_first]
      } else{
        realtime_df[dt, "pmi_m1_prevq_latest"] <- NA
        realtime_df[dt, "pmi_m2_prevq_latest"] <- NA
        realtime_df[dt, "pmi_m3_prevq_latest"] <- NA
      }
      
    } else {
      ### Now the real fiddly bit begins
      # What are the latest available estimates
      pmi_flash_current <- max(pmi_release_flash[which(pmi_release_flash <= today)])
      if (min(pmi_release_final) <= today){
        pmi_final_current <- max(pmi_release_final[which(pmi_release_final <= today)])
      } else {
        pmi_final_current <- pmi_flash_current
      }
      gdp_flash_current <- max(pmi_release_flash[which(pmi_release_flash <= today)])
      if (min(gdp_release_final) <= today){
        gdp_final_current <- max(gdp_release_final[which(gdp_release_final <= today)])
      } else {
        gdp_final_current <- gdp_flash_current
      }
      
      # Adjust for the very first vintages (first flash comes out before first final)
      if(today < str_remove(gdp_final_first,"release-")){
        gdp_final_current <- gdp_flash_current
      }
      if(today < str_remove(pmi_final_first,"release-")){
        pmi_final_current <- pmi_flash_current
      }
      
      # GDP 
      if (gdp_flash_current >= gdp_final_current){
        # If flash is more recent, use that
        realtime_df[dt, "gdp_latest"] <- gdp_flash_df[gdp_flash_row,paste0("release-", gdp_flash_current)]
        realtime_df[dt, "gdp_1lag_latest"] <- gdp_flash_df[(gdp_flash_row-1),paste0("release-", gdp_flash_current)]
        realtime_df[dt, "gdp_2lag_latest"] <- gdp_flash_df[(gdp_flash_row-2),paste0("release-", gdp_flash_current)]
        realtime_df[dt, "gdp_3lag_latest"] <- gdp_flash_df[(gdp_flash_row-3),paste0("release-", gdp_flash_current)]
        realtime_df[dt, "gdp_4lag_latest"] <- gdp_flash_df[(gdp_flash_row-4),paste0("release-", gdp_flash_current)]
        realtime_df[dt, "gdp_5lag_latest"] <- gdp_flash_df[(gdp_flash_row-5),paste0("release-", gdp_flash_current)]
        realtime_df[dt, "gdp_6lag_latest"] <- gdp_flash_df[(gdp_flash_row-6),paste0("release-", gdp_flash_current)]
      } else if (gdp_flash_current < gdp_final_current){
        # If final is more recent, use that
        realtime_df[dt, "gdp_latest"] <- gdp_final_df[gdp_final_row,paste0("release-", gdp_final_current)]
        realtime_df[dt, "gdp_1lag_latest"] <- gdp_final_df[(gdp_final_row-1),paste0("release-", gdp_final_current)]
        realtime_df[dt, "gdp_2lag_latest"] <- gdp_final_df[(gdp_final_row-2),paste0("release-", gdp_final_current)]
        realtime_df[dt, "gdp_3lag_latest"] <- gdp_final_df[(gdp_final_row-3),paste0("release-", gdp_final_current)]
        realtime_df[dt, "gdp_4lag_latest"] <- gdp_final_df[(gdp_final_row-4),paste0("release-", gdp_final_current)]
        realtime_df[dt, "gdp_5lag_latest"] <- gdp_final_df[(gdp_final_row-5),paste0("release-", gdp_final_current)]
        realtime_df[dt, "gdp_6lag_latest"] <- gdp_final_df[(gdp_final_row-6),paste0("release-", gdp_final_current)]
      }
      # PMI 
      if (pmi_flash_current >= pmi_final_current){
        realtime_df[dt, "pmi_m1_latest"] <- pmi_quarter_flash_df[
          which(pmi_quarter_flash_df$moq == 1), paste0("release-", pmi_flash_current)]
        realtime_df[dt, "pmi_m2_latest"] <- pmi_quarter_flash_df[
          which(pmi_quarter_flash_df$moq == 2), paste0("release-", pmi_flash_current)]
        realtime_df[dt, "pmi_m3_latest"] <- pmi_quarter_flash_df[
          which(pmi_quarter_flash_df$moq == 3), paste0("release-", pmi_flash_current)]
        realtime_df[dt, "pmi_m1_prevq_latest"] <- pmi_quarterlag_flash_df[
          which(pmi_quarterlag_flash_df$moq == 1), paste0("release-", pmi_flash_current)]
        realtime_df[dt, "pmi_m2_prevq_latest"] <- pmi_quarterlag_flash_df[
          which(pmi_quarterlag_flash_df$moq == 2), paste0("release-", pmi_flash_current)]
        realtime_df[dt, "pmi_m3_prevq_latest"] <- pmi_quarterlag_flash_df[
          which(pmi_quarterlag_flash_df$moq == 3), paste0("release-", pmi_flash_current)]
      } else if (pmi_flash_current < gdp_final_current){
        realtime_df[dt, "pmi_m1_latest"] <- pmi_quarter_final_df[
          which(pmi_quarter_final_df$moq == 1), paste0("release-", pmi_final_current)]
        realtime_df[dt, "pmi_m2_latest"] <- pmi_quarter_final_df[
          which(pmi_quarter_final_df$moq == 2), paste0("release-", pmi_final_current)]
        realtime_df[dt, "pmi_m3_latest"] <- pmi_quarter_final_df[
          which(pmi_quarter_final_df$moq == 3), paste0("release-", pmi_final_current)]
        realtime_df[dt, "pmi_m1_prevq_latest"] <- pmi_quarterlag_final_df[
          which(pmi_quarterlag_final_df$moq == 1), paste0("release-", pmi_final_current)]
        realtime_df[dt, "pmi_m2_prevq_latest"] <- pmi_quarterlag_final_df[
          which(pmi_quarterlag_final_df$moq == 2), paste0("release-", pmi_final_current)]
        realtime_df[dt, "pmi_m3_prevq_latest"] <- pmi_quarterlag_final_df[
          which(pmi_quarterlag_final_df$moq == 3), paste0("release-", pmi_final_current)]
      }
    } 
  } # end of loop over days
  
  # PMI daily and releases  
  realtime_df$pmi_daily <- rowMeans(realtime_df[,c("pmi_m1_latest", "pmi_m2_latest", "pmi_m3_latest")], na.rm = TRUE)
  realtime_df$pmi_daily[which(is.na(realtime_df$pmi_m1_latest))] <- realtime_df$pmi_m3_prevq_latest[which(is.na(realtime_df$pmi_m1_latest))]
  realtime_df$pmi_daily[which(is.na(realtime_df$pmi_daily))] <- realtime_df$pmi_m2_prevq_latest[which(is.na(realtime_df$pmi_daily))]
  realtime_df$pmi_releases <- rowSums(!is.na(realtime_df[,c("pmi_m1_latest", "pmi_m2_latest", "pmi_m3_latest")]))
  
  
  return(realtime_df)
}

### Euro Area
# Import  data
euro_pmi_df <- import_realtime("data/realtime_raw/RealTimeVintages_EAfinal.xls", "pmicomp")
euro_pmi_flash_df <- import_realtime("data/realtime_raw/RealTimeVintages_EAflash.xls", "pmicomp")
euro_gdp_df <- import_realtime("data/realtime_raw/RealTimeVintages_EAfinal.xls", "GDP")
euro_gdp_flash_df <- import_realtime("data/realtime_raw/RealTimeVintages_EAflash.xls", "GDP")
# Create realtime dataframe
euro_realtime_df <- create_realtime(euro_pmi_df, euro_pmi_flash_df, euro_gdp_df, euro_gdp_flash_df)
# Add in SPF and realtime projections
euro_spf_df <- import_spf("data/projections/spf_data.csv")
euro_realtime_df <- merge(euro_realtime_df, euro_spf_df[,c("date", "SPF_latest")], 
                          by = "date",all.x= TRUE)
euro_proj_df <- import_projections("data/projections/Vintages_projections_2005_20.xlsx", "EA")
euro_realtime_df <- merge(euro_realtime_df, euro_proj_df[,c("date", "ECB_proj_latest")], 
                          by = "date",all.x= TRUE)
# De-clutter by removing the intermediate dataframes
rm(euro_pmi_df, euro_pmi_flash_df, euro_gdp_df, euro_gdp_flash_df, euro_proj_df)
# Plot to check the data looks sensible
ggplot(euro_realtime_df,aes(x=date)) + geom_line(aes(y=gdp_5lag_latest/gdp_6lag_latest))+ 
  geom_line(aes(y=gdp_5lag_final/gdp_6lag_final),color= "red")
ggplot(euro_realtime_df,aes(x=date)) + geom_line(aes(y=pmi_daily))
ggplot(euro_realtime_df,aes(x=date)) + geom_line(aes(y=growth_final, color = "Growth")) + 
  geom_line(aes(y=SPF_latest, color = "SPF")) + 
  geom_line(aes(y=ECB_proj_latest, color = "ECB proj"))
# Export
write.csv(euro_realtime_df,"data/euro_dailyind.csv", row.names = FALSE)






######################## End of script ######################## 
