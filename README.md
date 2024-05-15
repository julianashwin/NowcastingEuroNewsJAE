# NowcastingEuroNewsJAE

This repository is replication code for Ashwin, J., Kalamara, E., &amp; Saiz, L. (2024). Nowcasting Euro area GDP with news sentiment: A tale of two crises. Journal of Applied Econometrics. 1–19. https://doi.org/10.1002/jae.3057

A few variables in the datasets are unavailable as they include sensitive or proprietary data:
* The raw news articles are extracted from Factiva, and so cannot be shared publicly. 
* The PMI data is proprietary and so cannot be shared publicly. 
* The ECB's internal projections are not sensitive and so cannot be shared publicly. 


There are some example files that show how the sentiment metrics themselves are calculated in the sentiment_metrics folder. The main folder then includes six files:
* 1_clean_realtime.R creates real time PMI, GDP and projections data
* 2_clean_daily_country_data.R creates daily sentiment, PMI and GDP data for each country
* 3_add_macrofin_bench.R  adds in benchmarks based macro and financial variables
* 4_correlation_analysis.R compares the different translation methodologies (see Section 2.3 of the paper)
* 5_daily_analysis.R runs some of the daily nowcasting exercises for the EA
* 6_annalysis_preds.R file produces the figures based on the nowcasting analysis for the paper

Most of the relevant variables are in the euro_daily_export.csv file in the data folder.
