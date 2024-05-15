#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 24 11:00:32 2021

@author: julianashwin
"""

import os
import pandas as pd
import numpy as np
import copy 
import math
import time
from tqdm import tqdm


os.chdir("/Users/julianashwin/Documents/GitHub/ECB_Nowcasting/general_sentiment_metrics")


# Identify the files
import_dir_old = "/Users/julianashwin/Documents/DPhil/Raw_Data/ECB_articles/translations_nodup/"
import_dir_new = "/Users/julianashwin/Documents/DPhil/Clean_Data/ECB_articles/new_translations/"
export_dir = "/Users/julianashwin/Documents/DPhil/Clean_Data/ECB_articles/vader/"


files_old = os.listdir(import_dir_old)
files_new = os.listdir(import_dir_new)


import nltk
from nltk.stem import WordNetLemmatizer
from nltk.corpus import wordnet as wn
from nltk.corpus import sentiwordnet as swn
from nltk.sentiment.vader import SentimentIntensityAnalyzer
from nltk import sent_tokenize, word_tokenize, pos_tag
 
 


sources = ["ECHOS",  "FIGARO", "LEMOND", "DWELT", "SDDZ", "GERCOL", "TAGSS", 
           "CORDES", "LAREP", "SOLE", "STMA", "EXPNSI", "MUNDO", "PAISN", "VNGDIA"]

source = "VNGDIA"


# Import source
if (source+"_trans.csv") in files_new:
    raw_text_df_new = pd.read_csv(import_dir_new + source +"_trans.csv")
    raw_text_df_new["an"] = raw_text_df_new["ID"]
    raw_text_df_new = raw_text_df_new[["an","date", "text"]]
    raw_text_df_new.fillna("",inplace=True)
    if (source+"_trans.csv") in files_old:
        raw_text_df_old = pd.read_csv(import_dir_old + source + "_trans.csv")
        raw_text_df_old.fillna("",inplace=True)
        raw_text_df_old["text"] = raw_text_df_old.title +" "+ raw_text_df_old.snippet +" "+ raw_text_df_old.body 
        raw_text_df_old = raw_text_df_old[["an","date", "text"]]
        raw_text_df = raw_text_df_old.append(raw_text_df_new)
    else:
        raw_text_df = raw_text_df_new
else:
    raw_text_df_old = pd.read_csv(import_dir_old + source + "_trans.csv")
    raw_text_df_old.fillna("",inplace=True)
    raw_text_df_old["text"] = raw_text_df_old.title +" "+ raw_text_df_old.snippet +" "+ raw_text_df_old.body 
    raw_text_df_old = raw_text_df_old[["an","date", "text"]]
    raw_text_df = raw_text_df_old
    
raw_text_df.reset_index(inplace=True, drop=True)
    




raw_text_df["vader_sum"] = np.zeros(len(raw_text_df))
raw_text_df["vader_mean"] = np.zeros(len(raw_text_df))
raw_text_df["vader_nwords"] = np.zeros(len(raw_text_df))



wnl = nltk.WordNetLemmatizer()
sid = SentimentIntensityAnalyzer()


for ii in tqdm(range(0,len(raw_text_df))):
    # Tokenize at sentence level
    doc=raw_text_df.text[ii]
    sentences = nltk.sent_tokenize(doc)
    no_sentences = len(sentences)
    score_list = np.zeros(no_sentences)
    if(no_sentences > 0):  # Prevents crash in case of no_sentences=0
        for j, sentence in enumerate(sentences):
            # get sentiment of each sentence and
            # store the normalized, weighted composite  sentiment score
            ## stores all sentences sent
            score_list[j] = (sid.polarity_scores(sentence).get('compound', ''))
        # average sentiment of each article
    
    # Populate df with sentiword scores
    raw_text_df.at[ii, 'vader_sum'] = np.sum(score_list)
    raw_text_df.at[ii, 'vader_mean'] = np.mean(score_list)
    raw_text_df.at[ii, 'vader_nwords'] = len(score_list)
            

# No need to save the text itself, as we have the an    
sent_df = raw_text_df[["an", "date", "vader_sum", "vader_mean", "vader_nwords"]]

sent_df.to_csv(export_dir + source + "_vader.csv",index=False)





"""
End of script
"""