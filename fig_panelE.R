#Purpose: Compare representative data to the EEGManyPipes data
#Project: EEGManyPipes
#Paper: Position Paper
#Author: D. Truebutschek, M. Vinding, & Y. Yang
#Date: 17-10-2022

################################################################################
#Necessary imports 
library(tidyverse)
library(optimbase)
library(ggthemes)
library(wesanderson)
library(RColorBrewer)

################################################################################
#Path definitions
if ( Sys.getenv("USER") == 'mcvinding' ){
  data.path <- '/Users/mcvinding/Documents/EEGManyPipelines/metadata_summary'
} else if (Sys.getenv("USERNAME") == 'Mikkel'){
  data.path <- 'C:/Users/Mikkel/Documents/EEGManyPipelines/metadata_summary'
} else if (Sys.getenv("USER") == 'darinka'){
  data.path <- '/home/darinka/Documents/EEGManyPipes/metadata_summary/data'
} else if (Sys.getenv("USERNAME") == 'darinka.truebutschek'){
  data.path <- 'C:/Users/darinka.truebutschek/Documents/EEGManyPipelines/metadata_summary/data'
} else {
  # Paths and file for Yu-Fang
  rm(list=ls()) 
  path= dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(path)
  getwd()
  data <- read.csv("final_data.csv")
}

setwd(data.path)

################################################################################
#Load data
data <- load('el_pubmed_cleaned.RData')

################################################################################
#Save it all as one big csv file
authors <- name
titles <- title
affiliations <- eeg_paps


df <- data.frame(authors, titles, affiliations)

write.csv(df, 'PubmedData.csv')


