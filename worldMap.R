#Purpose: Plot the world map for the EEGManyPipes sample
#Project: EEGManyPipes
#Paper: Position Paper
#Author: Y. Yang D. Truebutschek, & M. Vinding, 
#Date: 17-10-2022

################################################################################
#Necessary imports 

library(ggplot2)
library(ggrepel)
library(tidyverse)
library(rworldmap)
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
data <- read.csv("final_data.csv")

################################################################################
#Check how many within country vs. between country teams there are
data$country <- as.factor(data$country)

dat2analyze <- data %>%
  select(team, country) %>%
  arrange(team) %>%
  group_by(team) %>%
  mutate(teamSize=n()) %>%
  ungroup() 

dat2analyze$teamSize <- as.integer(dat2analyze$teamSize)  
    
dat2analyze_subset <- dat2analyze %>% 
  filter(teamSize > 1) %>%
  group_by(team) %>%
  mutate(same = n_distinct(country)) %>%
  ungroup()

dat2analyze_final <- dat2analyze_subset %>%
  filter(!duplicated(team))

sum(dat2analyze_final$same == 1)
sum(dat2analyze_final$same > 1)

################################################################################
#Prepare data for plotting
data$country <- as.factor(data$country)

#Extract # of teams for the different countries
df_frq <- data %>% 
  select(country) %>% 
  mutate(country = 
           case_when(country =="Korea" | country =="Republic of Korea" ~"South Korea",
                     country == "UAE"~"United Arab Emirates",
                     country == "USA" ~ "United States",
                     TRUE ~ as.character(country))) %>% 
  group_by(country) %>%   
  mutate(value1=n()) %>% 
  arrange(value1) %>% 
  select(value1,country) %>% 
  ungroup()

df_frq$value1 <- as.numeric(df_frq$value1)
head(df_frq)

#Grouping data
df_country1 <-unique(df_frq) #37 countries
df_country1 <- df_country1 %>%
  mutate(value =
           case_when((value1==1) ~ '1',
                     (value1==2) ~ '2',
                     (value1==3) ~ '3',
                     (value1 >=4 & value1 <=5) ~ '4',
                     (value1 >=6 & value1 <=10) ~ '5',
                     (value1 >=11 & value1 <=20) ~ '6',
                     (value1 >=21 & value1 <=40) ~ '7',
                     (value1 >40) ~ '8')) %>%

df_country <- df_country1

#Change col names
colnames(df_country)<- c('value', 'country', 'value_recoded')
df_country$country <- as.factor(df_country$country)
df_country$value_recoded <- as.factor(df_country$value_recoded)

################################################################################
#Plot

#Joining the data with a map 
df_country_map <- joinCountryData2Map(df_country, joinCode="NAME", nameJoinColumn="country", verbose=TRUE)

#Remove Antarctica from the world map 
df_country_new <- subset(df_country_map, continent != "Antarctica")

#Create colormap
YYPalette <- c('#D2E3E1', '#C3D9D7', '#B4D0CE', '#A5C7C4', '#96BDBA', '#87B4B0', '#78AAA6', '#69A19C')

#Create a map 
mapParams <- mapCountryData(df_country_new, 
                            nameColumnToPlot = "value_recoded",  
                            catMethod = 'categorical',
                            missingCountryCol = 'darkgray',#gray(.98), #"white"
                            addLegend = F, 
                            lwd = .8,
                            borderCol = 'white',
                            colourPalette= YYPalette)

do.call(addMapLegendBoxes, c(mapParams,
                             x='bottom',
                             horiz=T,
                             bg='transparent', 
                             bty='n'))



