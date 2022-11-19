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
  mutate(value_log = log10(value1))

#df_country <- df_country1%>% mutate(value= 
                                    #case_when(  (value1 <5 ) ~ "1-4",
                                                ##value1==1 ~"1",
                                                ##value1==2 ~"2",
                                                ##value1==3 ~"3",
                                                ##value1==4 ~"4",
                                                #(value1 <11 & value1 >4) ~ "5-9",
                                                #(value1 <21 & value1 >9)~ "10-19",
                                                #(value1 <31 & value1 >19)~ "20-29",
                                                #(value1 <41 & value1 >29)~ "30-39",
                                                #value1==96 ~"96")) %>% 
                                                #select(value1,updated_country)

df_country <- df_country1

#Change col names
colnames(df_country)<- c('value', 'country', 'value_log')
df_country$country <- as.factor(df_country$country)

################################################################################
#Plot

#Joining the data with a map 
df_country_map <- joinCountryData2Map(df_country, joinCode="NAME", nameJoinColumn="country", verbose=TRUE)

#Remove Antarctica from the world map 
df_country_new <- subset(df_country_map, continent != "Antarctica")

# add color palette
#  unique((df_country$value)) # 15 avleus
#YYPalette <- RColorBrewer::brewer.pal(9,"YlGnBu") #PuBuGn

YYPalette <- RColorBrewer::brewer.pal(17, 'Greens')

#Create a map 

#par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# def. map parameters
mapParams <- mapCountryData(df_country_new, 
                            nameColumnToPlot = "value",  
                            #catMethod='logFixedWidth',
                            #catMethod =  c(5,10,20,30,40,96), #"categorical",
                            #catMethod =  c(1, 2, 3, 6, 13, 21, 32, 93), #"categorical",
                            catMethod = c(1, 2, 3, 6, 13, 21, 32, 93),
                            missingCountryCol = gray(.98), #"white"
                            addLegend = F, 
                            lwd = .8,
                            borderCol = 'black',
                            colourPalette= YYPalette[2 : 8])
#colourPalette= "diverging")

#do.call(addMapLegend, c(mapParams, legendWidth=1, legendMar = 2))
do.call(addMapLegend, c(mapParams, legendLabels='all', 
                        legendIntervals='page'))




#do.call(addMapLegendBoxes, c(mapParams,
                             #x = 'bottom',
                            # title = "No. of teams",
                             #horiz = T,
                            # bg = "transparent",
                            # bty = "n", 
                            # #legendLabels="all",
                            # legendWidth=0.5))

ggsave("map1.png", width = 6, height = 6, dpi=600)



