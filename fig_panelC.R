#Purpose: Plot the main summary stats of the EEGManyPipes sample
#Project: EEGManyPipes
#Paper: Position Paper
#Author: D. Truebutschek, M. Vinding, & Y. Yang
#Date: 04-10-2022

################################################################################
#Necessary imports 
library(tidyverse)
length(optimbase)

################################################################################
#Path definitions
if ( Sys.getenv("USER") == 'mcvinding' ){
  data.path <- '/Users/mcvinding/Documents/EEGManyPipelines/metadata_summary'
} else if (Sys.getenv("USERNAME") == 'Mikkel'){
  data.path <- 'C:/Users/Mikkel/Documents/EEGManyPipelines/metadata_summary'
} else if (Sys.getenv("USER") == 'darinka'){
  data.path <- '/home/darinka/Documents/EEGManyPipes/metadata_summary/data'
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
#Gender distribution by team size
################################################################################
#First, add variable tracking team size (probably not most sophisticated way)
n_teams = length(unique(data$team))
teams <- data$team
team_sizes <- ones(length(teams), 1)

for (value in seq(n_teams)) {
  print(value)
  
  teamSize <- sum(teams == value)
  team_sizes[teams == value] <- teamSize
}

data$teamSize <- team_sizes  
data$teamSize <- as.factor(data$teamSize)

#Next, extract gender proportions
data$gender_recoded <- data$gender
data$gender_recoded <- as.character(data$gender_recoded)
data$gender_recoded[data$gender_recoded=='no-answer'] <- 'Unknown'
data$gender_recoded[data$gender_recoded=='NULL'] <- 'Unknown'
data$gender_recoded <- as.factor(data$gender_recoded)

genderXteam <- table(data$gender_recoded, by=data$teamSize)

#Prepare data for plotting
df_teamsize<-data.frame()
df_teamsize[1:3,1]<-c(1,2,3)
df_teamsize[1:3,2]<-c(31, 100, 261)
colnames(df_teamsize)<-c("teamSize","vector_teamSizes")

dat2plot <- data %>% 
  select(gender_recoded, teamSize) %>%
  group_by(teamSize, gender_recoded) %>%
  summarise(counts=n())
  #mutate(proportions=counts/df_teamsize$vector_teamSizes)

dat2plot<-merge(dat2plot,df_teamsize,by="teamSize")
dat2plot$proportions<-dat2plot$counts/dat2plot$vector_teamSizes

#Plot
ggplot(dat2plot, aes(x=teamSize, fill=gender_recoded, y=proportions)) +
  geom_bar(stat='identity')



