#Purpose: Plot the gender distribution of the EEGManyPipes sample
#Project: EEGManyPipes
#Paper: Trübutschek, D. et al. EEGManyPipelines: A large-scale, grass-root multi-analyst study of EEG analysis practices in the wild. (2022). doi:10.31222/osf.io/jq342
#Author:  Y. Yang, D. Truebutschek, & M. C. Vinding,
#Date: 04-10-2022

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
  data.path <- '/home/darinka/Documents/EEGManyPipes/Position_Paper/data'
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
#Gender distribution by team size
################################################################################
#First, add variable tracking team size (probably not most sophisticated way)
n_teams = length(unique(data$team))
teams <- data$team
team_sizes <- ones(length(teams), 1)

#for (value in seq(n_teams)) {
  #print(value)

  #teamSize <- sum(teams == value)
  #team_sizes[teams == value] <- teamSize
#}

for (value in teams) {
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
df_teamsize[1:3,2]<-c(30, 96, 270)
colnames(df_teamsize)<-c("teamSize","vector_teamSizes")

dat2plot <- data %>%
  select(gender_recoded, teamSize) %>%
  group_by(teamSize, gender_recoded) %>%
  summarise(counts=n())
  #mutate(proportions=counts/df_teamsize$vector_teamSizes)

dat2plot<-merge(dat2plot,df_teamsize,by="teamSize")
dat2plot$proportions<-dat2plot$counts/dat2plot$vector_teamSizes

#Plot
#my_colors = c('#FFDF00', '#B40F20', '#046c9A', '#FFAA33')
my_colors = c('#046c9A', '#046c9A', '#046c9A', '#046c9A')
my_colors = brewer.pal(n=8, name='RdBu')[8:-1:5]
my_alphas = c(1, 1, 1, 1)
theme_set(theme_classic())

################################################################################
#Plot data - pie-chart: Gender

#Teamsize 1
teamSize1 <- dat2plot[dat2plot$teamSize == 1, ]
teamSize1$gender_recoded <- factor(teamSize1$gender_recoded,
                                      levels=c('male', 'female', 'Unknown'))

g <- ggplot(teamSize1, aes(x='', y=proportions, fill=gender_recoded))+
  geom_bar(width = 1, stat = "identity")

pie <- g + coord_polar("y", start=0) +
   #Change colors
  scale_fill_manual(values=c('#38598A', '#FFDDBD', '#797C81'),
                    labels=c('Men', 'Women', 'Unknown')) +
  #Change theme
  theme_void() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.title=element_text(size=18, color='dimgray', face='bold'),
        legend.text=element_text(size=18, color='dimgray'),
        panel.border = element_blank(),
        aspect.ratio=1)

ggsave("EMP_Sample_Gender1.png", width=6, height=8, dpi=600)
ggsave("EMP_Sample_Gender1.svg", width=6, height=8, dpi=600)

#Teamsize 2
teamSize2 <- dat2plot[dat2plot$teamSize == 2, ]
teamSize2$gender_recoded <- factor(teamSize2$gender_recoded,
                                   levels=c('male', 'female', 'Unknown'))

g <- ggplot(teamSize2, aes(x='', y=proportions, fill=gender_recoded))+
  geom_bar(width = 1, stat = "identity")

pie <- g + coord_polar("y", start=0) +
  #Change colors
  scale_fill_manual(values=c('#38598A', '#FFDDBD', '#797C81'),
                    labels=c('Men', 'Women', 'Unknown')) +
  #Change theme
  theme_void() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.title=element_text(size=18, color='dimgray', face='bold'),
        legend.text=element_text(size=18, color='dimgray'),
        panel.border = element_blank(),
        aspect.ratio=1)

ggsave("EMP_Sample_Gender2.png", width=6, height=8, dpi=600)
ggsave("EMP_Sample_Gender2.svg", width=6, height=8, dpi=600)

#Teamsize 3
teamSize3 <- dat2plot[dat2plot$teamSize == 3, ]
teamSize3$gender_recoded <- factor(teamSize3$gender_recoded,
                                   levels=c('male', 'female',
                                            'diverse', 'Unknown'))

g <- ggplot(teamSize3, aes(x='', y=proportions, fill=gender_recoded))+
  geom_bar(width = 1, stat = "identity")

pie <- g + coord_polar("y", start=0) +
  #Change colors
  scale_fill_manual(values=c('#38598A', '#FFDDBD', '#B4D0CE', '#797C81'),
                    labels=c('Men', 'Women', 'Diverse', 'Unknown')) +
  #Change theme
  theme_void() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "left",
        legend.title=element_text(size=18, color='dimgray', face='bold'),
        legend.text=element_text(size=18, color='dimgray'),
        panel.border = element_blank(),
        aspect.ratio=1)

ggsave("EMP_Sample_Gender3.png", width=6, height=8, dpi=600)
ggsave("EMP_Sample_Gender3.svg", width=6, height=8, dpi=600)
