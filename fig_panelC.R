#Purpose: Plot the main summary stats of the EEGManyPipes sample
#Project: EEGManyPipes
#Paper: Position Paper
#Author: D. Truebutschek, M. Vinding, & Y. Yang
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
#my_colors = c('#FFDF00', '#B40F20', '#046c9A', '#FFAA33')
my_colors = c('#046c9A', '#046c9A', '#046c9A', '#046c9A')
my_colors = brewer.pal(n=8, name='RdBu')[8:-1:5]
my_alphas = c(1, 1, 1, 1)
theme_set(theme_classic())

g <- ggplot(dat2plot, aes(x=teamSize, fill=gender_recoded, y=counts)) +
  geom_bar(stat='identity') +
  #geom_text(aes(x=teamSize, y=counts, label=paste0(sprintf("%4.0f", round(proportions, digits=2)*100), '%'),
                                                   #group=gender_recoded),
            #position = position_stack(vjust=.5),
            #family='sans', size=3, fontface='bold', color='white') +
  geom_text(aes(x=teamSize, y=counts, label=sprintf("%4.0f", round(proportions, digits=2)*100),
                                                    group=gender_recoded),
            position = position_stack(vjust=0.5),
            family='sans', size=6, fontface='bold', color='white') +
  labs(x='Team size', y='Number of analysts', fill='Gender') +
  theme(axis.title=element_text(size=18, color='dimgray', face='bold'),
        axis.text=element_text(size=16, color='dimgray')) +
  #ylim(c(0, 261)) +
  theme(legend.title=element_text(size=16, color='dimgray', face='bold'), 
        legend.position='top',
        legend.justification='right',
        legend.margin=margin(t=0),
        legend.text=element_text(size=16, color='dimgray')) +
  #Re-order contents of legend to appear in the same order as the bars
  guides(fill=guide_legend(reverse=TRUE, title.position='top', title.hjust=.5)) +
  coord_flip() +
  scale_x_discrete(limits=rev, expand=c(0,0)) + 
  scale_y_continuous(limits=c(-1, 262), expand=c(0,0)) +                   
  scale_fill_manual(values=my_colors, labels=c('diverse', 'women', 'men', 'unknown')) +
  #scale_alpha_manual(values=my_alphas) +
  theme(aspect.ratio=.3) + 
  theme(plot.margin=margin(t=0),
        axis.title.y=element_text(angle=0, vjust=1.2,
                                  margin=margin(t=0, r=-75, b=0, l=0)))

ggsave("Descriptives_PanelC.png", width = 6, height = 4, dpi=600)












