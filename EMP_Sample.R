#Purpose: Plot the EMP sample data 
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
library(svglite)
library(forcats)
library(reshape2)

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

#Next, extract proportions of current position
data$job_position_recoded <- data$job_position_basedOnDeg
data$job_position_recoded <- as.character(data$job_position_recoded)
data$job_position_recoded[data$job_position_recoded=='Pre-doctoral student'] <- 'Pre-PhD'
data$job_position_recoded[data$job_position_recoded=='PhD student'] <- 'PhD student'
data$job_position_recoded[data$job_position_recoded=='Postdoc'] <- 'Postdoc'
data$job_position_recoded[data$job_position_recoded=='Group leader and faculty'] <- 'Independent group leader or above'
data$job_position_recoded[data$job_position_recoded=='Researchers with PhD'] <- 'Unclassified'
data$job_position_recoded[data$job_position_recoded=='Researcher without PhD'] <- 'Unclassified'
data$job_position_recoded[data$job_position_recoded=='Research assistants'] <- 'Unclassified'
data$job_position_recoded[data$job_position_recoded=='Outside academia'] <- 'Outside academia'
data$job_position_recoded <- as.factor(data$job_position_recoded)

jobsXteam <- table(data$job_position_recoded, by=data$teamSize)

#Then, extract # of eeg papers
data$eeg_papers_recoded <- data$eeg_papers
data$eeg_papers_recoded[data$eeg_papers_recoded==0] <- 0
data$eeg_papers_recoded[data$eeg_papers_recoded==1] <- 1
data$eeg_papers_recoded[data$eeg_papers_recoded==2] <- 2
data$eeg_papers_recoded[data$eeg_papers_recoded >=3 & data$eeg_papers_recoded <=5] <- 3
data$eeg_papers_recoded[data$eeg_papers_recoded >=6 & data$eeg_papers_recoded <=10] <- 6
data$eeg_papers_recoded[data$eeg_papers_recoded >=11 & data$eeg_papers_recoded <=30] <- 11
data$eeg_papers_recoded[data$eeg_papers_recoded >=31 & data$eeg_papers_recoded <=60] <- 31
data$eeg_papers_recoded[data$eeg_papers_recoded >60] <- 60
data$eeg_papers_recoded <- as.factor(data$eeg_papers_recoded)

papersXteam <- table(data$eeg_papers_recoded, by=data$teamSize)

#Prepare data for plotting
df_teamsize<-data.frame()
df_teamsize[1:3,1]<-c(1,2,3)
df_teamsize[1:3,2]<-c(31, 100, 261)
colnames(df_teamsize)<-c("teamSize","vector_teamSizes")

dat2plot_jobs <- data %>% 
  select(job_position_recoded, teamSize) %>%
  group_by(teamSize, job_position_recoded) %>%
  summarise(counts=n())

dat2plot_jobs <- merge(dat2plot_jobs, df_teamsize, by="teamSize")
dat2plot_jobs$proportions<-dat2plot_jobs$counts/dat2plot_jobs$vector_teamSizes

dat2plot_papers <- data %>% 
  select(eeg_papers_recoded, teamSize) %>%
  group_by(teamSize, eeg_papers_recoded) %>%
  summarise(counts=n())

dat2plot_papers <- merge(dat2plot_papers, df_teamsize, by="teamSize")
dat2plot_papers$proportions<-dat2plot_papers$counts/dat2plot_papers$vector_teamSizes

################################################################################
#Merge all of the data into a single plot
dat2plot <- bind_rows(dat2plot_papers, dat2plot_jobs)
dat2plot_melt <- melt(dat2plot[,c('teamSize', 'counts', 'proportions', 'eeg_papers_recoded','job_position_recoded')],
                      id = c('teamSize', 'counts', 'proportions'))

#Re-order factor levels
#dat2plot_melt$value <-factor(dat2plot_melt$value, levels=c('0', '1', '2', '3', '6', '11', '31', '60', 
                                                           #'Pre-PhD', 'PhD student', 'Postdoc', 'Independent group leader or above', 
                                                           #'Outside academia', 'Unclassified'))

#Re-order factor levels
dat2plot_melt$value <-factor(dat2plot_melt$value, levels=c('60', '31', '11', '6', '3', '2', '1', '0', 
                                                           'Unclassified', 'Outside academia', 'Independent group leader or above', 
                                                           'Postdoc', 'PhD student', 'Pre-PhD'))


################################################################################
#Plot data - vertical
#my_colors = c('#C3CDDC', '#AFBDD0', '#9CACC5', '#889BB9', '#748BAD', '#607AA1', '#4C6A96', '#38598A',
              #'#F9CAC3', '#F8BFB6', '#F7B4AA', '#F5A99E', '#F49F92', '#F39486')

#my_colors = c('#38598A', '#4C6A96', '#607AA1', '#748BAD', '#889BB9', '#9CACC5', '#AFBDD0','#C3CDDC', 
              #'#F39486', '#F49F92', '#F5A99E', '#F7B4AA', '#F8BFB6', '#F9CAC3')

my_colors = c('#38598A', '#4C6A96', '#607AA1', '#748BAD', '#889BB9', '#9CACC5', '#AFBDD0','#C3CDDC', 
              '#8C8281', '#F39486', '#F49F92', '#F5A99E', '#F7B4AA', '#F8BFB6')

g <- ggplot(remove_missing(dat2plot_melt, na.rm=FALSE), aes(x=variable, y=counts, fill=value)) +
  geom_bar(stat='identity', position='stack') +
  facet_wrap(~teamSize, strip.position='bottom') +
  #facet_grid(~teamSize, ) +
  
  #Adjust colors
  scale_fill_manual(values=my_colors, labels=c('>60', '31-60', '11-30', '6-10', '3-5', '2', '1', '0', 
                                               'Unknown', 'Outside academia', '>= Independent group leader', 
                                               'Postdoc', 'PhD student', '< PhD student')) +
  
  #Adjust x-axis
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.spacing = unit(0.1, "lines"))+
  geom_text(aes(x=1, y=-10, label='# EEG\npapers'), size=3.5, color='dimgray') +
  geom_text(aes(x=2, y=-10, label='Current\nposition'), size=3.5, color='dimgray') +
  
  #Adjust facets
  theme(strip.background=element_rect(fill='darkgray', color='darkgray')) +
  
  #Change axis labels and extend
  labs(x='Team size', y='Number of analysts') + 
  ylim(-10, 275) +
  
  #Change axis looks
  theme(axis.title=element_text(size=16, color='dimgray', face='bold'),
        axis.text=element_text(size=16, color='dimgray'), 
        axis.line = element_line(color='dimgray', size=1, linetype='solid'),
        axis.ticks = element_line(color='dimgray', size=1, linetype='solid')) +
  
  #Adjust legend
  theme(legend.title=element_text(size=12, color='dimgray', face='bold'), 
        #legend.position=c(.825, .9),
        legend.text=element_text(size=12, color='dimgray')) +
  guides(fill=guide_legend(ncol=2))
  




################################################################################
#Plot data - horizontal
my_colors = c('#BABABA', '#B2182B')

g <- ggplot(remove_missing(dat2plot_melt, na.rm=FALSE), aes(x=variable, y=counts, fill=value)) +
  geom_bar(stat='identity', position='stack') +
  facet_grid(rows=vars(teamSize), as.table=TRUE) +
  
  #Switch orientation
  coord_flip()





g <- ggplot(data, aes(fill=fct_rev(fill), x=fct_rev(continents), y=count, widths=.8))  +
  geom_bar(stat='identity', position=position_dodge(.8)) +
  
  #Switch orientation
  coord_flip() +
  
  #Add percentage labels to the individual bars
  geom_text(aes(x=fct_rev(continents), y=count, label=paste0(sprintf("%4.0f", round(count, digits=0)), '%'),
                group=fct_rev(fill)), vjust=0.5, hjust=0.3,
            position=position_dodge(.8),
            family='sans', size=3.5, fontface='bold', color='dimgray') +
  
  #Adjust colors
  scale_fill_manual(values=my_colors, labels=c('Pubmed authors', 'Analysts')) +
  
  #Adjust legend
  theme(legend.title=element_text(size=12, color='dimgray', face='bold'), 
        legend.position=c(.825, .9),
        legend.text=element_text(size=12, color='dimgray')) +
  guides(fill=guide_legend(reverse=TRUE)) +
  
  #Change axis labels
  labs(x=' ', y='Percent', fill='Sample') +  
  
  #Change the aspect ratio of the plot 
  theme(aspect.ratio=.3) + 
  
  #Remove unnecessary white space between axis and bars
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_continuous(limits=c(0, 73), expand=c(0,0)) +
  
  #Change axis looks
  theme(axis.title=element_text(size=16, color='dimgray', face='bold'),
        axis.text=element_text(size=16, color='dimgray'), 
        axis.line = element_line(color='dimgray', size=1, linetype='solid'),
        axis.ticks = element_line(color='dimgray', size=1, linetype='solid')) 
#axis.line.x = element_line(color='white', size=1, linetype='solid'),
#axis.ticks.x = element_line(color='white', size=1, linetype='solid')) +
#theme(plot.margin=unit(c(0.1,0,0,0), "null"),
#axis.title.y=element_text(angle=0, vjust=1.075, margin=margin(t=0, r=-55, b=0, l=0))) 


ggsave("Representative sample_horizontal.png", width = 6, height = 4, dpi=600)




################################################################################
#Plot data - vertical

#my_colors = brewer.pal(n=8, name='RdBu')#[c(1,8)]
my_colors = c('#B2182B', '#BABABA')
my_alphas = c(1, 1, 1, 1)
theme_set(theme_classic())

g <- ggplot(data, aes(fill=fill, x=continents, y=count, widths=.8))  +
  geom_bar(stat='identity', position=position_dodge(.8)) +
  
  #Add percentage labels to the individual bars
  geom_text(aes(x=continents, y=count, label=paste0(sprintf("%4.0f", round(count, digits=0)), '%'),
                group=fill), vjust=-0.5,
            position=position_dodge(.8),
            family='sans', size=3.5, fontface='bold', color='dimgray') +
  
  #Adjust colors
  scale_fill_manual(values=my_colors, labels=c('Analysts', 'Pubmed authors')) +
  
  #Adjust legend
  theme(legend.title=element_text(size=12, color='dimgray', face='bold'), 
        legend.position=c(.2,.8),
        legend.text=element_text(size=12, color='dimgray')) +
  
  #Change axis labels
  labs(x=' ', y='Percent', fill='Sample') +  

  #Change the aspect ratio of the plot 
  theme(aspect.ratio=.7) + 
  
  #Remove unnecessary white space between axis and bars
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_continuous(limits=c(0, 73), expand=c(0,0)) +
  
  #Change axis looks
  theme(axis.title=element_text(size=16, color='dimgray', face='bold'),
        axis.text=element_text(size=16, color='dimgray'), 
        axis.line = element_line(color='dimgray', size=1, linetype='solid'),
        axis.ticks = element_line(color='dimgray', size=1, linetype='solid')) +
        #axis.line.x = element_line(color='white', size=1, linetype='solid'),
        #axis.ticks.x = element_line(color='white', size=1, linetype='solid')) +
  theme(plot.margin=unit(c(0.1,0,0,0), "null"),
        axis.title.y=element_text(angle=0, vjust=1.075, margin=margin(t=0, r=-55, b=0, l=0))) 

  
ggsave("Representative sample_vertical.png", width = 6, height = 4, dpi=600)

















