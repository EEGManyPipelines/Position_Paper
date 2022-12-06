#Purpose: Plot the EMP sample data 
#Project: EEGManyPipes
#Paper: Position Paper
#Author: D. Truebutschek, Y. Yang, & M. Vinding
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
print(summary(data$eeg_papers_recoded))
ggplot(data, aes(x=eeg_papers_recoded)) + geom_histogram(binwidth=1, color='black', fill='white') #get a first feel of the data

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

#Last, extract average subjective EEG experience (from prior beliefs questionnaire)
data$average_expertise_recoded <- data$average_expertise
print(summary(data$average_expertise_recoded))
ggplot(data, aes(x=average_expertise_recoded)) + geom_histogram(binwidth=5, color='black', fill='white') #get a first feel of the data

data$average_expertise_recoded[data$average_expertise_recoded >=0 & data$average_expertise_recoded <=12.5] <- 101
data$average_expertise_recoded[data$average_expertise_recoded >=12.5 & data$average_expertise_recoded <=25] <- 110
data$average_expertise_recoded[data$average_expertise_recoded >=25 & data$average_expertise_recoded <=37.5] <- 120
data$average_expertise_recoded[data$average_expertise_recoded >=37.5 & data$average_expertise_recoded <=50] <- 130
data$average_expertise_recoded[data$average_expertise_recoded >=50 & data$average_expertise_recoded <=62.5] <- 140
data$average_expertise_recoded[data$average_expertise_recoded >=62.5 & data$average_expertise_recoded <=75] <- 150
data$average_expertise_recoded[data$average_expertise_recoded >=75 & data$average_expertise_recoded <=87.5] <- 160
data$average_expertise_recoded[data$average_expertise_recoded >=87.5 & data$average_expertise_recoded <=100] <- 170
data$average_expertise_recoded[is.na(data$average_expertise_recoded)] <- 180
data$average_expertise_recoded <- as.factor(data$average_expertise_recoded)

expertiseXteam <- table(data$average_expertise_recoded, by=data$teamSize)

#Prepare data for plotting
df_teamsize<-data.frame()
df_teamsize[1:3,1]<-c(1,2,3)
df_teamsize[1:3,2]<-c(30, 94, 273)
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

dat2plot_expertise <- data %>% 
  select(average_expertise_recoded, teamSize) %>%
  group_by(teamSize, average_expertise_recoded) %>%
  summarise(counts=n())

dat2plot_expertise <- merge(dat2plot_expertise, df_teamsize, by="teamSize")
dat2plot_expertise$proportions<-dat2plot_expertise$counts/dat2plot_expertise$vector_teamSizes

################################################################################
#Merge all of the data into a single plot
dat2plot <- bind_rows(dat2plot_expertise, dat2plot_papers, dat2plot_jobs)
dat2plot_melt <- melt(dat2plot[,c('teamSize', 'counts', 'proportions', 'average_expertise_recoded', 'eeg_papers_recoded','job_position_recoded')],
                      id = c('teamSize', 'counts', 'proportions'))

#Re-order factor levels
#dat2plot_melt$value <-factor(dat2plot_melt$value, levels=c('0', '1', '2', '3', '6', '11', '31', '60', 
                                                           #'Pre-PhD', 'PhD student', 'Postdoc', 'Independent group leader or above', 
                                                           #'Outside academia', 'Unclassified'))

#Re-order factor levels
dat2plot_melt$value <-factor(dat2plot_melt$value, levels=c('180', '170', '160', '150', '140', '130', '120', '110', '101',
                                                           '60', '31', '11', '6', '3', '2', '1', '0', 
                                                           'Unclassified', 'Outside academia', 'Independent group leader or above', 
                                                           'Postdoc', 'PhD student', 'Pre-PhD'))


################################################################################
#Plot data - vertical
#my_colors = c('#C3CDDC', '#AFBDD0', '#9CACC5', '#889BB9', '#748BAD', '#607AA1', '#4C6A96', '#38598A',
              #'#F9CAC3', '#F8BFB6', '#F7B4AA', '#F5A99E', '#F49F92', '#F39486')

#my_colors = c('#38598A', '#4C6A96', '#607AA1', '#748BAD', '#889BB9', '#9CACC5', '#AFBDD0','#C3CDDC', 
              #'#F39486', '#F49F92', '#F5A99E', '#F7B4AA', '#F8BFB6', '#F9CAC3')

#my_colors = c('#38598A', '#4C6A96', '#607AA1', '#748BAD', '#889BB9', '#9CACC5', '#AFBDD0','#C3CDDC',
              #'#8C8281', '#F39486', '#F49F92', '#F5A99E', '#F7B4AA', '#F8BFB6')

my_colors = c('#8C8281', '#F39486', '#F49F92', '#F5A99E', '#F7B4AA', '#F8bFB6', '#F9CAC3', '#FAD4CF', '#FDEAE7',
              '#D0708F', '#D57E9A', '#D98DA5', '#DE9BB1', '#E3A9BC', '#E8B8C7', '#ECC6D2', '#F1D4DD',
              '#8C8281', '#665585', '#756691', '#85779D', '#9488AA', '#A399B6')


g <- ggplot(remove_missing(dat2plot_melt, na.rm=FALSE), aes(x=variable, y=counts, fill=value)) +
  geom_bar(stat='identity', position='stack') +
  facet_wrap(~teamSize, strip.position='bottom') +
  #facet_grid(~teamSize, ) +
  
  #Add percentage labels to the individual bars
  #geom_text(aes(x=variable, y=counts, label=paste0(sprintf("%4.0f", round(proportions*100, digits=0)), '%'),
                #group=value), vjust=0.5, hjust=0,
            #position='stack',
            #family='sans', size=3.5, fontface='bold', color='dimgray') +
  
  #Adjust colors
  scale_fill_manual(values=my_colors, labels=c('Unknown/missing', '87.5-100%', '72.5-87.5%', '62.5-72.5%', '50-62.5%',
                                               '37.5-50%', '25-37.5%', '12.5-25%', '0-12.5%',
                                               '>60', '31-60', '11-30', '6-10', '3-5', '2', '1', '0', 
                                               'Unknown/missing', 'Outside academia', '>= Independent group leader', 
                                               'Postdoc', 'PhD student', '< PhD student')) +
  
  #Adjust x-axis
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), panel.spacing = unit(0.1, "lines")) +
  geom_text(aes(x=1, y=-10, label='Subj.\nexpertise'), size=3, color='dimgray') +
  geom_text(aes(x=2, y=-10, label='# EEG\npapers'), size=3, color='dimgray') +
  geom_text(aes(x=3, y=-10, label='Current\nposition'), size=3, color='dimgray') +
  
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
  
  #Adjust legend (not plotted together as size is unreadable otherwise).
  #theme(legend.title=element_text(size=12, color='dimgray', face='bold'), 
        ##legend.position=c(.825, .9),
        #legend.text=element_text(size=12, color='dimgray')) +
  #guides(fill=guide_legend(ncol=2))
  
  theme(legend.position='none')
  
ggsave("EMP_Sample_Expertise.png", width=6, height=4, dpi=600)

################################################################################
#Plot data - percentages: Subjective expertise

#Recode data to have missing data come in first
levels(dat2plot_expertise$average_expertise_recoded) <- c('101', '110', '120', '130', '140', '150', '160', '170', '90')
dat2plot_expertise$average_expertise_recoded <-factor(dat2plot_expertise$average_expertise_recoded, 
                                                      levels=c('90', '101', '110', '120', '130', '140', '150', '160', '170'))

#Average subjective expertise
g <- ggplot(dat2plot_expertise, aes(x=teamSize, y=average_expertise_recoded)) +
  geom_raster(aes(fill=proportions)) +
  
  #Change color
  scale_fill_gradient(limits=c(0, 0.3), breaks=c(0, 0.1, 0.2, 0.3), labels=c('0', '10', '20', '30'),
                      low='#FDEAE7', high='#F39486') +
  
  #Add percentage labels to tiles
  geom_text(aes(x=teamSize, y=average_expertise_recoded, label=paste0(sprintf("%4.0f", round(proportions*100, digits=0))),
  group=value), vjust=0.5, hjust=0.65,
  family='sans', size=8, fontface='bold', color='white') +
  
  #Change theme
  theme_classic() +

  #Change y-axis tick labels
  scale_y_discrete(labels=c('Missing', '12.5', '25.0', '37.5', '50.0', '62.5', '75.0', '87.5', '100.0')) +
  
  labs(x='Team size', y=' ') + 
  
  #Change legend
  guides(fill=guide_colorbar(title='Relative percentage', title.position='top',
         title.vjust=0.1, title.hjust=0.5, direction='horizontal', nbin=1000)) +
  
  #Add title
  ggtitle('Subjective EEG expertise') +

  #Change axis looks
  theme(axis.title=element_text(size=20, color='dimgray', face='bold'),
      axis.text=element_text(size=20, color='dimgray'), 
      axis.line = element_line(color='dimgray', size=1, linetype='solid'),
      axis.ticks = element_line(color='dimgray', size=1, linetype='solid'),
      plot.title = element_text(size=22, color='dimgray', face='bold', hjust=0.5),
      legend.title=element_text(size=18, color='dimgray', face='bold'),
      legend.text=element_text(size=18, color='dimgray'), 
      legend.position=c(.54, -.225),
      axis.text.y=element_blank(),
      aspect.ratio=1) 

ggsave("EMP_Sample_SubjExpertise.png", width=6, height=8, dpi=600)
ggsave("EMP_Sample_SubjExpertise.svg", width=6, height=8, dpi=600)

################################################################################
#Plot data - percentages: Number EEG papers

#EEG paper
g <- ggplot(dat2plot_papers, aes(x=teamSize, y=eeg_papers_recoded)) +
  geom_raster(aes(fill=proportions)) +
  
  #Change color
  scale_fill_gradient(limits=c(0, 0.4), breaks=c(0, 0.1, 0.2, 0.3, 0.4), labels=c('0', '10', '20', '30', '40'),
                      low='#F1D4DD', high='#D0708F') +
  
  #Add percentage labels to tiles
  geom_text(aes(x=teamSize, y=eeg_papers_recoded, label=paste0(sprintf("%4.0f", round(proportions*100, digits=0))),
                group=value), vjust=0.5, hjust=0.65,
            family='sans', size=8, fontface='bold', color='white') +
  
  #Change theme
  theme_classic() +
  
  #Change y-axis tick labels
  scale_y_discrete(labels=c('0', '1', '2', '3-5', '6-10', '11-30', '31-60', '>60')) +
  
  labs(x='Team size', y=' ') + 
  
  #Change legend
  guides(fill=guide_colorbar(title='Relative percentage', title.position='top',
                             title.vjust=0.1, title.hjust=0.5, direction='horizontal', nbin=1000)) +
  
  #Add title
  ggtitle('Number of EEG papers') +
  
  #Change axis looks
  #Change axis looks
  theme(axis.title=element_text(size=20, color='dimgray', face='bold'),
        axis.text=element_text(size=20, color='dimgray'), 
        axis.line = element_line(color='dimgray', size=1, linetype='solid'),
        axis.ticks = element_line(color='dimgray', size=1, linetype='solid'),
        plot.title = element_text(size=22, color='dimgray', face='bold', hjust=0.5),
        legend.title=element_text(size=18, color='dimgray', face='bold'),
        legend.text=element_text(size=18, color='dimgray'), 
        legend.position=c(.54, -.225),
        axis.text.y=element_blank(),
        aspect.ratio=1) 

ggsave("EMP_Sample_EEGPapers.png", width=6, height=8, dpi=600)
ggsave("EMP_Sample_EEGPapers.svg", width=6, height=8, dpi=600)

################################################################################
#Plot data - percentages: Job position

#Job position
g <- ggplot(dat2plot_jobs, aes(x=teamSize, y=job_position_recoded)) +
  geom_raster(aes(fill=proportions)) +
  
  #Change order of labels in axis
  scale_y_discrete(limits = c('Unclassified', 'Outside academia', 'Pre-PhD', 'PhD student', 
                              'Postdoc', 'Independent group leader or above'), 
                   labels = c('Unknown', 'Outside academia', '<PhD', 'PhD student', 
                              'Postdoc', '>=Group leader')) +
  
  #Change color
  scale_fill_gradient(limits=c(0, 0.5), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c('0', '10', '20', '30', '40', '50'),
                      low='#D1CCDA', high='#665585') +
  
  #Add percentage labels to tiles
  geom_text(aes(x=teamSize, y=job_position_recoded, label=paste0(sprintf("%4.0f", round(proportions*100, digits=0))),
                group=value), vjust=0.5, hjust=0.65,
            family='sans', size=8, fontface='bold', color='white') +
  
  #Change theme
  theme_classic() +
  
  #Change y-axis tick labels
  #scale_y_discrete(labels=c('Unknown', 'Outside academia', '<PhD', 'PhD student', 'Postdoc', '>Group leader')) +
  
  labs(x='Team size', y=' ') + 
  
  #Change legend
  guides(fill=guide_colorbar(title='Relative percentage', title.position='top',
                             title.vjust=0.1, title.hjust=0.5, direction='horizontal', nbin=1000)) +
  
  #Add title
  ggtitle('Academic seniority') +
  
  #Change axis looks
  #Change axis looks
  theme(axis.title=element_text(size=20, color='dimgray', face='bold'),
        axis.text=element_text(size=20, color='dimgray'), 
        axis.line = element_line(color='dimgray', size=1, linetype='solid'),
        axis.ticks = element_line(color='dimgray', size=1, linetype='solid'),
        plot.title = element_text(size=22, color='dimgray', face='bold', hjust=0.5),
        legend.title=element_text(size=18, color='dimgray', face='bold'),
        legend.text=element_text(size=18, color='dimgray'), 
        legend.position=c(.54, -.225),
        axis.text.y=element_blank(),
        aspect.ratio=1) 

ggsave("EMP_Sample_Academic seniority.png", width=6, height=8, dpi=600)
ggsave("EMP_Sample_Academic seniority.svg", width=6, height=8, dpi=600)









