#Purpose: Plot the gender distribution of the EEGManyPipes sample
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
df_teamsize[1:3,2]<-c(30, 94, 273)
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


################################################################################
#Plot data - bars: Gender
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

ggsave("Gender.png", width = 6, height = 4, dpi=600)

################################################################################
#Plot data - percentages: Gender

#Job position
g <- ggplot(dat2plot, aes(x=teamSize, y=gender_recoded)) +
  geom_raster(aes(fill=proportions)) +
  
  #Change order of labels in axis
  scale_y_discrete(limits = c('Unknown', 'diverse', 'female', 'male'), 
                   labels = c('Unknown', 'Diverse', 'Women', 'Men')) +
  
  #Change color
  scale_fill_gradient(limits=c(0, 0.7), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), labels=c('0', '10', '20', '30', '40', '50', '60', '70'),
                      low='#C3CDDC', high='#38598A') +
  
  #Add percentage labels to tiles
  geom_text(aes(x=teamSize, y=gender_recoded, label=paste0(sprintf("%4.0f", round(proportions*100, digits=0))),
                group=value), vjust=0.5, hjust=0.65,
            family='sans', size=8, fontface='bold', color='white') +
  
  #Change theme
  theme_classic() +

  labs(x='Team size', y=' ') + 
  
  #Change legend
  guides(fill=guide_colorbar(title='Relative percentage', title.position='top',
                             title.vjust=0.1, title.hjust=0.5, direction='horizontal', nbin=1000)) +
  
  #Add title
  ggtitle('Analysts gender') +
  
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

ggsave("EMP_Sample_Gender.png", width=6, height=8, dpi=600)
ggsave("EMP_Sample_Gender.svg", width=6, height=8, dpi=600)















