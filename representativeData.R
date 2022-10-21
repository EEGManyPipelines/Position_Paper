#Purpose: Plot the EMP sample data in comparison to the pubmed data
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
data <- read.csv("percent_countries_pubmed_emp_figure.csv")

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



################################################################################
#Plot data - horizontal
my_colors = c('#BABABA', '#B2182B')

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













