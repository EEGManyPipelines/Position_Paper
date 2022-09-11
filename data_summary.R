# DATA SUMMARIES
# Summaries of metadata variables and figures
#
# Ref: Position_paper
#

library(ggplot2)
library(ggrepel)
library(tidyverse)
library(rworldmap)
library(RColorBrewer)

# Paths
if ( Sys.getenv("USER") == 'mcvinding' ){
  data.path <- '/Users/mcvinding/Documents/EEGManyPipelines/metadata_summary/data'
}
setwd(data.path)

# Load data
load('data.RData')


# Paths and file for Yu-Fang
rm(list=ls()) 
path= dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
getwd()
data <- read.csv("final_data.csv")



################################################################################
# Teams
length(unique(data$team))
tabTeam <- data.frame(tabulate(data$team))
table(tabulate(data$team))
mean(tabulate(data$team))

ggplot(tabTeam, aes(x=tabulate.data.team.))+
  geom_bar(fill="grey", colour="black")+
  ggtitle('Team size')+
  labs(x="Team size", y='')+
  theme_bw()
ggsave("teamSize.jpg", width = 3, height = 3, dpi=600)

################################################################################
# Age
range(data$age)
mean(data$age)
median(data$age)
sd(data$age)

################################################################################
# Gender
table(data$gender)

################################################################################
# Number EEG papers
range(data$eeg_papers)
median(data$eeg_papers)
mean(data$eeg_papers)
sd(data$eeg_papers)

ggplot(data, aes(x=eeg_papers))+
  geom_histogram(binwidth=2, fill="grey", colour="black")+
  ggtitle('Peer-reviewd EEG papers')+
  scale_x_continuous(breaks=seq(0,150,25)) +
  labs(x="N papers", y='')+
  theme_bw()
ggsave("EEGpapersAll.jpg", width = 6, height = 3, dpi=600)

paperByTeam <- aggregate(data$eeg_papers, by=list(data$team), mean)
range(paperByteam$x)
median(paperByteam$x)
mean(paperByTeam$x)
sd(paperByTeam$x)

ggplot(paperByTeam, aes(x=x))+
  geom_histogram(binwidth=2, fill="grey", colour="black")+
  ggtitle('Averge peer-reviewed EEG papers per team')+
  labs(x="N papers", y='')+
  scale_x_continuous(breaks=seq(0,60,10)) +
  theme_bw()
ggsave("EEGpapersTeam.jpg", width = 6, height = 3, dpi=600)
  
################################################################################
# Years doing EEG
range(data$eeg_years)
median(data$eeg_years)
mean(data$eeg_years)
sd(data$eeg_years)

teamExperience <- aggregate(data$eeg_years, by=list(data$team), sum)
range(teamExperience$x)
median(teamExperience$x)
mean(teamExperience$x)
sd(teamExperience$x)

ggplot(teamExperience, aes(x=x))+
  geom_histogram(binwidth=2, fill="grey", colour="black")+
  ggtitle('Total number of years working with EEG per team')+
  labs(x="Sum/Years", y='')+
  scale_x_continuous(breaks=seq(0,60,10)) +
  theme_bw()
ggsave("EEGyearsTeam.jpg", width = 6, height = 3, dpi=600)

################################################################################
# Country
length(unique(data$country))
table(data$country)

# The map figure...

################################################################################
# Academic field
fieldData <- data.frame(table(data$eeg_field_edit))
fieldData$pct  <- round(fieldData$Freq / sum(fieldData$Freq) * 100, 1)
fieldData <- fieldData[order(-fieldData$Freq),]
fieldData$Var1 <- reorder(fieldData$Var1, -fieldData$Freq)

df2 <- fieldData %>% 
  mutate(csum = rev(cumsum(rev(Freq))), 
         pos = Freq/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Freq/2, pos))

ggplot(df2, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="black") +
  # geom_text(aes(label = paste0(pct, "%")),
  #           position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0) +
  labs(x = NULL, y = NULL) +
  geom_label_repel(aes(y = pos, label = paste0(Var1, " (",pct,"%)")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  theme_void() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none",
        panel.background = element_rect(fill = "white"))

ggsave("fieldPie.jpg", width = 6, height = 6, dpi=600)



################################################################################
# Demographic map 

data$country <- as.factor(data$country)
data$gender <- as.factor(data$gender)

df_frq<-data%>% group_by(country) %>%   
  mutate(value=n()) %>% ungroup() %>% arrange(value) 

df_frq$value <- as.numeric(df_frq$value )
head(df_frq)

# df_country is used only for mapping the demographic figure 
df_country <-df_frq %>% select(country,value) #eeg_years,eeg_papers,gender,
head(df_country)
df_country <-unique(df_country) 

# create a map 
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# Joining the data to a map 
df_country_map <- joinCountryData2Map(df_country, joinCode="ISO3", nameJoinColumn="Partner.ISO")
# Remove Antarctica from the world map 
df_country_new <- subset(df_country_map, continent != "Antarctica")

# add color palette
YYPalette <- RColorBrewer::brewer.pal(11,"PuOr")

# def. map parameters
mapParams <- mapCountryData(df_country_new, 
                            nameColumnToPlot="value",  
                            oceanCol = "azure2",
                            catMethod = "categorical",
                            missingCountryCol = gray(.8), #"white"
                            addLegend = F, 
                            # xlim=c(-10,19), ylim=c(40,56),
                           # borderCol ="black"
                           # mapTitle="Demography infographic of analysts", 
                            colourPalette= YYPalette)

# add legend and display map
do.call(addMapLegendBoxes, c(mapParams,
                             x = 'bottom',
                             title = "No. of teams",
                             horiz = TRUE,
                             bg = "transparent",
                             bty = "n"))




