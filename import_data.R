### IMPORT PARTICIPANT METADATA
# Read the raw csv file. Re-code variables. Save in R format.
#
# Ref: Postion_paper
# Created by Mikkel C. Vinding & Yu-Fang Yang 

# Paths and file for Mikkel
if ( Sys.getenv("USER") == 'mcvinding' ){
  proj.path <- '/Users/mcvinding/Documents/EEGManyPipelines/metadata_summary'
} else if (Sys.getenv("USERNAME") == 'Mikkel'){
  proj.path <- 'C:/Users/Mikkel/Documents/EEGManyPipelines/metadata_summary'
} else {
  # Paths and file for Yu-Fang
  rm(list=ls()) 
  path= dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(path)
  getwd()
  data <- read.csv("demographic_cleaned.csv")
}
setwd(proj.path)

data.path <- 'sourcedata'
out.path  <- 'data'
data.file <- 'demographic_cleaned_07092022.csv'

# Read csv data
data <- read.csv(file.path(proj.path, data.path, data.file))

###############################################################################
# Order and re-code data
data$team <- as.factor(data$team)

# Country
data$country <- ifelse(data$country=="United Arab Emirates", "UAE", data$country)
data$country <- ifelse(data$country=="portugal", "Portugal", data$country)
data$country <- ifelse(data$country=="The Netherlands", "Netherlands", data$country)
data$country <- ifelse(data$country=="Korea", "Republic of Korea", data$country)
data$country <- ifelse(data$country=="Catalonia", "Spain", data$country)

# Re-code job position
# data$job_position # <- This one is tricky!

# Re-code academic field
str_apneuro <- c("Applied Neuroscience", "clinical neuroscience", "Clinical Psychological Science ")
str_psychia <- c("psychiatric neurophysiology", "Psychiatry")
str_enginee <- c("Audio Engineering", "Engineering ", "DSP", "Neuroengineering", "signal processing")
str_compsci <- c("Computer science", "computer science", "artificial intelligence")
str_physics  <- c("physics")
str_biomede <- c("biomedical engineer, biomedical signal processing engineer", "BIOMEDICAL ENGINEERING",
  "Biomedical Engineering", "Biomedical engineering","biomedical engineering, neuroscience",
  "Brain Computer Interfaces","neuroscience & machine learning", "Neuroimaging", "neuroimaging",
  "Machine Learning for Neuroimaging Data Analysis")
str_neursci <- c("Auditory Neuroscience and Hearing","behavioural genetics, neuroscience","neurosciences",
  "human neuroscience", "NEUROSCIENCE", "Neuroscience", "neuroscience", "Neuroscience ","neuroscience EEG/fMRI",
  "Neuroscience for learning disabilities", "neuroscience of (audiovisual) speech perception",
  "neuroscience, biology", "neuroscience, cognitive psychology, digital signal processing, genetics",
  "Neuroscience, Psychology", "Neuroscience, psychology", "neuroscience, psychology", 
  "Neuroscience, psychology, neuropsychology", "Neuroscience, psychophysiology", 
  "Neuroscience, Psychophysiology, Statistical Modelling","Neurosciences", "Neurosciences")
str_neurolo <- c("neurology, neuroscience", "Neurophysiology", "neurophysiology","Electrophysiology")
# str_neruoim <- c()
str_comneur <- c("Computational Neuroscience","Computational neuroscience","computational neuroscience",
  "computational modelling")
str_psychol <- c("Biological Psychology","biological Psychology","Cognitive psychology","cognitive psychology",
  "episodic memory","Experimental Psychology","Experimental psychology","experimental psychology", "Experimental psychology ",
  "experimental psychology, neurology","Neuropsychology","personality and individual differences",
  "Personality Psychology, Psychophysiolgy and Neuroscience ","Psychology","psychology","Psychology ",
  "psychology ","Psychology and Neuroscience","Psychology and neuroscience","psychology and neuroscience",
  "Psychology, Cognitive Neuroscience","Psychology, cognitive neuroscience","psychology, developmental psychology, vision, cognition",
  "Psychology, Individual Differences, Psychological Assessment, Personality","Psychology, neuroscience",
  "psychology, neuroscience", "Psychology/educational neuroscience","Psychophysiology", "biological psychology")
str_lingust <- c("cognitive neuroscience of language","Psycho- and neurolinguistics","Psycho/Neurolinguistics",
  "cognitive neuroscience, linguistics","Cognitive Neuroscience/Psycholinguistics","Psycholinguistics",
  "psycholinguistics","psychology of language")
str_develop <- c("Developmental Psychology","Developmental Cognitive Neuroscience",
  "Developmental cognitive neuroscience", "developmental cognitive neuroscience","developmental cognitive science",
  "Human Development","Human development and family studies")
str_biomedi <- c("Biomedical")
# str_elctphy <- c() 
str_cognsci <- c("Cognitive Neuroscience","Cognitive neuroscience","cognitive neuroscience",
  "Cognitive Neuroscience ","Cognitive neuroscience ","cognitive neuroscience ","Cognitive neuroscience / cognitive psychology",
  "Cognitive Neuroscience / Psychology","cognitive neuroscience / psychology","cognitive neuroscience and neuropsychology",
  "cognitive neuroscience and psychology","cognitive neuroscience, education, mathematical cognition",
  "Cognitive Neuroscience, Psychology","cognitive neurosciences ","Cognitive Science","cognitive science",
  "Computational Cognitive Science","social neuroscience","olfaction", "cognitive neuroscience, psychphysics", "decision making")
str_marking <- c("marketing")

data$eeg_field_edit <- data$eeg_field
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_apneuro, "Applied/clinical science",              data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_psychia, "Psychiatry",                            data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_enginee, "Engineering",                           data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_compsci, "Computer Science",                      data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_physics, "Physics",                               data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_biomede, "Biomedical Engineering",                data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_neursci, "Neuroscience",                          data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_neurolo, "Neurology/Neurophysiology",             data$eeg_field_edit)
# data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_neruoim, "Neuroimaging",                          data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_comneur, "Computational Neuroscience",            data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_psychol, "Psychology",                            data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_lingust, "Linguistics and language",              data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_develop, "Developmental Psychology/Neuroscience", data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_biomedi, "Biomedicine",                           data$eeg_field_edit)
# data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_elctphy, "Electrophysiology",                     data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_cognsci, "Cognitive Science",                     data$eeg_field_edit)
data$eeg_field_edit <- ifelse(data$eeg_field_edit %in% str_marking, "Marketing",                             data$eeg_field_edit)

                 
# Save data
save(data, file=file.path(out.path, 'data.RData'))

# Save data: Yu-Fang
write.csv(data,'final_data.csv')
