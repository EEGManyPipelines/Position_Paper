### IMPORT PARTICIPANT METADATA
# Read the raw csv file. Re-code variables. Save in R format.
#
# Ref: Postion_paper
#
# Created by Mikkel C. Vinding & Yu-Fang Yang 

# Paths and file for Mikkel
if ( Sys.getenv("USER") == 'mcvinding' ){
  proj.path <- '/Users/mcvinding/Documents/EEGManyPipelines/metadata_summary'
} else if (Sys.getenv("USERNAME") == 'Mikkel'){
  proj.path <- 'C:/Users/Mikkel/Documents/EEGManyPipelines/metadata_summary'
} else if (Sys.getenv("USER") == 'darinka'){
  proj.path <- '/home/darinka/Documents/EEGManyPipes/metadata_summary'
} else {
  # Paths and file for Yu-Fang
  rm(list=ls()) 
  path= dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(path)
  getwd()
  data <- read.csv("demographic_cleaned_07092022.csv.csv")
}
setwd(proj.path)

data.path <- 'sourcedata'
out.path  <- 'data'
data.file <- 'demographic_cleaned_07092022.csv'

# Read csv data
data <- read.csv(file.path(proj.path, data.path, data.file))

# # Paths and file for Yu-Fang
# rm(list=ls()) 
# path= dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(path)
# getwd()
# data <- read.csv("demographic_cleaned_07092022.csv.csv")

###############################################################################
# Order and re-code data
data$team <- as.factor(data$team)

################################################################################
# Country
################################################################################
data$country <- ifelse(data$country=="United Arab Emirates", "UAE", data$country)
data$country <- ifelse(data$country=="portugal", "Portugal", data$country)
data$country <- ifelse(data$country=="The Netherlands", "Netherlands", data$country)
data$country <- ifelse(data$country=="Korea", "Republic of Korea", data$country)
data$country <- ifelse(data$country=="Catalonia", "Spain", data$country)

################################################################################
# Re-code job position
################################################################################
job_senior <- c("Academic / Professor", "Full professor", "Fulltime Professor", "Prof.",
  "Professor", "professor", "Professor of Psychology", "Tenure Track Professor",
  "Neuropsychology ", "Associate Professor", "Associate professor" , "department head",
  "Work group leader", "Senior Lecturer (Associate Professor)","Faculty, Group Leader",
  "head of research group", "Independent PI", "Lead Investigator ", "Leading researcher",
  "PI", "Staff Neurologist - principal investigator", "Director", "Director of Neuroscience BU",
  "Lab Director", "Lab leader", "Lab manager and researcher", "EEG Lab head","Coordinator Open Science",
  "Group leader"
)
job_junior <- c("Assistant Profeesor","Assistant Profesor","Assistant Professor",
  "Assistant professor", "assistant professor", "Assistant Professor ","assistant professor",
  "Assistent Prof", "Asssitant professor", "research assistant professor", "Tenured Assistant Professor",
  "Faculty member" , "Psychologist Researcher" , "Research Associate" , "Research associate",
  "research associate / doctoral researcher", "Research Fellow", "Research Scientist",
  "RESEARCHER", "Researcher", "researcher", "Senior Researcher", "Senior researcher",
  "Senior researcher (glorified pot-doc)", "Senior Researcher at IU", "Tenured researcher",
  "Junior Professor", "Neuroimaging Informatics Fellow", "assistant professor "
)
job_postdoc <- c("Max Planck - postdoc", "Post doc at Spaulding Rehabilitation Hospital/Harvard Medical School",
  "Post Doctoral Research Fellow", "Post Doctoral researcher", "Post-Doc", "Post-doc",
  "post-doc", "Post-Doc Researcher", "Post-doc researcher", "Post-doctoral fellow",
  "post-doctoral fellow", "Post-doctoral Fellow ", "Post-doctoral Researcher", "Post-doctoral researcher",
  "PostDoc", "Postdoc", "PostDoc position", "postdoc research fellow", "Postdoc Researcher",
  "Postdoc researcher", "postdoc, junior researcher", "Postdoctoral Fellow","Postdoctoral fellow",
  "Postdoctoral research associate", "Postdoctoral Research Fellow", "Postdoctoral research fellow",
  "Postdoctoral research fellow " , "Postdoctoral Researcher","Postdoctoral researcher",
  "postdoctoral researcher", "Postdoctoral researcher ", "Postdoctoral researchers",
  "Postdoctoral reserach associate", "Postdoctoral Scholar", "Postoctoral scientist",
  "pstdoc", "PhD graduate", "PostDoc ", "Associate research fellow"
)
job_phd <- c("Doctoral student", "Doctoral student/Research fellow", "Graduate assistant, PhD student ",
  "graduate school student", "Graduate student", "Integrative Neuroscience PhD candidate and teaching assistant",
  "junior research fellow, graduate phd student" ,"Ph.D candidate", "Ph.D. Student",
  "Ph.d. student", "PhD Candidate", "PhD candidate", "PhD candidate ", "PhD Neuroscience Student",
  "PhD Student", "PhD student", "Phd Student", "phd student", "phD student", "PhD student faculty of experimental psychology and faculty of neurology",
  "PhD-student", "Project employee - PhD student", "Research assistant / PhD Student",
  "Doctoral Researcher", "Ph.D.", "PhD", "PhD " , "Phd.", "PhD Position", "Phd ",
  "PhD student "
)
job_lectur <- c("Akademischer Rat a. Z. ", "Junior Lecturer", "Lecturer", "Lecturer / Resercher",
  "Lecturer at University of Granada", "Lecturer in Psychology", "Lecturer in Psychology / DECRA Fellow",
  "Academic (Senior Lecturer)", "Senior Lecturer", "Senior lecturer", "Senior Lecturer in Imaging",
  "Senior Lecturer in Psychology" ,"Senior Research Scientist"
)
job_student <- c("M.A student and teaching aid", "Master's Student", "Master's student",
  "Student", "Student", "University undergraduate student ", "Predoctoral researcher ",
  "Predoctoral researcher", "Scholarship Holder" , "Early Stage Research", "student"
)
job_ass <- c("Graduate Research Assistant", "Graduate research assistant","Post-Graduate Research assistant",
  "RA", "Research Assistant" , "Research assistant", "research assistant", "Research Assistant ",
  "Research Assistant - Neurophysiopathology Technician", "Teacher assistant", "Teaching Assistant",
  "Lab Manager", "Research Engineer", "Research engineer", "Junior Researcher", "Junior researcher",
  "Senior Human Sciences Technician ", "Senior Research Technician", "Technician at the Center of Neural Technologies and Machine Learning of Baltic Federal University",
  "Project Scientist", "project scientist", "DATA ANALYST", "Data Scientist for Neurotechnology",
  "Junior Research Fellow", "junior research assistant"
)
job_outside <- c("currently unemployed (since August 2021)", "Outside of academia",
  "Casual Academics", "Medical Equipment Expert at Vice-Chancellor for Food & Drug",
  "Proprietor"
)

data$job_position_edit <- data$job_position
data$job_position_edit <- ifelse(data$job_position_edit %in% job_senior,  "Senior Professor",              data$job_position_edit)
data$job_position_edit <- ifelse(data$job_position_edit %in% job_junior,  "Junior Professor/Researcher",   data$job_position_edit)
data$job_position_edit <- ifelse(data$job_position_edit %in% job_postdoc, "Postdoc/Fellow/Scholar",        data$job_position_edit)
data$job_position_edit <- ifelse(data$job_position_edit %in% job_phd,     "PhD student",                   data$job_position_edit)
data$job_position_edit <- ifelse(data$job_position_edit %in% job_lectur,  "Lecturer/Instructor",           data$job_position_edit)
data$job_position_edit <- ifelse(data$job_position_edit %in% job_student, "Pregraduate student",           data$job_position_edit)
data$job_position_edit <- ifelse(data$job_position_edit %in% job_ass,     "Research Assistant/Technician", data$job_position_edit)
data$job_position_edit <- ifelse(data$job_position_edit %in% job_outside, "Outside academia",              data$job_position_edit)



################################################################################
# Re-code academic field
################################################################################
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

################################################################################            
# Save data
################################################################################
save(data, file=file.path(out.path, 'data.RData'))

# Save data: Yu-Fang
write.csv(data,'final_data.csv')
