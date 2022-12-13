### IMPORT PARTICIPANT METADATA
# Read the raw csv file. Re-code variables. Save in R format.
#
# Trübutschek, D. et al. EEGManyPipelines: A large-scale, grass-root multi-analyst study of EEG analysis practices in the wild. (2022). doi:10.31222/osf.io/jq342
#
# Created by Mikkel C. Vinding & Yu-Fang Yang 

# Paths and file for Mikkel
if ( Sys.getenv("USER") == 'mcvinding' ){
  proj.path <- '/Users/mcvinding/Documents/EEGManyPipelines/metadata_summary'
} else if (Sys.getenv("USERNAME") == 'Mikkel'){
  proj.path <- 'C:/Users/Mikkel/Documents/EEGManyPipelines/metadata_summary'
} else if (Sys.getenv("USER") == 'darinka'){
  proj.path <- '/home/darinka/Documents/EEGManyPipes/metadata_summary'
} else if (Sys.getenv("USERNAME") == 'darinka.truebutschek'){
  proj.path <- 'C:/Users/darinka.truebutschek/Documents/EEGManyPipelines/metadata_summary'
} else {
  # Paths and file for Yu-Fang
  rm(list=ls())
  path= dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(path)
  getwd()
  data <- read.csv("demographic_cleaned_07092022.csv")
}
setwd(proj.path)

data.path <- 'sourcedata'
out.path  <- 'data'
data.file <- 'demographic_cleaned_07092022_teamID_combinedBeliefs_forPlottingOnly.csv'

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
data$country <- ifelse(data$country=="United Arab Emirates", "UAE", as.character(data$country))
data$country <- ifelse(data$country=="portugal", "Portugal", as.character(data$country))
data$country <- ifelse(data$country=="The Netherlands", "Netherlands", as.character(data$country))
data$country <- ifelse(data$country=="Korea", "Republic of Korea", as.character(data$country))
data$country <- ifelse(data$country=="Catalonia", "Spain", as.character(data$country))
data$country <- ifelse(data$country=="South Korea", "Republic of Korea", as.character(data$country))

data$country <- as.factor(data$country)

################################################################################
# Re-code highest degree
################################################################################
data$highest_degree_edit <- data$highest_degree

bachelorsDeg <- c('Bachelor', 'Bachelor \'s in Optometry', 'Bachelor\'s degree', 'Undergraduate Degree (B.Sc)',
                  'Under degree', 'Bachelors', 'BSocSc', 'Bachelor of Health Science (Honours) (AQF Level 8)',
                  'Bachelor\'s degree in Clinical Neurophysiology', 'BA', 'bachelor', 'BEng', 'Bachelor of Science',
                  'Bachelor of Arts (Psychology Major) ', 'B.A', 'B.Sc.', 'Bachelor of Engineering in Biomedical engineering and Electrical engineering',
                  'BSc Neuroscience with Honours', 'undergraduate', 'Undergraduate', 'Bachelor\'s in Optometry ')

mastersDeg <- c('Master degree', 'Diploma (Master-equivalent', 'Master', 'MA', 'MSc in Biology',
             'Master of Science', 'Masters', 'Master', 'Master degree', 'MSc', 'Master\'s in Psychology',
             'Master of Science in Cognitive Neuroscience', 'master', 'Master student', 'Master in Neurosciences',
             'MsC', 'Msc', 'Master degree in psychology', 'Master in PSYCHOLOGY', 'MSC', 'MRes Neurotechnology',
             'Master of Science', 'Master of Engineering', 'Masters in Technology in AI', 'MSc commputer system engineering',
             'Master in Engineering', 'Master of Science in Engineering Sciences', 'Master of Education', 'Master of education',
             'M.S.', 'Master\'s', 'Master of Science in Neuroscience', 'Diplom Psycholgie (Psychology, M.Sc. equivalent)',
             'Master of Science (Psychology)', 'M.Sc.', 'master of science', 'MSc in Neurocience', 'Master Degree', 'Master degree',
             'Master of Science (M.Sc.)', 'Master of Science in Biomedical Engineering', 'Master after Bachelor', 'Master in Psychology',
             'M.sc.', 'Master\'s degree', 'Master of Psychology', 'Master of Arts in Cognitive Neuroscience', 'Master of science', 'MSc.',
             'M.A. (Psychology)', 'Master in Cognitive Science', 'master\'s degree', 'MSc Cognitive Neuroscience and Clinical Neuropsychology',
             'MSc in Pschology', 'Diplom (equivalent to M. Sc.) in Psychology', 'M.Phil', 'M.Sc.', 'Master of Medicine', 'Master of Science in Cognitive Science',
             'PhD (not defended yet but manuscript accepted by reading committee)', 'Diploma (Master-equivalent)',
             'MSc computer system engineering', 'Master of education', 'MSc in Neuroscience', 'Master in Science', 'Research Master',
             'M.Sc.', 'MSc in Psychology', 'Master\'s degree', 'master degree', 'Master', 'Master of education', 'M.Sc. ',
             'Master´s degree', 'Master ', 'Master of education ', 'Postgraduate')

PhD <- c('Dr', 'PhD', 'Ph.D.', 'Dr. rer. nat.', 'PhD Heatlh Science', 'PhD ', 'PhD in Automation, Robotics and Bioengineering ',
         'PhD in Psychology', ' Doctoral degree', 'phd', 'PHD', 'PhD in biomedical sciences', 'Doctorate (PhD)', 'Dr. ',
         'Ph. D.', 'Ph.d', 'MD, PhD', 'Doctorate', 'PhD in Information and Communication Technologies', 'Ph.D', 'PD', 'Phd',
         'Doctor', 'PhD of engineering', 'P.h.D.', 'PhD Health Science', 'Dr. rer.nat.', 'Dr. rer. nat', 'PhD of engineering ')

habil <- c('Prof. ', 'PhD with Habilitation', 'German habilitation', 'habilitation', 'Habilitation', 'Prof. Dr.')

other <- c('Specialist Degree', 'high school diploma ')

data$highest_degree_edit <- ifelse(data$highest_degree_edit %in% bachelorsDeg,  'Bachelors', as.character(data$highest_degree_edit))
data$highest_degree_edit <- ifelse(data$highest_degree_edit %in% mastersDeg,  'Masters', as.character(data$highest_degree_edit))
data$highest_degree_edit <- ifelse(data$highest_degree_edit %in% PhD,  'PhD', as.character(data$highest_degree_edit))
data$highest_degree_edit <- ifelse(data$highest_degree_edit %in% habil,  'Habil', as.character(data$highest_degree_edit))
data$highest_degree_edit <- ifelse(data$highest_degree_edit %in% other,  'Other', as.character(data$highest_degree_edit))

data$highest_degree_edit <- as.factor(data$highest_degree_edit)

################################################################################
# Re-code job positions taking into account highest degree obtained
################################################################################
data$job_position_basedOnDeg <- as.character(data$job_position)

#Analysts with a bachelors's deg
job_phdStudent_deg <- c('Graduate student', 'Graduate assistant, PhD student ', 'PhD Student',
                        'Research assistant / PhD Student', 'PhD Neuroscience Student', 'graduate school student')
job_student_deg <- c('student', 'Student', 'Master\'s Student', 'M.A student and teaching aid',
                     'Master\'s student')
job_ass_deg <- c('Research Assistant - Neurophysiopathology Technician', 'Research Assistant',
                 'Technician at the Center of Neural Technologies and Machine Learning of Baltic Federal University',
                 'junior research assistant', 'Lab Manager', 'Research assistant')
job_other_deg <- c('Researcher')

data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors'] %in% job_phdStudent_deg,
         'PhD student', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors']))

data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors'] %in% job_student_deg,
         'Pre-doctoral student', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors']))

data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors'] %in% job_ass_deg,
         'Research assistants', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors']))

data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors'] %in% job_other_deg,
         'Researcher without PhD', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Bachelors']))

#Analysts with a master's deg
job_postdoc_deg <- c('Postdoctoral researchers')
job_phdStudent_deg <- c('PhD student', 'phd student', 'PhD Candidate', 'Phd Student', 'Ph.D candidate',
                        'PhD candidate', 'PhD-student', 'PhD', 'Ph.D. Student', 'PhD ', 'Graduate student', 'Phd ', 'PhD candidate ',
                        'Doctoral Researcher', 'Doctoral student', 'PhD student ',
                        'PhD student faculty of experimental psychology and faculty of neurology',
                        'Integrative Neuroscience PhD candidate and teaching assistant', 'Graduate Research Assistant',
                        'Graduate research assistant', 'Doctoral student/Research fellow', 'PhD Position', 'research associate / doctoral researcher',
                        'Ph.D.', 'Ph.d. student', 'junior research fellow, graduate phd student', 'PhD Student',
                        'Project employee - PhD student', 'PhD student', 'Scholarship Holder', 'Predoctoral researcher',
                        'Predoctoral researcher ')
job_ass_deg <- c('Research Assistant', 'Post-Graduate Research assistant', 'Teacher assistant')
job_other_deg <- c('Leading researcher', 'Researcher', 'Junior Research Fellow', 'Postdoctoral researcher', 'researcher',
                   'Research Fellow', 'Early Stage Research', 'Research Engineer', 'Junior researcher')

job_outside_deg <- c('Outside of academia', 'Medical Equipment Expert at Vice-Chancellor for Food & Drug', 'DATA ANALYST')

data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] %in% job_postdoc_deg,
         'Postdoc', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters']))

data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] %in% job_phdStudent_deg,
         'PhD student', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters']))

data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] %in% job_ass_deg,
         'Research assistants', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters']))

data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] %in% job_other_deg,
         'Researcher without PhD', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters']))

data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters'] %in% job_outside_deg,
         'Outside academia', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Masters']))

#Analysts with a PhD
job_faculty_deg <- c('Professor', 'Associate Profeesor', 'Assostant Profeesor', 'assistant professor', 'Professor of Psychology',
                     'Director of Neuroscience BU', 'Lab leader', 'Faculty, Group Leader', 'Academic (Senior Lecturer)',
                     'assistant professor', 'Full professor', 'Associate professor', 'Assistant Professor ',
                     'Director', 'Asssitant professor', 'Lab Director', 'EEG Lab head', 'Tenure Track Professor',
                     'Lecturer in Psychology', 'Senior Lecturer', 'Assistant Profesor', 'Academic / Professor',
                     'Senior Lecturer in Imaging', 'Lecturer', 'Senior Lecturer (Associate Professor)',
                     'Assistant professor', 'Senior lecturer', 'head of research group', 'department head',
                     'Junior Professor', 'Senior Lecturer in Psychology', 'Lecturer at University of Granada',
                     'research assistant professor', 'Faculty member', 'Associate Professor', 'assistant professor ',
                     'Tenured Assistant Professor', 'Assistant Profeesor', 'Assistant Professor', 'Prof.',
                     'Assistent Prof', 'Lecturer / Resercher', 'Staff Neurologist - principal investigator', 'Tenured researcher', 'Group leader')
job_postdoc_deg <- c('Postdoc', 'Postdoctoral research fellow', 'Post Doctoral researcher',
                     'Postdoctoral research fellow', 'Post-Doc Researcher', 'Senior researcher (glorified pot-doc)',
                     'post-doc', 'postdoc, junior researcher', 'PostDoc', 'Postdoctoral fellow', 'Post-doctoral Fellow ',
                     'Post Doctoral Research Fellow', 'Post doc at Spaulding Rehabilitation Hospital/Harvard Medical School',
                     'Postdoctoral Scholar', 'Postdoctoral Researcher', 'Postdoctoral Research Fellow',
                     'Postdoc Researcher', 'Post-doctoral fellow', 'Post-doctoral Researcher', 'Post-doc',
                     'Post-doctoral researcher', 'Post-Doc', 'Post-doc researcher', 'Postdoctoral researcher',
                     'Postdoc researcher', 'Postdoctoral research associate', 'post-doctoral fellow', 'postdoc research fellow',
                     'Max Planck - postdoc', 'postdoctoral researcher', 'Postdoctoral researcher ',
                     'Postdoctoral research associate', 'Postdoctoral Fellow', 'Postoctoral scientist', 'PostDoc position',
                     'pstdoc', 'Postdoctoral reserach associate', 'Lecturer in Psychology / DECRA Fellow', 'Postdoctoral research fellow ',
                     'Associate research fellow', 'Research Fellow', 'Research Associate', 'PostDoc ')

job_ass_deg <- c('Lab manager and researcher', 'Senior Research Technician', 'Akademischer Rat a. Z. ',
                 'Coordinator Open Science', 'RA', 'Senior Human Sciences Technician ', 'Teaching Assistant',
                 'research assistant', 'Research Assistant ')

job_other_deg <- c('PI', 'Research Scientist', 'Lead Investigator ', 'Senior Researcher', 'Junior Researcher',
                   'Research Assistant', 'Junior Lecturer', 'Independent PI', 'Neuroimaging Informatics Fellow',
                   'Psychologist Researcher', 'project scientist', 'Senior Researcher at IU', 'Researcher',
                   'Project Scientist', 'researcher', 'Research engineer', 'Senior researcher', 'RESEARCHER')

job_outside_deg <- c('Casual Academics', 'PhD graduate', 'Data Scientist for Neurotechnology',
                     'currently unemployed (since August 2021)', 'Proprietor')

data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] %in% job_faculty_deg,
         'Group leader and faculty', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD']))

data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] %in% job_postdoc_deg,
         'Postdoc', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD']))

data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] %in% job_ass_deg,
         'Research assistants', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD']))

data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] %in% job_other_deg,
         'Researchers with PhD', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD']))

data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD'] %in% job_outside_deg,
         'Outside academia', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='PhD']))

#Analysts with a habilitation
job_faculty_deg <- c('professor', 'Professor', 'Work group leader', 'Assistant Professor',
                     'Associate professor', 'Full professor', 'Fulltime Professor', 'Neuropsychology ')

job_other_deg <- c('Senior Research Scientist', 'Researcher')

data$job_position_basedOnDeg[data$highest_degree_edit=='Habil'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Habil'] %in% job_faculty_deg,
         'Group leader and faculty', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Habil']))

data$job_position_basedOnDeg[data$highest_degree_edit=='Habil'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Habil'] %in% job_other_deg,
         'Researchers with PhD', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Habil']))

#With other degrees
data$job_position_basedOnDeg[data$highest_degree_edit=='Other'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Other']=='University undergraduate student ',
         'Pre-doctoral student', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Other']))

data$job_position_basedOnDeg[data$highest_degree_edit=='Other'] <-
  ifelse(data$job_position_basedOnDeg[data$highest_degree_edit=='Other']=='Research associate',
         'Research assistants', as.character(data$job_position_basedOnDeg[data$highest_degree_edit=='Other']))

data$job_position_basedOnDeg <- as.factor(data$job_position_basedOnDeg)
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
