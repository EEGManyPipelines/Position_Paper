
# Paths
data.path <- '/Users/mcvinding/Documents/EEGManyPipelines/metadata_summary/sourcedata'
data.file <- 'demographic_cleaned.csv'

# Read data
data <- read.csv(file.path(data.path, data.file))

# Order and re-code data
data$team <- as.factor(data$team)

data$country <- ifelse(data$country=="United Arab Emirates", "UAE", data$country)
data$country <- ifelse(data$country=="portugal", "Portugal", data$country)
data$country <- ifelse(data$country=="The Netherlands", "Netherlands", data$country)
data$country <- ifelse(data$country=="Korea", "Republic of Korea", data$country)
data$country <- ifelse(data$country=="Catalonia", "Spain", data$country)

# data$job_position $<- This one is tricky!

# SUMMARIES
# Teams
length(unique(data$team))
table(tabulate(data$team))
mean(tabulate(data$team))

# Age
mean(data$age)
median(data$age)
range(data$age)
sd(data$age)

# Country
length(unique(data$country))
table(data$country)

# Gender
table(data$gender)








