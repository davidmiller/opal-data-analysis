require(ggplot2)
require(plyr)

if(!exists("OPALDATA")){
  OPALDATA <<- FALSE  
}

#
# Read an OPAL data table
#
read_table <- function(table_name){
  table_name <- deparse(substitute(table_name))
  return(read.csv(sprintf("%s%s.csv", OPALDATA, table_name)))
}

#
# View an OPAL data table in RStudio
#
view_table <- function (table_name){
  table_name <- deparse(substitute(table_name))
  table <- read.csv(sprintf("%s%s.csv", OPALDATA, table_name))
  View(table)
}

bar_plot <- function(data, aesthetics, title){
  ggplot(data, aesthetics) +
    geom_bar(stat="identity") +
    labs(title=title) +
    guides(fill=FALSE) +
    coord_flip()
}

#
# Plot an age distribution for a given demographic subset
#
plot_age_distribution <- function(demographics, title="Age Distribution"){
  demographics <- demographics[demographics$date_of_birth != "None",]
  demographics$year.of.birth <- substr(demographics$date_of_birth, 0, 4)
  age <- function(x)  2015 - as.integer(x)
  demographics$age <- age(demographics$year.of.birth)
  ages <- as.data.frame(table(demographics$age))
  names(ages) <- c("Age", "Frequency")  
  
  ggplot(ages, aes(x=Age, y=Frequency, fill=Age)) + 
    geom_bar(stat="identity") + 
    labs(title=title) + 
    guides(fill=FALSE) + 
    scale_x_discrete(breaks=c(20, 40, 60, 80))
}

#
# Plot an age distribution for this extract
#
age_distribution <- function(){  
  demographics <- read_table(demographics)
  plot_age_distribution(demographics)
}

#
# Plot The age distribution for a particular drug
#
age_distribution_for_drug <- function(drug){
  drug <- deparse(substitute(drug))
  antimicrobials <- read_table(antimicrobial)
  episode.ids <- antimicrobials[antimicrobials$drug == drug,]$episode_id
  demographics <- read_table(demographics)
  demographics.for.episodes <- demographics[demographics$episode_id %in% episode.ids,]
  numpatients <- as.character(nrow(demographics.for.episodes))
  title <- sprintf("Age distribution of %s patients treated with %s", numpatients, drug)
  plot_age_distribution(demographics.for.episodes, title=title)
}

#
# Plot frequent diagnoses for this extract
#
common_diagnoses <- function(freq=10){
  diagnoses <- read_table(diagnosis)
  conditions <- as.data.frame(table(diagnoses$condition))
  names(conditions) <- c("Condition", "Frequency")
  
  conditions <- conditions[conditions$Freq > freq,]
  conditions <- conditions[conditions$Condition != "",]
  
  bar_plot(conditions, aes(x=Condition, y=Frequency, fill=Condition), "Common Diagnoses")
}

#
# Plot frequent travel destinatinos for this extract
#
common_destinations <- function(freq=10){
  travel <- read_table(travel)
  destinations <- as.data.frame(table(travel$destination))
  names(destinations) <- c("Destination", "Frequency")
  
  destinations <- destinations[destinations$Frequency > freq,]
  destinations <- destinations[destinations$Destination != "",]
  
  bar_plot(destinations, aes(x=Destination, y=Frequency, fill=Destination), "Travel Destinations")
}


#
# Plot common tests
#
common_tests <- function(freq=20){
  tests <- as.data.frame(table(read_table(microbiology_test)$test))
  names(tests) <- c("Test", "Frequency")
  tests <- tests[tests$Frequency > freq,]
  tests <- tests[tests$Test != "",]
  bar_plot(tests, aes(x=Test, y=Frequency, fill=Test), "Common Investigations")
}

#
# Plot common drugs
#
common_drugs <- function(freq=20){
  drugs <- as.data.frame(table(read_table(antimicrobial)$drug))
  names(drugs) <- c("Drug", "Frequency")
  drugs <- drugs[drugs$Frequency > freq,]
  drugs <- drugs[drugs$Drug != "",]
  bar_plot(drugs, aes(x=Drug, y=Frequency, fill=Drug), "Common Treatments")
}

#
# Plot the diagnoses that led to a patient being treated with a 
# given drug.
#
diagnoses_for_drug <- function(drug, freq=2){
  drug <- deparse(substitute(drug))
  antimicrobials <- read_table(antimicrobial)
  episode.ids <- antimicrobials[antimicrobials$drug == drug,]$episode_id
  diagnoses <- read_table(diagnosis)

  diagnoses.for.episodes <- diagnoses[diagnoses$episode_id %in% episode.ids,]
  
  conditions <- as.data.frame(table(diagnoses.for.episodes$condition))
  
  names(conditions) <- c("Condition", "Frequency")
  
  conditions <- conditions[conditions$Frequency > freq,]
  conditions <- conditions[conditions$Condition != "",]
  
  bar_plot(conditions, aes(x=Condition, y=Frequency, fill=Condition), sprintf("%s%s", "Diagnoses for ", drug)) 
}

#
# Plot the drugs used to treat patients with a given diagnosis
#
drugs_for_diagnosis <- function(diagnosis, freq=2){
  diagnosis <- deparse(substitute(diagnosis))
  diagnoses <- read_table(diagnosis)
  episode.ids <- diagnoses[diagnoses$condition == diagnosis,]$episode_id
  antimicrobials <- read_table(antimicrobial)
  drugs.for.diagnosis <- antimicrobials[antimicrobials$episode_id %in% episode.ids,]
  drugs <- as.data.frame(table(drugs.for.diagnosis$drug))
  names(drugs) <- c("Drug", "Frequency")
  drugs <- drugs[drugs$Frequency > freq,]
  drugs <- drugs[drugs$Drug != "",]
  title = sprintf("%s %s", "Drugs used to treat patients with", diagnosis)
  bar_plot(drugs, aes(x=Drug, y=Frequency, fill=Drug), title)
}

#
# Plot length of stay
#
length_of_stay <- function(){  
  episodes <- read_table(episodes)
  episodes$los <- as.Date(episodes$discharge_date, "%Y-%m-%d") - as.Date(episodes$date_of_admission, "%Y-%m-%d")

  los <- as.data.frame(table(na.omit(episodes[episodes$los >= 0,])$los))
  names(los) <- c("LOS", "Frequency")  

  ggplot(los, aes(x=LOS, y=Frequency, fill=LOS)) + 
    geom_bar(stat="identity") + 
    labs(title="Length of stay", x="Days") + 
    guides(fill=FALSE) +
    scale_x_discrete(breaks=c(5, 10, 20, 30, 40, 60))
}

#
# Return Blood cultures
#
blood_cultures <- function(){
  tests <- read_table(microbiology_test)
  return(tests[tests$test == "Blood Culture",])
}

#
# Plot common organisms from Blood Cultures
#
common_culture_organisms <- function(freq=20){
  cultures <- blood_cultures()
  organisms <- as.data.frame(table(na.omit(cultures$organism)))
  names(organisms) <- c("Organism", "Frequency")
  
  organisms <- organisms[organisms$Frequency > freq, ]
  organisms <- organisms[organisms$Organism != "",]
  
  bar_plot(organisms, aes(x=Organism, y=Frequency, fill=Organism), "Common Blood Culture Organisms")
}

# plot_audit_counts <- function(audit.counts){
#   View(audit.counts)
#   ggplot(audit.counts, aes(reorder(x, y), x=Action, y=Count, fill=Action)) + 
#     geom_bar(stat="identity") + 
#     coord_flip() + 
#     labs(title="Clinical Advice Audit Activity")
# }
# 
# #
# # Plot Clinical advice audit checkboxes
# #
# advice_audits <- function(extract_dir){
#   advice <- read.csv(sprintf("%s%s", extract_dir, "clinical_advice.csv"))
#   ca.audit <- advice[,8:11]
#   numtrue <- function(x) sum(x == "True")
#   audit.counts <- colwise(numtrue)(ca.audit)
#   audit.counts <- data.frame(t(audit.counts))  
#   names(audit.counts) <- c("Count")
#   audit.counts$Action <- row.names(audit.counts)
#   plot_audit_counts(audit.counts)
# }
# 
# advice_audits_for_user <- function(extract_dir, user){
#   advice <- read.csv(sprintf("%s%s", extract_dir, "clinical_advice.csv"))
#   advice <- advice[advice$initials == user,]
#   ca.audit <- advice[,8:11]
#   numtrue <- function(x) sum(x == "True")
#   audit.counts <- colwise(numtrue)(ca.audit)
#   audit.counts <- data.frame(t(audit.counts))  
#   names(audit.counts) <- c("Count")
#   audit.counts$Action <- row.names(audit.counts)
#   plot_audit_counts(audit.counts)
# }