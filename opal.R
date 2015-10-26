require(ggplot2)
require(plyr)

view_table <- function (extract_dir, table_name){
  table <- read.csv(sprintf("%s%s.csv", extract_dir, deparse(substitute(table_name))))
  View(table)
}
read_table <- function(extract_dir, table_name){
  return(read.csv(sprintf("%s%s.csv", extract_dir, deparse(substitute(table_name)))))
}
#
# Plot an age distribution for this extract
#
age_distribution <- function(extract_dir){  
  demographics <- read.csv(sprintf("%s%s", extract_dir, "demographics.csv"))
  demographics$year.of.birth <- substr(demographics$date_of_birth, 0, 4)
  age <- function(x)  2015 - as.integer(x)
  demographics$age <- age(demographics$year.of.birth)
  ages <- as.data.frame(table(na.omit(demographics)$age))
  names(ages) <- c("Age", "Frequency")  
  ggplot(ages, aes(x=Age, y=Frequency, fill=Age)) + 
    geom_bar(stat="identity") + 
    labs(title="Age Distribution") + 
    guides(fill=FALSE) + 
    scale_x_discrete(breaks=c(20, 40, 60, 80))
}

#
# Plot frequent diagnoses for this extract
#
common_diagnoses <- function(extract_dir, freq=10){
  diagnoses <- read.csv(sprintf("%s%s", extract_dir, "diagnosis.csv"))
  conditions <- as.data.frame(table(diagnoses$condition))
  names(conditions) <- c("Condition", "Frequency")
  
  conditions <- conditions[conditions$Freq > freq,]
  conditions <- conditions[conditions$Condition != "",]
  
  ggplot(conditions, aes(x=Condition, y=Frequency, fill=Condition)) + 
    geom_bar(stat="identity") +
    labs(title="Common Diagnoses") +
    guides(fill=FALSE) +
    coord_flip()
}

#
# Plot frequent travel destinatinos for this extract
#
common_destinations <- function(extract_dir, freq=10){
  travel <- read.csv(sprintf("%s%s", extract_dir, "travel.csv"))
  destinations <- as.data.frame(table(travel$destination))
  names(destinations) <- c("Destination", "Frequency")
  
  destinations <- destinations[destinations$Frequency > freq,]
  destinations <- destinations[destinations$Destination != "",]
  
  ggplot(destinations, aes(x=Destination, y=Frequency, fill=Destination)) + 
    geom_bar(stat="identity") +
    labs(title="Travel Destinations") +
    guides(fill=FALSE) +
    coord_flip()
}


#
# Plot common tests
#
common_tests <- function(extract_dir, freq=20){
  tests <- as.data.frame(table(read_table(extract_dir, microbiology_test)$test))
  names(tests) <- c("Test", "Frequency")
  tests <- tests[tests$Frequency > freq,]
  tests <- tests[tests$Test != "",]

  ggplot(tests, aes(x=Test, y=Frequency, fill=Test)) +
    geom_bar(stat="identity") +
    labs(title="Common Investigations") +
    guides(fill=FALSE) + coord_flip()
}

common_drugs <- function(extract_dir, freq=20){
  drugs <- as.data.frame(table(read_table(extract_dir, antimicrobial)$drug))
  names(drugs) <- c("Drug", "Frequency")
  drugs <- drugs[drugs$Frequency > freq,]
  drugs <- drugs[drugs$Drug != "",]
  
  ggplot(drugs, aes(x=Drug, y=Frequency, fill=Drug)) +
    geom_bar(stat="identity") +
    labs(title="Common Treatments") +
    guides(fill=FALSE)+
    coord_flip()
}

diagnoses_for_drug <- function(extract_dir, drug, freq=2){
  drug <- deparse(substitute(drug))
  antimicrobials <- read_table(extract_dir, antimicrobial)
  episode.ids <- antimicrobials[antimicrobials$drug == drug,]$episode_id
  diagnoses <- read_table(extract_dir, diagnosis)

  diagnoses.for.episodes <- diagnoses[diagnoses$episode_id %in% episode.ids,]
  
  conditions <- as.data.frame(table(diagnoses.for.episodes$condition))
  
  names(conditions) <- c("Condition", "Frequency")
  
  conditions <- conditions[conditions$Frequency > freq,]
  conditions <- conditions[conditions$Condition != "",]
  
  ggplot(conditions, aes(x=Condition, y=Frequency, fill=Condition)) + 
    geom_bar(stat="identity") +
    labs(title=sprintf("%s%s", "Diagnoses for ", drug)) +
    guides(fill=FALSE) +
    coord_flip()
}

drugs_for_diagnosis <- function(extract_dir, diagnosis, freq=2){
  diagnosis <- deparse(substitute(diagnosis))
  diagnoses <- read_table(extract_dir, diagnosis)
  episode.ids <- diagnoses[diagnoses$condition == diagnosis,]$episode_id
  antimicrobials <- read_table(extract_dir, antimicrobial)
  drugs.for.diagnosis <- antimicrobials[antimicrobials$episode_id %in% episode.ids,]
  drugs <- as.data.frame(table(drugs.for.diagnosis$drug))
  names(drugs) <- c("Drug", "Frequency")
  drugs <- drugs[drugs$Frequency > freq,]
  drugs <- drugs[drugs$Drug != "",]
  ggplot(drugs, aes(x=Drug, y=Frequency, fill=Drug)) +
    geom_bar(stat="identity") +
    labs(title=sprintf("%s %s", "Drugs used to treat patients with", diagnosis)) +
    guides(fill=FALSE) + 
    coord_flip()
}

#
# Plot length of stay
#
length_of_stay <- function(extract_dir){  
  episodes <- read_table(extract_dir, episodes)
  episodes$los <- as.Date(episodes$discharge_date, "%Y-%m-%d") - as.Date(episodes$date_of_admission, "%Y-%m-%d")

  los <- as.data.frame(table(na.omit(episodes[episodes$los >= 0,])$los))
  names(los) <- c("LOS", "Frequency")  

  ggplot(los, aes(x=LOS, y=Frequency, fill=LOS)) + 
    geom_bar(stat="identity") + 
    labs(title="Length of stay", x="Days") + 
    guides(fill=FALSE) +
    scale_x_discrete(breaks=c(5, 10, 20, 30, 40, 60))
}

blood_cultures <- function(extract_dir){
  tests <- read_table(extract_dir, microbiology_test)
  return(tests[tests$test == "Blood Culture",])
}

common_culture_organisms <- function(extract_dir, freq=20){
  cultures <- blood_cultures(extract_dir)
  organisms <- as.data.frame(table(na.omit(cultures$organism)))
  names(organisms) <- c("Organism", "Frequency")
  
  organisms <- organisms[organisms$Frequency > freq, ]
  organisms <- organisms[organisms$Organism != "",]
  
  ggplot(organisms, aes(x=Organism, y=Frequency, fill=Organism))+ 
      geom_bar(stat="identity") + 
      labs(title="Common Blood Culture Organisms") +
      guides(fill=FALSE) +
      coord_flip()
}

plot_audit_counts <- function(audit.counts){
  View(audit.counts)
  ggplot(audit.counts, aes(reorder(x, y), x=Action, y=Count, fill=Action)) + 
    geom_bar(stat="identity") + 
    coord_flip() + 
    labs(title="Clinical Advice Audit Activity")
}

#
# Plot Clinical advice audit checkboxes
#
advice_audits <- function(extract_dir){
  advice <- read.csv(sprintf("%s%s", extract_dir, "clinical_advice.csv"))
  ca.audit <- advice[,8:11]
  numtrue <- function(x) sum(x == "True")
  audit.counts <- colwise(numtrue)(ca.audit)
  audit.counts <- data.frame(t(audit.counts))  
  names(audit.counts) <- c("Count")
  audit.counts$Action <- row.names(audit.counts)
  plot_audit_counts(audit.counts)
}

advice_audits_for_user <- function(extract_dir, user){
  advice <- read.csv(sprintf("%s%s", extract_dir, "clinical_advice.csv"))
  advice <- advice[advice$initials == user,]
  ca.audit <- advice[,8:11]
  numtrue <- function(x) sum(x == "True")
  audit.counts <- colwise(numtrue)(ca.audit)
  audit.counts <- data.frame(t(audit.counts))  
  names(audit.counts) <- c("Count")
  audit.counts$Action <- row.names(audit.counts)
  plot_audit_counts(audit.counts)
}