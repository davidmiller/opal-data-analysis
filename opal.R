require(ggplot2)
require(plyr)

#
# Plot an age distribution for this extract
#
age_distribution <- function(extract_dir){  
  episodes <- read.csv(sprintf("%s%s", extract_dir, "episodes.csv"))
  episodes$year.of.birth <- substr(episodes$date.of.birth, 0, 4)
  age <- function(x)  2015 - as.integer(x)
  episodes$age <- age(episodes$year.of.birth)
  ages <- as.data.frame(table(na.omit(episodes)$age))
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
common_diagnoses <- function(extract_dir){
  diagnoses <- read.csv(sprintf("%s%s", extract_dir, "diagnosis.csv"))
  conditions <- as.data.frame(table(diagnoses$condition))
  names(conditions) <- c("Condition", "Frequency")
  
  conditions <- conditions[conditions$Freq > 3,]
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
common_destinations <- function(extract_dir){
  travel <- read.csv(sprintf("%s%s", extract_dir, "travel.csv"))
  destinations <- as.data.frame(table(travel$destination))
  names(destinations) <- c("Destination", "Frequency")
  
  destinations <- destinations[destinations$Frequency > 1,]
  destinations <- destinations[destinations$Destination != "",]
  
  ggplot(destinations, aes(x=Destination, y=Frequency, fill=Destination)) + 
    geom_bar(stat="identity") +
    labs(title="Travel Destinations") +
    guides(fill=FALSE) +
    coord_flip()
}

#
# Plot length of stay
#
length_of_stay <- function(extract_dir){  
  episodes <- read.csv(sprintf("%s%s", extract_dir, "episodes.csv"))
  episodes$los <- as.Date(demographics$discharge.date) - as.Date(demographics$date.of.admission)

  los <- as.data.frame(table(na.omit(episodes[episodes$los >= 0,])$los))
  names(los) <- c("LOS", "Frequency")  

  ggplot(los, aes(x=LOS, y=Frequency, fill=LOS)) + 
    geom_bar(stat="identity") + 
    labs(title="Length of stay", x="Days") + 
    guides(fill=FALSE) +
    scale_x_discrete(breaks=c(5, 10, 20, 30, 40, 60))
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