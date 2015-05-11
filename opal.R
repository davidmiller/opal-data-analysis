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
  
  View(audit.counts)
  ggplot(audit.counts, aes(reorder(x, y), x=Action, y=Count, fill=Action)) + 
    geom_bar(stat="identity") + 
    coord_flip() + 
    labs(title="Clinical Advice Audit Activity")
}