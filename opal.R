require(ggplot2)

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