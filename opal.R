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

bar_plot <- function(data, aesthetics, title, filename=""){
  ggplot(data, aesthetics) +
    geom_bar(stat="identity") +
    labs(title=title) +
    guides(fill=FALSE) +
    coord_flip()
  
  if(filename != ""){
    ggsave(filename)
  }
}

episode_ids_for_pirmary_diagnosis <- function(condition){
  diagnoses <- read_table(primary_diagnosis)
  episode.ids <- diagnoses[diagnoses$Condition == condition,]$Episode
  return(episode.ids)
}

#
# Plot an age distribution for a given demographic subset
#
plot_age_distribution <- function(demographics, title="Age Distribution", filename=""){
  demographics <- demographics[demographics$Date.of.Birth != "None",]
  demographics$year.of.birth <- substr(demographics$Date.of.Birth, 0, 4)
  age <- function(x) 2018 - as.integer(x)
  demographics$age <- age(demographics$year.of.birth)
  ages <- as.data.frame(table(demographics$age))
  names(ages) <- c("Age", "Frequency")  
  
  ggplot(ages, aes(x=Age, y=Frequency, fill=Age)) + 
    geom_bar(stat="identity") + 
    labs(title=title) + 
    guides(fill=FALSE) + 
    scale_x_discrete(breaks=c(20, 40, 60, 80))
  if(filename != ""){
    ggsave(filename)
  }
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
  #drug <- deparse(substitute(drug))
  antimicrobials <- read_table(antimicrobial)
  episode.ids <- antimicrobials[antimicrobials$Drug == drug,]$Episode
  demographics <- read_table(demographics)
  demographics.for.episodes <- demographics[demographics$Episode %in% episode.ids,]
  numpatients <- as.character(nrow(demographics.for.episodes))
  title <- sprintf("Age distribution of %s patients treated with %s", numpatients, drug)
  plot_age_distribution(demographics.for.episodes, title=title)
}

age_distribution_for_primary_diagnosis <- function(condition){
  condition <- deparse(substitute(condition))
  demographics <- read_table(demographics)
  episode.ids <- episode_ids_for_pirmary_diagnosis(condition)
  episodes <- read_table(episode)
  patient.ids <- episodes[episodes$ID %in% episode.ids,]$Patient.ID
  title <- sprintf("Age distribution for patients with %s", deparse(substitute(condition)))
  our.demographics <- demographics[demographics$Patient %in% patient.ids,]
  plot_age_distribution(our.demographics, title=title)
}

#
# Plot frequent diagnoses for this extract
#
common_diagnoses <- function(freq=10, filename="", title="Common Diagnoses"){
  diagnoses <- read_table(diagnosis)
  conditions <- as.data.frame(table(diagnoses$Condition))
  names(conditions) <- c("Condition", "Frequency")
  
  conditions <- conditions[conditions$Freq > freq,]
  conditions <- conditions[conditions$Condition != "",]
  
  bar_plot(conditions, aes(x=Condition, y=Frequency, fill=Condition), title, filename=filename)
}

common_primary_diagnoses <- function(freq=10, filename="", title="Top Primary Diagnoses"){
  diagnoses <- read_table(primary_diagnosis)
  conditions <- as.data.frame(table(diagnoses$Condition))
  names(conditions) <- c("Condition", "Frequency")
  conditions <- conditions[conditions$Frequency > freq,]
  conditions <- conditions[conditions$Condition != "",]
  bar_plot(conditions, aes(x=Condition, y=Frequency, fill=Condition), title, filename=filename)
}

common_opat_infective_diagnosis <- function(freq=10){
  diagnoses <- read_table(opat_outcome)
  conditions <- as.data.frame(table(diagnoses$Infective.Diagnosis))
  names(conditions) <- c("Condition", "Frequency")
  conditions <- conditions[conditions$Frequency > freq,]
  conditions <- conditions[conditions$Condition != "",]
  bar_plot(conditions, aes(x=Condition, y=Frequency, fill=Condition), "Top OPAT Infective Diagnoses")  
}

temp_distribution <- function(
  title="Temperature distribution (C temperatures plausibly compatible with life in humans)",
  min=10, max=60){
  obs <- read_table(observation)
  obs <- obs[obs$Temperature < max,]
  obs <- obs[obs$Temperature > min,]
  temps <- as.data.frame(table(obs$Temperature))
  names(temps) <- c("Temperature", "Frequency")
  bar_plot(temps, aes(x=Temperature, y=Frequency, fill=Temperature), title)
}

febrile_episode_ids <- function(){
  obs <- read_table(observation)
  obs <- obs[obs$Temperature > 37.9,]  
  obs <- obs[obs$Temperature < 60,]
  return(obs$Episode)
}

fever_diagnoses <- function(freq=10){
  diagnoses <- read_table(diagnosis)
  diagnoses <- diagnoses[diagnoses$Episode %in% febrile_episode_ids(),]
  febrile.diagnoses <- as.data.frame(table(diagnoses$Condition))
  View(febrile.diagnoses)
  names(febrile.diagnoses) <- c("Condition", "Frequency")
  febrile.diagnoses <- febrile.diagnoses[febrile.diagnoses$Frequency > freq,]
  bar_plot(febrile.diagnoses, aes(x=Condition, y=Frequency, fill=Condition), 
           "Diagnoses for patients with a temperature > 37.9")
}

#
# Plot frequent travel destinatinos for this extract
#
common_destinations <- function(freq=10){
  travel <- read_table(travel)
  destinations <- as.data.frame(table(travel$Destination))
  names(destinations) <- c("Destination", "Frequency")
  
  destinations <- destinations[destinations$Frequency > freq,]
  destinations <- destinations[destinations$Destination != "",]
  
  bar_plot(destinations, aes(x=Destination, y=Frequency, fill=Destination), "Travel Destinations")
}


#
# Plot common tests
#
common_tests <- function(freq=20){
  tests <- as.data.frame(table(read_table(microbiology_test)$Test))
  names(tests) <- c("Test", "Frequency")
  tests <- tests[tests$Frequency > freq,]
  tests <- tests[tests$Test != "",]
  bar_plot(tests, aes(x=Test, y=Frequency, fill=Test), "Common Investigations")
}

#
# Plot common drugs
#
common_drugs <- function(freq=20, title="Common Treatments"){
  drugs <- as.data.frame(table(read_table(antimicrobial)$Drug))
  names(drugs) <- c("Drug", "Frequency")
  drugs <- drugs[drugs$Frequency > freq,]
  drugs <- drugs[drugs$Drug != "",]
  bar_plot(drugs, aes(x=Drug, y=Frequency, fill=Drug), title)
}

drug_days <- function(drug){
  drug <- deparse(substitute(drug))
  treatment <- read_table(antimicrobial)
  
  treatment <- treatment[treatment$End.Date != "None", ]
  treatment <- treatment[treatment$Start.Date != "None", ]
  treatment <- treatment[treatment$Drug == drug,]
  
  treatment$days <- as.Date(treatment$End.Date, "%Y-%m-%d") - as.Date(treatment$Start.Date, "%Y-%m-%d")
  treatment <- treatment[treatment$days > -1,]
  treatment$days <- treatment$days +1
  
  lengths <- as.data.frame(table(treatment$days))
  names(lengths) <- c("Days", "Freq")
  
  bar_plot(lengths, aes(x=Days, y=Freq, fill=Days), sprintf("Days treated with %s", drug))
}

#
# Plot the diagnoses that led to a patient being treated with a 
# given drug.
#
diagnoses_for_drug <- function(drug, freq=2){
  drug <- deparse(substitute(drug))
  antimicrobials <- read_table(antimicrobial)
  episode.ids <- antimicrobials[antimicrobials$Drug == drug,]$Episode
  diagnoses <- read_table(primary_diagnosis)

  diagnoses.for.episodes <- diagnoses[diagnoses$Episode %in% episode.ids,]
  
  conditions <- as.data.frame(table(diagnoses.for.episodes$Condition))
  
  names(conditions) <- c("Condition", "Frequency")
  
  conditions <- conditions[conditions$Frequency > freq,]
  conditions <- conditions[conditions$Condition != "",]
  
  bar_plot(conditions, aes(x=Condition, y=Frequency, fill=Condition), sprintf("%s%s", "Diagnoses treated with ", drug)) 
}

#
# Plot the drugs used to treat patients with a given diagnosis
#
drugs_for_diagnosis <- function(diagnosis, freq=2){
  diagnosis <- deparse(substitute(diagnosis))
  diagnoses <- read_table(primary_diagnosis)
  episode.ids <- diagnoses[diagnoses$Condition == diagnosis,]$Episode
  antimicrobials <- read_table(antimicrobial)
  drugs.for.diagnosis <- antimicrobials[antimicrobials$Episode %in% episode.ids,]
  drugs <- as.data.frame(table(drugs.for.diagnosis$Drug))
  names(drugs) <- c("Drug", "Frequency")
  drugs <- drugs[drugs$Frequency > freq,]
  drugs <- drugs[drugs$Drug != "",]
  title = sprintf("%s %s", "Drugs used to treat patients with", diagnosis)
  bar_plot(drugs, aes(x=Drug, y=Frequency, fill=Drug), title)
}

#
# Plot length of stay
#
los <- function(episodes){
  episodes$los <- as.Date(episodes$End, "%Y-%m-%d") - as.Date(episodes$Start, "%Y-%m-%d")
  return(episodes)
}

plot_los <- function(los, title="Length of stay"){
  ggplot(los, aes(x=LOS, y=Frequency, fill=LOS)) + 
    geom_bar(stat="identity") + 
    labs(title=title, x="Days") + 
    guides(fill=FALSE) +
    scale_x_discrete(breaks=c(5, 10, 20, 30, 40, 60))
}

length_of_stay <- function(length=0, title="Length of stay"){  
  episodes <- read_table(episode)
  episodes <- los(episodes)
  episodes <- episodes[episodes$los > length,]
  los <- as.data.frame(table(na.omit(episodes[episodes$los >= 0,])$los))
  names(los) <- c("LOS", "Frequency")  
  plot_los(los, title=title)
}

length_of_stay_for_primary_diagnosis <- function(condition){
  condition <- deparse(substitute(condition))
  episode.ids <- episode_ids_for_pirmary_diagnosis(condition)    
  episodes <- read_table(episode)
  our.episodes <- subset(episodes, ID %in% episode.ids)
  episodes.with.los <- los(our.episodes)
  our.los <- as.data.frame(table(na.omit(episodes.with.los$los)))
  names(our.los) <- c("LOS", "Frequency")    
  plot_los(our.los, title=sprintf("Length of stay: Patients with %s", condition))
}

length_of_stay_by_primary_diagnosis <- function(max=500, min_cases=10){
  conditions <- read_table(primary_diagnosis)
  conditions.above.min <- as.data.frame(table(conditions$Condition))
  names(conditions.above.min) <- c("Condition", "Freq")
  conditions.above.min <- conditions.above.min[conditions.above.min$Freq > min_cases,]

  episodes <- read_table(episode)
  episodes <- los(episodes)
  
  episodes <- episodes[episodes$los > -1,]
  episodes$los <- episodes$los +1
  episodes <- episodes[episodes$los < max,]
  
  episodes.with.diagnosis <- merge(conditions, episodes, by.x="Episode", by.y="ID")
  
  episodes.with.diagnosis <- data.frame(
    condition=episodes.with.diagnosis$Condition,
    los=episodes.with.diagnosis$los
  )
  episodes.with.diagnosis$los <- as.integer(episodes.with.diagnosis$los)
  episodes.with.diagnosis <- episodes.with.diagnosis[episodes.with.diagnosis$condition != "",]
  #View(episodes.with.diagnosis)
  #View(conditions.above.min)
  diagnoses <- conditions.above.min$Condition
  episodes <- episodes.with.diagnosis[episodes.with.diagnosis$condition %in% diagnoses,]
  #View(episodes)
  #episodes.with.diagnosis <- episodes.with.diagnosis[episodes.with.diagnosis$condtition %in% as.vector(conditions.above.min$Condition),]
  #View(episodes.with.diagnosis)
  ggplot(episodes, aes(x=condition, y=los)) + 
    geom_boxplot(outlier.size=2) + 
    stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
    #scale_x_discrete(labels=consultant.counts$ticks) +
    ggtitle("Length of stay by diagnoisis") +
    #xlab("Consultant namne (episode count)") +
    #2ylab("Length of stay in days")+
    coord_flip() 
  
}

length_of_stay_by_consultant <- function(max=500){
  consultants <- read_table(consultant_at_discharge)
  consultants <- consultants[consultants$Consultant != "",]
  episodes <- read_table(episode)
  episodes <- los(episodes)
  
  episodes <- episodes[episodes$los > -1,]
  episodes$los <- episodes$los +1
  episodes <- episodes[episodes$los < max,]
  #episodes <- episodes[na.omit(episodes$los),]
  episodes.with.consultant <- merge(consultants, episodes, by.x="Episode", by.y="ID")
  episodes.with.consultant <- data.frame(
    consultant=episodes.with.consultant$Consultant, 
    los=episodes.with.consultant$los
    )
  write.csv(episodes.with.consultant, "~/consultant.los.csv")

  episodes.with.consultant$los <- as.integer(episodes.with.consultant$los)
  
  consultant.counts <- as.data.frame(table(episodes.with.consultant$consultant))
  names(consultant.counts) <- c("consultant", "freq")
  consultant.counts <- consultant.counts[consultant.counts$consultant != "", ]
  consultant.counts$ticks <- sprintf("%s (%s)", consultant.counts$consultant, consultant.counts$freq)
  
  ggplot(episodes.with.consultant, aes(x=consultant, y=los)) + 
    geom_boxplot(outlier.size=2) + 
    stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
    scale_x_discrete(labels=consultant.counts$ticks) +
    ggtitle("Length of stay by consultant. Inpatients 12/6/16-17.\nZoomed plot ignoring LOS >75") +
    xlab("Consultant namne (episode count)") +
    ylab("Length of stay in days")+
    coord_flip()
  
  #return(episodes.with.consultant)
}

length_of_stay_for_category <- function(what, length=0){
  what <- deparse(substitute(what))
  episodes <- read_table(episode)
  episodes <- los(episodes)
  
  episodes <- episodes[episodes$los > length,]
  episodes <- episodes[episodes$Category.Name == what, ]
  
  los <- as.data.frame(table(na.omit(episodes$los)))
  names(los) <- c("LOS", "Frequency")  
  plot_los(los, title=sprintf("%s Length of Episode", what))  
}

start_by_month <- function(){
  episodes <- read_table(episode)
  episodes$month <- sapply(
    episodes$Start, 
    function(x) sprintf("%s-01", substr(x, 1, 7))
    )
  start.months <- as.data.frame(table(episodes$month))
  names(start.months)  <- c("Month", "Frequency")
  start.months <- start.months[start.months$Month != "-01",]  
  bar_plot(start.months, aes(x=Month, y=Frequency, fill=Month), 
           "Episodes by month")
}

#
# OPAT data
#
abx_with_opat_variables <- function(){
  abx <- read_table(antimicrobial)
  abx <- abx[abx$End.Date != "",]
  abx <- abx[abx$Start.Date != "",]
  abx <- abx[abx$Delivered.By != "",]
  delivery.routes <- c(
    "UCLH@Home (Non-NHS Community Nurse)",
    "OPAT Clinic",
    "District Nurse (NHS Community Nurse)",
    "Self"
    )
  abx <- abx[abx$Delivered.By %in% delivery.routes,]
  abx$days <- as.Date(abx$End.Date, "%Y-%m-%d") - as.Date(abx$Start.Date, "%Y-%m-%d")
  abx <- abx[abx$days > -1,]
  abx$days <- abx$days +1
  return(abx)
}

drug_days_by_deliverer <- function(){
  abx <- abx_with_opat_variables()
  delivery.routes <- c(
    "UCLH@Home (Non-NHS Community Nurse)",
    "OPAT Clinic",
    "District Nurse (NHS Community Nurse)",
    "Self"
  )
  days <- c(
    sum(abx[abx$Delivered.By == "UCLH@Home (Non-NHS Community Nurse)",]$days),
    sum(abx[abx$Delivered.By == "OPAT Clinic",]$days),
    sum(abx[abx$Delivered.By == "District Nurse (NHS Community Nurse)",]$days),
    sum(abx[abx$Delivered.By == "Self",]$days)
  )
  days.by.deliverer <- data.frame(delivery.routes, days)
  names(days.by.deliverer) <- c("Delivery.Route", "Drug.Days")
  bar_plot(days.by.deliverer,
         aes(y=Drug.Days, x=Delivery.Route, fill=Delivery.Route), 
           "Drug Days by Delivery Route")
}


#  drug <- deparse(substitute(drug))
#  treatment <- read_table(antimicrobial)
#  treatment <- treatment[treatment$End.Date != "None", ]
#  treatment <- treatment[treatment$Start.Date != "None", ]
#  treatment <- treatment[treatment$Drug == drug,]
#  treatment$days <- as.Date(treatment$End.Date, "%Y-%m-%d") - as.Date(treatment$Start.Date, "%Y-%m-%d")
#  treatment <- treatment[treatment$days > -1,]
#  treatment$days <- treatment$days +1
#  lengths <- as.data.frame(table(treatment$days))
#  names(lengths) <- c("Days", "Freq")
#  bar_plot(lengths, aes(x=Days, y=Freq, fill=Days), sprintf("Days treated with %s", drug))



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

advice_given_by <- function(freq=10){
  advice <- read_table(microbiology_input)
  given.by <- as.data.frame(table(advice$Advice.given.by))
  names(given.by) <- c("Person", "Frequency")
  given.by <- given.by[given.by$Frequency > freq,]
  bar_plot(given.by, aes(x=Person, y=Frequency, fill=Person), "Who gives advice?")
}

reasons_for_advice <- function(title="Reasons for Microbiology clinical advice", filename=""){
  advice <- read_table(microbiology_input)
  advice <- advice[advice$Reason.For.Interaction != "",]
  reasons <- as.data.frame((table(advice$Reason.For.Interaction)))
  names(reasons) <- c("Reason", "Frequency")
  bar_plot(reasons, aes(x=Reason, y=Frequency, fill=Reason), 
           title, filename=filename)
}

advice_per_patient <- function(){
  advice <- read_table(microbiology_input)
  patient.advice <- as.data.frame(table(
    as.data.frame(table(advice$Patient))$Freq
  ))
  names(patient.advice) <- c("Advice", "Frequency")
  bar_plot(patient.advice, 
           aes(x=Advice, y=Frequency, fill=Advice),
           "Clinical advice entries per patient")
}