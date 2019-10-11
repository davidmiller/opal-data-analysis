###
# R Script to generate Department of Infection summary reports
###

###
# Globals and dependencies
###
setwd("~/opal-data-analysis/")
OPALDATA <- "~/src/ohc/data/antifungal/"
TEAM_DISPLAY <- 'Antifungal list 1/10/18-30/9/19'
source("opal.R")

###
# Utility Functions
###
p <- function(output){
  cat(sprintf("%s\n", output))
}

###
# Data Processing begins
###
demographics <- read_table(demographics)
number.of.patients <- length(demographics$Patient)

culture.sets <- read_table(blood_culture_set)
number.of.lab_numbers <- length(unique(culture.sets$Lab.Number))

primary.diagnoses <- read_table(primary_diagnosis)
primary.diagnoses.entered <- primary.diagnoses[primary.diagnoses$Updated.By != "None",]

issues <- read_table(diagnosis)
number.of.issues <- length(issues$Patient)

clinical.advice <- read_table(microbiology_input)
number.of.advice.entries <- length(clinical.advice$Patient)
number.of.discussions <- length(clinical.advice[clinical.advice$Clinical.Discussion != "",]$Patient)
number.of.infection.control <- length(clinical.advice[clinical.advice$Infection.Control != "",]$Patient)
number.of.plans <- length(clinical.advice[clinical.advice$Agreed.Plan != "",]$Patient)

users <- read.csv("~/src/ohc/data/rfh.users.csv")
users$name <- sprintf("%s %s", users$first_name, users$last_name)
with.users <- merge.data.frame(clinical.advice, users, by.x="Created.By", by.y="id")
clinical.advice.breakdown <- as.data.frame(table(with.users$name))
names(clinical.advice.breakdown) <- c("Name", "Frequency")

###
# Begin outputing summary
###
plot_age_distribution(
  demographics,
  title=sprintf("Age Distribution: %s", TEAM_DISPLAY),
  filename=sprintf("%splots/age_distribution.png", OPALDATA)
  )
common_primary_diagnoses(
  title=sprintf("Common Primary Diagnoses: %s", TEAM_DISPLAY),
  filename=sprintf("%splots/common_primary_diagnoses.png", OPALDATA)
  )
reasons_for_advice(
  title=sprintf("Reasons for interaction: %s", TEAM_DISPLAY),
  filename=sprintf("%splots/reasons_for_interaction.png", OPALDATA)
)
bar_plot(
  as.data.frame(clinical.advice.breakdown), 
  aes(x=Name, y=Frequency, fill=Name), 
  sprintf("Number of Clinical Advice entries: %s", TEAM_DISPLAY),
  filename=sprintf("%splots/clinical.advice.breakdown.png", OPALDATA)
  )

p(sprintf("%s patients", number.of.patients))
p(sprintf("%s blood cultures recorded", number.of.lab_numbers))
p(sprintf("%s primary diagnoses", length(primary.diagnoses.entered$Patient)))
p(sprintf("%s issues", number.of.issues))
p(sprintf("%s clinical advice entries", number.of.advice.entries))
p(sprintf("%s clinical discussions", number.of.discussions))
p(sprintf("%s infection control entries", number.of.infection.control))
p(sprintf("%s plans", number.of.plans))
