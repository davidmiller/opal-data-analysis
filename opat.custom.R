# Set Data Directory and import libraries / OPAL functions
OPALDATA <- "~/src/ohc/dev/src/opatdata/6m/"
source('~/opal-data-analysis/opal.R')
library(dplyr)

# Read data
location <- read_table(location)
drugs    <- read_table(antimicrobial)

# Clean Data
iv.drugs <- drugs[drugs$route=="IV",]
opat.ivs <- iv.drugs[iv.drugs$delivered_by!="Inpatient Team",]
opat.ivs <- opat.ivs[opat.ivs$delivered_by!="",]
sorted.opat <- arrange(opat.ivs, start_date)
first.opat.drugs <- sorted.opat[!duplicated(sorted.opat$episode_id),]
first.opat.drugs$start <- as.Date(first.opat.drugs$start_date, format = "%Y-%m-%d")
opat.drugs <- first.opat.drugs[,c("episode_id", "start", "delivered_by", "drug")]

location$referral <- as.Date(location$opat_referral, format="%Y-%m-%d")
location$acceptance <- as.Date(location$opat_acceptance, format="%Y-%m-%d")
opat.dates <- location[,c("episode_id", "referral", "acceptance", "opat_referral_team")]

# Merge data
opat <- merge(opat.drugs, opat.dates)

# Calculate differences
opat$days.to.accept <- as.numeric(opat$acceptance - opat$referral)
opat$days.to.drug <- as.numeric((opat$start - opat$referral))

opat.minus <- opat[opat$days.to.drug < 0,]
opat.wait <- opat[opat$days.to.drug > 0,]

opat.wait.by.team <- opat.wait[,c("opat_referral_team", "days.to.drug")]
means.by.team <- aggregate(opat.wait.by.team, list(opat.wait.by.team$opat_referral_team), mean)
