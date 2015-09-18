require(ggplot2)
require(plyr)

#source('opal.R')

#
# Return a data frame with the number of ABX days per drug
#
abx_days_by_drug <- function(extract_dir){
  abx <- read.csv(sprintf("%s%s", extract_dir, "antimicrobial.csv"), strip.white=TRUE)
  # Kill rows we don't want
  abx <- abx[abx$delivered_by != "Inpatient Team",]
  abx <- abx[abx$delivered_by != "",]
  abx <- abx[abx$start_date != "None",]
  abx <- abx[abx$end_date != "None",]
  
  abx$duration <- as.Date(abx$end_date) - as.Date(abx$start_date)
  total.days <- ddply(abx, .(drug), summarize, total=sum(duration))
  return (total.days)
}