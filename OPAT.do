** Import Data and drop non-OPAT drugs **
import delimited "antimicrobial.csv"
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""

** Generate days per prescription **
gen start = date(start_date,"YMD")
gen end = date(end_date,"YMD")
gen duration = end-start

** Summate all the durations by drug **
bysort drug: egen totaldays = sum(duration)

** Collapse the data to give the summary for each drug - NOTE PRESERVE STEP**
preserve
collapse (max)totaldays, by(drug)

** The data table now lists each drug and the total number of days it was prescribed across the whole OPAT dataset **

** Uncollapse the dataset **
restore
