** The first three manipulations all use the antimicrobial.csv but for the purposes of ease I import the dataset afresh each time.  **
** The following two manipulation use the line.csv but again I import the dataset afresh each time **

**Manipulation 1: Antibiotic Days for each Drug **

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
export delimited "antibiotic_days_per_drug.csv"

** The data table now lists each drug and the total number of days it was prescribed across the whole OPAT dataset **

** Uncollapse the dataset **
restore

**Manipulation 2: Work out who was administering all the drugs for each person **

** We are using the antimicrobial dataset **
clear
import delimited "antimicrobial.csv"

** Drop drugs prescribed by inpatient team / where the delivered by field is blank - these are thought to also be inpatient prescriptions **
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""

** Clean the delivered by data - this section needs reviewing based on a tab delivered_by. This will be fixed when Delivered_by becomes a dropdown not a lookup list **
drop if delivered_by=="in patient"
replace delivered_by="Self" if delivered_by=="self"
replace delivered_by ="Carer" if delivered_by=="Carer / DN"

** Data is currently long. We therefore look at each entry and generate a score of 1 for each category based on each individual prescription ** 
gen carer =0
replace carer =1 if strpos(delivered_by,"Carer")
gen DN =0
replace DN =1 if strpos(delivered_by,"District Nurse")
gen GP =0
replace GP =1 if strpos(delivered_by,"GP")
gen OPAT =0
replace OPAT =1 if strpos(delivered_by,"OPAT Clinic")
gen Self =0
replace Self =1 if strpos(delivered_by,"Self")
gen UCLHatHome =0
replace UCLHatHome =1 if strpos(delivered_by,"UCLH@Home")

** We collapse the data across episode_id. This gives a score of 1 or 0 for each patient for each of the different ways they could have received drugs - e.g 1 if any of the prescriptions were delivered by a district nurse - Note the Preserve/Collapse step**

preserve
collapse (max) carer DN GP OPAT Self UCLHatHome,by(episode_id)

** Summate the different ways a person can receive drugs giving a score for total of number of different ways they received drugs **
gen numberofways = carer + DN + GP + OPAT + Self + UCLHatHome
export delimited "antibiotic_delivered_by.csv"

**Manipulation 3: Work out how long patients received IV Abx via the OPAT service **

** Import data **
import delimited "antimicrobial.csv"
** Remove non OPAT drugs **
** Records where delivered by is blank are thought to be drugs imported from inpatient records where the route of administration isn't recorded **
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""
** Drop drugs where route of administration == Oral **
drop if route=="Oral"
drop if route=="PO"

** convert the date strings in to dates **
gen start = date(start_date,"YMD")
gen end = date(end_date,"YMD")

** Collapse data across episode_id keeping the earliest start date and the latest finish date. NOTE PRESERVE STEP **
preserve
collapse (min)start (max)end,by(episode_id)

** Work out overall duration of non-Inpatient IV **
gen duration = end-start

** Get summary statistics **
sum duration,detail
export delimited "duration_opat.csv"

** Uncollapse the dataset **
restore


**Manipulation 4: Work out which lines people have used **

** Import data** 
clear 
import delimited "line.csv",varname(1)

** Generate a 0/1 column for each type of catheter. This currently includes a few extra lines because the data is not perfectly clean. In the long term this will be fixed by making line type a drop down not a look-up list **

gen leaderflex = 0 
replace leaderflex =1 if strpos(line_type,"Leader")
replace leaderflex =1 if strpos(line_type,"leder")
gen midline = 0 
replace midline =1 if strpos(line_type,"Midline")
gen PICC = 0 
replace PICC =1 if strpos(line_type,"PICC")
gen Peripheral = 0 
replace Peripheral =1 if strpos(line_type,"Peripheral")
gen Portacath = 0 
replace Portacath =1 if strpos(line_type,"Portacath")

** We are going to collapse the data by episode. This will give a score of 1 for each type of line the person used at any point across the episode. Note the collapse step therefore preserve is recommended. **
preserve
collapse (max) leaderflex midline PICC Peripheral Portacath,by(episode_id)

** Work out how many different types of line each person used **
gen numberofways = leaderflex + midline + PICC + Peripheral + Portacath
export delimited "line_types_used_per_person.csv"

**Manipulation 5: Work out how long each different line type was used on average across the dataset **

** Import Data **
import delimited "line.csv"
tab line_type
** This will be improved once we move line_type to==dropdown **

** Convert Date-Time to a STATA Date by extracting the data and then converting**
gen inserted_on = substr(insertion_datetime,1,10)
replace inserted_on = "." if inserted_on=="None"
gen inserted_date = date(inserted_on,"YMD")
gen removed_on = substr( removal_datetime,1,10)
replace removed_on = "." if removed_on=="None"
gen removed_date = date(removed_on,"YMD")
gen line_duration = removed_date - inserted_date

** Summarise the data by line type - NOTE PRESERVE STEP**
preserve
collapse (iqr) iqr=line_duration (p50) median=line_duration (min) min=line_duration (max) max=line_duration (mean) average=line_duration,by(line_type)

** Data table now shows the summary statistics for line duration for each line type **
export delimited "average_duration_line_type.csv"

** Restore Dataset **
restore

