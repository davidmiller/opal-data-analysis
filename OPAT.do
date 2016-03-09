** The first three manipulations all use the antimicrobial.csv but for the purposes of ease I import the dataset afresh each time.  **
** The following two manipulation use the line.csv but again I import the dataset afresh each time **

**Manipulation 1: Antibiotic Days for each Drug **

** Import Data and drop non-OPAT drugs **
clear
import delimited "antimicrobial.csv",varname(1) bindquotes(strict)
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
local c_date = c(current_date)
local c_time = c(current_time)
local c_time_date = "`c_date'"+"_" +"`c_time'"
local time_string = subinstr("`c_time_date'", ":", "_", .)
local time_string = subinstr("`time_string'", " ", "_", .)
export delimited "antibiotic_days_per_drug_`time_string'.csv"

** The data table now lists each drug and the total number of days it was prescribed across the whole OPAT dataset **

** Uncollapse the dataset **
restore

**Manipulation 2: Work out who was administering all the drugs for each person **

** We are using the antimicrobial dataset **
clear
import delimited "antimicrobial.csv",varname(1) bindquotes(strict)

** Drop drugs prescribed by inpatient team / where the delivered by field is blank - these are thought to also be inpatient prescriptions **
drop if delivered_by=="Inpatient Team"
drop if delivered_by==""
drop if route=="PO"

** Clean the delivered by data - this section needs reviewing based on a tab delivered_by. This will be fixed when Delivered_by becomes a dropdown not a lookup list **
drop if delivered_by=="in patient"
replace delivered_by="Self" if delivered_by=="self"
replace delivered_by ="Carer" if delivered_by=="Carer / DN"

** Data is currently long. We therefore look at each entry and generate a score of 1 for each category based on each individual prescription **
gen carer =0
replace carer =1 if strpos(delivered_by,"Carer")
replace carer =1 if strpos(delivered_by,"Family / Carer")
gen DN =0
replace DN =1 if strpos(delivered_by,"District Nurse")
gen GP =0
replace GP =1 if strpos(delivered_by,"GP")
replace GP =1 if strpos(delivered_by,"General Practioner")
gen OPAT =0
replace OPAT =1 if strpos(delivered_by,"OPAT Clinic")
gen Self =0
replace Self =1 if strpos(delivered_by,"Self")
gen UCLHatHome =0
replace UCLHatHome =1 if strpos(delivered_by,"UCLH@Home")
gen local_hospital =0
replace local_hospital =1 if strpos(delivered_by,"Local Hospital Day Unit")


** We collapse the data across episode_id. This gives a score of 1 or 0 for each patient for each of the different ways they could have received drugs - e.g 1 if any of the prescriptions were delivered by a district nurse - Note the Preserve/Collapse step**

preserve
collapse (max) carer DN GP OPAT Self UCLHatHome local_hospital,by(episode_id)

** Summate the different ways a person can receive drugs giving a score for total of number of different ways they received drugs **
gen numberofways = carer + DN + GP + OPAT + Self + UCLHatHome + local_hospital
local c_date = c(current_date)
local c_time = c(current_time)
local c_time_date = "`c_date'"+"_" +"`c_time'"
local time_string = subinstr("`c_time_date'", ":", "_", .)
local time_string = subinstr("`time_string'", " ", "_", .)
export delimited "individual_patient_antibiotic_delivered_by_`time_string'.csv

collapse (sum) carer (sum) DN (sum) GP (sum) OPAT (sum) Self (sum) UCLHatHome (sum) local_hospital
export delimited "summary_antibiotic_delivered_by_`time_string'.csv

** Uncollapse the dataset **
restore

**Manipulation 3: Work out how long patients received IV Abx via the OPAT service **

** Import data **
clear
import delimited "antimicrobial.csv",varname(1) bindquotes(strict)
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
gen duration = end-start
bysort episode_id: egen opat_duration = sum(duration)

** Collapse data across episode_id keeping the maimum opat duration value. NOTE THE PRESERVE STEP**
preserve
collapse (max)opat_duration, by(episode_id)
collapse (p50) median=opat_duration (mean) mean=opat_duration (iqr) iqr=opat_duration (min) minimum=opat_duration (max) maximum=opat_duration

** Get summary statistics **
local c_date = c(current_date)
local c_time = c(current_time)
local c_time_date = "`c_date'"+"_" +"`c_time'"
local time_string = subinstr("`c_time_date'", ":", "_", .)
local time_string = subinstr("`time_string'", " ", "_", .)
export delimited "duration_opat_`time_string'.csv"

** Uncollapse the dataset **
restore

**Manipulation 4: Work out which lines people have used **

** Import data**
clear
import delimited "line.csv",varname(1) bindquotes(strict)

** Generate a 0/1 column for each type of catheter. This currently includes a few extra lines because the data is not perfectly clean. In the long term this will be fixed by making line type a drop down not a look-up list **

gen hickman = 0
replace hickman =1 if strpos(line_type,"Hickman")
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
collapse (max) hickman leaderflex midline PICC Peripheral Portacath,by(episode_id)

** Work out how many different types of line each person used **
gen numberofways = hickman + leaderflex + midline + PICC + Peripheral + Portacath
local c_date = c(current_date)
local c_time = c(current_time)
local c_time_date = "`c_date'"+"_" +"`c_time'"
local time_string = subinstr("`c_time_date'", ":", "_", .)
local time_string = subinstr("`time_string'", " ", "_", .)
export delimited "line_types_used_per_person_`time_string'.csv"

collapse (sum) hickman (sum) leaderflex (sum) midline (sum) PICC (sum) Peripheral (sum) Portacath
export delimited "summary_types_of_line_`time_string'.csv

** Uncollapse the dataset **
restore

**Manipulation 5: Work out how long each different line type was used on average across the dataset **

** Import Data **
clear
import delimited "line.csv",varname(1) bindquotes(strict)
tab line_type

** This will be improved once we move line_type to==dropdown **
**Tidy up Line-Type because of Free-Text**
replace line_type ="Leaderflex" if strpos(line_type,"Leader")
replace line_type ="Leaderflex" if strpos(line_type,"leder")
replace line_type ="." if strpos(line_type,"removed")

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
collapse (p50) median=line_duration (mean) mean=line_duration (iqr) iqr=line_duration  (min) minimum=line_duration (max) maximum=line_duration, by(line_type)

** Data table now shows the summary statistics for line duration for each line type **
local c_date = c(current_date)
local c_time = c(current_time)
local c_time_date = "`c_date'"+"_" +"`c_time'"
local time_string = subinstr("`c_time_date'", ":", "_", .)
local time_string = subinstr("`time_string'", " ", "_", .)
export delimited "average_duration_line_type_`time_string'.csv"

** Uncollapse the dataset **
restore

**Where do our patients come from**
