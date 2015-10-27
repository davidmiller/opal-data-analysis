A collection of useful materials for dealing with OPAL data extracts

### How to analyse the data (R edition)

(We assume you're using RStudio.)

Source the `opal.R` file

Use the console window to set the directory in which the extract you're currently exploring lives.

    OPALDATA <<- "~/Downloads/ohc.2015-10-26/"

You can now start using the various utility functions to explore the data - for example:

    view_table(demographics)
    age_distribution()
    common_diagnoses()
    common_tests()
    
