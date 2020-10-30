* Purpose: This driver file is a simple interface to run
* all of the Stata files in _analysis_scripts

* Date: 2020-10-29

clear
set more off

// ssc install mmerge
// ssc install estout
// ssc install reghdfe
// ssc install ppmlhdfe
// ssc install dataout

// Generate the primary NUTS2 to NUTS2 regression results
do "_analysis_scripts/nuts2_to_nuts2_regress.do"

clear

// Generate the passenger rail regression results
do "_analysis_scripts/passenger_train_regress.do"

clear

// Generate the anti-EU sentiment regression results
do "_analysis_scripts/anti_eu_regress.do"

clear

// Generate a tex version of the train data availability table
do "_analysis_scripts/gen_appendix_table.do"

clear
