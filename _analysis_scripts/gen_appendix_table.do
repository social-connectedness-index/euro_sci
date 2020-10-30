* Purpose: Generate a tex version of the train data availability table
* Inputs: _intermediate_data/passenger_train_dat/appendix_table dat.csv
* Outputs: _output/appendix_train.tex
* Date: 2020-10-29

import delimited "_intermediate_data/passenger_train_dat/appendix_table dat.csv", clear

rename geo reporter

dataout, save("_output/appendix_train.tex") tex replace
