* Purpose: Generate the passenger rail regression results
* Inputs: _intermediate_data/final_regress_dat/passenger_rail_regress_dat
* Outputs: _output/passenger_train.tex
* Date: 2020-10-29

use "_intermediate_data/final_regress_dat/passenger_rail_regress_dat", replace

mmerge user_loc fr_loc using "_intermediate_data/final_regress_dat/nuts2_to_nuts2_regress_dat"

egen user_loc_group       = group(user_loc)
egen friend_loc_group     = group(fr_loc)
egen ctry_pair_group	  = group(ctry_pair)

eststo clear

eststo panel1: ppmlhdfe pass_rail_most_recent log_sci, absorb(user_loc_group friend_loc_group) vce(cluster user_loc_group friend_loc_group)
estadd local Nuts2_FE = "Y"
estadd local cntry_pair_FE = "N"
estadd local table_1_controls = "N"
eststo panel2: ppmlhdfe pass_rail_most_recent log_sci log_distance, absorb(user_loc_group friend_loc_group) vce(cluster user_loc_group friend_loc_group)
estadd local Nuts2_FE = "Y"
estadd local cntry_pair_FE = "N"
estadd local table_1_controls = "N"
eststo panel3: ppmlhdfe pass_rail_most_recent log_sci log_distance log_rail_time log_drive_time, absorb(user_loc_group friend_loc_group) vce(cluster user_loc_group friend_loc_group)
estadd local Nuts2_FE = "Y"
estadd local cntry_pair_FE = "N"
estadd local table_1_controls = "N"
eststo panel4: ppmlhdfe pass_rail_most_recent log_sci log_distance log_rail_time log_drive_time, absorb(user_loc_group friend_loc_group ctry_pair_group) vce(cluster user_loc_group friend_loc_group)
estadd local Nuts2_FE = "Y"
estadd local cntry_pair_FE = "Y"
estadd local table_1_controls = "N"
eststo panel5: ppmlhdfe pass_rail_most_recent log_sci log_distance log_rail_time log_drive_time diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, absorb(user_loc_group friend_loc_group ctry_pair_group) vce(cluster user_loc_group friend_loc_group)
estadd local Nuts2_FE = "Y"
estadd local cntry_pair_FE = "Y"
estadd local table_1_controls = "Y"

eststo panel6: ppmlhdfe pass_rail_2015 log_sci log_distance log_rail_time log_drive_time diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, absorb(user_loc_group friend_loc_group ctry_pair_group) vce(cluster user_loc_group friend_loc_group)
estadd local Nuts2_FE = "Y"
estadd local cntry_pair_FE = "Y"
estadd local table_1_controls = "Y"
eststo panel7: ppmlhdfe pass_rail_2010 log_sci log_distance log_rail_time log_drive_time diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, absorb(user_loc_group friend_loc_group ctry_pair_group) vce(cluster user_loc_group friend_loc_group)
estadd local Nuts2_FE = "Y"
estadd local cntry_pair_FE = "Y"
estadd local table_1_controls = "Y"
eststo panel8: ppmlhdfe pass_rail_2005 log_sci log_distance log_rail_time log_drive_time diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, absorb(user_loc_group friend_loc_group ctry_pair_group) vce(cluster user_loc_group friend_loc_group)
estadd local Nuts2_FE = "Y"
estadd local cntry_pair_FE = "Y"
estadd local table_1_controls = "Y"

esttab, se pr2

esttab, ///
	drop(_cons diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate 0.same_rlgdnm_wo_none 1.same_rlgdnm_wo_none 0.same_main_language 1.same_main_language industry_sim) ///
	varlabels(log_sci "log(SocialConnectedness)" log_distance "log(Distance in KM)" log_rail_time "log(Rail Time in Mins)" log_drive_time "log(Drive Time in Mins)")  ///
	mlabels("Most recent" "Most recent" "Most recent" "Most recent" "Most recent" "2015" "2010" "2005") ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("Nuts2_FE NUTS2 FEs" "cntry_pair_FE All Country Pair FEs" "table_1_controls Table 1 Controls" "N Number of Observations" "r2_p pseudo-\$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	collabels(none) ///
	mgroups("Dependent Variable: log(Passenger Rail Trips)", pattern(1 0 0 0 0 0 0 0) prefix(\multicolumn{8}{c}{)suffix(}))
	
// log close

esttab using "./_output/passenger_train.tex" , replace ///
	drop(_cons diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate 0.same_rlgdnm_wo_none 1.same_rlgdnm_wo_none 0.same_main_language 1.same_main_language industry_sim) ///
	varlabels(log_sci "log(SocialConnectedness)" log_distance "log(Distance in KM)" log_rail_time "log(Rail Time in Mins)" log_drive_time "log(Drive Time in Mins)")  ///
	mlabels("Most recent" "Most recent" "Most recent" "Most recent" "Most recent" "2015" "2010" "2005") ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("Nuts2_FE NUTS2 FEs" "cntry_pair_FE All Country Pair FEs" "table_1_controls Table 1 Controls" "N Number of Observations" "r2_p pseudo-\$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	collabels(none) ///
	mgroups("Dependent Variable: log(Passenger Rail Trips)", pattern(1 0 0 0 0 0 0 0) prefix(\multicolumn{8}{c}{)suffix(}))
	
cor log_distance log_rail_time log_drive_time

