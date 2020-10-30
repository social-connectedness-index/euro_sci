* Purpose: Generate the primary NUTS2 to NUTS2 regression results
* Inputs: _intermediate_data/final_regress_dat/nuts2_to_nuts2_regress_dat
* Outputs: _output/nuts2_to_nuts2_1.tex
* 		   _output/nuts2_to_nuts2_2.tex
* Date: 2020-10-29

use "_intermediate_data/final_regress_dat/nuts2_to_nuts2_regress_dat", clear

egen user_loc_group       	= group(user_loc)
egen friend_loc_group     	= group(fr_loc)
egen ctry_pair_group		= group(ctry_pair)
egen both_country_group		= group(both_country)

* Create non-linear distance controls
local group = 100
egen dist_group = cut(log_distance), group(`group')


/// Generate NUTS2 to NUTS2 Regress Table 1

eststo clear

eststo panel1: reghdfe log_sci log_distance, ///
	absorb(user_loc_group friend_loc_group) vce(cluster user_loc_group friend_loc_group) keepsingletons
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "N"
estadd local cntry_pair_FE = "N"
estadd local smpl = ""

eststo panel2: reghdfe log_sci log_distance i.country_same, ///
	absorb(user_loc_group friend_loc_group) vce(cluster user_loc_group friend_loc_group) keepsingletons
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "N"
estadd local cntry_pair_FE = "N"
estadd local smpl = ""

eststo panel3: reghdfe log_sci log_distance i.country_same i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group) vce(cluster user_loc_group friend_loc_group) keepsingletons
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "N"
estadd local cntry_pair_FE = "N"
estadd local smpl = ""

eststo panel4: reghdfe log_sci log_distance i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group) vce(cluster user_loc_group friend_loc_group) keepsingletons
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"
estadd local smpl = ""

eststo panel5: reghdfe log_sci log_distance diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group ctry_pair_group) vce(cluster user_loc_group friend_loc_group) keepsingletons
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "N"
estadd local cntry_pair_FE = "Y"
estadd local smpl = ""

eststo panel6: reghdfe log_sci log_distance diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim ///
	if country_same, ///
	absorb(user_loc_group friend_loc_group ctry_pair_group) vce(cluster user_loc_group friend_loc_group) keepsingletons
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "N"
estadd local cntry_pair_FE = "Y"
estadd local smpl = "Same country"

eststo panel7: reghdfe log_sci log_distance diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim ///
	if !country_same, ///
	absorb(user_loc_group friend_loc_group ctry_pair_group) vce(cluster user_loc_group friend_loc_group) keepsingletons
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "N"
estadd local cntry_pair_FE = "Y"
estadd local smpl = "Diff country"

esttab, ///
	drop(0.country_same 0.country_border 0.same_rlgdnm_wo_none 0.same_main_language _cons) /// 
	varlabels(log_distance "log(Distance in KM)" 1.country_same "Same Country" 1.country_border "Border Country" diff_share_low_edu "$\Delta$ Share Pop Low Edu (\%)" diff_median_age "$\Delta$ Median Age" diff_income_thous "$\Delta$ Avg Income (k \euro)" diff_unemp_rate "$\Delta$ Unemployment (%)" 1.same_rlgdnm_wo_none "Same Religion" 1.same_main_language "Same Language" industry_sim "Industry Similarity") ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("Nuts2_Region_FE NUTS2 FEs" "indiv_same_country Indiv. Same Country FEs" "cntry_pair_FE All Country Pair FEs" "smpl Sample" "N Number of Observations" "r2 \$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	mlabels(,none) collabels(none) ///
	mgroups("Dependent Variable: log(SCI)", pattern(1 0 0 0 0 0 0) prefix(\multicolumn{7}{c}{)suffix(}))

esttab using "./_output/nuts2_to_nuts2_1.tex" , replace /// 
	drop(0.country_same 0.country_border 0.same_rlgdnm_wo_none 0.same_main_language _cons) /// 
	varlabels(log_distance "log(Distance in KM)" 1.country_same "Same Country" 1.country_border "Border Country" diff_share_low_edu "$\Delta$ Share Pop Low Edu (\%)" diff_median_age "$\Delta$ Median Age" diff_income_thous "$\Delta$ Avg Income (k \euro)" diff_unemp_rate "$\Delta$ Unemployment (\%)" 1.same_rlgdnm_wo_none "Same Religion" 1.same_main_language "Same Language" industry_sim "Industry Similarity") ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("Nuts2_Region_FE NUTS2 FEs" "indiv_same_country Indiv. Same Country FEs" "cntry_pair_FE All Country Pair FEs" "smpl Sample" "N Number of Observations" "r2 \$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	mlabels(,none) collabels(none) ///
	mgroups("Dependent Variable: log(SCI)", pattern(1 0 0 0 0 0 0) prefix(\multicolumn{7}{c}{)suffix(}))


/// Generate NUTS2 to NUTS2 Regress Table 2 (Historical Data)

eststo clear

eststo panel1: reghdfe log_sci i.country_same i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group dist_group) vce(cluster user_loc_group friend_loc_group)
estadd local distance_cntrls = "Y"
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

eststo panel2: reghdfe log_sci i.country_same i.both_czechoslovakia i.both_yugoslavia i.both_west_de i.both_east_de i.both_soviet_union i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group dist_group) vce(cluster user_loc_group friend_loc_group)
estadd local distance_cntrls = "Y"
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

eststo panel3: reghdfe log_sci i.country_same i.both_west_de i.both_east_de i.both_soviet_union i.both_uk_1960 i.both_czechoslovakia i.both_yugoslavia i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group dist_group) vce(cluster user_loc_group friend_loc_group)
estadd local distance_cntrls = "Y"
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

eststo panel4: reghdfe log_sci i.country_same i.both_uk_1960 i.both_de_1930 i.both_west_de i.both_east_de i.both_soviet_union i.both_czechoslovakia i.both_yugoslavia i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group dist_group) vce(cluster user_loc_group friend_loc_group)
estadd local distance_cntrls = "Y"
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

eststo panel5: reghdfe log_sci i.country_same i.both_austro_hung_empire i.both_de_1900 i.both_se_no_1900 i.both_de_1930 i.both_uk_1960 i.both_west_de i.both_east_de i.both_soviet_union i.both_czechoslovakia i.both_yugoslavia i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group dist_group) vce(cluster user_loc_group friend_loc_group)
estadd local distance_cntrls = "Y"
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

esttab, ///
	drop(0.country_same 1.country_same 0.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate 0.same_rlgdnm_wo_none 1.same_rlgdnm_wo_none 0.same_main_language 1.same_main_language industry_sim 0.both_czechoslovakia 0.both_yugoslavia 0.both_west_de 0.both_east_de 0.both_soviet_union 0.both_uk_1960 0.both_de_1930 0.both_de_1900 0.both_austro_hung_empire 0.both_se_no_1900 _cons) /// 
	varlabels(log_distance "log(Distance in KM)" 1.country_border "Border Country" 1.both_czechoslovakia "Both Czechoslovakia" 1.both_yugoslavia "Both Yugoslavia" 1.both_east_de "Both East Germany" 1.both_west_de "Both West Germany" 1.both_soviet_union "Both Soviet Union" 1.both_uk_1960 "Both United Kingdom 1960" 1.both_de_1930 "Both Germany 1930" 1.both_de_1900 "Both German Empire 1900"  1.both_se_no_1900 "Both United Sweden-Norway" 1.both_austro_hung_empire "Both Austro-Hungarian Empire 1900" 1.country_1900_same "Same Country 1900" 1.country_1930_same "Same Country 1930" 1.country_1960_same "Same Country 1960" 1.country_1990_same "Same Country 1990")  ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("distance_cntrls Distance Controls" "demo_cntrls Table 1 Controls" "Nuts2_Region_FE NUTS2 FEs" "indiv_same_country Indiv. Same Country FEs" "N Number of Observations" "r2 \$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	mlabels("" "1990" "1960" "1930" "1900") collabels(none) ///
	mgroups("Dependent Variable: log(SCI)", pattern(1 0 0 0 0) prefix(\multicolumn{5}{c}{)suffix(}))
	
esttab using "./_output/nuts2_to_nuts2_2.tex" , replace /// 
	drop(0.country_same 1.country_same 0.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate 0.same_rlgdnm_wo_none 1.same_rlgdnm_wo_none 0.same_main_language 1.same_main_language industry_sim 0.both_czechoslovakia 0.both_yugoslavia 0.both_west_de 0.both_east_de 0.both_soviet_union 0.both_uk_1960 0.both_de_1930 0.both_de_1900 0.both_austro_hung_empire 0.both_se_no_1900 _cons) /// 
	varlabels(log_distance "log(Distance in KM)" 1.country_border "Border Country" 1.both_czechoslovakia "Both Czechoslovakia" 1.both_yugoslavia "Both Yugoslavia" 1.both_east_de "Both East Germany" 1.both_west_de "Both West Germany" 1.both_soviet_union "Both Soviet Union" 1.both_uk_1960 "Both United Kingdom 1960" 1.both_de_1930 "Both Germany 1930" 1.both_de_1900 "Both German Empire 1900"  1.both_se_no_1900 "Both United Sweden-Norway" 1.both_austro_hung_empire "Both Austro-Hungarian Empire 1900" 1.country_1900_same "Same Country 1900" 1.country_1930_same "Same Country 1930" 1.country_1960_same "Same Country 1960" 1.country_1990_same "Same Country 1990")  ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("distance_cntrls Distance Controls" "demo_cntrls Table 1 Controls" "Nuts2_Region_FE NUTS2 FEs" "indiv_same_country Indiv. Same Country FEs" "N Number of Observations" "r2 \$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	mlabels("" "1990" "1960" "1930" "1900") collabels(none) ///
	mgroups("Dependent Variable: log(SCI)", pattern(1 0 0 0 0) prefix(\multicolumn{5}{c}{)suffix(}))
	
	
////////////////////
	
/// Historical table using log(distance) instead of distance groups

eststo clear

eststo panel1: reghdfe log_sci log_distance i.country_same i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group) vce(cluster user_loc_group friend_loc_group)
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

eststo panel2: reghdfe log_sci log_distance i.country_same i.both_czechoslovakia i.both_yugoslavia i.both_west_de i.both_east_de i.both_soviet_union i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group) vce(cluster user_loc_group friend_loc_group)
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

eststo panel3: reghdfe log_sci log_distance i.country_same i.both_west_de i.both_east_de i.both_soviet_union i.both_uk_1960 i.both_czechoslovakia i.both_yugoslavia i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group) vce(cluster user_loc_group friend_loc_group)
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

eststo panel4: reghdfe log_sci log_distance i.country_same i.both_uk_1960 i.both_de_1930 i.both_west_de i.both_east_de i.both_soviet_union i.both_czechoslovakia i.both_yugoslavia i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group) vce(cluster user_loc_group friend_loc_group)
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

eststo panel5: reghdfe log_sci log_distance i.country_same i.both_austro_hung_empire i.both_de_1900 i.both_se_no_1900 i.both_de_1930 i.both_uk_1960 i.both_west_de i.both_east_de i.both_soviet_union i.both_czechoslovakia i.both_yugoslavia i.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate i.same_rlgdnm_wo_none i.same_main_language industry_sim, ///
	absorb(user_loc_group friend_loc_group both_country_group) vce(cluster user_loc_group friend_loc_group)
estadd local demo_cntrls = "Y"
estadd local Nuts2_Region_FE = "Y"
estadd local indiv_same_country = "Y"
estadd local cntry_pair_FE = "N"

esttab, ///
	drop(0.country_same 1.country_same 0.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate 0.same_rlgdnm_wo_none 1.same_rlgdnm_wo_none 0.same_main_language 1.same_main_language industry_sim 0.both_czechoslovakia 0.both_yugoslavia 0.both_west_de 0.both_east_de 0.both_soviet_union 0.both_uk_1960 0.both_de_1930 0.both_de_1900 0.both_austro_hung_empire 0.both_se_no_1900 _cons) /// 
	varlabels(log_distance "log(Distance in KM)" 1.country_border "Border Country" 1.both_czechoslovakia "Both Czechoslovakia" 1.both_yugoslavia "Both Yugoslavia" 1.both_east_de "Both East Germany" 1.both_west_de "Both West Germany" 1.both_soviet_union "Both Soviet Union" 1.both_uk_1960 "Both United Kingdom 1960" 1.both_de_1930 "Both Germany 1930" 1.both_de_1900 "Both German Empire 1900"  1.both_se_no_1900 "Both United Sweden-Norway" 1.both_austro_hung_empire "Both Austro-Hungarian Empire 1900" 1.country_1900_same "Same Country 1900" 1.country_1930_same "Same Country 1930" 1.country_1960_same "Same Country 1960" 1.country_1990_same "Same Country 1990")  ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("demo_cntrls Table 1 Controls" "Nuts2_Region_FE NUTS2 FEs" "indiv_same_country Indiv. Same Country FEs" "N Number of Observations" "r2 \$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	mlabels(,none) collabels(none) ///
	mgroups("Dependent Variable: log(SCI)", pattern(1 0 0 0 0) prefix(\multicolumn{5}{c}{)suffix(}))
	
esttab using "./_output/nuts2_to_nuts2_2.tex" , replace /// 
	drop(0.country_same 1.country_same 0.country_border diff_share_low_edu diff_median_age diff_income_thous diff_unemp_rate 0.same_rlgdnm_wo_none 1.same_rlgdnm_wo_none 0.same_main_language 1.same_main_language industry_sim 0.both_czechoslovakia 0.both_yugoslavia 0.both_west_de 0.both_east_de 0.both_soviet_union 0.both_uk_1960 0.both_de_1930 0.both_de_1900 0.both_austro_hung_empire 0.both_se_no_1900 _cons) /// 
	varlabels(log_distance "log(Distance in KM)" 1.country_border "Border Country" 1.both_czechoslovakia "Both Czechoslovakia" 1.both_yugoslavia "Both Yugoslavia" 1.both_east_de "Both East Germany" 1.both_west_de "Both West Germany" 1.both_soviet_union "Both Soviet Union" 1.both_uk_1960 "Both United Kingdom 1960" 1.both_de_1930 "Both Germany 1930" 1.both_de_1900 "Both German Empire 1900"  1.both_se_no_1900 "Both United Sweden-Norway" 1.both_austro_hung_empire "Both Austro-Hungarian Empire 1900" 1.country_1900_same "Same Country 1900" 1.country_1930_same "Same Country 1930" 1.country_1960_same "Same Country 1960" 1.country_1990_same "Same Country 1990")  ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("demo_cntrls Table 1 Controls" "Nuts2_Region_FE NUTS2 FEs" "indiv_same_country Indiv. Same Country FEs" "N Number of Observations" "r2 \$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	mlabels(,none) collabels(none) ///
	mgroups("Dependent Variable: log(SCI)", pattern(1 0 0 0 0) prefix(\multicolumn{5}{c}{)suffix(}))
	
///////////////
