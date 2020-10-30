* Purpose: Generate the anti-EU sentiment regression results
* Inputs: _intermediate_data/final_regress_dat/eurobarometer_regress_dat
* Outputs: _output/anti_eu.tex
* Date: 2020-10-29

use "_intermediate_data/final_regress_dat/eurobarometer_regress_dat", clear

* Create non-linear controls
cap drop *group
local group = 10
egen inc_group = cut(income_thous), group(`group')
egen ur_group  = cut(unemp_rate), group(`group')
egen fb_group  = cut(share_other_eu), group(`group')
egen me_group  = cut(manufacturing_emp), group(`group')
egen ce_group  = cut(construction_emp), group(`group')
egen pe_group  = cut(professional_emp), group(`group')

eststo clear

* Version of "Trust in EU"
eststo panel1: reghdfe Trust_in_EU share_EU_connections_out_country, noabsorb keepsingletons
estadd local region_demo = "N"
estadd local region_fb = "N"
estadd local cntry_FE = "N"

eststo panel2: reghdfe Trust_in_EU share_EU_connections_out_country ED0_2 ED3_4 ED5_8 Y0_9 Y10_19 Y20_29 Y30_39 Y40_49 Y50_59 Y60_69 Y70_MAX i.ur_group i.inc_group i.me_group i.pe_group, noabsorb keepsingletons
estadd local region_demo = "Y"
estadd local region_fb = "N"
estadd local cntry_FE = "N"

eststo panel3: reghdfe Trust_in_EU share_EU_connections_out_country ED0_2 ED3_4 ED5_8 Y0_9 Y10_19 Y20_29 Y30_39 Y40_49 Y50_59 Y60_69 Y70_MAX i.ur_group i.inc_group i.me_group i.pe_group i.fb_g, noabsorb keepsingletons
estadd local region_demo = "Y"
estadd local region_fb = "Y"
estadd local cntry_FE = "N"

eststo panel4: reghdfe Trust_in_EU share_EU_connections_out_country ED0_2 ED3_4 ED5_8 Y0_9 Y10_19 Y20_29 Y30_39 Y40_49 Y50_59 Y60_69 Y70_MAX i.ur_group i.inc_group i.me_group i.pe_group i.fb_g, absorb(country_fe) keepsingletons
estadd local region_demo = "Y"
estadd local region_fb = "Y"
estadd local cntry_FE = "Y"

use "_intermediate_data/final_regress_dat/brookings_regress_dat", clear

* Create non-parametric controls
cap drop *group
local group = 10
egen inc_group = cut(income_thous), group(`group')
egen ur_group  = cut(unemp_rate), group(`group')
egen fb_group  = cut(share_other_eu), group(`group')
egen me_group  = cut(manufacturing_emp), group(`group')
egen ce_group  = cut(construction_emp), group(`group')
egen pe_group  = cut(professional_emp), group(`group')

* Version of "Trust in EU"
eststo panel5: reghdfe	Anti_EU_vote share_EU_connections_out_country, noabsorb keepsingletons
estadd local region_demo = "N"
estadd local region_fb = "N"
estadd local cntry_FE = "N"

eststo panel6: reghdfe Anti_EU_vote share_EU_connections_out_country ED0_2 ED3_4 ED5_8 Y0_9 Y10_19 Y20_29 Y30_39 Y40_49 Y50_59 Y60_69 Y70_MAX i.ur_group i.inc_group i.me_group i.pe_group, noabsorb keepsingletons
estadd local region_demo = "Y"
estadd local region_fb = "N"
estadd local cntry_FE = "N"

eststo panel7: reghdfe Anti_EU_vote share_EU_connections_out_country ED0_2 ED3_4 ED5_8 Y0_9 Y10_19 Y20_29 Y30_39 Y40_49 Y50_59 Y60_69 Y70_MAX i.ur_group i.inc_group i.me_group i.pe_group i.fb_g, noabsorb keepsingletons
estadd local region_demo = "Y"
estadd local region_fb = "Y"
estadd local cntry_FE = "N"

eststo panel8: reghdfe Anti_EU_vote share_EU_connections_out_country ED0_2 ED3_4 ED5_8 Y0_9 Y10_19 Y20_29 Y30_39 Y40_49 Y50_59 Y60_69 Y70_MAX i.ur_group i.inc_group i.me_group i.pe_group i.fb_g, absorb(country_fe) keepsingletons
estadd local region_demo = "Y"
estadd local region_fb = "Y"
estadd local cntry_FE = "Y"

esttab, ///
	drop(ED* Y* *ur_group *inc_group *me_group *pe_group *fb_group _cons) ///
	varlabels(share_EU_connections_out_country "Share of European connections outside of country (\%)")  ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("region_demo Demographic/Econ Controls" "region_fb Share Foregin Born Controls" "cntry_FE Country Fixed Effects" "N Number of Observations" "r2 \$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	mlabels(,none) collabels(none) ///
	mgroups("Share Trust in EU (\%)" "Share Vote for Anti-EU Parties (\%)", pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span)

esttab using "./_output/anti_eu.tex" , replace /// 
	drop(ED* Y* *ur_group *inc_group *me_group *pe_group *fb_group _cons) ///
	varlabels(share_EU_connections_out_country "Share of European connections outside of country (\%)")  ///
	cells(b(star fmt(%9.3f)) se(par) gaps) star(* 0.10 ** 0.05 *** 0.01) ///
	noobs scalars("region_demo Demographic/Econ Controls" "region_fb Share Foregin Born Controls" "cntry_FE Country Fixed Effects" "N Number of Observations" "r2 \$R^2\$") sfmt(%9.0gc %9.0gc %9.0gc %9.0gc %9.3f) ///
	mlabels(,none) collabels(none) ///
	mgroups("Share Trust in EU (\%)" "Share Vote for Anti-EU Parties (\%)", pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span)
