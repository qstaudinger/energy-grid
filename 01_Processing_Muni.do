//
//Preparation for real network charges
//
clear
import delimited input\inflation.csv, delimiter(";") varnames(1) clear
drop inflation
replace vpi = subinstr(vpi, ",", ".", .)
destring vpi, replace
save "inflation.dta",replace

//
// Zuordnung Tranmission Net Operator
//
cd C:\Users\User\Documents\Studium\DICE-SHK\Netzentgelte\Dataproject
import delimited input\ÜNetzentgelte.csv, delimiter(";") varnames(1) clear

foreach var of varlist hölp1 höap1 hölp2 höap2 uslp1 usap1 uslp2 usap2 {
replace `var' = subinstr(`var', ",", ".", .)
	destring `var', replace
}
drop uslp1 usap1 uslp2 usap2 hölp1 höap1
rename anbieter regelgebiet_name
generate trans_charge = (50000000000*höap2+18000000*hölp2*100)/50000000000
save "transmission_charges.dta", replace

import delimited input\nns_csv_windows_1252_Regelgebiete.csv, delimiter(";") varnames(1) clear
rename regelgebiet_nr regelgebiet_num
save "regelgebiete.dta", replace

*Verknüpfung Netze zu Übertragungsnetzbetreiber
import delimited input\nns_csv_windows_1252_Netze.csv, delimiter(";") varnames(1) clear
merge m:1 regelgebiet_num using "regelgebiete", nogen keep(3)
keep netz_nr regelgebiet_name regelgebiet_num
save "netze.dta", replace
expand 14
generate year = 2010
bys netz_nr: replace year = 2010 + _n - 1
merge m:1 regelgebiet_name year using "transmission_charges"
drop _merge 

label var hölp2 		 	"Performance Price transmission" 
label var höap2			 	"Working Price transmission"
label var trans_charge   	"Transmission Charge"

save "transmission_network_charges.dta", replace


*****************************************************************
* Load E'Net Data & Derive Network Charges At Muni X Year Level *
*****************************************************************

set more off
pause on

//
// First Load Original CSV-Sheet Providing Grid-Level Data
// Note: CSV covers all available network charges per grid in one sheet
//

tempfile grid_level_data

import delimited input\nns_csv_windows_1252_Netznutzungsentgelt.csv, delimiter(";") varnames(1) clear

* Drop Data Error 
drop if netz_nr == 0

* Keep Effective Network Charges Only
keep if id == 1

* Year of Respective Network Charge
gen year_effective_tarif_from = real(substr(gueltig_seit, -4, 4))
gen year_effective_tarif_to = real(substr(gueltig_bis, -4, 4))

* Get Rid of Information for 2023 
*drop if (year_effective_tarif_from == 2023 & year_effective_tarif_to == 2999) | ///
		(year_effective_tarif_from == 2023 & year_effective_tarif_to == 2023)
* Note: 2999 equivalent to = tarif still effective		

* For those Tarifs Still Effective, Replace Year to 2023 and Date to 31.12.2023
replace year_effective_tarif_to = 2023 if year_effective_tarif_from <= 2023 & year_effective_tarif_to == 2999

gen tarif_effective_from = date(gueltig_seit, "DMY")
gen tarif_effective_to = date(gueltig_bis, "DMY")
format tarif_effective_from tarif_effective_to %tdDD.NN.CCYY
replace tarif_effective_to = date("31.12.2023", "DMY") if tarif_effective_to == date("31.12.2999", "DMY") & tarif_effective_to !=.

* For Those Network Charges Not Changing on 1.1.2020 Set Effective Start Date of Network Charge To This Date  
replace year_effective_tarif_from = 2000 if year_effective_tarif_from < 2000
replace tarif_effective_from = date("01.01.2000", "DMY") if tarif_effective_from < date("01.01.2000", "DMY")  & tarif_effective_from !=.

* Identify First Observation Date Per Network
bys netz_nr: egen first_date = min(tarif_effective_from)

//
// Network Charges Change Within A Year Or Remain Applicable Via Multiple Years
//

* Identify Tarifs That Remain Constant Over Multiple Years
gen yrs_covered_tarif = year_effective_tarif_to - year_effective_tarif_from + 1

* Expand Dataset Accordingly
expand yrs_covered_tarif, gen(tmp_expanded_count)
bys netz_nr tarif_effective_from: egen expanded = max(tmp_expanded_count)
bys netz_nr tarif_effective_from: gen expanded_count = _n
bys netz_nr tarif_effective_from: gen nr_expansions = _N
gen year = year_effective_tarif_from + expanded_count - 1
label var year "Year"

* Identify Relevant Tarifs Within A Year 
replace tarif_effective_to = mdy(12,31,year) if expanded == 1 & expanded_count == 1 
replace tarif_effective_from = mdy(01,01,year) if expanded == 1 & expanded_count == nr_expansions 

replace tarif_effective_to = mdy(12,31,year) if expanded == 1 & expanded_count != 1 & expanded_count != nr_expansions 
replace tarif_effective_from = mdy(01,01,year) if expanded == 1 & expanded_count != 1 & expanded_count != nr_expansions 

* Count Days Within A Year A Given Tarif Was Effective 
gen days_tarif_effective = tarif_effective_to - tarif_effective_from + 1 
bys netz_nr year: egen total_days = total(days_tarif_effective)

* Calculate Network Charges According to BNetA formula
gen hh_nw_charge    = (ns_o_lm_hh_gp*100 + 3500*ns_o_lm_hh_ap)/3500
gen busi_nw_charge = (ns_o_lm_gw_gp*100 + 50000*ns_o_lm_gw_ap)/50000
gen ind_nw_charge   = (ms_2_lp*4000*100 + ms_2_ap*24000000)/24000000

//////////////////////////////////////////////
*Change weight, if data is not existent for whole year (busi)
*The same goes for hh (but all hh cases are included when weighing for busi)
* Flag Prices Equal To Zero Within A Year
bys netz_nr year: egen flag_missing_prices = max(busi_nw_charge == 0)
bys netz_nr year: egen new_total_days = total(days_tarif_effective) if busi_nw_charge != 0 & flag_missing_prices == 1

* For Years With and Without Price Data -> Adjust Calcuated Days With Price Data
replace total_days = new_total_days if busi_nw_charge !=0 & flag_missing_prices == 1

* Generate Weights
gen weight = days_tarif_effective / total_days
replace weight = 0 if flag_missing_prices == 1 & busi_nw_charge == 0
//////////////////////////////////////////////

* Derive Average Weighted Tarifs By Netz_NR and Year 
collapse (mean) hh_nw_charge busi_nw_charge ind_nw_charge ns_o_lm_hh_gp ns_o_lm_hh_ap ms_2_lp ms_2_ap ///
				(max) flag_missing_prices [aw=weight], by (netz_nr year)
			
* Set Network Charges Equal to Zero As Missing 				
local charges hh_nw_charge busi_nw_charge ind_nw_charge
foreach charge of local charges {
	replace `charge' =. if `charge' == 0
}

*Connect transmission network charges
merge m:m netz_nr year using "transmission_network_charges"
drop if _merge ==2
drop _merge

* Label Variables
label var hh_nw_charge   "Average Network Charge For Households"
label var busi_nw_charge "Average Network Charge For Businesses"
label var ind_nw_charge  "Average Network Charge for Industries"
label var ns_o_lm_hh_gp  "Average Basic Charge for Households"
label var ns_o_lm_hh_ap	 "Average Working Price for Households"
label var ms_2_lp	     "Average Performance Price for Industry"
label var ms_2_ap		 "Average Working Price for Industry"
label var year			 "Year"
label var netz_nr		 "Grid Number"

save "grid_level_data", replace

* Note: Grid-Level Data Unbalanced Over Time (New Grids Entering)
bys netz_nr: gen obs = _N
bys netz_nr: egen first_year = min(year)

//
// Assign Grid-Level Network Charges to Respective Municipalities
//

* Load Original Dataset				
import delimited input\nns_csv_windows_1252_PLZ_Netzbetreiber.csv, delimiter(";") varnames(1) clear

* Year of Respective Network-Municipality Assignment 
gen year_assignment_from = real(substr(gueltig_seit, -4, 4))
gen year_assignment_to = real(substr(gueltig_bis, -4, 4))

* Get Rid of Information for 2023 
*drop if (year_assignment_from == 2023 & year_assignment_to == 2999) | ///
		(year_assignment_from == 2023 & year_assignment_to == 2023)
* Note: 2999 equivalent to = tarif still effective		

* For those Assignments Still In Place, Replace Year to 2023 and Date to 31.12.2023
replace year_assignment_to = 2023 if year_assignment_to == 2999 | year_assignment_to == 2023

* For those Assignments at Start of Observation Period, Set Assignment Year to 2000
replace year_assignment_from = 2000 if year_assignment_from < 2000 

* Generate Exact Dates of Assignment
gen assignment_effective_from = date(gueltig_seit, "DMY")
gen assignment_effective_to = date(gueltig_bis, "DMY")
format assignment_effective_from assignment_effective_to %tdDD.NN.CCYY

replace assignment_effective_to = mdy(12,31,2023) if assignment_effective_to == mdy(12,31,2999)
replace assignment_effective_to = mdy(12,31,2023) if assignment_effective_to > mdy(12,31,2023)
replace assignment_effective_from = mdy(01,01,2000) if assignment_effective_from < mdy(01,01,2000) & assignment_effective_to > mdy(01,01,2000)
drop if assignment_effective_to < mdy(01,01,2000) 

gen yrs_effective_assignment = year_assignment_to - year_assignment_from + 1

// Expand Dataset (Each Year - Network Assignment in One Line)
expand yrs_effective_assignment, gen(tmp_expanded_count)
bys netz_nr plz ort assignment_effective_from: egen expanded = max(tmp_expanded_count)
bys netz_nr plz ort assignment_effective_from: gen expanded_count = _n
bys netz_nr plz ort assignment_effective_from: gen nr_expansions = _N
gen year = year_assignment_from + expanded_count - 1

* Identify Relevant Assignments Within A Year 
replace assignment_effective_to = mdy(12,31,year) if expanded == 1 & expanded_count == 1 
replace assignment_effective_from = mdy(01,01,year) if expanded == 1 & expanded_count == nr_expansions 

replace assignment_effective_to = mdy(12,31,year) if expanded == 1 & expanded_count != 1 & expanded_count != nr_expansions 
replace assignment_effective_from = mdy(01,01,year) if expanded == 1 & expanded_count != 1 & expanded_count != nr_expansions 

gen days_assignment_effective = assignment_effective_to - assignment_effective_from + 1 
bys plz ort year: egen total_days = total(days_assignment_effective)
gen weight = days_assignment_effective / total_days

* In Case There Is More Than Network-Muni Assignment Per Year, Choose the Longest Assignment 
bys plz ort year: egen max_weight = max(weight)
keep if max_weight == weight

* Special Case: Seßlach, Where Max-Weight is Not Unique - Use Latest Observation
bys plz ort year: gen obs = _N
bys plz ort year assignment_effective_to: gen counter = _n
drop if counter != obs & obs != 1
drop obs counter

bys plz ort year: gen total_obs = _N
assert total_obs == 1

* Drop Years 2000 and 2001 (According To Data Provider, Data not Representative/Complete)
drop if inlist(year,2000,2001)

//
// For Some Plz-Ort-Year Combinations, There Are Multiple Providers
// Alternatives: Take Main Provider vs. Take Average Over All Providers
//

//
// Code Alternative A: Main Provider Only
//

tempfile plz_ort_main_grid

preserve

* Keep Relevant Variables
keep netz_nr plz ort gemeinde gemeindekennziffer year einwohner

* Add Grid-Level Prices 
merge m:1 netz_nr year using "grid_level_data", gen(merge_alt1) keepus(hh_nw_charge busi_nw_charge ind_nw_charge flag_missing_price regelgebiet_num regelgebiet_name) 
keep if merge_alt == 3
drop merge_alt1

rename hh_nw_charge hh_nw_charge_main
rename busi_nw_charge busi_nw_charge_main
rename ind_nw_charge ind_nw_charge_main

label var hh_nw_charge_main   "Average Network Charge For Households - Main Provider"
label var busi_nw_charge_main "Average Network Charge For Businesses - Main Provider"
label var ind_nw_charge_main  "Average Network Charge for Industries - Main Provider"

save "plz_ort_main_grid", replace

restore

//
// Code Alternative B: Simple Average Over All Providers
//

tempfile plz_ort_avg_all_grids
tempfile all_grid_combinations

preserve

local vars netz_nr weiteres_netz1 weiteres_netz2 weiteres_netz3 ///
		   weiteres_netz4_nsp weiteres_netz5_nsp netz_nr_msp ///
		   weiteres_netz1_msp weiteres_netz2_msp weiteres_netz3_msp ///
		   weiteres_netz4_msp weiteres_netz5_msp 
local i = 1 
foreach var of local vars {
	rename `var' netz_nr`i'
	local i = `i' + 1
}

* Keep Relevant Variables
keep netz_nr? netz_nr?? plz ort gemeinde gemeindekennziffer year einwohner

gen id = _n
reshape long netz_nr, i(id) j(grid_id)
drop grid_id
drop if netz_nr == 0
duplicates drop

merge m:1 year netz_nr using "grid_level_data", gen(merge_alt2) keepus(hh_nw_charge busi_nw_charge ind_nw_charge ns_o_lm_hh_gp ns_o_lm_hh_ap ms_2_lp ms_2_ap hölp2 höap2 trans_charge flag_missing_price regelgebiet_num regelgebiet_name) 
keep if merge_alt2 == 3
drop merge_alt2

save "all_grid_combinations", replace

* Take Mean Over All Providers by PLZ X Ort X Year Cell  
collapse (first) gemeinde einwohner gemeindekennziffer regelgebiet_num regelgebiet_name ///
		 (mean) hh_nw_charge busi_nw_charge ind_nw_charge (max) flag_missing_price, by(plz ort year)

rename hh_nw_charge hh_nw_charge_mean
rename busi_nw_charge busi_nw_charge_mean
rename ind_nw_charge ind_nw_charge_mean 
		 
save "plz_ort_avg_all_grids", replace

restore

//
// Code Alternative C: Weighted Average Over All Providers (Weight: Number of Streets Served)
//

tempfile plz_ort_all_grids_weighted

// Load Street-Grid Assignment Data
import delimited input\nns_csv_windows_1252_Netze_Strassen.csv, ///
	   delimiter(";") varnames(1) clear
  
* Correct one inconsistency in writing 
replace strasse = "Dorfstrasse" if strasse == "Dorfstraße" & ort == "Naumburg" & ortsteil == "Neuflemmingen"	   
	   
* Year of Respective Grid-Street Assignment 
gen year_assignment_from = real(substr(gueltig_seit, -4, 4))
gen year_assignment_to = real(substr(gueltig_bis, -4, 4))

* Get Rid of Information for 2023 
drop if (year_assignment_from == 2023 & year_assignment_to == 2999) | ///
		(year_assignment_from == 2023 & year_assignment_to == 2023)
* Note: 2999 equivalent to = tarif still effective		

* For those Assignments Still In Place, Replace Year to 2023 and Date to 31.12.2023
replace year_assignment_to = 2023 if year_assignment_to == 2999

* For those Assignments at Start of Observation Period, Set Assignment Year to 2000
replace year_assignment_from = 2000 if year_assignment_from < 2000 

* Generate Exact Dates of Assignment
gen assignment_effective_from = date(gueltig_seit, "DMY")
gen assignment_effective_to = date(gueltig_bis, "DMY")
format assignment_effective_from assignment_effective_to %tdDD.NN.CCYY

replace assignment_effective_to = mdy(12,31,2023) if assignment_effective_to == mdy(12,31,2999)
replace assignment_effective_to = mdy(12,31,2023) if assignment_effective_to > mdy(12,31,2023)
replace assignment_effective_from = mdy(01,01,2000) if assignment_effective_from < mdy(01,01,2000) & assignment_effective_to > mdy(01,01,2000)
drop if assignment_effective_to < mdy(01,01,2000) 

gen yrs_effective_assignment = year_assignment_to - year_assignment_from + 1

// Expand Dataset (Each Year - Network Assignment in One Line)
expand yrs_effective_assignment, gen(tmp_expanded_count)
bys plz ort ortsteil strasse assignment_effective_from: egen expanded = max(tmp_expanded_count)
bys plz ort ortsteil strasse assignment_effective_from: gen expanded_count = _n
bys plz ort ortsteil strasse assignment_effective_from: gen nr_expansions = _N
gen year = year_assignment_from + expanded_count - 1

* Identify Relevant Assignments Within A Year 
replace assignment_effective_to = mdy(12,31,year) if expanded == 1 & expanded_count == 1 
replace assignment_effective_from = mdy(01,01,year) if expanded == 1 & expanded_count == nr_expansions 

replace assignment_effective_to = mdy(12,31,year) if expanded == 1 & expanded_count != 1 & expanded_count != nr_expansions 
replace assignment_effective_from = mdy(01,01,year) if expanded == 1 & expanded_count != 1 & expanded_count != nr_expansions 

gen days_assignment_effective = assignment_effective_to - assignment_effective_from + 1 
bys plz ort ortsteil strasse year: egen total_days = total(days_assignment_effective)

gen weight = days_assignment_effective / total_days

* In Case There Is More Than Grid-Straßen Assignment Per Year, Choose the Assignment With the Longest Duration
bys plz ort ortsteil strasse year: egen max_weight = max(weight)
keep if max_weight == weight	
	   
// Expand For Each Relevant Grid Number

local vars netz_nsp netz_nr_msp netz_nr_hsp 
*Ignore: netz_*um* variables
local i = 1 
foreach var of local vars {
	rename `var' netz_nr`i'
	local i = `i' + 1
}
save "weighted_net_plz.dta", replace

* Keep Relevant Variables
keep netz_nr? plz ort ortsteil strasse assignment_* year

* Reshape Long To Account for Multiple Grids Per Street
gen id = _n
reshape long netz_nr, i(id) j(grid_id)
drop grid_id
duplicates drop

*Zähle Straßen pro Netz-Nr PLZ ORT YEAR + Collapse
gen counter = 1

collapse (count) nr_streets_served=counter, by (plz ort netz_nr year)

bys plz ort year: egen total_streets = total(nr_streets_served)
gen weight = nr_streets_served / total_streets

* Drop Years 2000 and 2001 (According To Data Provider Information Incomplete)
drop if inlist(year,2000,2001)

merge 1:1 plz ort netz_nr year using "all_grid_combinations", gen(merge_alt3) keep(2 3)
replace weight = 1 if weight ==.

* Take Mean Over All Providers by PLZ X Ort X Year Cell  
collapse (first) gemeinde einwohner gemeindekennziffer regelgebiet_num regelgebiet_name ///
		 (mean) hh_nw_charge busi_nw_charge ind_nw_charge ns_o_lm_hh_gp ns_o_lm_hh_ap ms_2_lp ms_2_ap hölp2 höap2 trans_charge (max) flag_missing_price[aw=weight],  by(plz ort year) 

rename hh_nw_charge hh_nw_charge_wmean
rename busi_nw_charge busi_nw_charge_wmean
rename ind_nw_charge ind_nw_charge_wmean 
rename ns_o_lm_hh_gp ns_o_lm_hh_gp_wmean
rename ns_o_lm_hh_ap ns_o_lm_hh_ap_wmean
rename ms_2_lp ms_2_lp_wmean
rename ms_2_ap ms_2_ap_wmean
rename hölp2 hölp2_wmean 
rename höap2 höap2_wmean
rename trans_charge trans_charge_wmean

  
save "plz_ort_all_grids_weighted", replace

//
// Now, Merge All Three Different Alternatives of Network Charges and Netdata
// 

use  "plz_ort_all_grids_weighted", clear
merge 1:1 plz ort year using "plz_ort_avg_all_grids", keepusing(hh_nw_charge_mean busi_nw_charge_mean ind_nw_charge_mean flag_missing_price regelgebiet_num regelgebiet_name) nogen assert(3)
merge 1:1 plz ort year using "plz_ort_main_grid", keepusing(hh_nw_charge_main busi_nw_charge_main ind_nw_charge_main flag_missing_price regelgebiet_num regelgebiet_name) nogen keep(3) 

merge m:1 year using "inflation", nogen keep(3)
generate hh_nw_charge_wmean_real = hh_nw_charge_wmean/vpi 
generate busi_nw_charge_wmean_real = busi_nw_charge_wmean/vpi
generate ind_nw_charge_wmean_real = ind_nw_charge_wmean/vpi
generate ns_o_lm_hh_gp_wmean_real = ns_o_lm_hh_gp_wmean/vpi
generate ns_o_lm_hh_ap_wmean_real = ns_o_lm_hh_ap_wmean/vpi
generate ms_2_lp_wmean_real = ms_2_lp_wmean/vpi
generate ms_2_ap_wmean_real = ms_2_ap_wmean/vpi
generate hölp2_wmean_real = hölp2/vpi
generate höap2_wmean_real = höap2/vpi
generate trans_charge_wmean_real = trans_charge/vpi

//
// Last, Collapse At Municipality X Year Level
//

collapse (first) gemeinde einwohner regelgebiet_num regelgebiet_name (mean) hh_nw* busi_nw* ind_nw* ns* ms* hölp2* höap2* trans* (max) flag_missing_price, by(gemeindekennziffer year) 
drop if gemeindekennziffer < 1001000
rename gemeindekennziffer muni_id
rename gemeinde muni_name
drop einwohner

label var year 					"Year"
label var muni_id 				"Municipality Identifier"
label var muni_name 			"Municipality Name"
label var hh_nw_charge_main   	"Average Network Charge For Households - Main Provider"
label var busi_nw_charge_main 	"Average Network Charge For Businesses - Main Provider"
label var ind_nw_charge_main  	"Average Network Charge for Industries - Main Provider"
label var hh_nw_charge_mean   	"Average Network Charge For Households - All Providers"
label var busi_nw_charge_mean 	"Average Network Charge For Businesses - All Providers"
label var ind_nw_charge_mean  	"Average Network Charge for Industries - All Providers"
label var hh_nw_charge_wmean  	"Weighted Average Network Charge For Households - All Providers"
label var busi_nw_charge_wmean 	"Weighted Average Network Charge For Businesses - All Providers"
label var ind_nw_charge_wmean  	"Weighted Average Network Charge for Industries - All Providers"
label var flag_missing_price 	"Price not consistent available"
label var ns_o_lm_hh_gp_wmean 	    "Weighted Average Basic Charge for Households - All Providers"
label var ns_o_lm_hh_ap_wmean	    "Weighted Average Working Price for Households - All Providers"
label var ms_2_lp_wmean	    	    "Weighted Average Performance Price for Industry - All Providers"
label var ms_2_ap_wmean		 		"Weighted Average Working Price for Industry - All Providers"
label var hh_nw_charge_wmean_real   "Weighted Average Real Network Charge For Households - All Providers"	
label var busi_nw_charge_wmean_real	"Weighted Average Real Network Charge For Businesses - All Providers"
label var ind_nw_charge_wmean_real  "Weighted Average Real Network Charge for Industries - All Providers"
label var ns_o_lm_hh_gp_wmean_real  "Weighted Average Real Basic Charge for Households - All Providers"
label var ns_o_lm_hh_ap_wmean_real  "Weighted Average Real Working Price for Households - All Providers"
label var ms_2_lp_wmean_real	    "Weighted Average Real Performance Price for Industry - All Providers"
label var ms_2_ap_wmean_real 		"Weighted Average Real Working Price for Industry - All Providers"
label var hölp2_wmean_real 			"Performance Price transmission real" 
label var höap2_wmean_real			"Working Price transmission real"
label var trans_charge_wmean_real 	"Transmission Charge real"
label var regelgebiet_num        	"Regelgebiet Nummer"
label var regelgebiet_name			"Regelgebiet Betreiber"
save "munipical_network_charges.dta", replace

//
//Take net data (power grid length, load, etc.)
//

clear
import delimited input\nns_csv_windows_1252_Netzdaten.csv, delimiter(";") varnames(1) 
gen year = year(date(stand, "DMY"))

*Löschen (leerer) irrelevanter Daten für Arbeitsspeicher
drop if year == 1900

*Dieses eine spezielle Datum lösche ich, weil es das einzige ist, wo sowohl am 01.1. als auch am 31.12. ein Eintrag vorliegt
gen date_num = date(stand, "DMY")
format date_num %td
drop if date_num == mdy(1, 1, 2018)
drop if keine_netzdaten == 1
drop date_num stand letzter_datenstand


foreach var of varlist stromkreislaenge_ns_ka stromkreislaenge_ms_ka stromkreislaenge_hs_ka stromkreislaenge_hoes_ka stromkreislaenge_ns_fr stromkreislaenge_ms_fr stromkreislaenge_hs_fr stromkreislaenge_hoes_fr stromkreislaenge_ha_fr stromkreislaenge_ha_ka verluste_ns verluste_ms_ns verluste_ms verluste_hs_ms verluste_hs verluste_hoes_hs verluste_hoes versflaeche_ns versflaeche_ms versflaeche_hs geographflaeche_ns geographflaeche_ms geographflaeche_hs arbeit_ns arbeit_ms_ns arbeit_ms arbeit_hs_ms arbeit_hs arbeit_hoes_hs arbeit_hoes instleisttrafo_ms_ns instleisttrafo_hs_ms instleisttrafo_hoes_hs{
    replace `var' = subinstr(`var', ",", ".", .)
	destring `var', replace
}

*Zero values
foreach var of varlist einwohnerzahl versflaeche_ns versflaeche_ms versflaeche_hs geographflaeche_ns geographflaeche_ms geographflaeche_hs entnahmestellen_ns entnahmestellen_ms_ns entnahmestellen_ms entnahmestellen_hs_ms entnahmestellen_hs entnahmestellen_hoes_hs entnahmestellen_hoes arbeit_ns arbeit_ms_ns arbeit_ms arbeit_hs_ms arbeit_hs arbeit_hoes_hs arbeit_hoes instleisttrafo_ms_ns instleisttrafo_hs_ms instleisttrafo_hoes_hs stromkreislaenge_ns_ka stromkreislaenge_ms_ka stromkreislaenge_hs_ka stromkreislaenge_hoes_ka stromkreislaenge_ns_fr stromkreislaenge_ms_fr stromkreislaenge_hs_fr stromkreislaenge_hoes_fr stromkreislaenge_ha_fr stromkreislaenge_ha_ka verluste_ns verluste_ms_ns verluste_ms verluste_hs_ms verluste_hs verluste_hoes_hs verluste_hoes{
	replace `var' =. if `var'==0 & (geographflaeche_ns+geographflaeche_ms+geographflaeche_hs)>0
}

*Summarizing variables
generate gesamt_geofläche = geographflaeche_ns + geographflaeche_ms + geographflaeche_hs
generate gesamt_versflache = versflaeche_ns + versflaeche_ms + versflaeche_hs
generate gesamt_entnahmestellen = entnahmestellen_ns + entnahmestellen_ms_ns + entnahmestellen_ms + entnahmestellen_hs_ms + entnahmestellen_hs + entnahmestellen_hoes_hs + entnahmestellen_hoes
generate gesamt_arbeit = arbeit_ns + arbeit_ms_ns + arbeit_ms + arbeit_hs_ms + arbeit_hs + arbeit_hoes_hs + arbeit_hoes
generate gesamt_stromkreislänge = stromkreislaenge_ns_ka + stromkreislaenge_ms_ka + stromkreislaenge_hs_ka + stromkreislaenge_hoes_ka + stromkreislaenge_ns_fr + stromkreislaenge_ms_fr + stromkreislaenge_hs_fr + stromkreislaenge_hoes_fr + stromkreislaenge_ha_fr + stromkreislaenge_ha_ka
generate gesamt_stromkreislänge_fr = stromkreislaenge_ns_fr + stromkreislaenge_ms_fr + stromkreislaenge_hs_fr + stromkreislaenge_hoes_fr + stromkreislaenge_ha_fr
generate gesamt_stromkreislänge_ka = stromkreislaenge_ns_ka + stromkreislaenge_ms_ka + stromkreislaenge_hs_ka + stromkreislaenge_hoes_ka + stromkreislaenge_ha_ka
generate gesamt_stromkreislänge_fr_pro = gesamt_stromkreislänge_fr/ gesamt_stromkreislänge
generate gesamt_stromkreislänge_ka_pro = gesamt_stromkreislänge_ka/ gesamt_stromkreislänge
generate gesamt_trafo = instleisttrafo_ms_ns + instleisttrafo_hs_ms + instleisttrafo_hoes_hs
generate gesamt_verluste = verluste_ns + verluste_ms_ns + verluste_ms + verluste_hs_ms + verluste_hs + verluste_hoes_hs + verluste_hoes


*Variablen pro qm
generate qkm_entnahmestellen = gesamt_entnahmestellen/gesamt_geofläche
generate qkm_arbeit = gesamt_arbeit/gesamt_geofläche
generate qkm_stromkreislänge = gesamt_stromkreislänge/gesamt_geofläche
generate qkm_verluste = gesamt_verluste/gesamt_geofläche

*Relevant für NS (hh, busi) und MS (Ind) pro qm
generate qkm_entnahmestellen_ns = entnahmestellen_ns/geographflaeche_ns
generate qkm_arbeit_ns = arbeit_ns/geographflaeche_ns
generate qkm_stromkreislänge_ns = (stromkreislaenge_ns_ka+stromkreislaenge_ns_fr)/geographflaeche_ns
generate qkm_stromkreislänge_ha = (stromkreislaenge_ha_fr+stromkreislaenge_ha_ka)/geographflaeche_ns
generate qkm_entnahmestellen_ms = entnahmestellen_ms/geographflaeche_ms
generate qkm_arbeit_ms = arbeit_ms/geographflaeche_ms
generate qkm_stromkreislaenge_ms = (stromkreislaenge_ms_ka+stromkreislaenge_ms_fr)/geographflaeche_ms

save "netzdata.dta",replace

*street weighted networks per muni
use weighted_net_plz
* Keep Relevant Variables
keep netz_nr? plz ort ortsteil strasse assignment_* year gemeinde gemeindekennziffer

* Reshape Long To Account for Multiple Grids Per Street
gen id = _n
reshape long netz_nr, i(id) j(grid_id)
drop grid_id
duplicates drop

*Zähle Straßen pro Netz-Nr PLZ ORT YEAR + Collapse
gen counter = 1

collapse (first) gemeinde gemeindekennziffer (count) nr_streets_served=counter, by (plz ort netz_nr year)

bys plz ort year: egen total_streets = total(nr_streets_served)
gen weight = nr_streets_served / total_streets

* Drop Years 2000 and 2001 (According To Data Provider Information Incomplete)
drop if inlist(year,2000,2001)

* Add weighted plz-net connection
merge m:1 netz_nr year using "netzdata", gen(merge_alt1)
keep if merge_alt == 3

//
// Last, Collapse At Municipality X Year Level
//

collapse (first) gemeinde (mean) qkm*, by(gemeindekennziffer year)
drop if gemeindekennziffer < 1001000
rename gemeindekennziffer muni_id
rename gemeinde muni_name

label var year 						"Year"
label var muni_id 					"Municipality Identifier"
label var muni_name 				"Municipality Name"
label var qkm_entnahmestellen 		"Entnahmestellen pro qm"
label var qkm_arbeit 				"Arbeit pro qm"
label var qkm_stromkreislänge 		"Stromkreislänge pro qm"
label var qkm_verluste				"Verluste pro qm"
label var qkm_entnahmestellen_ns 	"Entnahmestellen NS pro qm"
label var qkm_arbeit_ns      		"Arbeit NS pro qm"
label var qkm_stromkreislänge_ns 	"Stromkreislänge pro qm"
label var qkm_stromkreislänge_ha 	"Stromkreislänge Haushalte pro qm"
label var qkm_entnahmestellen_ms 	"Entnahmestellen MS pro qm"
label var qkm_arbeit_ms 			"Arbeit MS pro qm"
label var qkm_stromkreislaenge_ms	"Stromkreislänge pro qm"

save "netzdaten_muni.dta", replace

//
//Merge Netdata and network charges and transmission network
//
use munipical_network_charges, clear
merge 1:1 muni_id year using "netzdaten_muni"
drop if _merge== 2
drop _merge
sort muni_id year
save "final_data_muni.dta", replace

