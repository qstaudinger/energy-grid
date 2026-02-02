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


*Merge tranmission network charges
merge m:m netz_nr year using "transmission_network_charges"
drop if _merge ==2
drop _merge

*Erstellen von Realpreisen
merge m:1 year using "inflation", nogen keep(3)
generate hh_nw_charge_real = hh_nw_charge/vpi 
generate busi_nw_charge_real = busi_nw_charge/vpi
generate ind_nw_charge_real = ind_nw_charge/vpi
generate ns_o_lm_hh_gp_real = ns_o_lm_hh_gp/vpi
generate ns_o_lm_hh_ap_real = ns_o_lm_hh_ap/vpi
generate ms_2_lp_real = ms_2_lp/vpi
generate ms_2_ap_real = ms_2_ap/vpi
generate hölp2_real = hölp2/vpi
generate höap2_real = höap2/vpi
generate trans_charge_real = trans_charge/vpi

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

keep qkm* netz_nr einwohner year
save "netzdata.dta",replace


//
//Merge Netdata and network charges and transmission network
//
use grid_level_data, clear
merge 1:1 netz_nr year using "netzdata"
drop if _merge== 2
drop _merge
sort netz_nr year

*Labeln
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
label var hh_nw_charge_real   		"Real Network Charge For Households - All Providers"	
label var busi_nw_charge_real		"Real Network Charge For Businesses - All Providers"
label var ind_nw_charge_real    "Real Network Charge for Industries - All Providers"
label var ns_o_lm_hh_gp_real    "Real Basic Charge for Households - All Providers"
label var ns_o_lm_hh_ap_real    "Real Working Price for Households - All Providers"
label var ms_2_lp_real	  	    "Real Performance Price for Industry - All Providers"
label var ms_2_ap_real 			"Real Working Price for Industry - All Providers"
label var hölp2_real 			"Performance Price transmission real" 
label var höap2_real			"Working Price transmission real"
label var trans_charge_real 	"Transmission Charge real"

save "2023_clean_enet_data_net.dta", replace
