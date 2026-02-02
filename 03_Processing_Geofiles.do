tempfile geodaten
import delimited input\Landwirtschaft_2016.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2016
save "landwirtschaft_2016", replace
import delimited input\Landwirtschaft_2017.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2017
save "landwirtschaft_2017", replace
import delimited input\Landwirtschaft_2018.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2018
save "landwirtschaft_2018", replace
import delimited input\LandwLandwirtschaft_2019.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2019
save "landwirtschaft_2019", replace
import delimited input\Landwirtschaft_2020.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2020
save "landwirtschaft_2020", replace
import delimited input\Landwirtschaft_2021.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2021
append using "landwirtschaft_2020"
append using "landwirtschaft_2019"
append using "landwirtschaft_2018"
append using "landwirtschaft_2017"
append using "landwirtschaft_2016"
rename v3 agriculture
save "landwirtschaft", replace

//////////////////////////////////////////////////////////////////////////////
import delimited input\Verkehr_Siedlung_2016.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2016
save "Verkehr_Siedlung_2016", replace
import delimited input\Verkehr_Siedlung_2017.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2017
save "Verkehr_Siedlung_2017", replace
import delimited input\Verkehr_Siedlung_2018.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2018
save "Verkehr_Siedlung_2018", replace
import delimited input\Verkehr_Siedlung_2019.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2019
save "Verkehr_Siedlung_2019", replace
import delimited input\Verkehr_Siedlung_2020.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2020
save "Verkehr_Siedlung_2020", replace
import delimited input\Verkehr_Siedlung_2021.csv, delimiter(";") varnames(1) clear
drop in 1/9
generate year=2021

append using "Verkehr_Siedlung_2020"
append using "Verkehr_Siedlung_2019"
append using "Verkehr_Siedlung_2018"
append using "Verkehr_Siedlung_2017"
append using "Verkehr_Siedlung_2016"
rename v3 traffic

merge 1:1 v1 year using "landwirtschaft"
drop anteilderflãchefãrlandwirtschaft _merge
rename v1 muni_id
rename anteilsiedlungsundverkehrsflãche muni_name
order muni_id muni_name year traffic agriculture

save "Geodaten.dta", replace
