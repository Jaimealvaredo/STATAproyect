*** Setting the DO-File:
clear
set more off
capture log close

*** Setting work directory
cd h:\downloads\Examen_Stata

*** Install necessary programs:
ssc install coefplot, replace
ssc install xtabond2, replace
ssc install estout, replace
ssc install outreg2, replace
ssc install grstyle, replace
ssc install palettes, replace
ssc install colrspace, replace

*** Graphical settings:
grstyle clear
set scheme s2color
grstyle init
grstyle set plain, box
grstyle color background white
grstyle set color Set1
grstyle yesno draw_major_hgrid yes
grstyle yesno draw_major_ygrid yes
grstyle color major_grid gs8
grstyle linepattern major_grid dot
grstyle numstyle legend_cols 2
grstyle linestyle legend none
grstyle set legend 4, inside 
grstyle color ci_area gs12%50

*** Use "main_dataset.dta"
use "h:\downloads\Examen_Stata\main_dataset.dta", clear

***We create a new variable called “region” using the appropriate dummy variables

gen region = ""
replace region = "North America" if wb_nam == 1
replace region = "Latin America & Caribbean" if wb_lac == 1
replace region = "East Asia & Pacific" if wb_eap == 1
replace region = "South Asia" if wb_sas == 1
replace region = "Sub-Saharan Africa" if wb_ssa == 1
replace region = "Middle East & North Africa" if wb_mena == 1
replace region = "Europe" if europe == 1 & postsov == 0
replace region = "Transition Economies" if postsov == 1


/* We need to adjust the region variable so that regions with too few countries
are merged. We’ll drop countries with a population under 0.5 million 
(as in the Figure 2 DO-file and combine “North America” with  “Latin America & Caribbean” 
*/

* Drop very small countries (pop in millions < 0.5)
drop if pop < 0.5

* Drop specific outliers (as in the Figure 2 do-file)
drop if NAMES_STD=="Maldives"
drop if NAMES_STD=="Cape Verde"

* Check frequencies of each region 
tab region

* Combine North America with Latin America & Caribbean. Personally , I believe 55 ovservations is not enough for a boxplot.
* Therefore, I will merge south Asia with East Asia & Pacific too.
replace region = "Latin America & Caribbean" if region=="North America"
replace region = "America" if region == "Latin America & Caribbean"
replace region = "East Asia & Pacific" if region=="South Asia"
replace region = "Asia" if region == "East Asia & Pacific"
/* For the box plot:
Rotate the x axis labels by 45° so they don’t overlap.

Interpretation of graph in pdf
*/
graph box judgespc, over(period, label(angle(45))) by(region, note("") title("Judges per 100k inhabitants: Box Plots by Region"))

* save graph
graph export "BoxPlots_by_Region.png", replace

*------------------------------------------------------------------------------------
*exercise 2

clear all


use main_dataset.dta, clear

*** Running OLS-FE Regressions:

*** Basic model - Specification 1
eststo ols1: xtreg growth L.ln_gdppc L.ln_judgespc i.period, fe

*** Specification 2 - Same as above, used for comparison with GMM
eststo ols2: xtreg growth L.ln_gdppc L.ln_judgespc i.period, fe

*** Specification 3 - Adding schooling, investment, government consumption, and polity2
eststo ols3: xtreg growth L.ln_gdppc L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 i.period, fe

*** Specification 4 - Adding fertility variable
eststo ols4: xtreg growth L.ln_gdppc L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 L.ln_fertil i.period, fe

*** Specification 5 - Adding openness
eststo ols5: xtreg growth L.ln_gdppc L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 L.ln_fertil L.open i.period, fe

*** Specification 6 - Adding inflation (cpi)
eststo ols6: xtreg growth L.ln_gdppc L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 L.ln_fertil L.open L.cpi i.period, fe

*** Verify stored estimates
estimates dir

*** Running GMM Estimations:

*** Specification 2 - Basic model using GMM
eststo gmm2: xtabond2 growth L.ln_gdppc L.ln_judgespc i.period, gmm(L.ln_gdppc, laglimits(1 5) collapse) gmm(L.ln_judgespc, laglimits(4 7) collapse) iv(i.period, p) robust noleveleq twostep small

*** Specification 3 - Including schooling, investment, government consumption, and polity2
eststo gmm3: xtabond2 growth L.ln_gdppc L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 i.period, gmm(L.ln_gdppc, laglimits(1 5) collapse) gmm(L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2, laglimits(4 7) collapse) iv(i.period, p) robust noleveleq twostep small

*** Specification 4 - Adding fertility (ln_fertil)
eststo gmm4: xtabond2 growth L.ln_gdppc L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 L.ln_fertil i.period, gmm(L.ln_gdppc, laglimits(1 5) collapse) gmm(L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 L.ln_fertil, laglimits(4 7) collapse) iv(i.period, p) robust noleveleq twostep small

*** Specification 5 - Adding openness
eststo gmm5: xtabond2 growth L.ln_gdppc L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 L.ln_fertil L.open i.period, gmm(L.ln_gdppc, laglimits(1 5) collapse) gmm(L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 L.ln_fertil L.open, laglimits(4 7) collapse) iv(i.period, p) robust noleveleq twostep small

*** Specification 6 - Including inflation (cpi)
eststo gmm6: xtabond2 growth L.ln_gdppc L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 L.ln_fertil L.open L.cpi i.period, gmm(L.ln_gdppc, laglimits(1 5) collapse) gmm(L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 L.ln_fertil L.open L.cpi, laglimits(4 7) collapse) iv(i.period, p) robust noleveleq twostep small

*** Verify stored GMM estimates
estimates dir

*** Plotting Coefficients:

coefplot (ols2, drop(_cons) keep(L.ln_judgespc) label("OLS-FE: Spec2") mcolor(blue)) (gmm2, drop(_cons) keep(L.ln_judgespc) label("GMM: Spec2") mcolor(blue)) (ols3, drop(_cons) keep(L.ln_judgespc) label("OLS-FE: Spec3") mcolor(red)) (gmm3, drop(_cons) keep(L.ln_judgespc) label("GMM: Spec3") mcolor(red)) (ols4, drop(_cons) keep(L.ln_judgespc) label("OLS-FE: Spec4") mcolor(green)) (gmm4, drop(_cons) keep(L.ln_judgespc) label("GMM: Spec4") mcolor(green)) (ols5, drop(_cons) keep(L.ln_judgespc) label("OLS-FE: Spec5") mcolor(orange)) (gmm5, drop(_cons) keep(L.ln_judgespc) label("GMM: Spec5") mcolor(orange)) (ols6, drop(_cons) keep(L.ln_judgespc) label("OLS-FE: Spec6") mcolor(purple)) (gmm6, drop(_cons) keep(L.ln_judgespc) label("GMM: Spec6") mcolor(purple)), xline(0) title("Comparison of Coefficient on log(Judges per Capita)") note("OLS-FE vs. GMM estimates (95% CIs) across Specifications 2-6") legend(position(3) ring(0) col(1)) ylabel(, angle(90))
graph export "coefficient_plot.png", replace
esttab using "results_table.rtf", replace

*------------------------------------------------------------------------
clear all
* Load V-Dem dataset
use V-Dem-CY-Full+Others-v14.dta, clear


summarize v2clrelig, detail
* The variable seems broadly consistent. However, we need to check for potential missing values or problematic data points next.
* This command counts how many observations of v2peedueq are missing.
count if missing(v2clrelig)
/* We chose Freedom of Religion (v2clrelig) due to its strong data coverage and economic relevance. Beyond its robustness, religious freedom is closely linked to institutional quality, social stability, and human capital development—factors that foster economic growth. Including this variable allows us to capture the broader impact of societal openness on economic performance.
*/
* Create a new variable that groups years into five-year intervals
* Create 5-year periods to match main dataset periods
gen period = .
replace period = 1970 if inrange(year, 1970, 1974)
replace period = 1975 if inrange(year, 1975, 1979)
replace period = 1980 if inrange(year, 1980, 1984)
replace period = 1985 if inrange(year, 1985, 1989)
replace period = 1990 if inrange(year, 1990, 1994)
replace period = 1995 if inrange(year, 1995, 1999)
replace period = 2000 if inrange(year, 2000, 2004)
replace period = 2005 if inrange(year, 2005, 2009)
replace period = 2010 if inrange(year, 2010, 2014)
replace period = 2015 if inrange(year, 2015, 2019)

rename country_name NAMES_STD


* Check for duplicate country-period combinations
duplicates report NAMES_STD period

* Collapse data by taking the mean for each country and period
collapse (mean) v2clrelig, by(NAMES_STD period)

* Check for any remaining duplicates
duplicates report NAMES_STD period

* List a few observations to confirm data looks correct
list NAMES_STD period v2clrelig if _n <= 20
* Count missing period values
count if missing(period)

* Drop observations where period is missing
drop if missing(period)




* Save the cleaned dataset
save vdem_freedom_religion_cleaned.dta, replace



* Load the main replication dataset
use main_dataset.dta, clear

* Merge using standardized country names and period
merge 1:1 NAMES_STD period using "vdem_freedom_religion_cleaned.dta"

* Check the merge result
tabulate _merge

drop _merge

save merged_main_dataset.dta, replace


* we only need these
keep NAMES_STD period v2clrelig
*Only one country for judicial index
collapse v2clrelig , by(NAMES_STD)

* merging for MARKER var
merge 1:1 NAMES_STD using shape.dta

* not neccessary< variiable
drop _merge

*Some cleaning of data done by the author
replace _ID=238 if NAMES_STD=="Serbia"

drop if NAMES_STD=="Yugoslavia" 


* map computation
ssc install spmap
* Calculate the mean of v2clrelig
summarize v2clrelig

* Store the mean value rounded to two decimal places
local mean_value = round(r(mean), 0.01)

* Map with mean displayed in the legend
spmap v2clrelig using coord.dta, id(_ID) ///
    fcolor(Blues2) clm(q) ///
    legend(size(medium) subtitle("Mean: `mean_value'")) ///
    title("Global Distribution of Religious Freedom", size(large)) ///
    note("Data Source: V-Dem Dataset | Higher scores indicate greater freedom", size(medium))



graph export "religius_freedom_map.png", replace

* ----------------------------------------------------------------------------------------
*exercise 5
* A)
* Load dataset

* Clean and set memory
clear all
set maxvar 10000

use "main_dataset.dta", clear

* Generate democracy variable using Polity2 score
gen democracy = .
replace democracy = 1 if polity2 > 5   // Democracies
replace democracy = 0 if polity2 <= 5  // Non-Democracies

* Label for clarity
label define democracy_label 0 "Non-Democracy" 1 "Democracy"
label values democracy democracy_label

* Separate dataset for democracies
preserve
keep if democracy == 1

* GMM estimation for Democracies
xtabond2 growth L.ln_gdppc L.ln_judgespc ///
L.yr_sch L.csh_i L.csh_g L.polity2 ///
c.L.ln_judgespc#c.L.ln_gdppc, ///
gmm(L.ln_gdppc, laglimits(1 5) collapse) ///
gmm(L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 c.L.ln_judgespc#c.L.ln_gdppc, laglimits(4 7) collapse) ///
iv(i.period, p) robust noleveleq twostep small

* Marginal effects for democracies
margins, dydx(L.ln_judgespc) at(L.ln_gdppc=(6(0.5)12.5)) level(90)
marginsplot, graphregion(color(white)) recast(line) plotopts(lcolor("blue")) recastci(rline) ciopts(lpattern(dash) lcolor(blue)) ///
yline(0, lpattern(".-.") lcolor(gray)) title("Marginal Effects for Democracies") ytitle("Effect of ATJ on Growth") xtitle("log(GDP per capita)")


* Save the marginal effects graph for Democracies
graph export "Marginal_Effects_Democracies.png", replace
* Restore original dataset
restore

* Filter for non-democracies
keep if democracy == 0

* GMM estimation for Non-Democracies
xtabond2 growth L.ln_gdppc L.ln_judgespc ///
L.yr_sch L.csh_i L.csh_g L.polity2 ///
c.L.ln_judgespc#c.L.ln_gdppc, ///
gmm(L.ln_gdppc, laglimits(1 5) collapse) ///
gmm(L.ln_judgespc L.yr_sch L.csh_i L.csh_g L.polity2 c.L.ln_judgespc#c.L.ln_gdppc, laglimits(4 7) collapse) ///
iv(i.period, p) robust noleveleq twostep small

* Marginal effects for non-democracies
margins, dydx(L.ln_judgespc) at(L.ln_gdppc=(6(0.5)12.5)) level(90)
marginsplot, graphregion(color(white)) recast(line) plotopts(lcolor("red")) recastci(rline) ciopts(lpattern(dash) lcolor(red)) ///
yline(0, lpattern(".-.") lcolor(gray)) title("Marginal Effects for Non-Democracies") ytitle("Effect of ATJ on Growth") xtitle("log(GDP per capita)")

* Save the marginal effects graph for Non-Democracies
graph export "Marginal_Effects_NonDemocracies.png", replace

