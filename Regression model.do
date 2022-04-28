import delimited "loadings.csv", varnames(1) clear 
rename dtmdimnamesdocs gvkey
drop v1

replace gvkey = subinstr(gvkey,".txt","",.)

merge 1:1 gvkey using "biopharma_2020.dta"

foreach var of varlist topic1-topic24 {
egen indmean_`var' = mean(`var') if topic1 != . 
}

gen distinctiveness = 0 if topic1 != .
foreach var of varlist topic1-topic24  {
gen dist_`var' = abs(`var' - indmean_`var') 
replace dist_`var' = 0 if dist_`var' == . & `var' != . 
replace distinctiveness = distinctiveness + dist_`var'
}

foreach var of varlist ni {
	gen `var'_cr = sign(`var') * abs(`var')^(1/3) 
}

reg ni_cr c.distinctiveness, vce(robust)
reg ni_cr c.distinctiveness##c.distinctiveness, vce(robust)

sum distinctiveness
margins, at(distinctiveness = (0.97 (0.01) 1.99))
marginsplot, noci


import delimited "loadings_small.csv", varnames(1) clear 
rename smalldtmdimnamesdocs gvkey
drop v1

replace gvkey = subinstr(gvkey,".txt","",.)

merge 1:1 gvkey using "biopharma_2020.dta"

foreach var of varlist topic1-topic29 {
egen indmean_`var' = mean(`var') if topic1 != . 
}

gen distinctiveness = 0 if topic1 != .
foreach var of varlist topic1-topic29  {
gen dist_`var' = abs(`var' - indmean_`var') 
replace dist_`var' = 0 if dist_`var' == . & `var' != . 
replace distinctiveness = distinctiveness + dist_`var'
}

foreach var of varlist ni {
	gen `var'_cr = sign(`var') * abs(`var')^(1/3) 
}

reg ni_cr c.distinctiveness, vce(robust)
reg ni_cr c.distinctiveness##c.distinctiveness, vce(robust)

sum distinctiveness
margins, at(distinctiveness = (0.69 (0.01) 1.99))
marginsplot, noci
