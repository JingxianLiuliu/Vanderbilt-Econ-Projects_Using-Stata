clear
global datadir "C:\Users\Lenovo-V\Desktop\econ7910\ECON_7910_Final_Project_Data"
cd "$datadir"
cap log close
log using "Log_FinalDEc10.smcl", replace

set more off
/*Question 2_ DATA ANALYSIS DO FILES: TABLE 1 and TABLE 2*/



use "namelist.dta" 
 ********************************************************************
 

* Each school is a distinct data point, weighted by number of pupils
	keep if visit==981 
	collapse sex elg98 stdgap yrbirth wgrp* (count) np=pupid, by (sch98v1) 
 
 * Create worm group indicators
	gen wgrp1 = (wgrp==1) 
	gen wgrp2 = (wgrp==2) 
	gen wgrp3 = (wgrp==3)
**** TABLE 1: PANEL A
*t-test without weight
bys wgrp: summ sex elg98 stdgap yrbirth 
	foreach var in sex elg98 stdgap yrbirth { 
		regress `var' wgrp1 wgrp2 
		}
**** TABLE 2: PANEL A
	*t-test with weights
	bys wgrp: summ sex elg98 stdgap yrbirth [aw=np] 
	foreach var in sex elg98 stdgap yrbirth { 
		regress `var' wgrp1 wgrp2 [aw=np] 
		} 
	
	
	* Incorporate data from Pupil Questionnaire 
	
	keep sch98v1 wgrp* 
	rename sch98v1 schid 
	sort schid 
	save schoolnum, replace 
	clear 
	use "pupq.dta" 

* Only keep pupils with 1998 data
	drop if pupdate_98_1=="" &  schid_98_2==. 

* Incorporate treatment group variable
	gen schid = schid_98_2 
	sort schid 
	
	merge m:1 schid using schoolnum 
	tab _merge  
	drop _merge 

* Create measure of pre-program school attendance based on # days absent in previous four weeks
	gen preatt_98 = (20-absdays_98_6)/20 

* Create indicator for "Household Has Livestock"
	gen     Ilivestock_98 = (cows_98_23==1 | goats_98_24==1 | sheep_98_25==1 | pigs_98_26==1) 
	replace Ilivestock_98 = . if (cows_98_23==. | goats_98_24==. | sheep_98_25==. | pigs_98_26==.) 

* Create indicator for "Child Sick Often"
	gen     Isoften_98 = (fallsick_98_37==3) 
	replace Isoften_98 = . if fallsick_98_37==. 

* Create indicator for "Child Clean"
	gen     Iclean_98 = (clean_98_15==1) 
	replace Iclean_98 = . if clean_98_15==. 

* Each school is a distinct data point, weighted by number of pupils
collapse preatt_98 havelatr_98_33 Ilivestock_98 waz_98 bloodst_98_58 Isoften_98 malaria_98_48 Iclean_98 ///
	wgrp* (count) np = pupid, by(schid) 
	

**** TABLE 1: PANEL B
*t-test without weight
bys wgrp: summ preatt_98 havelatr_98_33 Ilivestock_98 waz_98 bloodst_98_58 Isoften_98 malaria_98_48 Iclean_98  
	foreach var in preatt_98 havelatr_98_33 Ilivestock_98 waz_98 bloodst_98_58 Isoften_98 malaria_98_48 Iclean_98 { 
		regress `var' wgrp1 wgrp2  
		}

**** TABLE 2: PANEL B
		*t-test with weight
		bys wgrp: summ preatt_98 havelatr_98_33 Ilivestock_98 waz_98 bloodst_98_58 Isoften_98 malaria_98_48 Iclean_98 [aw=np] 
	foreach var in preatt_98 havelatr_98_33 Ilivestock_98 waz_98 bloodst_98_58 Isoften_98 malaria_98_48 Iclean_98 { 
		regress `var' wgrp1 wgrp2 [aw=np] 
		} 
	clear
 

* Use School data
	use "schoolvar.dta" 

* Create worm group indicators

	gen wgrp1 = (wgrp==1) 
	gen wgrp2 = (wgrp==2) 
	gen wgrp3 = (wgrp==3) 

* Normalize 1996 mock tests to be in units of individual std dev, equivalent to 1998, 1999
	replace mk96_s = mk96_s*(0.4357)/(0.8318) 
	

**** TABLE 1 & 2: PANEL C

	bys wgrp: summ mk96_s distlake pup_pop latr_pup z_inf98  
	foreach var in mk96_s distlake pup_pop latr_pup z_inf98 { 
			regress `var' wgrp1 wgrp2 
		} 
		
	
	********************************************
	********************************************
	/*DATA ANALYSIS DO FILES: TABLE 3*/
*Question 2 Table 3
clear
* Incorporate child gender and age information with parasitological exam data
	use "namelist.dta" 
	
	collapse sex yrbirth sch98v1, by (pupid) 
	sort pupid 
	save namelistQ2, replace 
	gen wgrp1 = (wgrp==1) 
	gen wgrp2 = (wgrp==2) 
	gen wgrp3 = (wgrp==3)
	use "wormed.dta" 
	
	drop sch98v1 
	sort pupid 
	merge pupid using namelistQ2 
	tab _merge  
	drop _merge 
	save namelist2, replace 
	clear 

* Incorporate treatment group information
	use "schoolvar.dta" 
	keep schid distlake wgrp 
	rename schid sch98v1 
	sort sch98v1 
	save namelist, replace 
	clear 
	use namelist2 
	*tab wgrp, gen(wgrp)
	drop wgrp1 
	sort sch98v1 
	merge sch98v1 using namelist 
	tab _merge  
	drop _merge 
	drop if hw98==. 

* Change units for average infection intensity variables from 100 milligrams to grams
	replace hw98 = hw98*10 
	replace al98 = al98*10 
	replace sm98 = sm98*10 
	replace tt98 = tt98*10 

* Prevalence of infection
	* Hookwork
		summ any_hw98 
	* Roundworm
		summ any_al98 	
	* Schistosomiasis
		summ any_sm98  
	* Whipworm
		summ any_tt98 
	* At least one infection
		summ any_98 
		* Born since 1985
			summ any_98 if (yrbirth>=1985 & yrbirth~=.) 
		* Born before 1985
			summ any_98 if yrbirth<1985 
		* Female
			summ any_98 if sex==0
		* Male
			summ any_98 if sex==1
	* At least two & three infections
		gen atleast2=0 if numinf98!=.
		replace atleast2=1 if numinf98>=2 & numinf98!=.
		tab atleast2
		gen atleast3=0 if numinf98!=.
		replace atleast3=1 if numinf98>=3 & numinf98!=.
		tab atleast3

* Prevalence of moderate-heavy infection
	* Hookworm
		summ hw98_ics 
	* Roundworm
		summ al98_who 
	* Schistosomiasis
		summ sm98_who 
	* Whipworm
		summ tt98_ics 
	* At least one infection
		summ any_ics98 
		* Born since 1985
			summ any_ics98 if (yrbirth>=1985 & yrbirth~=.) 
		* Born before 1985
			summ any_ics98 if yrbirth<1985 
		* Female 
			summ any_ics98 if sex==0
		* Male
			summ any_ics98 if sex==1
	* At least two & three infections
		gen atleast2i=0 if numics98!=.
		replace atleast2i=1 if numics98>=2 & numics98!=.
		
		summ atleast2i
		gen atleast3i=0 if numics98!=.
		replace atleast3i=1 if numics98>=3 & numics98!=.
		
		summ atleast3i

* Average worm load
	* Hookworm
		summ hw98 
	* Roundworm
		summ al98 
	* Schistosomiasis
		summ sm98 
	* Whipworm
		summ tt98

		*************************************************
		*************************************************
		clear
	*Question 3, TABLE 4 do file
* Incorporate child gender and age information with parasitological exam data
	use "namelist.dta" 
collapse sex yrbirth sch98v1 wgrp, by (pupid) 
	sort pupid 
	 keep if wgrp==1
	keep if sex==1

save namelist_sub, replace
 
	use "wormed.dta" 
	
	drop sch98v1 
	sort pupid 
	merge pupid using namelist_sub 
	keep if sex==1
	
	tab _merge  
	drop _merge 
	save namelist_sub2, replace 
	clear 

* Incorporate treatment group information
	use "schoolvar.dta" 

	keep schid distlake wgrp
	
	rename schid sch98v1 
	sort sch98v1 
	 keep if wgrp==1
	save namelist_sub, replace 
	clear 
	use namelist_sub2 
	
	sort sch98v1 
	merge sch98v1 using namelist_sub 
	tab _merge  
	drop _merge 
	drop if hw98==. 

* Change units for average infection intensity variables from 100 milligrams to grams
	replace hw98 = hw98*10 
	replace al98 = al98*10 
	replace sm98 = sm98*10 
	replace tt98 = tt98*10 

* Prevalence of infection
	* Hookwork
		summ any_hw98   
	* Roundworm
		summ any_al98  
	* Schistosomiasis
		summ any_sm98  
	* Whipworm
		summ any_tt98  
	* At least one infection
		summ any_98 
		* Born since 1985
			summ any_98 if  (yrbirth>=1985 & yrbirth~=.) 
		* Born before 1985
			summ any_98 if  yrbirth<1985 
		
	* At least two & three infections
		gen atleast2=0 if numinf98!=.
		replace atleast2=1 if numinf98>=2 & numinf98!=.
		summ atleast2  
		gen atleast3=0 if numinf98!=.
		replace atleast3=1 if numinf98>=3 & numinf98!=.
		summ atleast3  

* Prevalence of moderate-heavy infection
	* Hookworm
		summ hw98_ics 
	* Roundworm
		summ al98_who  
	* Schistosomiasis
		summ sm98_who  
	* Whipworm
		summ tt98_ics  
	* At least one infection
		summ any_ics98   
		* Born since 1985
			summ any_ics98 if  (yrbirth>=1985 & yrbirth~=.) 
		* Born before 1985
			summ any_ics98 if  yrbirth<1985 
		
	* At least two & three infections
		gen atleast2i=0 if numics98!=.
		replace atleast2i=1 if numics98>=2 & numics98!=.
		
		summ atleast2i  
	
		gen atleast3i=0 if numics98!=. 
		replace atleast3i=1 if numics98>=3 & numics98!=.  
		
		summ atleast3i 

* Average worm load
	* Hookworm
		summ hw98 
	* Roundworm
		summ al98 
	* Schistosomiasis
		summ sm98 
	* Whipworm
		summ tt98 

****************************************************
clear
*Question 4

use "namelist"
	
* Merge Compliance data with Namelist data
		rename schid sch 
	drop Tmonths 
	reshape wide date sch std obs prs sex Isem*, i(pupid) j(visit) /*for males only*/
	sort pupid 
	save "question4final", replace 
	use "comply" 
	sort pupid 
	merge pupid using "question4final" 
	aorder
	
	drop if wgrp==. 
    tab wgrp, gen(wgrp)
	gen any98=.
	replace any98 = 0 if (a981==0 | a982==0 | p98==0)
	replace any98 = 1 if (a981==1 | a982==1 | p98==1)
	 
	gen any99=.
	replace any99 = 0 if (a991==0 | a992==0 | p99==0) 
	replace any99 = 1 if (a991==1 | a992==1 | p99==1)
	
****TABLE 5,6****

	* Girls < 13
		* Any medical treatment in 1998
			bys wgrp: tab any98 if elg98 == 1
		* Albendazole
			bys wgrp: tab a981  if elg98 == 1 
		* Praziquantel
			tab p98 if elg98 == 1 &  wgrp1==1 & psch98==1
			tab p98 if elg98 == 1 & wgrp2==1
			tab p98 if elg98 == 1 & wgrp3==1
		* Albendazole
			bys wgrp: tab a982  if elg98 == 1 

	* Girls >= 13
		* Any medical treatment in 1998
			bys wgrp: tab any98 if elg98==0 
		* Albendazole
			bys wgrp: tab a981 if elg98==0 
		* Praziquantel
			tab p98 if elg98==0 & wgrp1==1 & psch98==1
			tab p98 if elg98==0 & (wgrp2==1) 
			tab p98 if elg98==0 & (wgrp3==1) 
		* Albendazole
			bys wgrp: tab a982 if elg98==0 

	
														
	*Males*
		* Any medical treatment in 1998
			bys wgrp: tab any98 if sex981 == 1
				
		* Albendazole r1
			bys wgrp: tab a981  if sex981 == 1 
		* Praziquantel
			tab p98 if sex981 == 1 &  wgrp1==1 & psch98==1
			
		* Albendazole r2
			bys wgrp: tab a982  if sex982 == 1 

	


	* Drop pupils in standard 8 in 1998
		drop if std981==8 
		
	* Girls < 13
		* Any medical treatment in 1999
			bys wgrp: tab any99 if elg99 == 1 
		* Albendazole
			bys wgrp: tab a991  if elg99 == 1 
		* Praziquantel
			bys wgrp: tab p99   if elg99 == 1 & psch99==1
		* Albendazole
			bys wgrp: tab a992  if elg99 == 1 

	* Girls >= 13
		* Any medical treatment in 1999
			bys wgrp: tab any99 if elg99 == 0 
		* Albendazole
			bys wgrp: tab a991  if elg99 == 0 
		* Praziquantel
			bys wgrp: tab p99   if elg99 == 0 & psch99==1
		* Albendazole
			bys wgrp: tab a992  if elg99 == 0
			
	*Males
		*Any medical treatment in 1999
			bys wgrp: tab any99 if sex981 == 1
		* Albendazole
			bys wgrp: tab a991  if sex991 == 1 
		* Praziquantel
			bys wgrp: tab p99   if sex991 == 1 & psch99==1
		* Albendazole
			bys wgrp: tab a992  if sex991 == 1 

***************************************************
clear
*Question 5

use "namelist.dta", clear
set more off

* Incorporate data on treatment group and from pupil questionnaire
	keep if visit >= 991
	keep pupid wgrp sap1 sap2 sap3 sap4 sch98v1
	codebook pupid
	duplicates report
	duplicates drop
	codebook pupid

	merge 1:1 pupid using "pupq.dta"

	keep if _merge == 3
	drop _merge


**** TABLE 7, PANEL B 
	tab wgrp, gen(wgrp)	
	summ soften_99_39 haz99 waz_99 if wgrp==1
	summ soften_99_39 haz99 waz_99 if wgrp==2
	drop if wgrp == 3
	foreach var in soften_99_39 haz99 waz_99 { 
		regress `var' wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		} 
		
	reg soften_99_39 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	reg haz99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	reg waz_99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	
**** TABLE 7, PANELS B AND C
	summ clean_99_13 shoes_99_10 boilwat_99_29 if wgrp==1
	summ clean_99_13 shoes_99_10 boilwat_99_29 if wgrp==2 | wgrp==3

	drop if wgrp == 3
	bys wgrp: summ clean_99_13 shoes_99_10 boilwat_99_29

	foreach var in clean_99_13 shoes_99_10 boilwat_99_29 { 
		regress `var' wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		} 
		
	reg clean_99_13 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
	reg shoes_99_10 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
	reg boilwat_99_29 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
	
* Incorporate data on eligibility and parasitological exams
	use "namelist.dta", clear
	keep if visit==981 
	keep pupid sch98v1 wgrp elg98 elg99 
	sort pupid 
	save "f1.dta", replace 

	use "wormed.dta", clear
	sort pupid 
	merge pupid using "f1.dta"
	tab _merge 
	keep if _merge==3 
	drop _merge 

**** TABLE 7, PANEL A 
	gen wgrp1 = (wgrp==1) 
	gen wgrp2 = (wgrp==2) 
	gen wgrp3 = (wgrp==3)
	
* create schistosomiasis infection rate 
	gen sm99 = (sm99_a + sm99_b)*10 
	gen modheavy_sm99= (sm99>250) if sm99 !=. 
	gen any_sm99= (sm99>0) if sm99 !=. 

* create Hookworm infection rate 
	gen hw99= (hw99_a+ hw99_b) *10 
	gen modheavy_hw99= (hw99>750) if hw99 !=. 
	gen any_hw99=(hw99>0) if hw99 !=. 

* create Roundworm infection rate 
	gen rw99= (al99_a+ al99_b) *10 
	gen modheavy_rw99= (rw99>5000) if rw99 !=. 
	gen any_rw99= (rw99>0) if rw99 !=. 

* create Whipworm infection rate 
	gen ww99= (tt99_a+ tt99_b) *10 
	gen modheavy_ww99= (ww99>400) if ww99 !=. 
	gen any_ww99= (ww99>0) if ww99 !=. 

* create any moderate-heavy infection rate 
	egen any_modheavy99= rowtotal (modheavy_sm99 modheavy_hw99 modheavy_rw99 modheavy_ww99), m 
	replace any_modheavy99=1 if any_modheavy99>0 & !missing(any_modheavy99) 

summ  ww99 sm99 hw99 rw99 if wgrp==1
summ  any_modheavy99 modheavy_sm99 modheavy_hw99 modheavy_rw99 modheavy_ww99 if wgrp==1
summ  any_modheavy99 modheavy_sm99 modheavy_hw99 modheavy_rw99 modheavy_ww99 if wgrp==2
	foreach var in any_modheavy99 modheavy_sm99 modheavy_hw99 modheavy_rw99 modheavy_ww99 { 
		regress `var' wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		} 

*the codes above equals to the following codes
reg any_modheavy99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
reg modheavy_sm99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
reg modheavy_hw99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
reg modheavy_rw99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
reg modheavy_ww99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)

**** TABLE 7, PANEL B: HB AND ANEMIA ONLY
* Generate low HB indicator
	gen hb100 = (hb<100) 
	replace hb100=. if hb==. 

	summ hb hb100 if wgrp==1
	summ hb hb100 if wgrp==2
	foreach var in hb hb100 { 
		regress `var' wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		} 	
		
	reg hb wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	reg hb100 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		
****** For subsample
	use "namelist.dta", clear
	keep if sex==1
	set more off
	
* Incorporate data on treatment group and from pupil questionnaire
	keep if visit >= 991
	
	keep pupid wgrp sap1 sap2 sap3 sap4 sch98v1
	codebook pupid
	duplicates report
	duplicates drop
	codebook pupid

	merge 1:1 pupid using "pupq.dta"

	keep if _merge == 3
	drop _merge


**** TABLE 7, PANEL B 
	tab wgrp, gen(wgrp)	
	summ soften_99_39 haz99 waz_99 if wgrp==1
	summ soften_99_39 haz99 waz_99 if wgrp==2
	drop if wgrp == 3
	foreach var in soften_99_39 haz99 waz_99 { 
		regress `var' wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		} 
		
	reg soften_99_39 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	reg haz99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	reg waz_99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 


**** TABLE 7, PANELS B AND C
	summ clean_99_13 shoes_99_10 boilwat_99_29 if wgrp==1
	summ clean_99_13 shoes_99_10 boilwat_99_29 if wgrp==2 | wgrp==3

	drop if wgrp == 3
	bys wgrp: summ clean_99_13 shoes_99_10 boilwat_99_29

	foreach var in clean_99_13 shoes_99_10 boilwat_99_29 { 
		regress `var' wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		} 
		
	reg clean_99_13 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	reg shoes_99_10 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	reg boilwat_99_29 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 

* Incorporate data on eligibility and parasitological exams
	use "namelist.dta", clear
	keep if sex==1
	keep if visit==981 
	keep pupid sch98v1 wgrp elg98 elg99 
	sort pupid 
	save "f1.dta", replace 

	use "wormed.dta"
	sort pupid 
	merge pupid using "f1.dta"
	tab _merge 
	keep if _merge==3 
	drop _merge 

**** TABLE 7, PANEL A 
	gen wgrp1 = (wgrp==1) 
	gen wgrp2 = (wgrp==2) 
	gen wgrp3 = (wgrp==3)

* create schistosomiasis infection rate 
	gen sm99 = (sm99_a + sm99_b)*10 
	gen modheavy_sm99= (sm99>250) if sm99 !=. 
	gen any_sm99= (sm99>0) if sm99 !=. 

* create Hookworm infection rate 
	gen hw99= (hw99_a+ hw99_b) *10 
	gen modheavy_hw99= (hw99>750) if hw99 !=. 
	gen any_hw99=(hw99>0) if hw99 !=. 

* create Roundworm infection rate 
	gen rw99= (al99_a+ al99_b) *10 
	gen modheavy_rw99= (rw99>5000) if rw99 !=. 
	gen any_rw99= (rw99>0) if rw99 !=. 

* create Whipworm infection rate 
	gen ww99= (tt99_a+ tt99_b) *10 
	gen modheavy_ww99= (ww99>400) if ww99 !=. 
	gen any_ww99= (ww99>0) if ww99 !=. 

* create any moderate-heavy infection rate 
	egen any_modheavy99= rowtotal (modheavy_sm99 modheavy_hw99 modheavy_rw99 modheavy_ww99), m 
	replace any_modheavy99=1 if any_modheavy99>0 & !missing(any_modheavy99) 

summ  ww99 sm99 hw99 rw99 if wgrp==1
summ  any_modheavy99 modheavy_sm99 modheavy_hw99 modheavy_rw99 modheavy_ww99 if wgrp==1
summ  any_modheavy99 modheavy_sm99 modheavy_hw99 modheavy_rw99 modheavy_ww99 if wgrp==2

	foreach var in any_modheavy99 modheavy_sm99 modheavy_hw99 modheavy_rw99 modheavy_ww99 { 
		regress `var' wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		} 

reg any_modheavy99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
reg modheavy_sm99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
reg modheavy_hw99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
reg modheavy_rw99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
reg modheavy_ww99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)


**** TABLE 7, PANEL B: HB AND ANEMIA ONLY
* Generate low HB indicator
	gen hb100 = (hb<100) 
	replace hb100=. if hb==. 
	
	summ hb hb100 if wgrp==1
	summ hb hb100 if wgrp==2
	foreach var in hb hb100 { 
		regress `var' wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		} 	
		
	reg hb wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	reg hb100 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
***************************************************
*Question 6

use "wormed.dta", clear

set more off

* create schistosomiasis infection rate 
	gen sm99 = (sm99_a + sm99_b)*10 
	gen modheavy_sm99= (sm99>250) if sm99 !=. 
	gen any_sm99= (sm99>0) if sm99 !=. 

* create Hookworm infection rate 
	gen hw99= (hw99_a+ hw99_b) *10 
	gen modheavy_hw99= (hw99>750) if hw99 !=. 
	gen any_hw99=(hw99>0) if hw99 !=. 

* create Roundworm infection rate 
	gen rw99= (al99_a+ al99_b) *10 
	gen modheavy_rw99= (rw99>5000) if rw99 !=. 
	gen any_rw99= (rw99>0) if rw99 !=. 

* create Whipworm infection rate 
	gen ww99= (tt99_a+ tt99_b) *10 
	gen modheavy_ww99= (ww99>400) if ww99 !=. 
	gen any_ww99= (ww99>0) if ww99 !=. 

* create any moderate-heavy infection rate 
	egen any_modheavy99= rowtotal (modheavy_sm99 modheavy_hw99 modheavy_rw99 modheavy_ww99), m 
	replace any_modheavy99=1 if any_modheavy99>0 & !missing(any_modheavy99) 

* create moderate-heavy geohelminth (hookworm, roundworm, or whipworm) infection rate 
	egen modheavy_geo99= rowtotal (modheavy_hw99 modheavy_rw99 modheavy_ww99), m 
	replace modheavy_geo99=1 if modheavy_geo99>0 & !missing(modheavy_geo99) 

	save "wormed1.dta", replace

*Merge database: wormed1, namelist, and schoolvar 
*merge namelist & wormed 1 as namelistMERGEwormed1 99.dta 

	use "namelist.dta", clear
	
	keep if visit==991 
	keep if elg99==1 
	isid pupid 
	merge 1:1 pupid using "wormed1.dta"
	tab _merge 
	keep if _merge==3 
	codebook pupid 
	drop _merge 

	save "wormed2.dta", replace

 * 1:m merge with school to control for 1996 school district exam score 

	use "schoolvar.dta", clear
	rename schid sch98v1 
	merge 1:m sch98v1 using"wormed2.dta"
	keep if _merge==3 
	codebook pupid 
	drop _merge

*Create variables used in regression: group dummy variable and control variable 
* create group dummy 
	tab wgrp, gen (wgrp) 
	tab wgrp wgrp1 

*when account for SAP, egen a row total for SAP1 SAP SAP3 SAP4? 
	egen sap=rowtotal (sap1 sap2 sap3 sap4), m 
	save "wormed3.dta", replace

*Do probit regression on groups, cluster at 1998 school level 

	use "wormed3.dta", clear

	
****** For all eligible pupils: 

* Any mod-heavy infection
	probit any_modheavy99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	margins, dydx (wgrp1) 

* moderate-heavy schistosomiasis infection 
	probit modheavy_sm99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
	margins, dydx (wgrp1) 

* moderate heavy geohelminth infection 
	probit modheavy_geo99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
	margins, dydx (wgrp1)



****** For subsample - Eligible Male Pupils
	keep if sex==1 

	probit any_modheavy99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
	margins, dydx (wgrp1) 

	probit modheavy_sm99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
	margins, dydx (wgrp1) 
	
	probit modheavy_geo99 wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
	margins, dydx (wgrp1)

****************************************************

*Question 7

clear
set more off
use "namelist", clear
	sort pupid visit 
	keep if visit==981 
	keep pupid std 
	rename std std98v1 
	sort pupid 
	save question7, replace 

	use "namelist" 
	sort pupid 
	merge m:1 pupid using question7 
	tab _merge  
	drop _merge 
	sort pupid 
	keep pupid visit std std98v1 prs wgrp* sch98v1 elg98 sex yrbirth 
	save question7, replace 

	***Table 11***
	/*First year whole sample*/
	use question7 
	keep if (visit>981 & visit<993 & elg98==1) 
	collapse prs wgrp* (count) np = pupid, by(sch98v1) 
	bys wgrp: summ prs [aw=np] 
	tab wgrp, gen(wgrp)
	regress prs wgrp1 [aw=np] 
	regress prs wgrp1 wgrp2 [aw=np] 
	clear 
	
	/*males */
	use question7 
	keep if (visit>981 & visit<993 & elg98~=. & sex==1) 
	collapse prs wgrp* (count) np = pupid, by(sch98v1) 
	summ prs [aw=np] 
	bys wgrp: summ prs [aw=np] 
	tab wgrp, gen(wgrp)
	regress prs wgrp1 [aw=np] 
	regress prs wgrp1 wgrp2 [aw=np] 
	clear 

	/*Second year whole sample*/
	use question7 
	keep if (visit>992 & visit<999 & elg98==1) 
	collapse prs wgrp* (count) np = pupid, by(sch98v1) 
	summ prs [aw=np] 
	bys wgrp: summ prs [aw=np] 
	tab wgrp, gen(wgrp)
	regress prs wgrp1 wgrp2 [aw=np] 
	clear 
	
	/*males */
	use question7 
	keep if (visit>992 & visit<999 & elg98~=. & sex==1) 
	collapse prs wgrp* (count) np = pupid, by(sch98v1) 
	summ prs [aw=np] 
	bys wgrp: summ prs [aw=np]
	tab wgrp, gen(wgrp)
	regress prs wgrp1 wgrp2 [aw=np] 
	clear

****************************************************

*Question 8
* Incorporate Namelist data
	use "namelist" 

* Create further school assistance controls
	gen     Y98 = 0 
	replace Y98 = 1 if (visit>980 & visit<990) 
	gen Y98sap1 = sap1*Y98 
	gen Y98sap2 = sap2*Y98 
	gen Y98sap3 = sap3*Y98 
	gen Y98sap4 = sap4*Y98 
	save "namelistq8final", replace 

* Incorporate school and zonal variables 
	use "schoolvar" 
	keep schid z9899* distlake pup_pop zoneid z_inf98 
	rename schid sch98v1 
	sort sch98v1 
	save "schoolvarq8final", replace 
	clear 
	use "namelistq8final" 
	sort sch98v1 
	merge sch98v1 using "schoolvarq8final" 
	sum _merge 
	drop _merge 


* Generate year measure
	gen     yr = . 
	replace yr = 1 if (visit>981 & visit<993) 
	replace yr = 2 if (visit>992 & visit<999) 

* Create treatment indicators
	* First year of treatment
	gen     t1 = 0 
	replace t1 = 1 if (wgrp==1 & visit>981 & visit<993) 
	replace t1 = 1 if (wgrp==2 & visit>992 & visit<999) 
	replace t1 = . if wgrp==. 
	* Second year of treatment
	gen     t2 = 0 
	replace t2 = 1 if (wgrp==1 & visit>992 & visit<999) 
	replace t2 = .  if wgrp==. 

* Other indicators
	gen t1e = elg98*t1 
	gen t2e = elg98*t2 

* Create standard-specific measure of zonal infection rate
	gen     p1 = z9899_34 
	replace p1 = z9899_56 if (std98v1==5 | std98v1==6) 
	replace p1 = z9899_78 if (std98v1==7 | std98v1==8) 
	drop z9899* 

* Create standard indicators, based on 1998 visit 1 standard
	gen std_fs = std98v1 if (std98v1>-1 & std98v1<9) 
	replace std_fs = -1 if (std98v1==55) 
	tab std_fs, gen(Istd) 
	summ Istd* 
	drop Istd10 std_fs 
	save "namelistq8final", replace 
	clear 

* Incorporate compliance data
	use "comply" 
	gen any98=.
	replace any98=0 if a981==0 | a982==0| p98==0
	replace any98=1 if a981==1 | a982==1| p98==1
	gen any99=.
	replace any99=0 if a991==0 | a992==0| p99==0
	replace any99=1 if a991==1 | a992==1| p99==1
	keep pupid any98 any99 
	sort pupid 
	save "complyq8final", replace 
	clear 
	use "namelistq8final" 
	sort pupid 
	merge pupid using "complyq8final" 
	tab _merge  
	drop _merge 

	compress 
		save "namelistq8final", replace 

* Collapse data by pupil and year, where YEAR1 = 982-992, YEAR2 = 983-998
	sort pupid yr 
	drop if yr==. 
	collapse (mean) sch98v1 prs t1 t2 elg98 p1 sex ///
		Y98sap1 Y98sap2 Y98sap3 Y98sap4 sap1 sap2 sap3 sap4 ///
		Istd1 Istd2 Istd3 Istd4 Istd5 Istd6 Istd7 Istd8 Istd9 ///
		Isem1 Isem2 Isem3 ///
		any98 any99 wgrp (sum) obs ///
		if (t1~=. & elg98~=. & sch98v1~=.  &  p1~=. & Istd2~=.), by(pupid yr)
	keep e* t* p* sap* Y98sap* sch98v1 prs* Istd* Isem* sch* obs yr  sex

* Create an indicator for whether the school received treatment
	gen     t_any = 0 
	replace t_any=1 if (t1==1 | t2==1) 
	replace t_any=. if t1==. | t2==. 
	save "namelistq8final", replace 

**** TABLE 12 ***
	sum prs [aw=obs] if  (t1~=. & elg98~=. & sch98v1~=. & p1~=. & Istd2~=.) 
	/*any treatment*/
	regress prs t_any elg98 p1 Y98sap* sap* Istd* Isem* [aw=obs] if (t1~=. & elg98~=. & sch98v1~=. & p1~=. & Istd2~=.), robust cluster(sch98v1) 
	/*first and second year treatment*/
	regress prs t1 t2 elg98 p1 Y98sap* sap* Istd* Isem* [aw=obs] if (t1~=. & elg98~=. & sch98v1~=. & p1~=. & Istd2~=.), robust cluster(sch98v1) 


	/*males*/
	keep if sex==1
	/*any treatment*/
	regress prs t_any p1 Y98sap* sap* Istd* Isem* [aw=obs] if (t1~=. & elg98~=. & sch98v1~=. & p1~=. & Istd2~=.), robust cluster(sch98v1) 
	/*first and second year treatment*/	
	regress prs t1 t2 p1 Y98sap* sap* Istd* Isem* [aw=obs] if (t1~=. & elg98~=. & sch98v1~=. & p1~=. & Istd2~=.), robust cluster(sch98v1) 

**************************************************
