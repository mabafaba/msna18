* ------------------------------------------------------------------------------
* MCNA WASH Syntax .do File
* A.Gualtieri - Junior Assessment Officer
* 19/08/2018
* V1.0
* ------------------------------------------------------------------------------


*** WASH ***
	
	*use "C:\Users\ACTED\Desktop\MCNA_Household Dataset_130818"
	
	*% of HH with no access to sufficient quantity drinking water
	codebook water_tank_cap water_tank_ref water_tank_ppl
	codebook water_tank
	
	tab water_tank_cap,m
	tab water_tank_cap hh_type , m
	
	recode water_tank_cap (88=.)  (100=1000) (2044 = 2000) (10000 = 1000), gen(wash_cap)
	codebook wash_cap
	
	recode water_tank_ref (88=.), gen(wash_ref)
	codebook wash_ref
	
	recode water_tank_ppl (88=.), gen(wash_ppl)
	codebook wash_ppl
	
	gen wash_water =.
		replace wash_water =. if wash_cap == .
		replace wash_water = ((wash_cap*wash_ref)/wash_ppl)/7
	tab wash_water, m

	
	*MCNA indicator
	codebook wash_water water_tank
	br water_tank wash_water, nolabel
	
	gen MCNA_wash_water =. 
		replace MCNA_wash_water = 1 if water_tank == 1
		replace MCNA_wash_water = 1 if water_tank == 2 & wash_water < 50 & !missing(wash_water)
		replace MCNA_wash_water = 0 if water_tank == 2 & wash_water >= 50 & !missing(wash_water)
		
	codebook MCNA_wash_water
	*drop MCNA_wash_water
	br water_tank wash_water MCNA_wash_water, nolabel
	
	
	tab district hh_type [aw=weight_nat], sum(MCNA_wash_water) means noobs
	tab governorate hh_type [aw=weight_nat], sum(MCNA_wash_water) means noobs
	
	tab hh_type MCNA_wash_water [aw=weight_nat], row nofreq m
	
	*Weights across population
	codebook hh_type
	tab hh_type MCNA_wash_water [aw=weight_cross], row nofreq m
	
	tab district hh_type [aw=weight_cross], sum(MCNA_wash_water) means noobs
	
	*% of HH without access to safe water of good quality
	codebook water_treat
	
	gen wash_treat =.
		replace wash_treat = 0 if water_not == 1
		replace wash_treat = 1 if water_boil == 1 | water_filter == 1 | water_chlor == 1 | water_wother == 1
	codebook wash_treat
	
	br water_source wash_treat
	codebook water_source
	
	gen MCNA_wash_treat =.
		replace MCNA_wash_treat = 1 if wash_treat == 1
		replace MCNA_wash_treat = 0 if wash_treat == 0 | water_source == 6
		
	codebook MCNA_wash_treat
	br water_source wash_treat MCNA_wash_treat
	
	*drop MCNA_wash_treat
	
	tab district hh_type [aw=weight_nat], sum(MCNA_wash_treat) means noobs
	tab	governorate hh_type [aw=weight_nat], sum(MCNA_wash_treat) means noobs
	
	tab hh_type MCNA_wash_treat [aw=weight_nat], row nofreq
	
	*% of HH without access to private or communal latrines
	codebook wash_latrines
	
	gen MCNA_wash_lat=.
		replace MCNA_wash_lat = 1 if wash_lat_public == 1 | wash_lat_none == 1 | wash_lat_communal == 1
		replace MCNA_wash_lat = 0 if wash_lat_private == 1
	codebook MCNA_wash_lat
	
	*drop MCNA_wash_lat
	
	tab district hh_type [aw=weight_nat], sum(MCNA_wash_lat) means noobs
	tab governorate hh_type [aw=weight_nat], sum(MCNA_wash_lat) means noobs
	
	tab hh_type MCNA_wash_lat[aw=weight_nat], row nofreq
	
	*% of HH reporting being unaware of appropriate hygiene promotion messages or having no access hygiene items
	codebook hyg_practices
	codebook hygfem_items
	
	recode hyg_practices (.=.) (1 = 1) (2 = 0), gen(wash_pract)
	recode hygfem_items  (.=.) (1 = 1) (2 = 0), gen(wash_items)
	codebook wash_pract wash_items
	
	gen MCNA_wash_hyg =.
		replace MCNA_wash_hyg = 0 if wash_pract == 0 | wash_items == 0
		replace MCNA_wash_hyg = 1 if wash_pract == 1 | wash_items == 1
	codebook MCNA_wash_hyg
	
	tab district hh_type [aw=weight_nat], sum(MCNA_wash_hyg) means noobs
	tab governorate hh_type [aw=weight_nat], sum(MCNA_wash_hyg) means noobs
	
	tab hh_type MCNA_wash_hyg [aw=weight_nat], row nofreq
	
	*% of HH reporting no access to waste collection or communal garbage bins
	codebook waste
	
	recode waste (.=.) (1 4/6 = 1) (2 3 = 0), gen(MCNA_wash_waste)
	codebook MCNA_wash_waste
	
	tab district hh_type [aw=weight_nat], sum(MCNA_wash_waste) means noobs
	tab governorate hh_type [aw=weight_nat], sum(MCNA_wash_waste) means noobs
	
	tab hh_type MCNA_wash_waste [aw=weight_nat], row nofreq
	
*-------------------------------------------------------------------------------
* VULNERABIITY INDEX - Addressing missing values
*-------------------------------------------------------------------------------
  
	local indicators MCNA_wash_water MCNA_wash_treat MCNA_wash_lat MCNA_wash_pract MCNA_wash_waste
		
	foreach var in `indicators' {
		gen `var'_miss = 00
			replace `var'_miss = 1 if `var' == .
			lab var `var'_miss "Missing values in `var'"
		}
		
	sum *_miss
	
	egen n_missing = rowmiss(`indicators')
		lab var n_missing "Number of missing"
		
	gen missing = (n_missing>0)
		lab var missing "At least on missing"
		
	sum missing
	drop *_miss
	drop n_missing
	drop missing
	
*-------------------------------------------------------------------------------
* VULNERABIITY INDEX - Addressing missing values
*-------------------------------------------------------------------------------	
	
	mpi d1(MCNA_wash_water) d2(MCNA_wash_treat MCNA_wash_waste) d3(MCNA_wash_lat MCNA_wash_hyg) w1(0.3) w2(0.15 0.15) w3(0.2 0.2), cutoff(0.25) deprivedscore(wash_score)
	codebook wash_score
	
	tab district hh_type [aw=weight_nat], sum(wash_score) means noobs
	tab governorate hh_type [aw=weight_nat], sum(wash_score) means noobs
	
	recode wash_score (0/0.20=0) (0.21/1=1), gen(wash_score1)
	codebook wash_score1
	*drop wash_score1
	
	tab hh_type wash_score1 [aw=weight_nat], row nofreq
	
	*Across population
	tab district hh_type [aw=weight_cross], sum(wash_score) means noobs
	tab governorate hh_type [aw=weight_cross], sum(wash_score) means noobs
