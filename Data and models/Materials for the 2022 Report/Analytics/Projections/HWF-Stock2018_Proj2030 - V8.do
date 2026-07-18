****************************************
**                                    **
** HWF stock 2018 and projection 2030 (2) **
**                                    **
**                                    **
** MB                                 **
****************************************
* Last update 19/05/2022
set more off
clear
cd "C:\Users\boniolm\OneDrive - World Health Organization\Articles\Published\Workforce 2018 and projection to 2030"
* Import each csv files: countrycode, pop, numworkers
*import delimited "C:\Users\boniolm\OneDrive - World Health Organization\Articles\Workforce 2018 and projection to 2030\AllData-20201021.csv"
*drop ïtodel0 todel1 todel2 todel3 todel4 todel5 todel6
*drop if iso3=="PRI" | iso3=="TKL"
*save AllData, replace
*describe

clear
import delimited "C:\Users\boniolm\OneDrive - World Health Organization\Articles\Published\Workforce 2018 and projection to 2030\Rawdata-20220104.csv"

describe
rename orgunitlevel2 region
rename organisation~me country
rename organisation~de iso3
rename periodid year
rename populationun pop
rename nhwa_medicald~l medtot
rename nhwa_generalm~s medgp
rename nhwa_speciali~n medspe
rename nhwa_medicald~f mednfd
rename nhwa_medicald~o medage1
rename v18 medage2
rename v19 medage3
rename v20 medage4
rename v21 medage5
rename v22 medage6
rename nhwa_graduate~t medgrad
rename nhwa_nursingp~l nurtot
rename nhwa_nursingp~o nurpro
rename nhwa_nursinga~a nurassoc
rename nhwa_nursesno~c nurnfd
rename nhwa_nursingp~g nurage1
rename v29 nurage2
rename v30 nurage3
rename v31 nurage4
rename v32 nurage5
rename v33 nurage6
rename v34 nurgrad
rename nhwa_midwifer~a midtot
rename nhwa_midwifer~k midpro
rename nhwa_midwifer~o midassoc
rename nhwa_midwives~t midnfd
rename nhwa_midwifer~g midage1
rename v40 midage2
rename v41 midage3
rename v42 midage4
rename v43 midage5
rename v44 midage6
rename v45 midgrad
rename nhwa_dentists~l dentot
rename nhwa_denti~25yr denage1
rename nhwa_denti~2534 denage2
rename nhwa_denti~3544 denage3
rename nhwa_denti~4554 denage4
rename nhwa_denti~5564 denage5
rename nhwa_denti~65yr denage6
rename v53 dengrad
rename nhwa_pharmaci~l phatot
rename nhwa_pharmaci~2 phaage1
rename v56 phaage2
rename nhwa_pharmaci~3 phaage3
rename nhwa_pharmaci~4 phaage4
rename nhwa_pharmaci~5 phaage5
rename nhwa_pharmaci~6 phaage6
rename v61 phagrad

drop orgunitlevel1 orgunitlevel3 organisationu~d organisationu~n periodname periodcode perioddescrip~n
save AllData, replace




* medical doctors
clear
use AllData
keep region country iso3 year pop medtot medgp medspe mednfd medage1 medage2 medage3 medage4 medage5 medage6 medgrad 
replace medtot=. if medtot==0
replace medgp=. if medgp==0
replace medspe=. if medspe==0
replace mednfd=. if mednfd==0
replace medage1=. if medage1==0
replace medage2=. if medage2==0
replace medage3=. if medage3==0
replace medage4=. if medage4==0
replace medage5=. if medage5==0
replace medage6=. if medage6==0
replace medgrad=. if medgrad==0
egen medNbpoints=count(medtot), by(iso3)
generate Meddensity=medtot/pop*10000 if medtot!=.
save MedData , replace
* number of countries with data per region
tab region if medNbpoints>0 & year==2020
* countries with missing data
tab country if medNbpoints==0
* extracting latest density before 2020
egen medMaxyear=max(year) if medtot!=. & year<2021, by(iso3)
drop if medMaxyear!=year
rename Meddensity MaxMeddensity
*Max year by region
tab  medMaxyear region
* number of points by region
tab  medNbpoints region
* aggregation max year density
keep iso3 MaxMeddensity medMaxyear
save MaxYear,replace
clear
use MedData
merge m:1 iso3 using MaxYear
drop _merge
* sorting by region country and descending year
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
** extracting latest graduation stat
egen medgradMaxyear=max(year) if medgrad!=., by(iso3)
generate Maxmedgrad=.
replace Maxmedgrad=medgrad if medgradMaxyear==year
egen medgradMaxyear2=max(medgradMaxyear), by(iso3)
egen Maxmedgrad2=max(Maxmedgrad) , by(iso3)
drop medgradMaxyear Maxmedgrad
rename medgradMaxyear2 medgradMaxyear
rename Maxmedgrad2 Maxmedgrad
* number of countries reporting graduates by region
tab region if medgradMaxyear!=. & year==2018
** extracting latest percentage of age above 55 stat
* replacing with 0 if at least age 35-44 is not missing (otherwise considered as missing)
replace medage1=0 if medage1==. & medage3!=.
replace medage2=0 if medage2==. & medage3!=.
replace medage4=0 if medage4==. & medage3!=.
replace medage5=0 if medage5==. & medage3!=.
replace medage6=0 if medage6==. & medage3!=.
generate medage55pct=.
replace medage55pct=(medage5+medage6)/(medage1+medage2+medage3+medage4+medage5+medage6) if medage3!=.
* remove values equal to zero
replace medage55pct=. if medage55pct==0
* extract latest
egen medageMaxyear=max(year) if medage55pct!=., by(iso3)
generate Maxmedage55pct=.
generate Maxmedage1=.
generate Maxmedage2=.
generate Maxmedage3=.
generate Maxmedage4=.
generate Maxmedage5=.
generate Maxmedage6=.
replace Maxmedage55pct=medage55pct if medageMaxyear==year
replace Maxmedage1=medage1 if medageMaxyear==year
replace Maxmedage2=medage2 if medageMaxyear==year
replace Maxmedage3=medage3 if medageMaxyear==year
replace Maxmedage4=medage4 if medageMaxyear==year
replace Maxmedage5=medage5 if medageMaxyear==year
replace Maxmedage6=medage6 if medageMaxyear==year
egen Maxmedage55pct2=max(Maxmedage55pct) , by(iso3)
egen Maxmedage1_2=max(Maxmedage1) , by(iso3)
egen Maxmedage2_2=max(Maxmedage2) , by(iso3)
egen Maxmedage3_2=max(Maxmedage3) , by(iso3)
egen Maxmedage4_2=max(Maxmedage4) , by(iso3)
egen Maxmedage5_2=max(Maxmedage5) , by(iso3)
egen Maxmedage6_2=max(Maxmedage6) , by(iso3)
egen medageMaxyear2=max(medageMaxyear), by(iso3)
drop medageMaxyear Maxmedage55pct Maxmedage1 Maxmedage2 Maxmedage3 Maxmedage4 Maxmedage5 Maxmedage6
rename medageMaxyear2 medageMaxyear
rename Maxmedage55pct2 Maxmedage55pct
rename Maxmedage1_2 Maxmedage1
rename Maxmedage2_2 Maxmedage2
rename Maxmedage3_2 Maxmedage3
rename Maxmedage4_2 Maxmedage4
rename Maxmedage5_2 Maxmedage5
rename Maxmedage6_2 Maxmedage6
tab region if medageMaxyear!=. & year==2020
save MedData , replace


* nursing personnel
clear
use AllData
keep region country iso3 year pop nurtot nurpro nurassoc nurnfd nurage1 nurage2 nurage3 nurage4 nurage5 nurage6 nurgrad
replace nurtot=. if nurtot==0
replace nurpro=. if nurpro==0
replace nurassoc=. if nurassoc==0
replace nurnfd=. if nurnfd==0
replace nurage1=. if nurage1==0
replace nurage2=. if nurage2==0
replace nurage3=. if nurage3==0
replace nurage4=. if nurage4==0
replace nurage5=. if nurage5==0
replace nurage6=. if nurage6==0
replace nurgrad=. if nurgrad==0
* correcting a glitch for Canada
* replace nurtot=368664 if year==2018 & iso3=="CAN"
egen nurNbpoints=count(nurtot), by(iso3)
generate Nurdensity=nurtot/pop*10000 if nurtot!=.
save NurData , replace
* number of countries with data per region
tab region if nurNbpoints>0 & year==2020
* countries with missing data
tab country if nurNbpoints==0
* extracting latest density before 2020
egen nurMaxyear=max(year) if nurtot!=. & year<2021, by(iso3)
drop if nurMaxyear!=year
rename Nurdensity MaxNurdensity
*Max year by region
tab  nurMaxyear region
* number of points by region
tab  nurNbpoints region
* aggregation max year density
keep iso3 MaxNurdensity nurMaxyear
save MaxYear,replace
clear
use NurData
merge m:1 iso3 using MaxYear
drop _merge
* sorting by region country and descending year
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
** extracting latest graduation stat
egen nurgradMaxyear=max(year) if nurgrad!=., by(iso3)
generate Maxnurgrad=.
replace Maxnurgrad=nurgrad if nurgradMaxyear==year
egen nurgradMaxyear2=max(nurgradMaxyear), by(iso3)
egen Maxnurgrad2=max(Maxnurgrad) , by(iso3)
drop nurgradMaxyear Maxnurgrad
rename nurgradMaxyear2 nurgradMaxyear
rename Maxnurgrad2 Maxnurgrad
tab region if nurgradMaxyear!=. & year==2018
** extracting latest percentage of age above 55 stat
* replacing with 0 if at least age 35-44 is not missing (otherwise considered as missing)
replace nurage1=0 if nurage1==. & nurage3!=.
replace nurage2=0 if nurage2==. & nurage3!=.
replace nurage4=0 if nurage4==. & nurage3!=.
replace nurage5=0 if nurage5==. & nurage3!=.
replace nurage6=0 if nurage6==. & nurage3!=.
generate nurage55pct=.
replace nurage55pct=(nurage5+nurage6)/(nurage1+nurage2+nurage3+nurage4+nurage5+nurage6) if nurage3!=.
* remove values equal to zero
replace nurage55pct=. if nurage55pct==0
* extract latest
egen nurageMaxyear=max(year) if nurage55pct!=., by(iso3)
generate Maxnurage55pct=.
generate Maxnurage1=.
generate Maxnurage2=.
generate Maxnurage3=.
generate Maxnurage4=.
generate Maxnurage5=.
generate Maxnurage6=.
replace Maxnurage55pct=nurage55pct if nurageMaxyear==year
replace Maxnurage1=nurage1 if nurageMaxyear==year
replace Maxnurage2=nurage2 if nurageMaxyear==year
replace Maxnurage3=nurage3 if nurageMaxyear==year
replace Maxnurage4=nurage4 if nurageMaxyear==year
replace Maxnurage5=nurage5 if nurageMaxyear==year
replace Maxnurage6=nurage6 if nurageMaxyear==year
egen Maxnurage55pct2=max(Maxnurage55pct) , by(iso3)
egen Maxnurage1_2=max(Maxnurage1) , by(iso3)
egen Maxnurage2_2=max(Maxnurage2) , by(iso3)
egen Maxnurage3_2=max(Maxnurage3) , by(iso3)
egen Maxnurage4_2=max(Maxnurage4) , by(iso3)
egen Maxnurage5_2=max(Maxnurage5) , by(iso3)
egen Maxnurage6_2=max(Maxnurage6) , by(iso3)
egen nurageMaxyear2=max(nurageMaxyear), by(iso3)
drop nurageMaxyear Maxnurage55pct Maxnurage1 Maxnurage2 Maxnurage3 Maxnurage4 Maxnurage5 Maxnurage6
rename nurageMaxyear2 nurageMaxyear
rename Maxnurage55pct2 Maxnurage55pct
rename Maxnurage1_2 Maxnurage1
rename Maxnurage2_2 Maxnurage2
rename Maxnurage3_2 Maxnurage3
rename Maxnurage4_2 Maxnurage4
rename Maxnurage5_2 Maxnurage5
rename Maxnurage6_2 Maxnurage6
tab region if nurageMaxyear!=. & year==2020
save NurData , replace


* midwifery personnel
clear
use AllData
keep region country iso3 year pop nurtot midtot midpro midassoc midnfd midage1 midage2 midage3 midage4 midage5 midage6 midgrad
replace nurtot=. if nurtot==0
replace midtot=. if midtot==0
replace midpro=. if midpro==0
replace midassoc=. if midassoc==0
replace midnfd=. if midnfd==0
replace midage1=. if midage1==0
replace midage2=. if midage2==0
replace midage3=. if midage3==0
replace midage4=. if midage4==0
replace midage5=. if midage5==0
replace midage6=. if midage6==0
replace midgrad=. if midgrad==0
* manually correct an error for Mongolia (outlier unreliable)
* replace midtot=. if iso3=="MNG" & year>2012
egen midNbpoints=count(midtot), by(iso3)
generate Middensity=midtot/pop*10000 if midtot!=.
save MidData , replace
* number of countries with data per region
tab region if midNbpoints>0 & year==2020
* countries with missing data
tab country if midNbpoints==0
* extracting latest density before 2020
egen nurMaxyear=max(year) if nurtot!=. & year<2021, by(iso3)
egen midMaxyear=max(year) if midtot!=. & year<2021, by(iso3)
egen nur2Maxyear=max(nurMaxyear), by(iso3)
drop nurMaxyear
rename nur2Maxyear nurMaxyear
drop if midMaxyear!=year
rename Middensity MaxMiddensity
*Max year by region
tab  midMaxyear region
* number of points by region
tab  midNbpoints region
* aggregation max year density
keep iso3 MaxMiddensity midMaxyear nurMaxyear
save MaxYear,replace
clear
use MidData
merge m:1 iso3 using MaxYear
drop _merge
* sorting by region country and descending year
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
* Some countries changed their reporting of midwifery data (ex: India) and now report midwifery data merged with nursing. 
* As a result the max year for midwifery data is lower then max year for nursing data (max year for midwifery corresponding to period where 
* both were reported). In these cases, there is a risk of double counting and only the nursing density should be used, so a flag is created 
* and the max year and density is removing for midwifery in these situations.
generate flagnurmiddoublecount=0
replace flagnurmiddoublecount=1 if midMaxyear<nurMaxyear
replace MaxMiddensity=. if midMaxyear<nurMaxyear
replace midMaxyear=. if midMaxyear<nurMaxyear
drop nurtot nurMaxyear
** extracting latest graduation stat
egen midgradMaxyear=max(year) if midgrad!=., by(iso3)
generate Maxmidgrad=.
replace Maxmidgrad=midgrad if midgradMaxyear==year
egen midgradMaxyear2=max(midgradMaxyear), by(iso3)
egen Maxmidgrad2=max(Maxmidgrad) , by(iso3)
drop midgradMaxyear Maxmidgrad
rename midgradMaxyear2 midgradMaxyear
rename Maxmidgrad2 Maxmidgrad
tab region if midgradMaxyear!=. & year==2020
** extracting latest percentage of age above 55 stat
* replacing with 0 if at least age 35-44 is not missing (otherwise considered as missing)
replace midage1=0 if midage1==. & midage3!=.
replace midage2=0 if midage2==. & midage3!=.
replace midage4=0 if midage4==. & midage3!=.
replace midage5=0 if midage5==. & midage3!=.
replace midage6=0 if midage6==. & midage3!=.
generate midage55pct=.
replace midage55pct=(midage5+midage6)/(midage1+midage2+midage3+midage4+midage5+midage6) if midage3!=.
* remove values equal to zero
replace midage55pct=. if midage55pct==0
* extract latest
egen midageMaxyear=max(year) if midage55pct!=., by(iso3)
generate Maxmidage55pct=.
generate Maxmidage1=.
generate Maxmidage2=.
generate Maxmidage3=.
generate Maxmidage4=.
generate Maxmidage5=.
generate Maxmidage6=.
replace Maxmidage55pct=midage55pct if midageMaxyear==year
replace Maxmidage1=midage1 if midageMaxyear==year
replace Maxmidage2=midage2 if midageMaxyear==year
replace Maxmidage3=midage3 if midageMaxyear==year
replace Maxmidage4=midage4 if midageMaxyear==year
replace Maxmidage5=midage5 if midageMaxyear==year
replace Maxmidage6=midage6 if midageMaxyear==year
egen Maxmidage55pct2=max(Maxmidage55pct) , by(iso3)
egen Maxmidage1_2=max(Maxmidage1) , by(iso3)
egen Maxmidage2_2=max(Maxmidage2) , by(iso3)
egen Maxmidage3_2=max(Maxmidage3) , by(iso3)
egen Maxmidage4_2=max(Maxmidage4) , by(iso3)
egen Maxmidage5_2=max(Maxmidage5) , by(iso3)
egen Maxmidage6_2=max(Maxmidage6) , by(iso3)
egen midageMaxyear2=max(midageMaxyear), by(iso3)
drop midageMaxyear Maxmidage55pct Maxmidage1 Maxmidage2 Maxmidage3 Maxmidage4 Maxmidage5 Maxmidage6
rename midageMaxyear2 midageMaxyear
rename Maxmidage55pct2 Maxmidage55pct
rename Maxmidage1_2 Maxmidage1
rename Maxmidage2_2 Maxmidage2
rename Maxmidage3_2 Maxmidage3
rename Maxmidage4_2 Maxmidage4
rename Maxmidage5_2 Maxmidage5
rename Maxmidage6_2 Maxmidage6
tab region if midageMaxyear!=. & year==2020
save MidData , replace


* dentists
clear
use AllData
keep region country iso3 year pop dentot denage1 denage2 denage3 denage4 denage5 denage6 dengrad
replace dentot=. if dentot==0
replace denage1=. if denage1==0
replace denage2=. if denage2==0
replace denage3=. if denage3==0
replace denage4=. if denage4==0
replace denage5=. if denage5==0
replace denage6=. if denage6==0
replace dengrad=. if dengrad==0
* correcting a glitch for Canada
* replace dentot=24731 if year==2018 & iso3=="CAN"
egen denNbpoints=count(dentot), by(iso3)
generate Dendensity=dentot/pop*10000 if dentot!=.
save DenData , replace
* number of countries with data per region
tab region if denNbpoints>0 & year==2020
* countries with missing data
tab country if denNbpoints==0
* extracting latest density before 2020
egen denMaxyear=max(year) if dentot!=. & year<2021, by(iso3)
drop if denMaxyear!=year
rename Dendensity MaxDendensity
*Max year by region
tab  denMaxyear region
* number of points by region
tab  denNbpoints region
* aggregation max year density
keep iso3 MaxDendensity denMaxyear
save MaxYear,replace
clear
use DenData
merge m:1 iso3 using MaxYear
drop _merge
* sorting by region country and descending year
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
** extracting latest graduation stat
egen dengradMaxyear=max(year) if dengrad!=., by(iso3)
generate Maxdengrad=.
replace Maxdengrad=dengrad if dengradMaxyear==year
egen dengradMaxyear2=max(dengradMaxyear), by(iso3)
egen Maxdengrad2=max(Maxdengrad) , by(iso3)
drop dengradMaxyear Maxdengrad
rename dengradMaxyear2 dengradMaxyear
rename Maxdengrad2 Maxdengrad
tab region if dengradMaxyear!=. & year==2020
** extracting latest percentage of age above 55 stat
* replacing with 0 if at least age 35-44 is not missing (otherwise considered as missing)
replace denage1=0 if denage1==. & denage3!=.
replace denage2=0 if denage2==. & denage3!=.
replace denage4=0 if denage4==. & denage3!=.
replace denage5=0 if denage5==. & denage3!=.
replace denage6=0 if denage6==. & denage3!=.
generate denage55pct=.
replace denage55pct=(denage5+denage6)/(denage1+denage2+denage3+denage4+denage5+denage6) if denage3!=.
* remove values equal to zero
replace denage55pct=. if denage55pct==0
* extract latest
egen denageMaxyear=max(year) if denage55pct!=., by(iso3)
generate Maxdenage55pct=.
generate Maxdenage1=.
generate Maxdenage2=.
generate Maxdenage3=.
generate Maxdenage4=.
generate Maxdenage5=.
generate Maxdenage6=.
replace Maxdenage55pct=denage55pct if denageMaxyear==year
replace Maxdenage1=denage1 if denageMaxyear==year
replace Maxdenage2=denage2 if denageMaxyear==year
replace Maxdenage3=denage3 if denageMaxyear==year
replace Maxdenage4=denage4 if denageMaxyear==year
replace Maxdenage5=denage5 if denageMaxyear==year
replace Maxdenage6=denage6 if denageMaxyear==year
egen Maxdenage55pct2=max(Maxdenage55pct) , by(iso3)
egen Maxdenage1_2=max(Maxdenage1) , by(iso3)
egen Maxdenage2_2=max(Maxdenage2) , by(iso3)
egen Maxdenage3_2=max(Maxdenage3) , by(iso3)
egen Maxdenage4_2=max(Maxdenage4) , by(iso3)
egen Maxdenage5_2=max(Maxdenage5) , by(iso3)
egen Maxdenage6_2=max(Maxdenage6) , by(iso3)
egen denageMaxyear2=max(denageMaxyear), by(iso3)
drop denageMaxyear Maxdenage55pct Maxdenage1 Maxdenage2 Maxdenage3 Maxdenage4 Maxdenage5 Maxdenage6
rename denageMaxyear2 denageMaxyear
rename Maxdenage55pct2 Maxdenage55pct
rename Maxdenage1_2 Maxdenage1
rename Maxdenage2_2 Maxdenage2
rename Maxdenage3_2 Maxdenage3
rename Maxdenage4_2 Maxdenage4
rename Maxdenage5_2 Maxdenage5
rename Maxdenage6_2 Maxdenage6
tab region if denageMaxyear!=. & year==2020
save DenData , replace


* pharmacists
clear
use AllData
keep region country iso3 year pop phatot phaage1 phaage2 phaage3 phaage4 phaage5 phaage6 phagrad
replace phatot=. if phatot==0
replace phaage1=. if phaage1==0
replace phaage2=. if phaage2==0
replace phaage3=. if phaage3==0
replace phaage4=. if phaage4==0
replace phaage5=. if phaage5==0
replace phaage6=. if phaage6==0
replace phagrad=. if phagrad==0
egen phaNbpoints=count(phatot), by(iso3)
generate Phadensity=phatot/pop*10000 if phatot!=.
save PhaData , replace
* number of countries with data per region
tab region if phaNbpoints>0 & year==2020
* countries with missing data
tab country if phaNbpoints==0
* extracting latest phasity before 2020
egen phaMaxyear=max(year) if phatot!=. & year<2021, by(iso3)
drop if phaMaxyear!=year
rename Phadensity MaxPhadensity
*Max year by region
tab  phaMaxyear region
* number of points by region
tab  phaNbpoints region
* aggregation max year phasity
keep iso3 MaxPhadensity phaMaxyear
save MaxYear,replace
clear
use PhaData
merge m:1 iso3 using MaxYear
drop _merge
* sorting by region country and descending year
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
** extracting latest graduation stat
egen phagradMaxyear=max(year) if phagrad!=., by(iso3)
generate Maxphagrad=.
replace Maxphagrad=phagrad if phagradMaxyear==year
egen phagradMaxyear2=max(phagradMaxyear), by(iso3)
egen Maxphagrad2=max(Maxphagrad) , by(iso3)
drop phagradMaxyear Maxphagrad
rename phagradMaxyear2 phagradMaxyear
rename Maxphagrad2 Maxphagrad
tab region if phagradMaxyear!=. & year==2020
** extracting latest percentage of age above 55 stat
* replacing with 0 if at least age 35-44 is not missing (otherwise considered as missing)
replace phaage1=0 if phaage1==. & phaage3!=.
replace phaage2=0 if phaage2==. & phaage3!=.
replace phaage4=0 if phaage4==. & phaage3!=.
replace phaage5=0 if phaage5==. & phaage3!=.
replace phaage6=0 if phaage6==. & phaage3!=.
generate phaage55pct=.
replace phaage55pct=(phaage5+phaage6)/(phaage1+phaage2+phaage3+phaage4+phaage5+phaage6) if phaage3!=.
* remove values equal to zero
replace phaage55pct=. if phaage55pct==0
* extract latest
egen phaageMaxyear=max(year) if phaage55pct!=., by(iso3)
generate Maxphaage55pct=.
generate Maxphaage1=.
generate Maxphaage2=.
generate Maxphaage3=.
generate Maxphaage4=.
generate Maxphaage5=.
generate Maxphaage6=.
replace Maxphaage55pct=phaage55pct if phaageMaxyear==year
replace Maxphaage1=phaage1 if phaageMaxyear==year
replace Maxphaage2=phaage2 if phaageMaxyear==year
replace Maxphaage3=phaage3 if phaageMaxyear==year
replace Maxphaage4=phaage4 if phaageMaxyear==year
replace Maxphaage5=phaage5 if phaageMaxyear==year
replace Maxphaage6=phaage6 if phaageMaxyear==year
egen Maxphaage55pct2=max(Maxphaage55pct) , by(iso3)
egen Maxphaage1_2=max(Maxphaage1) , by(iso3)
egen Maxphaage2_2=max(Maxphaage2) , by(iso3)
egen Maxphaage3_2=max(Maxphaage3) , by(iso3)
egen Maxphaage4_2=max(Maxphaage4) , by(iso3)
egen Maxphaage5_2=max(Maxphaage5) , by(iso3)
egen Maxphaage6_2=max(Maxphaage6) , by(iso3)
egen phaageMaxyear2=max(phaageMaxyear), by(iso3)
drop phaageMaxyear Maxphaage55pct Maxphaage1 Maxphaage2 Maxphaage3 Maxphaage4 Maxphaage5 Maxphaage6
rename phaageMaxyear2 phaageMaxyear
rename Maxphaage55pct2 Maxphaage55pct
rename Maxphaage1_2 Maxphaage1
rename Maxphaage2_2 Maxphaage2
rename Maxphaage3_2 Maxphaage3
rename Maxphaage4_2 Maxphaage4
rename Maxphaage5_2 Maxphaage5
rename Maxphaage6_2 Maxphaage6
tab region if phaageMaxyear!=. & year==2020
save PhaData , replace



* Bringing all data in one data set
clear
use MedData
merge m:1 region country iso3 year pop using NurData
drop _merge
merge m:1 region country iso3 year pop using MidData
drop _merge
merge m:1 region country iso3 year pop using DenData
drop _merge
merge m:1 region country iso3 year pop using PhaData
drop _merge
replace country="Cote d'Ivoire" if iso3=="CIV"
save SDG3cDataAge, replace
drop medgp medspe mednfd medage1 medage2 medage3 medage4 medage5 medage6 nurpro ///
 nurassoc nurnfd nurage1 nurage2 nurage3 nurage4 nurage5 nurage6 midpro midassoc ///
 midnfd midage1 midage2 midage3 midage4 midage5 midage6 denage1 denage2 denage3 ///
 denage4 denage5 denage6 phaage1 phaage2 phaage3 phaage4 phaage5 phaage6
 save SDG3cData, replace
 
 
 
 
 

 * comparing age groups across occupations to see which one match the best
clear
use SDG3cDataAge
keep region country iso3 year pop ///
medtot medage1 medage2 medage3 medage4 medage5 medage6 ///
nurtot nurage1 nurage2 nurage3 nurage4 nurage5 nurage6 ///
midtot midage1 midage2 midage3 midage4 midage5 midage6 ///
dentot denage1 denage2 denage3 denage4 denage5 denage6 ///
phatot phaage1 phaage2 phaage3 phaage4 phaage5 phaage6
keep if medage4!=.
keep if nurage4!=.
collapse (sum) medtot medage1 medage2 medage3 medage4 medage5 medage6 ///
nurtot nurage1 nurage2 nurage3 nurage4 nurage5 nurage6 ///
midtot midage1 midage2 midage3 midage4 midage5 midage6 ///
dentot denage1 denage2 denage3 denage4 denage5 denage6 ///
phatot phaage1 phaage2 phaage3 phaage4 phaage5 phaage6, by (region country iso3)




* Getting UNPP data
* using script to extract UNPP with WBI and ISO3 codes
do "C:\Users\boniolm\OneDrive - World Health Organization\Databases\UNPP\ExtractUNPP2019.do"
cd "C:\Users\boniolm\OneDrive - World Health Organization\Articles\Published\Workforce 2018 and projection to 2030"
save UNPP,replace


clear
use UNPP
drop if year!=2030
rename poptot pop2030
keep iso3 wbi pop2030
merge 1:m iso3 using SDG3cData
drop _merge
save SDG3cData, replace
clear
use UNPP
drop if year!=2021
rename poptot pop2021
keep iso3 pop2021
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2022
rename poptot pop2022
keep iso3 pop2022
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2023
rename poptot pop2023
keep iso3 pop2023
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2024
rename poptot pop2024
keep iso3 pop2024
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2025
rename poptot pop2025
keep iso3 pop2025
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2026
rename poptot pop2026
keep iso3 pop2026
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2027
rename poptot pop2027
keep iso3 pop2027
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2028
rename poptot pop2028
keep iso3 pop2028
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2029
rename poptot pop2029
keep iso3 pop2029
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2032
rename poptot pop2032
keep iso3 pop2032
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2018
rename poptot pop2018
generate pop2018age1=pop20_24
generate pop2018age2=pop25_29+pop30_34
generate pop2018age3=pop35_39+pop40_44
generate pop2018age4=pop45_49+pop50_54
generate pop2018age5=pop55_59+pop60_64
generate pop2018age6=pop65_69
keep iso3 pop2018 pop2018age1 pop2018age2 pop2018age3 pop2018age4 pop2018age5 pop2018age6
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace
clear
use UNPP
drop if year!=2020
rename poptot pop2020
generate pop2020age1=pop20_24
generate pop2020age2=pop25_29+pop30_34
generate pop2020age3=pop35_39+pop40_44
generate pop2020age4=pop45_49+pop50_54
generate pop2020age5=pop55_59+pop60_64
generate pop2020age6=pop65_69
keep iso3 pop2020 pop2020age1 pop2020age2 pop2020age3 pop2020age4 pop2020age5 pop2020age6
merge 1:m iso3 using SDG3cData
drop _merge
sort region iso3 year
save SDG3cData, replace






**** Computing baseline 2020 ****
clear
use SDG3cData
drop if year!=2020
keep region country iso3 MaxMeddensity medMaxyear MaxNurdensity nurMaxyear MaxMiddensity midMaxyear flagnurmiddoublecount MaxDendensity denMaxyear MaxPhadensity phaMaxyear
order region country iso3 MaxMeddensity medMaxyear MaxNurdensity nurMaxyear MaxMiddensity midMaxyear flagnurmiddoublecount MaxDendensity denMaxyear MaxPhadensity phaMaxyear
tab region if MaxMeddensity!=.
tab region if MaxNurdensity!=.
tab region if MaxMiddensity!=.
tab region if flagnurmiddoublecount==0
tab region if MaxDendensity!=.
tab region if MaxPhadensity!=.
*histogram MaxMeddensity, kdensity
*histogram MaxNurdensity, kdensity
*histogram MaxMiddensity, kdensity
*histogram MaxDendensity, kdensity
*histogram MaxPhadensity, kdensity
save GPW13Baseline, replace
****** >>>>>>>>>>>>>>>>>>>>>>>>>>
export delimited using "C:\Users\boniolm\OneDrive - World Health Organization\Articles\Published\Workforce 2018 and projection to 2030\HWF-Baseline-2020-20220104.csv", replace










**** Computing baseline 2013 based on latest as of 2013 ****
clear
use SDG3cData
drop if year>2013
keep iso3 wbi region country year pop medtot nurtot midtot dentot phatot
save SDG3cData2013, replace
**************** Med
clear
use SDG3cData2013
egen medMaxyear=max(year) if medtot!=. & year<2014, by(iso3)
drop if medMaxyear!=year
generate MaxMeddensity=medtot/pop*10000
keep iso3 MaxMeddensity medMaxyear
save MaxYear,replace
clear
use SDG3cData2013
merge m:1 iso3 using MaxYear
drop _merge
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
save SDG3cData2013, replace
**************** Nur
clear
use SDG3cData2013
egen nurMaxyear=max(year) if nurtot!=. & year<2014, by(iso3)
drop if nurMaxyear!=year
generate MaxNurdensity=nurtot/pop*10000
keep iso3 MaxNurdensity nurMaxyear
save MaxYear,replace
clear
use SDG3cData2013
merge m:1 iso3 using MaxYear
drop _merge
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
save SDG3cData2013, replace
**************** Mid
clear
use SDG3cData2013
egen midMaxyear=max(year) if midtot!=. & year<2014, by(iso3)
drop if midMaxyear!=year
generate Maxmiddensity=midtot/pop*10000
keep iso3 Maxmiddensity midMaxyear
save MaxYear,replace
clear
use SDG3cData2013
merge m:1 iso3 using MaxYear
drop _merge
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
generate flagnurmiddoublecount=0
replace flagnurmiddoublecount=1 if midMaxyear-nurMaxyear!=0 & midMaxyear!=.
replace Maxmiddensity=. if midMaxyear-nurMaxyear!=0
replace midMaxyear=. if midMaxyear-nurMaxyear!=0
save SDG3cData2013, replace
**************** Den
clear
use SDG3cData2013
egen denMaxyear=max(year) if dentot!=. & year<2014, by(iso3)
drop if denMaxyear!=year
generate MaxDendensity=dentot/pop*10000
keep iso3 MaxDendensity denMaxyear
save MaxYear,replace
clear
use SDG3cData2013
merge m:1 iso3 using MaxYear
drop _merge
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
save SDG3cData2013, replace
**************** Pha
clear
use SDG3cData2013
egen phaMaxyear=max(year) if phatot!=. & year<2014, by(iso3)
drop if phaMaxyear!=year
generate MaxPhadensity=phatot/pop*10000
keep iso3 MaxPhadensity phaMaxyear
save MaxYear,replace
clear
use SDG3cData2013
merge m:1 iso3 using MaxYear
drop _merge
generate sortyear=-year
sort region iso3 sortyear
drop sortyear
save SDG3cData2013, replace
***********************
rename wbi WBI
replace WBI="LIN" if iso3=="SSD"
*Dominica and Saint kitt => LMI as Cuba
replace WBI="LMI" if iso3=="DMA"
replace WBI="LMI" if iso3=="KNA"
*ANdorra , Monaco, San Marion => HIN as France
replace WBI="HIN" if iso3=="AND"
replace WBI="HIN" if iso3=="MCO"
replace WBI="HIN" if iso3=="SMR"
*Cook Island, Marshall, Niue, Nauru, Palau, Tuvalu=> LMI as Fiji
replace WBI="LMI" if iso3=="COK"
replace WBI="LMI" if iso3=="MHL"
replace WBI="LMI" if iso3=="NIU"
replace WBI="LMI" if iso3=="NRU"
replace WBI="LMI" if iso3=="PLW"
replace WBI="LMI" if iso3=="TUV"
**********************
tab WBI region if year==2013
egen emeddensity2013=mean(MaxMeddensity) , by(WBI)
replace emeddensity2013=MaxMeddensity if MaxMeddensity!=.
generate emedtot2013=round(emeddensity2013*pop/10000,1)
egen enurdensity2013=mean(MaxNurdensity) , by(WBI)
replace enurdensity2013=MaxNurdensity if MaxNurdensity!=.
generate enurtot2013=round(enurdensity2013*pop/10000,1)
egen emiddensity2013=mean(Maxmiddensity) , by(WBI)
replace emiddensity2013=Maxmiddensity if Maxmiddensity!=.
replace emiddensity2013=0 if flagnurmiddoublecount==1
generate emidtot2013=round(emiddensity2013*pop/10000,1)
egen edendensity2013=mean(MaxDendensity) , by(WBI)
replace edendensity2013=MaxDendensity if MaxDendensity!=.
generate edentot2013=round(edendensity2013*pop/10000,1)
egen ephadensity2013=mean(MaxPhadensity) , by(WBI)
replace ephadensity2013=MaxPhadensity if MaxPhadensity!=.
generate ephatot2013=round(ephadensity2013*pop/10000,1)
drop if year!=2013
save SDG3cData2013, replace




















**** Computing projections ****
** filling gaps with estimates
clear
use SDG3cData
drop if year!=2020
rename wbi WBI
keep region country iso3 WBI year pop2020 pop2020age1 pop2020age2 pop2020age3 pop2020age4 pop2020age5 pop2020age6 pop2021 pop2022 pop2023 pop2024 pop2025 ///
pop2026 pop2027 pop2028 pop2029 pop2030 pop2032 pop ///
medNbpoints MaxMeddensity medMaxyear medgradMaxyear Maxmedgrad medageMaxyear Maxmedage55pct Maxmedage1 Maxmedage2 Maxmedage3 Maxmedage4 Maxmedage5 Maxmedage6 ///
nurNbpoints MaxNurdensity nurMaxyear nurgradMaxyear Maxnurgrad nurageMaxyear Maxnurage55pct Maxnurage1 Maxnurage2 Maxnurage3 Maxnurage4 Maxnurage5 Maxnurage6 ///
midNbpoints MaxMiddensity midMaxyear flagnurmiddoublecount midgradMaxyear Maxmidgrad midageMaxyear Maxmidage55pct Maxmidage1 Maxmidage2 Maxmidage3 Maxmidage4 Maxmidage5 Maxmidage6 ///
denNbpoints MaxDendensity denMaxyear dengradMaxyear Maxdengrad denageMaxyear Maxdenage55pct Maxdenage1 Maxdenage2 Maxdenage3 Maxdenage4 Maxdenage5 Maxdenage6 ///
phaNbpoints MaxPhadensity phaMaxyear phagradMaxyear Maxphagrad phaageMaxyear Maxphaage55pct Maxphaage1 Maxphaage2 Maxphaage3 Maxphaage4 Maxphaage5 Maxphaage6 
order region country iso3 WBI year pop2020 pop2020age1 pop2020age2 pop2020age3 pop2020age4 pop2020age5 pop2020age6 ///
pop2021 pop2022 pop2023 pop2024 pop2025 pop2026 pop2027 pop2028 pop2029 pop2030 pop2032 pop ///
medNbpoints MaxMeddensity medMaxyear medgradMaxyear Maxmedgrad medageMaxyear Maxmedage55pct Maxmedage1 Maxmedage2 Maxmedage3 Maxmedage4 Maxmedage5 Maxmedage6 ///
nurNbpoints MaxNurdensity nurMaxyear nurgradMaxyear Maxnurgrad nurageMaxyear Maxnurage55pct Maxnurage1 Maxnurage2 Maxnurage3 Maxnurage4 Maxnurage5 Maxnurage6 ///
midNbpoints MaxMiddensity midMaxyear flagnurmiddoublecount midgradMaxyear Maxmidgrad midageMaxyear Maxmidage55pct Maxmidage1 Maxmidage2 Maxmidage3 Maxmidage4 Maxmidage5 Maxmidage6 ///
denNbpoints MaxDendensity denMaxyear dengradMaxyear Maxdengrad denageMaxyear Maxdenage55pct Maxdenage1 Maxdenage2 Maxdenage3 Maxdenage4 Maxdenage5 Maxdenage6 ///
phaNbpoints MaxPhadensity phaMaxyear phagradMaxyear Maxphagrad phaageMaxyear Maxphaage55pct Maxphaage1 Maxphaage2 Maxphaage3 Maxphaage4 Maxphaage5 Maxphaage6 
 
 save eSDG3cData, replace
* replacing missing WBI
*South Sudan => LIN as Sierra Leone
replace WBI="LIN" if iso3=="SSD"
*Dominica and Saint kitt => LMI as Cuba
replace WBI="LMI" if iso3=="DMA"
replace WBI="LMI" if iso3=="KNA"
*ANdorra , Monaco, San Marion => HIN as France
replace WBI="HIN" if iso3=="AND"
replace WBI="HIN" if iso3=="MCO"
replace WBI="HIN" if iso3=="SMR"
*Cook Island, Marshall, Niue, Nauru, Palau, Tuvalu=> LMI as Fiji
replace WBI="LMI" if iso3=="COK"
replace WBI="LMI" if iso3=="MHL"
replace WBI="LMI" if iso3=="NIU"
replace WBI="LMI" if iso3=="NRU"
replace WBI="LMI" if iso3=="PLW"
replace WBI="LMI" if iso3=="TUV"


* filling population 2023 and 2030 applying same as population 2020 (no other source, would not have big impact here)
tab country if pop2030==.
replace pop2021=pop if pop2021==.
replace pop2022=pop if pop2022==.
replace pop2023=pop if pop2023==.
replace pop2024=pop if pop2024==.
replace pop2025=pop if pop2025==.
replace pop2026=pop if pop2026==.
replace pop2027=pop if pop2027==.
replace pop2028=pop if pop2028==.
replace pop2029=pop if pop2029==.
replace pop2030=pop if pop2030==.
replace pop2032=pop if pop2032==.

*medical doctors
* filling missing density for medical doctors with WBI average, creating emeddensity and emedtot
egen emeddensity2020=mean(MaxMeddensity) , by(WBI)
replace emeddensity2020=MaxMeddensity if MaxMeddensity!=.
generate emedtot2020=round(emeddensity2020*pop/10000,1)
* filling missing graduates for medical doctors with WBI average of grad/stock ratio applied to stock 2020, creating emedgrad
generate medgradstock=Maxmedgrad/emedtot2020
replace medgradstock=. if medgradstock>0.5
egen emedgrad=mean(medgradstock) , by(WBI)
by WBI, sort : summarize medgradstock
replace emedgrad=medgradstock if medgradstock!=.
replace emedgrad=round(emedgrad*emedtot2020,1)
drop medgradstock
* filling missing pct age above 55 for medical doctors with population based structure if missing
* replacing 0 for age 64+ by the redistribution of 55+ (in category 55-64) with population ratios
replace Maxmedage5=round(Maxmedage5*pop2020age5/(pop2020age5+pop2020age6),1) if Maxmedage6==0 & pop2020age5!=.
replace Maxmedage6=round(Maxmedage5*pop2020age6/(pop2020age5+pop2020age6),1) if Maxmedage6==0 & pop2020age5!=.
replace Maxmedage5=round(Maxmedage5*2/3,1) if Maxmedage6==0 & pop2020age5==.
replace Maxmedage6=round(Maxmedage5/2,1) if Maxmedage6==0 & pop2020age5==.
generate medagepct55_64=Maxmedage5/(Maxmedage1+Maxmedage2+Maxmedage3+Maxmedage4+Maxmedage5+Maxmedage6)
generate medagepct64_=Maxmedage6/(Maxmedage1+Maxmedage2+Maxmedage3+Maxmedage4+Maxmedage5+Maxmedage6)
histogram medagepct55_64
histogram medagepct64_
* replacing by popultion distribution if missing
replace medagepct55_64=pop2020age5/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if medagepct55_64==.
replace medagepct64_=pop2020age6/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if medagepct64_==.
* replacing by pct average by region when population data are not available
 egen emedagepct55_64=mean(medagepct55_64) , by(region)
egen emedagepct64_=mean(medagepct64_) , by(region)
replace medagepct55_64=emedagepct55_64 if medagepct55_64==.
replace medagepct64_=emedagepct64_ if medagepct64_==.
drop emedagepct55_64 emedagepct64_
* Projected stock: using 0.7 as entry rate in HLM as per OECD rate of licensed/active 
generate emedtot2021=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(1-1)+1*emedgrad*0.7,1)
generate emedtot2022=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(2-1)+2*emedgrad*0.7,1)
generate emedtot2023=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(3-1)+3*emedgrad*0.7,1)
generate emedtot2024=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(4-1)+4*emedgrad*0.7,1)
generate emedtot2025=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(5-1)+5*emedgrad*0.7,1)
generate emedtot2026=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(6-1)+6*emedgrad*0.7,1)
generate emedtot2027=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(7-1)+7*emedgrad*0.7,1)
generate emedtot2028=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(8-1)+8*emedgrad*0.7,1)
generate emedtot2029=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(9-1)+9*emedgrad*0.7,1)
generate emedtot2030=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(10-1)+10*emedgrad*0.7,1)
generate emedtot2032=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(12-1)+12*emedgrad*0.7,1)
generate emeddensity2021=emedtot2021/pop2021*10000
generate emeddensity2022=emedtot2022/pop2022*10000
generate emeddensity2023=emedtot2023/pop2023*10000
generate emeddensity2024=emedtot2024/pop2024*10000
generate emeddensity2025=emedtot2025/pop2025*10000
generate emeddensity2026=emedtot2026/pop2026*10000
generate emeddensity2027=emedtot2027/pop2027*10000
generate emeddensity2028=emedtot2028/pop2028*10000
generate emeddensity2029=emedtot2029/pop2029*10000
generate emeddensity2030=emedtot2030/pop2030*10000
generate emeddensity2032=emedtot2032/pop2032*10000


*nursing personnel
* filling missing density for nursing with WBI average, creating enurdensity and enurtot
egen enurdensity2020=mean(MaxNurdensity) , by(WBI)
replace enurdensity2020=MaxNurdensity if MaxNurdensity!=.
generate enurtot2020=round(enurdensity2020*pop/10000,1)
* filling missing graduates for nurses with WBI average of grad/stock ratio applied to stock 2020, creating enurgrad
generate nurgradstock=Maxnurgrad/enurtot2020
replace nurgradstock=. if nurgradstock>0.5
egen enurgrad=mean(nurgradstock) , by(WBI)
by WBI, sort : summarize nurgradstock
replace enurgrad=nurgradstock if nurgradstock!=.
replace enurgrad=round(enurgrad*enurtot2020,1)
drop nurgradstock
* filling missing pct age above 55 for nurses with population based structure if missing
* replacing 0 for age 64+ by the redistribution of 55+ (in category 55-64) with population ratios
replace Maxnurage5=round(Maxnurage5*pop2020age5/(pop2020age5+pop2020age6),1) if Maxnurage6==0 & pop2020age5!=.
replace Maxnurage6=round(Maxnurage5*pop2020age6/(pop2020age5+pop2020age6),1) if Maxnurage6==0 & pop2020age5!=.
replace Maxnurage5=round(Maxnurage5*2/3,1) if Maxnurage6==0 & pop2020age5==.
replace Maxnurage6=round(Maxnurage5/2,1) if Maxnurage6==0 & pop2020age5==.
generate nuragepct55_64=Maxnurage5/(Maxnurage1+Maxnurage2+Maxnurage3+Maxnurage4+Maxnurage5+Maxnurage6)
generate nuragepct64_=Maxnurage6/(Maxnurage1+Maxnurage2+Maxnurage3+Maxnurage4+Maxnurage5+Maxnurage6)
histogram nuragepct55_64
histogram nuragepct64_
* replacing by popultion distribution if missing
replace nuragepct55_64=pop2020age5/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if nuragepct55_64==.
replace nuragepct64_=pop2020age6/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if nuragepct64_==.
* replacing by pct average by region when population data are not available
 egen enuragepct55_64=mean(nuragepct55_64) , by(region)
egen enuragepct64_=mean(nuragepct64_) , by(region)
replace nuragepct55_64=enuragepct55_64 if nuragepct55_64==.
replace nuragepct64_=enuragepct64_ if nuragepct64_==.
drop enuragepct55_64 enuragepct64_
* Projected stock: using 0.7 as entry rate in HLM as per OECD rate of licensed/active 
generate enurtot2021=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(1-1)+1*enurgrad*0.7,1)
generate enurtot2022=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(2-1)+2*enurgrad*0.7,1)
generate enurtot2023=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(3-1)+3*enurgrad*0.7,1)
generate enurtot2024=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(4-1)+4*enurgrad*0.7,1)
generate enurtot2025=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(5-1)+5*enurgrad*0.7,1)
generate enurtot2026=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(6-1)+6*enurgrad*0.7,1)
generate enurtot2027=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(7-1)+7*enurgrad*0.7,1)
generate enurtot2028=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(8-1)+8*enurgrad*0.7,1)
generate enurtot2029=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(9-1)+9*enurgrad*0.7,1)
generate enurtot2030=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(10-1)+10*enurgrad*0.7,1)
generate enurtot2032=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(12-1)+12*enurgrad*0.7,1)
generate enurdensity2021=enurtot2021/pop2021*10000
generate enurdensity2022=enurtot2022/pop2022*10000
generate enurdensity2023=enurtot2023/pop2023*10000
generate enurdensity2024=enurtot2024/pop2024*10000
generate enurdensity2025=enurtot2025/pop2025*10000
generate enurdensity2026=enurtot2026/pop2026*10000
generate enurdensity2027=enurtot2027/pop2027*10000
generate enurdensity2028=enurtot2028/pop2028*10000
generate enurdensity2029=enurtot2029/pop2029*10000
generate enurdensity2030=enurtot2030/pop2030*10000
generate enurdensity2032=enurtot2032/pop2032*10000


*Midwifery personnel
* filling missing density for midwifery with WBI average, creating emiddensity and emidtot
egen emiddensity2020=mean(MaxMiddensity) , by(WBI)
replace emiddensity2020=MaxMiddensity if MaxMiddensity!=.
generate emidtot2020=round(emiddensity2020*pop/10000,1)
* filling missing graduates for midwifery with WBI average of grad/stock ratio applied to stock 2020, creating emidgrad
generate midgradstock=Maxmidgrad/emidtot2020
replace midgradstock=. if midgradstock>0.5
egen emidgrad=mean(midgradstock) , by(WBI)
by WBI, sort : summarize midgradstock
replace emidgrad=midgradstock if midgradstock!=.
replace emidgrad=round(emidgrad*emidtot2020,1)
drop midgradstock
* filling missing pct age above 55 for midwifery with population based structure if missing
* replacing 0 for age 64+ by the redistribution of 55+ (in category 55-64) with population ratios
replace Maxmidage5=round(Maxmidage5*pop2020age5/(pop2020age5+pop2020age6),1) if Maxmidage6==0 & pop2020age5!=.
replace Maxmidage6=round(Maxmidage5*pop2020age6/(pop2020age5+pop2020age6),1) if Maxmidage6==0 & pop2020age5!=.
replace Maxmidage5=round(Maxmidage5*2/3,1) if Maxmidage6==0 & pop2020age5==.
replace Maxmidage6=round(Maxmidage5/2,1) if Maxmidage6==0 & pop2020age5==.
generate midagepct55_64=Maxmidage5/(Maxmidage1+Maxmidage2+Maxmidage3+Maxmidage4+Maxmidage5+Maxmidage6)
generate midagepct64_=Maxmidage6/(Maxmidage1+Maxmidage2+Maxmidage3+Maxmidage4+Maxmidage5+Maxmidage6)
histogram midagepct55_64
histogram midagepct64_
* replacing by popultion distribution if missing
replace midagepct55_64=pop2020age5/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if midagepct55_64==.
replace midagepct64_=pop2020age6/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if midagepct64_==.
* replacing by pct average by region when population data are not available
 egen emidagepct55_64=mean(midagepct55_64) , by(region)
egen emidagepct64_=mean(midagepct64_) , by(region)
replace midagepct55_64=emidagepct55_64 if midagepct55_64==.
replace midagepct64_=emidagepct64_ if midagepct64_==.
drop emidagepct55_64 emidagepct64_
* Projected stock: using 0.7 as entry rate in HLM as per OECD rate of licensed/active 
generate emidtot2021=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(1-1)+1*emidgrad*0.7,1)
generate emidtot2022=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(2-1)+2*emidgrad*0.7,1)
generate emidtot2023=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(3-1)+3*emidgrad*0.7,1)
generate emidtot2024=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(4-1)+4*emidgrad*0.7,1)
generate emidtot2025=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(5-1)+5*emidgrad*0.7,1)
generate emidtot2026=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(6-1)+6*emidgrad*0.7,1)
generate emidtot2027=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(7-1)+7*emidgrad*0.7,1)
generate emidtot2028=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(8-1)+8*emidgrad*0.7,1)
generate emidtot2029=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(9-1)+9*emidgrad*0.7,1)
generate emidtot2030=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(10-1)+10*emidgrad*0.7,1)
generate emidtot2032=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(12-1)+12*emidgrad*0.7,1)
generate emiddensity2021=emidtot2021/pop2021*10000
generate emiddensity2022=emidtot2022/pop2022*10000
generate emiddensity2023=emidtot2023/pop2023*10000
generate emiddensity2024=emidtot2024/pop2024*10000
generate emiddensity2025=emidtot2025/pop2025*10000
generate emiddensity2026=emidtot2026/pop2026*10000
generate emiddensity2027=emidtot2027/pop2027*10000
generate emiddensity2028=emidtot2028/pop2028*10000
generate emiddensity2029=emidtot2029/pop2029*10000
generate emiddensity2030=emidtot2030/pop2030*10000
generate emiddensity2032=emidtot2032/pop2032*10000
* putting missing for countries where midwifery is likely included in the count as previously reported separately
replace emiddensity2020=. if flagnurmiddoublecount==1
replace emidtot2020=. if flagnurmiddoublecount==1
replace emidgrad=. if flagnurmiddoublecount==1
replace midagepct55_64=. if flagnurmiddoublecount==1
replace midagepct64_=. if flagnurmiddoublecount==1
replace emidtot2021=. if flagnurmiddoublecount==1
replace emidtot2022=. if flagnurmiddoublecount==1
replace emidtot2023=. if flagnurmiddoublecount==1
replace emidtot2024=. if flagnurmiddoublecount==1
replace emidtot2025=. if flagnurmiddoublecount==1
replace emidtot2026=. if flagnurmiddoublecount==1
replace emidtot2027=. if flagnurmiddoublecount==1
replace emidtot2028=. if flagnurmiddoublecount==1
replace emidtot2029=. if flagnurmiddoublecount==1
replace emidtot2030=. if flagnurmiddoublecount==1
replace emidtot2032=. if flagnurmiddoublecount==1
replace emiddensity2021=. if flagnurmiddoublecount==1
replace emiddensity2022=. if flagnurmiddoublecount==1
replace emiddensity2023=. if flagnurmiddoublecount==1
replace emiddensity2024=. if flagnurmiddoublecount==1
replace emiddensity2025=. if flagnurmiddoublecount==1
replace emiddensity2026=. if flagnurmiddoublecount==1
replace emiddensity2027=. if flagnurmiddoublecount==1
replace emiddensity2028=. if flagnurmiddoublecount==1
replace emiddensity2029=. if flagnurmiddoublecount==1
replace emiddensity2030=. if flagnurmiddoublecount==1
replace emiddensity2032=. if flagnurmiddoublecount==1



*dentists
* filling missing density for dentists with WBI average, creating edendensity and edentot
egen edendensity2020=mean(MaxDendensity) , by(WBI)
replace edendensity2020=MaxDendensity if MaxDendensity!=.
generate edentot2020=round(edendensity2020*pop/10000,1)
* filling missing graduates for dentists with WBI average of grad/stock ratio applied to stock 2020, creating edengrad
generate dengradstock=Maxdengrad/edentot2020
replace dengradstock=. if dengradstock>0.5
egen edengrad=mean(dengradstock) , by(WBI)
by WBI, sort : summarize dengradstock
replace edengrad=dengradstock if dengradstock!=.
replace edengrad=round(edengrad*edentot2020,1)
drop dengradstock
* filling missing pct age above 55 for dentists with population based structure if missing
* replacing 0 for age 64+ by the redistribution of 55+ (in category 55-64) with population ratios
replace Maxdenage5=round(Maxdenage5*pop2020age5/(pop2020age5+pop2020age6),1) if Maxdenage6==0 & pop2020age5!=.
replace Maxdenage6=round(Maxdenage5*pop2020age6/(pop2020age5+pop2020age6),1) if Maxdenage6==0 & pop2020age5!=.
replace Maxdenage5=round(Maxdenage5*2/3,1) if Maxdenage6==0 & pop2020age5==.
replace Maxdenage6=round(Maxdenage5/2,1) if Maxdenage6==0 & pop2020age5==.
generate denagepct55_64=Maxdenage5/(Maxdenage1+Maxdenage2+Maxdenage3+Maxdenage4+Maxdenage5+Maxdenage6)
generate denagepct64_=Maxdenage6/(Maxdenage1+Maxdenage2+Maxdenage3+Maxdenage4+Maxdenage5+Maxdenage6)
histogram denagepct55_64
histogram denagepct64_
* replacing by popultion distribution if missing
replace denagepct55_64=pop2020age5/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if denagepct55_64==.
replace denagepct64_=pop2020age6/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if denagepct64_==.
* replacing by pct average by region when population data are not available
 egen edenagepct55_64=mean(denagepct55_64) , by(region)
egen edenagepct64_=mean(denagepct64_) , by(region)
replace denagepct55_64=edenagepct55_64 if denagepct55_64==.
replace denagepct64_=edenagepct64_ if denagepct64_==.
drop edenagepct55_64 edenagepct64_
* Projected stock: using 0.7 as entry rate in HLM as per OECD rate of licensed/active 
generate edentot2021=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(1-1)+1*edengrad*0.7,1)
generate edentot2022=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(2-1)+2*edengrad*0.7,1)
generate edentot2023=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(3-1)+3*edengrad*0.7,1)
generate edentot2024=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(4-1)+4*edengrad*0.7,1)
generate edentot2025=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(5-1)+5*edengrad*0.7,1)
generate edentot2026=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(6-1)+6*edengrad*0.7,1)
generate edentot2027=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(7-1)+7*edengrad*0.7,1)
generate edentot2028=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(8-1)+8*edengrad*0.7,1)
generate edentot2029=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(9-1)+9*edengrad*0.7,1)
generate edentot2030=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(10-1)+10*edengrad*0.7,1)
generate edentot2032=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(12-1)+12*edengrad*0.7,1)
generate edendensity2021=edentot2021/pop2021*10000
generate edendensity2022=edentot2022/pop2022*10000
generate edendensity2023=edentot2023/pop2023*10000
generate edendensity2024=edentot2024/pop2024*10000
generate edendensity2025=edentot2025/pop2025*10000
generate edendensity2026=edentot2026/pop2026*10000
generate edendensity2027=edentot2027/pop2027*10000
generate edendensity2028=edentot2028/pop2028*10000
generate edendensity2029=edentot2029/pop2029*10000
generate edendensity2030=edentot2030/pop2030*10000
generate edendensity2032=edentot2032/pop2032*10000



*Pharmacists
* filling missing density for Pharmacists with WBI average, creating enurdensity and enurtot
egen ephadensity2020=mean(MaxPhadensity) , by(WBI)
replace ephadensity2020=MaxPhadensity if MaxPhadensity!=.
generate ephatot2020=round(ephadensity2020*pop/10000,1)
* filling missing graduates for Pharmacists with WBI average of grad/stock ratio applied to stock 2020, creating ephagrad
generate phagradstock=Maxphagrad/ephatot2020
replace phagradstock=. if phagradstock>0.5
egen ephagrad=mean(phagradstock) , by(WBI)
by WBI, sort : summarize phagradstock
replace ephagrad=phagradstock if phagradstock!=.
replace ephagrad=round(ephagrad*ephatot2020,1)
drop phagradstock
* filling missing pct age above 55 for Pharmacists with population based structure if missing
* replacing 0 for age 64+ by the redistribution of 55+ (in category 55-64) with population ratios
replace Maxphaage5=round(Maxphaage5*pop2020age5/(pop2020age5+pop2020age6),1) if Maxphaage6==0 & pop2020age5!=.
replace Maxphaage6=round(Maxphaage5*pop2020age6/(pop2020age5+pop2020age6),1) if Maxphaage6==0 & pop2020age5!=.
replace Maxphaage5=round(Maxphaage5*2/3,1) if Maxphaage6==0 & pop2020age5==.
replace Maxphaage6=round(Maxphaage5/2,1) if Maxphaage6==0 & pop2020age5==.
generate phaagepct55_64=Maxphaage5/(Maxphaage1+Maxphaage2+Maxphaage3+Maxphaage4+Maxphaage5+Maxphaage6)
generate phaagepct64_=Maxphaage6/(Maxphaage1+Maxphaage2+Maxphaage3+Maxphaage4+Maxphaage5+Maxphaage6)
histogram phaagepct55_64
histogram phaagepct64_
* replacing by popultion distribution if missing
replace phaagepct55_64=pop2020age5/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if phaagepct55_64==.
replace phaagepct64_=pop2020age6/(pop2020age1+pop2020age2+pop2020age3+pop2020age4+pop2020age5+pop2020age6) ///
 if phaagepct64_==.
* replacing by pct average by region when population data are not available
 egen ephaagepct55_64=mean(phaagepct55_64) , by(region)
egen ephaagepct64_=mean(phaagepct64_) , by(region)
replace phaagepct55_64=ephaagepct55_64 if phaagepct55_64==.
replace phaagepct64_=ephaagepct64_ if phaagepct64_==.
drop ephaagepct55_64 ephaagepct64_
* Projected stock: using 0.7 as entry rate in HLM as per OECD rate of licensed/active 
generate ephatot2021=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(1-1)+1*ephagrad*0.7,1)
generate ephatot2022=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(2-1)+2*ephagrad*0.7,1)
generate ephatot2023=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(3-1)+3*ephagrad*0.7,1)
generate ephatot2024=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(4-1)+4*ephagrad*0.7,1)
generate ephatot2025=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(5-1)+5*ephagrad*0.7,1)
generate ephatot2026=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(6-1)+6*ephagrad*0.7,1)
generate ephatot2027=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(7-1)+7*ephagrad*0.7,1)
generate ephatot2028=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(8-1)+8*ephagrad*0.7,1)
generate ephatot2029=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(9-1)+9*ephagrad*0.7,1)
generate ephatot2030=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(10-1)+10*ephagrad*0.7,1)
generate ephatot2032=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(12-1)+12*ephagrad*0.7,1)
generate ephadensity2021=ephatot2021/pop2021*10000
generate ephadensity2022=ephatot2022/pop2022*10000
generate ephadensity2023=ephatot2023/pop2023*10000
generate ephadensity2024=ephatot2024/pop2024*10000
generate ephadensity2025=ephatot2025/pop2025*10000
generate ephadensity2026=ephatot2026/pop2026*10000
generate ephadensity2027=ephatot2027/pop2027*10000
generate ephadensity2028=ephatot2028/pop2028*10000
generate ephadensity2029=ephatot2029/pop2029*10000
generate ephadensity2030=ephatot2030/pop2030*10000
generate ephadensity2032=ephatot2032/pop2032*10000

save SDGData4Analysis, replace


clear
use SDGData4Analysis
keep region country iso3 ///
emedtot2020 emeddensity2020 enurtot2020 enurdensity2020 emidtot2020 emiddensity2020 edentot2020 edendensity2020 ephatot2020 ephadensity2020 ///
emedtot2021 emeddensity2021 enurtot2021 enurdensity2021 emidtot2021 emiddensity2021 edentot2021 edendensity2021 ephatot2021 ephadensity2021 ///
emedtot2022 emeddensity2022 enurtot2022 enurdensity2022 emidtot2022 emiddensity2022 edentot2022 edendensity2022 ephatot2022 ephadensity2022 ///
emedtot2023 emeddensity2023 enurtot2023 enurdensity2023 emidtot2023 emiddensity2023 edentot2023 edendensity2023 ephatot2023 ephadensity2023 ///
emedtot2024 emeddensity2024 enurtot2024 enurdensity2024 emidtot2024 emiddensity2024 edentot2024 edendensity2024 ephatot2024 ephadensity2024 ///
emedtot2025 emeddensity2025 enurtot2025 enurdensity2025 emidtot2025 emiddensity2025 edentot2025 edendensity2025 ephatot2025 ephadensity2025 ///
emedtot2026 emeddensity2026 enurtot2026 enurdensity2026 emidtot2026 emiddensity2026 edentot2026 edendensity2026 ephatot2026 ephadensity2026 ///
emedtot2027 emeddensity2027 enurtot2027 enurdensity2027 emidtot2027 emiddensity2027 edentot2027 edendensity2027 ephatot2027 ephadensity2027 ///
emedtot2028 emeddensity2028 enurtot2028 enurdensity2028 emidtot2028 emiddensity2028 edentot2028 edendensity2028 ephatot2028 ephadensity2028 ///
emedtot2029 emeddensity2029 enurtot2029 enurdensity2029 emidtot2029 emiddensity2029 edentot2029 edendensity2029 ephatot2029 ephadensity2029 ///
emedtot2030 emeddensity2030 enurtot2030 enurdensity2030 emidtot2030 emiddensity2030 edentot2030 edendensity2030 ephatot2030 ephadensity2030
generate HWFdensityGPW142020=emeddensity2020+enurdensity2020+emiddensity2020
generate HWFdensityGPW142021=emeddensity2021+enurdensity2021+emiddensity2021
generate HWFdensityGPW142022=emeddensity2022+enurdensity2022+emiddensity2022
generate HWFdensityGPW142023=emeddensity2023+enurdensity2023+emiddensity2023
generate HWFdensityGPW142024=emeddensity2024+enurdensity2024+emiddensity2024
generate HWFdensityGPW142025=emeddensity2025+enurdensity2025+emiddensity2025
generate HWFdensityGPW142026=emeddensity2026+enurdensity2026+emiddensity2026
generate HWFdensityGPW142027=emeddensity2027+enurdensity2027+emiddensity2027
generate HWFdensityGPW142028=emeddensity2028+enurdensity2028+emiddensity2028
generate HWFdensityGPW142029=emeddensity2029+enurdensity2029+emiddensity2029
generate HWFdensityGPW142030=emeddensity2030+enurdensity2030+emiddensity2030

replace HWFdensityGPW142020=emeddensity2020+enurdensity2020 if HWFdensityGPW142020==.
replace HWFdensityGPW142021=emeddensity2021+enurdensity2021 if HWFdensityGPW142021==.
replace HWFdensityGPW142022=emeddensity2022+enurdensity2022 if HWFdensityGPW142022==.
replace HWFdensityGPW142023=emeddensity2023+enurdensity2023 if HWFdensityGPW142023==.
replace HWFdensityGPW142024=emeddensity2024+enurdensity2024 if HWFdensityGPW142024==.
replace HWFdensityGPW142025=emeddensity2025+enurdensity2025 if HWFdensityGPW142025==.
replace HWFdensityGPW142026=emeddensity2026+enurdensity2026 if HWFdensityGPW142026==.
replace HWFdensityGPW142027=emeddensity2027+enurdensity2027 if HWFdensityGPW142027==.
replace HWFdensityGPW142028=emeddensity2028+enurdensity2028 if HWFdensityGPW142028==.
replace HWFdensityGPW142029=emeddensity2029+enurdensity2029 if HWFdensityGPW142029==.
replace HWFdensityGPW142030=emeddensity2030+enurdensity2030 if HWFdensityGPW142030==.


order region country iso3 ///
HWFdensityGPW142020 HWFdensityGPW142021 HWFdensityGPW142022 HWFdensityGPW142023 HWFdensityGPW142024 HWFdensityGPW142025 HWFdensityGPW142026 ///
HWFdensityGPW142027 HWFdensityGPW142028 HWFdensityGPW142029 HWFdensityGPW142030 ///
emedtot2020 emeddensity2020 enurtot2020 enurdensity2020 emidtot2020 emiddensity2020 edentot2020 edendensity2020 ephatot2020 ephadensity2020 ///
emedtot2021 emeddensity2021 enurtot2021 enurdensity2021 emidtot2021 emiddensity2021 edentot2021 edendensity2021 ephatot2021 ephadensity2021 ///
emedtot2022 emeddensity2022 enurtot2022 enurdensity2022 emidtot2022 emiddensity2022 edentot2022 edendensity2022 ephatot2022 ephadensity2022 ///
emedtot2023 emeddensity2023 enurtot2023 enurdensity2023 emidtot2023 emiddensity2023 edentot2023 edendensity2023 ephatot2023 ephadensity2023 ///
emedtot2024 emeddensity2024 enurtot2024 enurdensity2024 emidtot2024 emiddensity2024 edentot2024 edendensity2024 ephatot2024 ephadensity2024 ///
emedtot2025 emeddensity2025 enurtot2025 enurdensity2025 emidtot2025 emiddensity2025 edentot2025 edendensity2025 ephatot2025 ephadensity2025 ///
emedtot2026 emeddensity2026 enurtot2026 enurdensity2026 emidtot2026 emiddensity2026 edentot2026 edendensity2026 ephatot2026 ephadensity2026 ///
emedtot2027 emeddensity2027 enurtot2027 enurdensity2027 emidtot2027 emiddensity2027 edentot2027 edendensity2027 ephatot2027 ephadensity2027 ///
emedtot2028 emeddensity2028 enurtot2028 enurdensity2028 emidtot2028 emiddensity2028 edentot2028 edendensity2028 ephatot2028 ephadensity2028 ///
emedtot2029 emeddensity2029 enurtot2029 enurdensity2029 emidtot2029 emiddensity2029 edentot2029 edendensity2029 ephatot2029 ephadensity2029 ///
emedtot2030 emeddensity2030 enurtot2030 enurdensity2030 emidtot2030 emiddensity2030 edentot2030 edendensity2030 ephatot2030 ephadensity2030

sort region iso3

save GPW13Projection, replace
****** >>>>>>>>>>>>>>>>>>>>>>>>>>
export delimited using "C:\Users\boniolm\OneDrive - World Health Organization\Articles\Published\Workforce 2018 and projection to 2030\HWF-Estimate and projection-2020-2030-20220104.csv", replace



/* ************* */
/* ************* */
/*    ANALYSIS   */
/* ************* */
/* ************* */

* SHORTAGE THRESHOLD AND SHORTAGE ESTIMATION IN 2013
clear 
use SDG3cData2013
generate popM=pop/1000000
tabstat popM, statistics( sum ) by(region)
tabstat emedtot2013 enurtot2013 emidtot2013 edentot2013 ephatot2013, statistics( sum ) by(region)
*** Inequity 2013
tabstat popM emedtot2013 enurtot2013 emidtot2013 edentot2013 ephatot2013, statistics( sum ) by(WBI)

generate SIDS="No"
replace SIDS="Yes" if inlist(iso3,"ATG", "BHS", "BHR", "BRB", "BLZ", "CPV", "COM", "CUB", "DMA")
replace SIDS="Yes" if inlist(iso3,"DOM", "FJI", "GRD", "GNB", "GUY", "HTI", "JAM", "KIR", "MDV")
replace SIDS="Yes" if inlist(iso3,"MHL", "MUS", "FSM", "NRU", "PLW", "PNG", "WSM", "STP", "SYC")
replace SIDS="Yes" if inlist(iso3,"SGP", "SLB", "SUR", "TLS", "TON", "TTO", "TUV", "VUT")
tabstat popM emedtot2013 enurtot2013 emidtot2013 edentot2013 ephatot2013, statistics( sum ) by(SIDS)


generate allHWF2013=emedtot2013+enurtot2013+emidtot2013+edentot2013+ephatot2013
generate allHWFdens2013=allHWF2013/pop*10000
tabstat allHWFdens2013, statistics(n median mean min max) by(region)
* median is 52.34837
* by occupation
tabstat emeddensity2013, statistics(n median mean min max) by(region)
* median med 12.83805 
tabstat enurdensity2013, statistics(n median mean min max) by(region)
* median nur 30.90968
tabstat emiddensity2013 if flagnurmiddoublecount!=1, statistics(n median mean min max) by(region)
* median mid 3.815949 
tabstat edendensity2013, statistics(n median mean min max) by(region)
* median den 1.685815 
tabstat ephadensity2013, statistics(n median mean min max) by(region)
* median pha 1.948534 
* Shortage in 2013
generate MedShortage2013=(12.83805-emeddensity2013)*pop/10000
replace MedShortage2013=0 if MedShortage2013<0
generate NurShortage2013=(30.90968-enurdensity2013)*pop/10000
replace NurShortage2013=0 if NurShortage2013<0
generate MidShortage2013=(3.815949-emiddensity2013)*pop/10000
replace MidShortage2013=0 if MidShortage2013<0
replace MidShortage2013=0 if flagnurmiddoublecount==1
generate DenShortage2013=(1.685815-edendensity2013)*pop/10000
replace DenShortage2013=0 if DenShortage2013<0
generate PhaShortage2013=(1.948534-ephadensity2013)*pop/10000
replace PhaShortage2013=0 if PhaShortage2013<0
generate Shortage2013=MedShortage2013+NurShortage2013+MidShortage2013+DenShortage2013+PhaShortage2013
generate OtherShortage2013=0
* Adding share of other occupations to the shortage based on relative proportion of other occupations
replace OtherShortage2013=Shortage2013*0.4455 if region=="AFR"
replace OtherShortage2013=Shortage2013*0.2706 if region=="AMR"
replace OtherShortage2013=Shortage2013*0.4290 if region=="EMR"
replace OtherShortage2013=Shortage2013*0.1945 if region=="EUR"
replace OtherShortage2013=Shortage2013*0.4874 if region=="SEAR"
replace OtherShortage2013=Shortage2013*0.2717 if region=="WPR"
replace Shortage2013=Shortage2013+OtherShortage2013
tabstat MedShortage2013 NurShortage2013 MidShortage2013 DenShortage2013 ///
PhaShortage2013 OtherShortage2013 Shortage2013, statistics( sum ) by(region)

tabstat emeddensity2013 enurdensity2013 emiddensity2013 edendensity2013 ///
ephadensity2013 pop, statistics( sum ) by(iso3)

tabstat Shortage2013, statistics( sum ) by(region)

tabstat allHWFdens2013, statistics(n median mean min max) by(region)
generate allHWFplusdens2013=.
replace allHWFplusdens2013=allHWFdens2013*1.4455 if region=="AFR"
replace allHWFplusdens2013=allHWFdens2013*1.2706 if region=="AMR"
replace allHWFplusdens2013=allHWFdens2013*1.4290 if region=="EMR"
replace allHWFplusdens2013=allHWFdens2013*1.1945 if region=="EUR"
replace allHWFplusdens2013=allHWFdens2013*1.4874 if region=="SEAR"
replace allHWFplusdens2013=allHWFdens2013*1.2717 if region=="WPR"
tabstat allHWFplusdens2013, statistics(n median mean min max) by(region)

list iso3 allHWFplusdens2013 allHWFdens2013 emeddensity2013 enurdensity2013 emiddensity2013 edendensity2013 ephadensity2013 if region=="AFR", noobs separator(120)


rename pop pop2013
keep iso3 region WBI emeddensity2013 enurdensity2013 emiddensity2013 edendensity2013 ephadensity2013 pop2013

save eden2013, replace



clear 
use SDGData4Analysis
*** Statistics on completeness Appendix T1
* counting countries with data
tab medMaxyear
tab nurMaxyear
tab midMaxyear
tab denMaxyear
tab phaMaxyear
* counting countries with recent data 2016-2020
tab medMaxyear if medMaxyear>2015
tab nurMaxyear if nurMaxyear>2015
tab midMaxyear if midMaxyear>2015
tab denMaxyear if denMaxyear>2015
tab phaMaxyear if phaMaxyear>2015
* counting countries with at least 5 datapoints
tab medMaxyear if medNbpoints>4
tab nurMaxyear if nurNbpoints>4
tab midMaxyear if midNbpoints>4
tab denMaxyear if denNbpoints>4
tab phaMaxyear if phaNbpoints>4
* counting countries with graduates reported
tab region if medgradMaxyear!=.
tab region if nurgradMaxyear!=.
tab region if midgradMaxyear!=.
tab region if dengradMaxyear!=.
tab region if phagradMaxyear!=.
* counting countries with age reported
tab region if medageMaxyear!=.
tab region if nurageMaxyear!=.
tab region if midageMaxyear!=.
tab region if denageMaxyear!=.
tab region if phaageMaxyear!=.
* listing countries with no data reported (based on stock, assuming that if no stock reported, other statistics are not)
list country if medMaxyear==., compress noobs sep(100)
list country if nurMaxyear==., compress noobs sep(100)
list country if midMaxyear==. & flagnurmiddoublecount==0, compress noobs sep(100)
list country if denMaxyear==., compress noobs sep(100)
list country if phaMaxyear==., compress noobs sep(100)
* number of countries where statistics on midwfery were included in nursing stock => to define the denominator for midwifery (not 194 countries)
tab region if flagnurmiddoublecount==1


*** Global health workforce in 2020
tabstat emedtot2020 enurtot2020 emidtot2020 edentot2020 ephatot2020, statistics(sum ) by(region)
tabstat emedtot2020 enurtot2020 emidtot2020 edentot2020 ephatot2020, statistics(sum ) by(WBI)

generate coeffreg=.
replace coeffreg=1.4455 if region=="AFR"
replace coeffreg=1.2706 if region=="AMR"
replace coeffreg=1.4290 if region=="EMR"
replace coeffreg=1.1945 if region=="EUR"
replace coeffreg=1.4874 if region=="SEAR"
replace coeffreg=1.2717 if region=="WPR"
generate ALLocc2020plus=(emedtot2020+enurtot2020+emidtot2020+edentot2020+ephatot2020)*coeffreg
replace ALLocc2020plus=(emedtot2020+enurtot2020+edentot2020+ephatot2020)*coeffreg if ALLocc2020plus==.
tabstat ALLocc2020plus, statistics(sum ) by(WBI)

generate ALLocc2030plus=(emedtot2030+enurtot2030+emidtot2030+edentot2030+ephatot2030)*coeffreg
replace ALLocc2030plus=(emedtot2030+enurtot2030+edentot2030+ephatot2030)*coeffreg if ALLocc2030plus==.
tabstat ALLocc2030plus, statistics(sum ) by(WBI)

tabstat pop2020 ALLocc2020plus if inlist(WBI,"LMI","LIN"), statistics(sum ) 
tabstat pop2030 ALLocc2030plus if inlist(WBI,"LMI","LIN"), statistics(sum ) 

tabstat pop2020 ALLocc2020plus if inlist(WBI,"HIN","UMI"), statistics(sum ) 
tabstat pop2030 ALLocc2030plus if inlist(WBI,"HIN","UMI"), statistics(sum ) 
tabstat pop2030 , statistics(sum ) by(WBI)


* All occupation
generate allocc2020=emedtot2020+enurtot2020+edentot2020+ephatot2020
replace allocc2020=allocc2020+emidtot2020 if emidtot2020!=.
replace allocc2020=allocc2020*1.4455 if region=="AFR"
replace allocc2020=allocc2020*1.2706 if region=="AMR"
replace allocc2020=allocc2020*1.429 if region=="EMR"
replace allocc2020=allocc2020*1.1945 if region=="EUR"
replace allocc2020=allocc2020*1.4874 if region=="SEAR"
replace allocc2020=allocc2020*1.2717 if region=="WPR"

list iso3 allocc2020, noobs sep(200)

drop allocc2020


*** Inequity 2020
generate popM=pop/1000000
tabstat popM emedtot2020 enurtot2020 emidtot2020 edentot2020 ephatot2020, statistics( sum ) by(WBI)

generate SIDS="No"
replace SIDS="Yes" if inlist(iso3,"ATG", "BHS", "BHR", "BRB", "BLZ", "CPV", "COM", "CUB", "DMA")
replace SIDS="Yes" if inlist(iso3,"DOM", "FJI", "GRD", "GNB", "GUY", "HTI", "JAM", "KIR", "MDV")
replace SIDS="Yes" if inlist(iso3,"MHL", "MUS", "FSM", "NRU", "PLW", "PNG", "WSM", "STP", "SYC")
replace SIDS="Yes" if inlist(iso3,"SGP", "SLB", "SUR", "TLS", "TON", "TTO", "TUV", "VUT")
tabstat popM emedtot2020 enurtot2020 emidtot2020 edentot2020 ephatot2020, statistics( sum ) by(SIDS)
drop SIDS

tabstat popM, statistics(sum ) by(region)





*** Shortage 2020
generate MedShortage2020=(12.83805-emeddensity2020)*pop/10000
replace MedShortage2020=0 if MedShortage2020<0
generate NurShortage2020=(30.90968-enurdensity2020)*pop/10000
replace NurShortage2020=0 if NurShortage2020<0
generate MidShortage2020=(3.815949-emiddensity2020)*pop/10000
replace MidShortage2020=0 if MidShortage2020<0
replace MidShortage2020=0 if flagnurmiddoublecount==1
generate DenShortage2020=(1.685815-edendensity2020)*pop/10000
replace DenShortage2020=0 if DenShortage2020<0
generate PhaShortage2020=(1.948534-ephadensity2020)*pop/10000
replace PhaShortage2020=0 if PhaShortage2020<0
generate Shortage2020=MedShortage2020+NurShortage2020+MidShortage2020+DenShortage2020+PhaShortage2020
generate OtherShortage2020=0
* Adding share of other occupations to the shortage based on relative proportion of other occupations
replace OtherShortage2020=Shortage2020*0.4455 if region=="AFR"
replace OtherShortage2020=Shortage2020*0.2706 if region=="AMR"
replace OtherShortage2020=Shortage2020*0.4290 if region=="EMR"
replace OtherShortage2020=Shortage2020*0.1945 if region=="EUR"
replace OtherShortage2020=Shortage2020*0.4874 if region=="SEAR"
replace OtherShortage2020=Shortage2020*0.2717 if region=="WPR"
replace Shortage2020=Shortage2020+OtherShortage2020
tabstat MedShortage2020 NurShortage2020 MidShortage2020 DenShortage2020 ///
PhaShortage2020 OtherShortage2020 Shortage2020, statistics( sum ) by(region)




*** Global health workforce in 2030
tabstat emedtot2030 enurtot2030 emidtot2030 edentot2030 ephatot2030, statistics( sum ) by(region)
generate popM2030=pop2030/1000000
tabstat popM2030, statistics(sum ) by(region)


generate MedShortage2030=(12.83805-emeddensity2030)*pop/10000
replace MedShortage2030=0 if MedShortage2030<0
generate NurShortage2030=(30.90968-enurdensity2030)*pop/10000
replace NurShortage2030=0 if NurShortage2030<0
generate MidShortage2030=(3.815949-emiddensity2030)*pop/10000
replace MidShortage2030=0 if MidShortage2030<0
replace MidShortage2030=0 if flagnurmiddoublecount==1
generate DenShortage2030=(1.685815-edendensity2030)*pop/10000
replace DenShortage2030=0 if DenShortage2030<0
generate PhaShortage2030=(1.948534-ephadensity2030)*pop/10000
replace PhaShortage2030=0 if PhaShortage2030<0
generate Shortage2030=MedShortage2030+NurShortage2030+MidShortage2030+DenShortage2030+PhaShortage2030
generate OtherShortage2030=0
* Adding share of other occupations to the shortage based on relative proportion of other occupations
replace OtherShortage2030=Shortage2030*0.4455 if region=="AFR"
replace OtherShortage2030=Shortage2030*0.2706 if region=="AMR"
replace OtherShortage2030=Shortage2030*0.4290 if region=="EMR"
replace OtherShortage2030=Shortage2030*0.1945 if region=="EUR"
replace OtherShortage2030=Shortage2030*0.4874 if region=="SEAR"
replace OtherShortage2030=Shortage2030*0.2717 if region=="WPR"
replace Shortage2030=Shortage2030+OtherShortage2030
tabstat MedShortage2030 NurShortage2030 MidShortage2030 DenShortage2030 ///
PhaShortage2030 OtherShortage2030 Shortage2030, statistics( sum ) by(region)


tabstat MedShortage2030 NurShortage2030 MidShortage2030 DenShortage2030 ///
PhaShortage2030 OtherShortage2030 Shortage2030 if iso3=="NGA", statistics( sum ) by(country)
tabstat MedShortage2030 NurShortage2030 MidShortage2030 DenShortage2030 ///
PhaShortage2030 OtherShortage2030 Shortage2030 if iso3=="ZWE", statistics( sum ) by(country)

tabstat emedtot2020 enurtot2020 edentot2020 ephatot2020 if iso3=="ZWE", statistics( sum ) by(country)
tabstat emedtot2030 enurtot2030 edentot2030 ephatot2030 if iso3=="ZWE", statistics( sum ) by(country)


tabstat Shortage2020 Shortage2030 if iso3=="MMR", statistics( sum ) by(country)

generate SIDS="No"
replace SIDS="Yes" if inlist(iso3,"ATG", "BHS", "BHR", "BRB", "BLZ", "CPV", "COM", "CUB", "DMA")
replace SIDS="Yes" if inlist(iso3,"DOM", "FJI", "GRD", "GNB", "GUY", "HTI", "JAM", "KIR", "MDV")
replace SIDS="Yes" if inlist(iso3,"MHL", "MUS", "FSM", "NRU", "PLW", "PNG", "WSM", "STP", "SYC")
replace SIDS="Yes" if inlist(iso3,"SGP", "SLB", "SUR", "TLS", "TON", "TTO", "TUV", "VUT")
tabstat Shortage2020 Shortage2030, statistics( sum ) by(SIDS)





*** Broad statistics (shortage and densities) 2020
tabstat Shortage2020, statistics( sum ) by(region)
replace pop2020=pop if pop2020==.
generate allHWFdens2020=((emedtot2020+enurtot2020+emidtot2020+edentot2020+ephatot2020)/pop2020)*10000
replace allHWFdens2020=((emedtot2020+enurtot2020+edentot2020+ephatot2020)/pop2020)*10000 if allHWFdens2020==.

tabstat allHWFdens2020, statistics(n median mean min max) by(region)
generate allHWFplusdens2020=.
replace allHWFplusdens2020=allHWFdens2020*1.4455 if region=="AFR"
replace allHWFplusdens2020=allHWFdens2020*1.2706 if region=="AMR"
replace allHWFplusdens2020=allHWFdens2020*1.4290 if region=="EMR"
replace allHWFplusdens2020=allHWFdens2020*1.1945 if region=="EUR"
replace allHWFplusdens2020=allHWFdens2020*1.4874 if region=="SEAR"
replace allHWFplusdens2020=allHWFdens2020*1.2717 if region=="WPR"
tabstat allHWFplusdens2020, statistics(n median mean min max) by(region)

list iso3 allHWFplusdens2020 allHWFdens2020 emeddensity2020 enurdensity2020 emiddensity2020 edendensity2020 ephadensity2020 if region=="AFR", noobs separator(120)
***** FLAG *****


*** Broad statistics (shortage and densities) 2030
tabstat Shortage2030, statistics( sum ) by(region)
replace pop2030=pop if pop2030==.
generate allHWFdens2030=((emedtot2030+enurtot2030+emidtot2030+edentot2030+ephatot2030)/pop2030)*10000
replace allHWFdens2030=((emedtot2030+enurtot2030+edentot2030+ephatot2030)/pop2030)*10000 if allHWFdens2030==.

tabstat allHWFdens2030, statistics(n median mean min max) by(region)
generate allHWFplusdens2030=.
replace allHWFplusdens2030=allHWFdens2030*1.4455 if region=="AFR"
replace allHWFplusdens2030=allHWFdens2030*1.2706 if region=="AMR"
replace allHWFplusdens2030=allHWFdens2030*1.4290 if region=="EMR"
replace allHWFplusdens2030=allHWFdens2030*1.1945 if region=="EUR"
replace allHWFplusdens2030=allHWFdens2030*1.4874 if region=="SEAR"
replace allHWFplusdens2030=allHWFdens2030*1.2717 if region=="WPR"
tabstat allHWFplusdens2030, statistics(n median mean min max) by(region)


list iso3 allHWFplusdens2030 allHWFdens2030 emeddensity2030 enurdensity2030 emiddensity2030 edendensity2030 ephadensity2030 if region=="AFR", noobs separator(120)

list iso3 allHWFplusdens2020 , noobs separator(200)



*** Add safeguard list countries SGLc yes vs no
generate SGLc="No"
replace SGLc="Yes" if inlist(iso3, "AFG", "AGO", "BGD", "BEN", "BFA", "BDI", "CMR", "CAF", "TCD")
replace SGLc="Yes" if inlist(iso3, "COG", "CIV", "COD", "DJI", "GNQ", "ERI", "ETH", "GAB", "GMB")
replace SGLc="Yes" if inlist(iso3, "GHA", "GIN", "GNB", "HTI", "KIR", "LSO", "LBR", "MDG", "MWI")
replace SGLc="Yes" if inlist(iso3, "MLI", "MRT", "FSM", "MOZ", "NPL", "NER", "NGA", "PAK", "PNG")
replace SGLc="Yes" if inlist(iso3, "SEN", "SLE", "SLB", "SOM", "SSD", "SDN", "TGO", "UGA", "TZA")
replace SGLc="Yes" if inlist(iso3, "VUT", "YEM")
tabstat Shortage2020 Shortage2030, statistics( sum ) by(SGLc)

list iso3 Shortage2020, noobs sep(200)
list iso3 Shortage2030, noobs sep(200)

*** other shortage maintain density replacing those retiring
* workforce in 2030 not accounting for graduates
generate enogradmedtot2030=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(10-1),1)
generate enogradnurtot2030=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(10-1),1)
generate enogradmidtot2030=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(10-1),1)
generate enograddentot2030=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(10-1),1)
generate enogradphatot2030=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(10-1),1)

* ageing workforce
generate retiremedtot2030=round(emedtot2020-(emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(10-1),1)
generate retirenurtot2030=round(enurtot2020-(enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(10-1),1)
generate retiremidtot2030=round(emidtot2020-(emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(10-1),1)
generate retiredentot2030=round(edentot2020-(edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(10-1),1)
generate retirephatot2030=round(ephatot2020-(ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(10-1),1)
replace retiremidtot2030=0 if retiremidtot2030==.
generate allret2030=retiremedtot2030+retirenurtot2030+retiremidtot2030+retiredentot2030+retirephatot2030
tabstat allret2030 retiremedtot2030 retirenurtot2030 retiremidtot2030 retiredentot2030 retirephatot2030, statistics( sum ) by(region)
generate OtherRet2030=0
replace OtherRet2030=allret2030*0.4455 if region=="AFR"
replace OtherRet2030=allret2030*0.2706 if region=="AMR"
replace OtherRet2030=allret2030*0.4290 if region=="EMR"
replace OtherRet2030=allret2030*0.1945 if region=="EUR"
replace OtherRet2030=allret2030*0.4874 if region=="SEAR"
replace OtherRet2030=allret2030*0.2717 if region=="WPR"
generate allretplus2030=allret2030+OtherRet2030
tabstat allretplus2030, statistics( sum ) by(region)
tabstat allretplus2030, statistics( sum ) by(WBI)



generate gapmed2030=(emeddensity2020/10000)*pop2030-enogradmedtot2030
generate gapnur2030=(enurdensity2020/10000)*pop2030-enogradnurtot2030
generate gapmid2030=(emiddensity2020/10000)*pop2030-enogradmidtot2030
generate gapden2030=(edendensity2020/10000)*pop2030-enograddentot2030
generate gappha2030=(ephadensity2020/10000)*pop2030-enogradphatot2030
replace gapmid2030=0 if gapmid2030==.
generate allgap2030=round(gapmed2030+gapnur2030+gapmid2030+gapden2030+gappha2030,1)
generate Othergap2030=0
replace Othergap2030=allgap2030*0.4455 if region=="AFR"
replace Othergap2030=allgap2030*0.2706 if region=="AMR"
replace Othergap2030=allgap2030*0.4290 if region=="EMR"
replace Othergap2030=allgap2030*0.1945 if region=="EUR"
replace Othergap2030=allgap2030*0.4874 if region=="SEAR"
replace Othergap2030=allgap2030*0.2717 if region=="WPR"
generate allgapplus2030=allgap2030+Othergap2030
tabstat allgapplus2030, statistics( sum ) by(region)
tabstat allgapplus2030, statistics( sum ) by(WBI)



tabstat pop2020 pop2030, statistics( sum ) by(region)




* sensitivity with 50% absorption capacity
generate SEemedtot2030=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(10-1)+10*emedgrad*0.5,1)
generate SEenurtot2030=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(10-1)+10*enurgrad*0.5,1)
generate SEemidtot2030=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(10-1)+10*emidgrad*0.5,1)
replace SEemidtot2030=0 if SEemidtot2030==.
generate SEedentot2030=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(10-1)+10*edengrad*0.5,1)
generate SEephatot2030=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(10-1)+10*ephagrad*0.5,1)
generate SEall2030=round(SEemedtot2030+SEenurtot2030+SEemidtot2030+SEedentot2030+SEephatot2030,1)
generate SEOther2030=0
replace SEOther2030=SEall2030*0.4455 if region=="AFR"
replace SEOther2030=SEall2030*0.2706 if region=="AMR"
replace SEOther2030=SEall2030*0.4290 if region=="EMR"
replace SEOther2030=SEall2030*0.1945 if region=="EUR"
replace SEOther2030=SEall2030*0.4874 if region=="SEAR"
replace SEOther2030=SEall2030*0.2717 if region=="WPR"
generate SEallplus2030=SEall2030+SEOther2030
tabstat SEallplus2030, statistics( sum ) by(region)

generate SEMedShortage2030=(12.83805-SEemedtot2030/pop2030*10000)*pop/10000
replace SEMedShortage2030=0 if SEMedShortage2030<0
generate SENurShortage2030=(30.90968-SEenurtot2030/pop2030*10000)*pop/10000
replace SENurShortage2030=0 if SENurShortage2030<0
generate SEMidShortage2030=(3.815949-SEemidtot2030/pop2030*10000)*pop/10000
replace SEMidShortage2030=0 if SEMidShortage2030<0
replace SEMidShortage2030=0 if flagnurmiddoublecount==1
generate SEDenShortage2030=(1.685815-SEedentot2030/pop2030*10000)*pop/10000
replace SEDenShortage2030=0 if SEDenShortage2030<0
generate SEPhaShortage2030=(1.948534-SEephatot2030/pop2030*10000)*pop/10000
replace SEPhaShortage2030=0 if SEPhaShortage2030<0
generate SEShortage2030=SEMedShortage2030+SENurShortage2030+SEMidShortage2030+SEDenShortage2030+SEPhaShortage2030
generate SEOtherShortage2030=0
* Adding share of other occupations to the shortage based on relative proportion of other occupations
replace SEOtherShortage2030=SEShortage2030*0.4455 if region=="AFR"
replace SEOtherShortage2030=SEShortage2030*0.2706 if region=="AMR"
replace SEOtherShortage2030=SEShortage2030*0.4290 if region=="EMR"
replace SEOtherShortage2030=SEShortage2030*0.1945 if region=="EUR"
replace SEOtherShortage2030=SEShortage2030*0.4874 if region=="SEAR"
replace SEOtherShortage2030=SEShortage2030*0.2717 if region=="WPR"
replace SEShortage2030=SEShortage2030+SEOtherShortage2030
tabstat SEShortage2030, statistics( sum ) by(region)



* sensitivity with 30% absorption capacity
generate SE30emedtot2030=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(10-1)+10*emedgrad*0.3,1)
generate SE30enurtot2030=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(10-1)+10*enurgrad*0.3,1)
generate SE30emidtot2030=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(10-1)+10*emidgrad*0.3,1)
replace SE30emidtot2030=0 if SE30emidtot2030==.
generate SE30edentot2030=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(10-1)+10*edengrad*0.3,1)
generate SE30ephatot2030=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(10-1)+10*ephagrad*0.3,1)
generate SE30all2030=round(SE30emedtot2030+SE30enurtot2030+SE30emidtot2030+SE30edentot2030+SE30ephatot2030,1)
generate SE30Other2030=0
replace SE30Other2030=SE30all2030*0.4455 if region=="AFR"
replace SE30Other2030=SE30all2030*0.2706 if region=="AMR"
replace SE30Other2030=SE30all2030*0.4290 if region=="EMR"
replace SE30Other2030=SE30all2030*0.1945 if region=="EUR"
replace SE30Other2030=SE30all2030*0.4874 if region=="SEAR"
replace SE30Other2030=SE30all2030*0.2717 if region=="WPR"
generate SE30allplus2030=SE30all2030+SE30Other2030
tabstat SE30allplus2030, statistics( sum ) by(region)

generate SE30MedShortage2030=(12.83805-SE30emedtot2030/pop2030*10000)*pop/10000
replace SE30MedShortage2030=0 if SE30MedShortage2030<0
generate SE30NurShortage2030=(30.90968-SE30enurtot2030/pop2030*10000)*pop/10000
replace SE30NurShortage2030=0 if SE30NurShortage2030<0
generate SE30MidShortage2030=(3.815949-SE30emidtot2030/pop2030*10000)*pop/10000
replace SE30MidShortage2030=0 if SE30MidShortage2030<0
replace SE30MidShortage2030=0 if flagnurmiddoublecount==1
generate SE30DenShortage2030=(1.685815-SE30edentot2030/pop2030*10000)*pop/10000
replace SE30DenShortage2030=0 if SE30DenShortage2030<0
generate SE30PhaShortage2030=(1.948534-SE30ephatot2030/pop2030*10000)*pop/10000
replace SE30PhaShortage2030=0 if SE30PhaShortage2030<0
generate SE30Shortage2030=SE30MedShortage2030+SE30NurShortage2030+SE30MidShortage2030+SE30DenShortage2030+SE30PhaShortage2030
generate SE30OtherShortage2030=0
* Adding share of other occupations to the shortage based on relative proportion of other occupations
replace SE30OtherShortage2030=SE30Shortage2030*0.4455 if region=="AFR"
replace SE30OtherShortage2030=SE30Shortage2030*0.2706 if region=="AMR"
replace SE30OtherShortage2030=SE30Shortage2030*0.4290 if region=="EMR"
replace SE30OtherShortage2030=SE30Shortage2030*0.1945 if region=="EUR"
replace SE30OtherShortage2030=SE30Shortage2030*0.4874 if region=="SEAR"
replace SE30OtherShortage2030=SE30Shortage2030*0.2717 if region=="WPR"
replace SE30Shortage2030=SE30Shortage2030+SE30OtherShortage2030
tabstat SE30Shortage2030, statistics( sum ) by(region)




* sensitivity with 00% absorption capacity
generate SE00emedtot2030=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(10-1)+10*emedgrad*0,1)
generate SE00enurtot2030=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(10-1)+10*enurgrad*0,1)
generate SE00emidtot2030=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(10-1)+10*emidgrad*0,1)
replace SE00emidtot2030=0 if SE00emidtot2030==.
generate SE00edentot2030=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(10-1)+10*edengrad*0,1)
generate SE00ephatot2030=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(10-1)+10*ephagrad*0,1)
generate SE00all2030=round(SE00emedtot2030+SE00enurtot2030+SE00emidtot2030+SE00edentot2030+SE00ephatot2030,1)
generate SE00Other2030=0
replace SE00Other2030=SE00all2030*0.4455 if region=="AFR"
replace SE00Other2030=SE00all2030*0.2706 if region=="AMR"
replace SE00Other2030=SE00all2030*0.4290 if region=="EMR"
replace SE00Other2030=SE00all2030*0.1945 if region=="EUR"
replace SE00Other2030=SE00all2030*0.4874 if region=="SEAR"
replace SE00Other2030=SE00all2030*0.2717 if region=="WPR"
generate SE00allplus2030=SE00all2030+SE00Other2030
tabstat SE00allplus2030, statistics( sum ) by(region)

generate SE00MedShortage2030=(12.83805-SE00emedtot2030/pop2030*10000)*pop/10000
replace SE00MedShortage2030=0 if SE00MedShortage2030<0
generate SE00NurShortage2030=(30.90968-SE00enurtot2030/pop2030*10000)*pop/10000
replace SE00NurShortage2030=0 if SE00NurShortage2030<0
generate SE00MidShortage2030=(3.815949-SE00emidtot2030/pop2030*10000)*pop/10000
replace SE00MidShortage2030=0 if SE00MidShortage2030<0
replace SE00MidShortage2030=0 if flagnurmiddoublecount==1
generate SE00DenShortage2030=(1.685815-SE00edentot2030/pop2030*10000)*pop/10000
replace SE00DenShortage2030=0 if SE00DenShortage2030<0
generate SE00PhaShortage2030=(1.948534-SE00ephatot2030/pop2030*10000)*pop/10000
replace SE00PhaShortage2030=0 if SE00PhaShortage2030<0
generate SE00Shortage2030=SE00MedShortage2030+SE00NurShortage2030+SE00MidShortage2030+SE00DenShortage2030+SE00PhaShortage2030
generate SE00OtherShortage2030=0
* Adding share of other occupations to the shortage based on relative proportion of other occupations
replace SE00OtherShortage2030=SE00Shortage2030*0.4455 if region=="AFR"
replace SE00OtherShortage2030=SE00Shortage2030*0.2706 if region=="AMR"
replace SE00OtherShortage2030=SE00Shortage2030*0.4290 if region=="EMR"
replace SE00OtherShortage2030=SE00Shortage2030*0.1945 if region=="EUR"
replace SE00OtherShortage2030=SE00Shortage2030*0.4874 if region=="SEAR"
replace SE00OtherShortage2030=SE00Shortage2030*0.2717 if region=="WPR"
replace SE00Shortage2030=SE00Shortage2030+SE00OtherShortage2030
tabstat SE00Shortage2030, statistics( sum ) by(region)






* sensitivity with varying absorption capacity 70% (HIC), 60%, 50%, 40% (LIC)
generate vabso=0
replace vabso=0.7 if WBI=="HIN"
replace vabso=0.6 if WBI=="UMI"
replace vabso=0.5 if WBI=="LMI"
replace vabso=0.4 if WBI=="LIN"

generate SEvabsoemedtot2030=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(10-1)+10*emedgrad*vabso,1)
generate SEvabsoenurtot2030=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(10-1)+10*enurgrad*vabso,1)
generate SEvabsoemidtot2030=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(10-1)+10*emidgrad*vabso,1)
replace SEvabsoemidtot2030=0 if SEvabsoemidtot2030==.
generate SEvabsoedentot2030=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(10-1)+10*edengrad*vabso,1)
generate SEvabsoephatot2030=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(10-1)+10*ephagrad*vabso,1)
generate SEvabsoall2030=round(SEvabsoemedtot2030+SEvabsoenurtot2030+SEvabsoemidtot2030+SEvabsoedentot2030+SEvabsoephatot2030,1)
generate SEvabsoOther2030=0
replace SEvabsoOther2030=SEvabsoall2030*0.4455 if region=="AFR"
replace SEvabsoOther2030=SEvabsoall2030*0.2706 if region=="AMR"
replace SEvabsoOther2030=SEvabsoall2030*0.4290 if region=="EMR"
replace SEvabsoOther2030=SEvabsoall2030*0.1945 if region=="EUR"
replace SEvabsoOther2030=SEvabsoall2030*0.4874 if region=="SEAR"
replace SEvabsoOther2030=SEvabsoall2030*0.2717 if region=="WPR"
generate SEvabsoallplus2030=SEvabsoall2030+SEvabsoOther2030
tabstat SEvabsoallplus2030, statistics( sum ) by(region)

generate SEvabsoMedShortage2030=(12.83805-SEvabsoemedtot2030/pop2030*10000)*pop/10000
replace SEvabsoMedShortage2030=0 if SEvabsoMedShortage2030<0
generate SEvabsoNurShortage2030=(30.90968-SEvabsoenurtot2030/pop2030*10000)*pop/10000
replace SEvabsoNurShortage2030=0 if SEvabsoNurShortage2030<0
generate SEvabsoMidShortage2030=(3.815949-SEvabsoemidtot2030/pop2030*10000)*pop/10000
replace SEvabsoMidShortage2030=0 if SEvabsoMidShortage2030<0
replace SEvabsoMidShortage2030=0 if flagnurmiddoublecount==1
generate SEvabsoDenShortage2030=(1.685815-SEvabsoedentot2030/pop2030*10000)*pop/10000
replace SEvabsoDenShortage2030=0 if SEvabsoDenShortage2030<0
generate SEvabsoPhaShortage2030=(1.948534-SEvabsoephatot2030/pop2030*10000)*pop/10000
replace SEvabsoPhaShortage2030=0 if SEvabsoPhaShortage2030<0
generate SEvabsoShortage2030=SEvabsoMedShortage2030+SEvabsoNurShortage2030+SEvabsoMidShortage2030+SEvabsoDenShortage2030+SEvabsoPhaShortage2030
generate SEvabsoOtherShortage2030=0
* Adding share of other occupations to the shortage based on relative proportion of other occupations
replace SEvabsoOtherShortage2030=SEvabsoShortage2030*0.4455 if region=="AFR"
replace SEvabsoOtherShortage2030=SEvabsoShortage2030*0.2706 if region=="AMR"
replace SEvabsoOtherShortage2030=SEvabsoShortage2030*0.4290 if region=="EMR"
replace SEvabsoOtherShortage2030=SEvabsoShortage2030*0.1945 if region=="EUR"
replace SEvabsoOtherShortage2030=SEvabsoShortage2030*0.4874 if region=="SEAR"
replace SEvabsoOtherShortage2030=SEvabsoShortage2030*0.2717 if region=="WPR"
replace SEvabsoShortage2030=SEvabsoShortage2030+SEvabsoOtherShortage2030
tabstat SEvabsoShortage2030, statistics( sum ) by(region)



/* Extract for SSL revision
 
keep iso3 region country WBI MaxMeddensity MaxNurdensity MaxMiddensity flagnurmiddoublecount MaxDendensity MaxPhadensity ///
emeddensity2020 enurdensity2020 emiddensity2020 edendensity2020 ephadensity2020 Shortage2020 OtherShortage2020

tabstat Shortage2020, statistics( sum ) by(region)
export excel using "C:\Users\boniolm\OneDrive - World Health Organization\HWF\Code on Migration\Export-HWF2020 e shortage-20220405.xls", firstrow(variables) replace
*/


/* Export for Nelly for two scenarios */
/*
* scenario 1 50% absorption capacity
generate SE50emedtot2025=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(5-1)+5*emedgrad*0.5,1)
generate SE50enurtot2025=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(5-1)+5*enurgrad*0.5,1)
generate SE50emidtot2025=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(5-1)+10*emidgrad*0.5,1)
replace SE50emidtot2025=0 if SE50emidtot2025==.
generate SE50edentot2025=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(5-1)+5*edengrad*0.5,1)
generate SE50ephatot2025=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(5-1)+5*ephagrad*0.5,1)
* scenario 2 30% absorption capacity
generate SE30emedtot2025=round((emedtot2020*(1-medagepct64_))*(1-0.1*medagepct55_64)^(5-1)+5*emedgrad*0.3,1)
generate SE30enurtot2025=round((enurtot2020*(1-nuragepct64_))*(1-0.1*nuragepct55_64)^(5-1)+5*enurgrad*0.3,1)
generate SE30emidtot2025=round((emidtot2020*(1-midagepct64_))*(1-0.1*midagepct55_64)^(5-1)+5*emidgrad*0.3,1)
replace SE30emidtot2025=0 if SE30emidtot2025==.
generate SE30edentot2025=round((edentot2020*(1-denagepct64_))*(1-0.1*denagepct55_64)^(5-1)+5*edengrad*0.3,1)
generate SE30ephatot2025=round((ephatot2020*(1-phaagepct64_))*(1-0.1*phaagepct55_64)^(5-1)+5*ephagrad*0.3,1)
generate SE50emeddensity2025=SE50emedtot2025/pop2025*10000
generate SE50enurdensity2025=SE50enurtot2025/pop2025*10000
generate SE50emiddensity2025=SE50emidtot2025/pop2025*10000
generate SE50edendensity2025=SE50edentot2025/pop2025*10000
generate SE50ephadensity2025=SE50ephatot2025/pop2025*10000
generate SE30emeddensity2025=SE30emedtot2025/pop2025*10000
generate SE30enurdensity2025=SE30enurtot2025/pop2025*10000
generate SE30emiddensity2025=SE30emidtot2025/pop2025*10000
generate SE30edendensity2025=SE30edentot2025/pop2025*10000
generate SE30ephadensity2025=SE30ephatot2025/pop2025*10000
replace SE50emidtot2025=. if flagnurmiddoublecount==1
replace SE30emidtot2025=. if flagnurmiddoublecount==1
replace SE50emiddensity2025=. if flagnurmiddoublecount==1
replace SE30emiddensity2025=. if flagnurmiddoublecount==1

keep region country iso3 SE50emedtot2025 SE50enurtot2025 SE50emidtot2025 SE50edentot2025 SE50ephatot2025 SE30emedtot2025 ///
SE30enurtot2025 SE30emidtot2025 SE30edentot2025 SE30ephatot2025 SE50emeddensity2025 SE50enurdensity2025 SE50emiddensity2025 ///
SE50edendensity2025 SE50ephadensity2025 SE30emeddensity2025 SE30enurdensity2025 SE30emiddensity2025 SE30edendensity2025 SE30ephadensity2025
export delimited using "C:\Users\boniolm\OneDrive - World Health Organization\Articles\Workforce 2018 and projection to 2030\HWF-Covid-19-Scenarios-2025-20220310.csv", replace
*/
/* ************************************** */


**** Other shortages 

tabstat pop2020 emeddensity2020 enurdensity2020 emiddensity2020 edendensity2020 ephadensity2020, statistics( sum ) by(iso3)
tabstat pop2030 emeddensity2030 enurdensity2030 emiddensity2030 edendensity2030 ephadensity2030 , statistics( sum ) by(iso3)

keep iso3 region WBI pop2020 emeddensity2020 enurdensity2020 emiddensity2020 edendensity2020 ephadensity2020 ///
pop2030 emeddensity2030 enurdensity2030 emiddensity2030 edendensity2030 ephadensity2030 

save eden202030, replace

merge 1:1 iso3 region WBI using eden2013
replace pop2020=pop2030 if pop2020==.
replace emiddensity2020=0 if emiddensity2020==.
replace emiddensity2030=0 if emiddensity2030==.
drop _merge
save eden20132030,replace
clear
import delimited "C:\Users\boniolm\OneDrive - World Health Organization\Databases\UHCSCI2017.csv"
merge 1:1 iso3 using eden20132030
drop _merge
generate edenall2013=emeddensity2013+enurdensity2013+emiddensity2013+edendensity2013+ephadensity2013
generate edenall2020=emeddensity2020+enurdensity2020+emiddensity2020+edendensity2020+ephadensity2020
generate edenall2030=emeddensity2030+enurdensity2030+emiddensity2030+edendensity2030+ephadensity2030
generate alldenplus2013=0
generate alldenplus2020=0
generate alldenplus2030=0
replace alldenplus2013=edenall2013*1.4455 if region=="AFR"
replace alldenplus2013=edenall2013*1.2706 if region=="AMR"
replace alldenplus2013=edenall2013*1.4290 if region=="EMR"
replace alldenplus2013=edenall2013*1.1945 if region=="EUR"
replace alldenplus2013=edenall2013*1.4874 if region=="SEAR"
replace alldenplus2013=edenall2013*1.2717 if region=="WPR"
replace alldenplus2020=edenall2020*1.4455 if region=="AFR"
replace alldenplus2020=edenall2020*1.2706 if region=="AMR"
replace alldenplus2020=edenall2020*1.4290 if region=="EMR"
replace alldenplus2020=edenall2020*1.1945 if region=="EUR"
replace alldenplus2020=edenall2020*1.4874 if region=="SEAR"
replace alldenplus2020=edenall2020*1.2717 if region=="WPR"
replace alldenplus2030=edenall2030*1.4455 if region=="AFR"
replace alldenplus2030=edenall2030*1.2706 if region=="AMR"
replace alldenplus2030=edenall2030*1.4290 if region=="EMR"
replace alldenplus2030=edenall2030*1.1945 if region=="EUR"
replace alldenplus2030=edenall2030*1.4874 if region=="SEAR"
replace alldenplus2030=edenall2030*1.2717 if region=="WPR"
generate popM2030=pop2030/1000000

tabstat alldenplus2013 alldenplus2020 alldenplus2030 popM2030 uhcsci, statistics(sum) by(iso3)
tabstat alldenplus2013 alldenplus2020 alldenplus2030 popM2030 uhcsci if region=="WPR", statistics(sum) by(iso3)

tabstat uhcsci, statistics(n p50 mean min max) by(region)



generate shortage2013UHC50=(14.9-alldenplus2013)/10000*pop2013
generate shortage2013UHC60=(34.6-alldenplus2013)/10000*pop2013
generate shortage2013UHC70=(80.2-alldenplus2013)/10000*pop2013
generate shortage2013UHC75=(122.3-alldenplus2013)/10000*pop2013
generate shortage2013UHC80=(186.3-alldenplus2013)/10000*pop2013
generate shortage2013UHC85=(283.8-alldenplus2013)/10000*pop2013
replace shortage2013UHC50=0 if shortage2013UHC50<0
replace shortage2013UHC60=0 if shortage2013UHC60<0
replace shortage2013UHC70=0 if shortage2013UHC70<0
replace shortage2013UHC75=0 if shortage2013UHC75<0
replace shortage2013UHC80=0 if shortage2013UHC80<0
replace shortage2013UHC85=0 if shortage2013UHC85<0

generate shortage2020UHC50=(14.9-alldenplus2020)/10000*pop2020
generate shortage2020UHC60=(34.6-alldenplus2020)/10000*pop2020
generate shortage2020UHC70=(80.2-alldenplus2020)/10000*pop2020
generate shortage2020UHC75=(122.3-alldenplus2020)/10000*pop2020
generate shortage2020UHC80=(186.3-alldenplus2020)/10000*pop2020
generate shortage2020UHC85=(283.8-alldenplus2020)/10000*pop2020
replace shortage2020UHC50=0 if shortage2020UHC50<0
replace shortage2020UHC60=0 if shortage2020UHC60<0
replace shortage2020UHC70=0 if shortage2020UHC70<0
replace shortage2020UHC75=0 if shortage2020UHC75<0
replace shortage2020UHC80=0 if shortage2020UHC80<0
replace shortage2020UHC85=0 if shortage2020UHC85<0

generate shortage2030UHC50=(14.9-alldenplus2030)/10000*pop2030
generate shortage2030UHC60=(34.6-alldenplus2030)/10000*pop2030
generate shortage2030UHC70=(80.2-alldenplus2030)/10000*pop2030
generate shortage2030UHC75=(122.3-alldenplus2030)/10000*pop2030
generate shortage2030UHC80=(186.3-alldenplus2030)/10000*pop2030
generate shortage2030UHC85=(283.8-alldenplus2030)/10000*pop2030
replace shortage2030UHC50=0 if shortage2030UHC50<0
replace shortage2030UHC60=0 if shortage2030UHC60<0
replace shortage2030UHC70=0 if shortage2030UHC70<0
replace shortage2030UHC75=0 if shortage2030UHC75<0
replace shortage2030UHC80=0 if shortage2030UHC80<0
replace shortage2030UHC85=0 if shortage2030UHC85<0


tabstat shortage2013UHC50 shortage2013UHC60 shortage2013UHC70 shortage2013UHC80 , statistics( sum ) by(region)
tabstat shortage2020UHC50 shortage2020UHC60 shortage2020UHC70 shortage2020UHC80 , statistics( sum ) by(region)
tabstat shortage2030UHC50 shortage2030UHC60 shortage2030UHC70 shortage2030UHC80 , statistics( sum ) by(region)



/* Generate shortage for WPRO
generate shortage2013MedianWPRO=(76.85-alldenplus2013)/10000*pop2013
generate shortage2020MedianWPRO=(76.85-alldenplus2013)/10000*pop2020
generate shortage2030MedianWPRO=(76.85-alldenplus2013)/10000*pop2030
replace shortage2013MedianWPRO=0 if shortage2013MedianWPRO<0
replace shortage2020MedianWPRO=0 if shortage2020MedianWPRO<0
replace shortage2030MedianWPRO=0 if shortage2030MedianWPRO<0

keep if region=="WPR"
export excel using "C:\Users\boniolm\OneDrive - World Health Organization\NHWA\Data\Extracts\WPRO shortage\WPRO-Shortages-20220404.xls",  firstrow(variables) replace
*/



*** Create maps for all health workforce
clear
import excel "C:\Users\boniolm\OneDrive - World Health Organization\Databases\MapTemplate_generalized_2013\MapTemplate_generalized_2013\general_2013-ID.xlsx", sheet("Sheet1") firstrow
generate id=OrderID
rename ISO_3_CODE iso3
merge m:1 iso3 using SDGData4Analysis
drop _merge
keep iso3 id WBI region pop emedtot2020 enurtot2020 emidtot2020 edentot2020 ephatot2020
replace emidtot2020=0 if emidtot2020==. & enurtot2020!=.
generate eHWFdensity2020=10000*(emedtot2020+enurtot2020+emidtot2020+edentot2020+ephatot2020)/pop

summarize eHWFdensity2020, detail
histogram eHWFdensity2020
* Interesting colors sequentials (9 colors max): Blues, PuBu, Greys
* Interesting colors diverging (9 colors max): BuRd, RdBu

spmap eHWFdensity2020 using "general_2013.dta", id(id) fcolor(RdBu) ///
 ocolor(black ..) osize(vthin ..) legend(position(9)) ///
 clmethod(custom) clbreak(0 5 10 20 30 50 75 100 125 150 400) ///
 legend(label(1 "missing") ///
 label(2 "<= 5") ///
 label(3 "5 to 9") ///
 label(4 "10 to 19") ///
 label(5 "20 to 29") ///
 label(6 "30 to 49") ///
 label(7 "50 to 74") ///
 label(8 "75 to 99") ///
 label(9 "100 to 124") ///
 label(10 "125 to 149") ///
 label(11 "150+")) ///
 legorder(lohi) ///
 legtitle("Health workforce density per 10,000 population") ///
 ti("", size(medium)) ///
 subtitle("", size(small)) ///
 note("* Latest available density as of 2020.", size(vsmall)) ///
 caption("incl. medical doctors, nursing personnel, midwifery personnel, dentists, pharmacists", size(vsmall))







/* ############################################################################## */
/* ############################################################################## */
/* ############################################################################## */
/* ############################################################################## */
/* #####         #####         #####          ######       #######  ####### ##### */
/* #####  ###### #####  ####### ####  #############  #######  ####  #####  ###### */
/* #####  ####### ####  ####### ####  #############  #######  ####  ###  ######## */
/* #####         #####  ###### #####       ########  #######  ####     ########## */
/* #####  ###### #####        ######  #############         # ####  ##  ######### */
/* #####  ######  ####  ######  ####  #############  #######  ####  ###  ######## */
/* #####  ####### ####  ######  ####  #############  #######  ####  ####  ####### */
/* #####         #####  ######  ####          #####  #######  ####  #####  ###### */
/* ############################################################################## */
/* ############################################################################## */
/* ############################################################################## */
/* ############################################################################## */
/* ############################################################################## */



































































* Create occupation specific maps
clear
import excel "C:\Users\boniolm\OneDrive - World Health Organization\Databases\MapTemplate_generalized_2013\MapTemplate_generalized_2013\general_2013-ID.xlsx", sheet("Sheet1") firstrow
generate id=OrderID
rename ISO_3_CODE iso3
merge m:1 iso3 using SDGData4Analysis
drop _merge

summarize emeddensity2020, detail
histogram emeddensity2020
* Interesting colors sequentials (9 colors max): Blues, PuBu, Greys
* Interesting colors diverging (9 colors max): BuRd, RdBu

spmap emeddensity2020 using "general_2013.dta", id(id) fcolor(RdBu) ///
 ocolor(black ..) osize(vthin ..) legend(position(9)) ///
 clmethod(custom) clbreak(0 1 5 10 15 20 25 30 40 50 100) ///
 legend(label(1 "missing") ///
 label(2 "<= 1") ///
 label(3 "1 to 5") ///
 label(4 "5 to 9") ///
 label(5 "10 to 14") ///
 label(6 "15 to 19") ///
 label(7 "20 to 24") ///
 label(8 "25 to 29") ///
 label(9 "30 to 39") ///
 label(10 "40 to 49") ///
 label(11 "50+")) ///
 legorder(lohi) ///
 legtitle("Medical doctors density per 10,000 pop.") ///
 ti("", size(medium)) ///
 subtitle("", size(small)) ///
 note("* Latest available density as of 2020.", size(vsmall)) ///
 caption("Source: National Health Workforce Accounts, WHO 2020.", size(vsmall))

generate enurmiddensity2020=enurdensity2020
replace enurmiddensity2020=enurdensity2020+emiddensity2020 if emiddensity2020!=. 
 summarize enurmiddensity2020, detail
histogram enurmiddensity2020
* Interesting colors sequentials (9 colors max): Blues, PuBu, Greys
* Interesting colors diverging (9 colors max): BuRd, RdBu
spmap enurmiddensity2020 using "general_2013.dta", id(id) fcolor(RdBu) ///
 ocolor(black ..) osize(vthin ..) legend(position(9)) ///
 clmethod(custom) clbreak(0 1 5 10 20 30 40 50 75 100 200) ///
 legend(label(1 "missing") ///
 label(2 "<= 1") ///
 label(3 "1 to 5") ///
 label(4 "5 to 9") ///
 label(5 "10 to 19") ///
 label(6 "20 to 29") ///
 label(7 "30 to 39") ///
 label(8 "40 to 49") ///
 label(9 "50 to 75") ///
 label(10 "75 to 100") ///
 label(11 "100+")) ///
 legorder(lohi) ///
 legtitle("Nursing and midwifery personnel density per 10,000 pop.") ///
 ti("", size(medium)) ///
 subtitle("", size(small)) ///
 note("* Latest available density as of 2020.", size(vsmall)) ///
 caption("Source: National Health Workforce Accounts, WHO 2020.", size(vsmall))



summarize edendensity2020, detail
histogram edendensity2020
* Interesting colors sequentials (9 colors max): Blues, PuBu, Greys
* Interesting colors diverging (9 colors max): BuRd, RdBu
spmap edendensity2020 using "general_2013.dta", id(id) fcolor(RdBu) ///
 ocolor(black ..) osize(vthin ..) legend(position(9)) ///
 clmethod(custom) clbreak(0 0.5 1 1.5 2 3 4 5 6 10 30) ///
 legend(label(1 "missing") ///
 label(2 "<= 0.5") ///
 label(3 "0.5 to 0.9") ///
 label(4 "1 to 1.4") ///
 label(5 "1.5 to 1.9") ///
 label(6 "2 to 2.9") ///
 label(7 "3 to 3.9") ///
 label(8 "4 to 4.9") ///
 label(9 "5 to 5.9") ///
 label(10 "6 to 9.9") ///
 label(11 "10+")) ///
 legorder(lohi) ///
 legtitle("Dentists density per 10,000 pop.") ///
 ti("", size(medium)) ///
 subtitle("", size(small)) ///
 note("* Latest available density as of 2020.", size(vsmall)) ///
 caption("Source: National Health Workforce Accounts, WHO 2020.", size(vsmall))


summarize ephadensity2020, detail
histogram ephadensity2020
* Interesting colors sequentials (9 colors max): Blues, PuBu, Greys
* Interesting colors diverging (9 colors max): BuRd, RdBu
spmap ephadensity2020 using "general_2013.dta", id(id) fcolor(RdBu) ///
 ocolor(black ..) osize(vthin ..) legend(position(9)) ///
 clmethod(custom) clbreak(0 0.5 1 1.5 2 3 4 5 6 10 30) ///
 legend(label(1 "missing") ///
 label(2 "<= 0.5") ///
 label(3 "0.5 to 0.9") ///
 label(4 "1 to 1.4") ///
 label(5 "1.5 to 1.9") ///
 label(6 "2 to 2.9") ///
 label(7 "3 to 3.9") ///
 label(8 "4 to 4.9") ///
 label(9 "5 to 5.9") ///
 label(10 "6 to 9.9") ///
 label(11 "10+")) ///
 legorder(lohi) ///
 legtitle("Pharmacists density per 10,000 pop.") ///
 ti("", size(medium)) ///
 subtitle("", size(small)) ///
 note("* Latest available density as of 2020.", size(vsmall)) ///
 caption("Source: National Health Workforce Accounts, WHO 2020.", size(vsmall))


