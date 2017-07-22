set more off
cd "C:\Users\kdas\Dropbox\WASHB-cognitive-development-datasets"
* diarrhea morbidity year 1
use "washb-bangladesh-diar.dta", clear
merge m:1 dataid using "02.WASHB_Endline_Arm_Identification.dta"
keep if svy==1
keep if tchild==1
keep dataid childid  diar7d arm block
tempfile diar_y1
save `diar_y1'
* diarrhea morbidity year 2
use "washb-bangladesh-diar.dta", clear
merge m:1 dataid using "02.WASHB_Endline_Arm_Identification.dta"
keep if svy==2
keep if tchild==1
keep dataid childid  diar7d arm block
tempfile diar_y2
save `diar_y2'
* mom depression year 1
use "washb-bangladesh-momdepression-year1.dta", clear
keep dataid childid arm z_midline_depression
tempfile momdep_y1
save  `momdep_y1'
* mom depression year 2
use "washb-bangladesh-momdepression-year2.dta", clear
keep dataid childid arm z_endline_depression
tempfile momdep_y2
save `momdep_y2'
* easq y2
use "washb-bangladesh-easq-year2.dta", clear
keep dataid childid arm block z_com z_motor z_personal
tempfile  easq2
save `easq2'
****************************
*Table 12
****************************
use `diar_y1.dta', clear
merge 1:1 dataid childid using `easq2.dta'

glm z_com diar7d i.arm i.block, cluster(block)   
glm z_motor diar7d i.arm i.block, cluster(block)
glm z_personal diar7d i.arm i.block, cluster(block)

use `diar_y2.dta' , clear
merge 1:1 dataid childid using `easq2.dta'

glm z_com diar7d i.arm i.block, cluster(block)   
glm z_motor diar7d i.arm i.block, cluster(block)
glm z_personal diar7d i.arm i.block, cluster(block)

use `momdep_y1', clear
merge 1:1 dataid childid using `easq2.dta'

glm z_com    z_midline_depression i.arm i.block, cluster(block)
glm z_motor    z_midline_depression i.arm i.block, cluster(block)
glm z_personal    z_midline_depression i.arm i.block, cluster(block)

use `momdep_y2', clear
merge 1:1 dataid childid using `easq2.dta'

glm z_com    z_endline_depression i.arm i.block, cluster(block)
glm z_motor    z_endline_depression i.arm i.block, cluster(block)
glm z_personal    z_endline_depression i.arm i.block, cluster(block)
***************************
*Table 13
***************************
use `diar_y1.dta', clear
merge 1:1 dataid childid using `momdep_y1'
keep if _merge==3
glm z_midline_depression i.diar7d i.block, cluster(block)

use `diar_y2.dta', clear
merge 1:1 dataid childid using `momdep_y2'
keep if _merge==3
glm z_endline_depression i.diar7d i.block, cluster(block)

*********************************************************

