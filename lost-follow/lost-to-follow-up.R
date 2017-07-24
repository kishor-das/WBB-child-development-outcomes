#---------------------------
# tabs10-lost-to-follow-up.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# check whether enrollment chara-
# cteristics are balanced over 
# households we followed and 
# lost to follow households
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-enrol.dta
#            washb-bangladesh-lost-follow-up.dta
# output files:
#            lost-balance.Rdata
#---------------------------

#---------------------------
# preamble
#---------------------------
rm(list = ls())

library(foreign) # for read.dta() function
library(plyr)    # for rename() function 
library(washb)   # https://ben-arnold.github.io/washb/

# function formatting results
dformat<-function(x,d){
  x<-round(x,d)+0.000 # removing signed zero
  x<-formatC(x,format="f",digits = d)
  return(x)
}


#-------------------------------
# load the analysis dataset
#-------------------------------
enrol<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-enrol.dta")
lost<-  read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-lost-follow-up.dta")

lost<-rename(lost, replace=c("status_endline"="status"))
enrol<-merge(enrol,lost,by="dataid")

group<-c("","Complete","Incomplete")

#--------------------------------------------
# enroll characteristics summary measures
# by followed and lost to folowed households
#--------------------------------------------

n_households<-NULL
n_households[""]<-"No. of compounds"
for(grp in group[2:3]){
  n_households[grp]<-table(enrol$status)[[grp]]
}

n_clust<-NULL
n_clust[""]<-"No. of clusters"
  ind<-enrol$status==group[2]
  n_clust[group[2]]<-length(unique(enrol[ind,]$cluster))  
  n_clust[group[3]]<-720-as.numeric(n_clust[group[2]])
  


mom_age<-NULL
mom_age[""]<-"\u005cMyIndent    Age in year"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  mom_age[grp]<-sprintf("%1.f",round(mean(enrol[ind,]$momage,na.rm=TRUE),0))
}

mom_edu<-NULL
mom_edu[""]<-"\u005cMyIndent    Years of education"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  mom_edu[grp]<-sprintf("%1.f",mean(enrol[ind,]$momeduy,na.rm=TRUE)) 
}

dad_edu<- NULL
dad_edu[""]<-"\u005cMyIndent    Years of education"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  dad_edu[grp]<-sprintf("%1.f",mean(enrol[ind,]$dadeduy,na.rm=TRUE))
}

dad_agri<-NULL
dad_agri[""]<-"\u005cMyIndent    Works in agriculture (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  dad_agri[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$dadagri,na.rm=TRUE),3)*100)
}

hh_np<-NULL
hh_np[""]<-"\u005cMyIndent    Number of persons"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  hh_np[grp]<-sprintf("%1.f",mean(enrol[ind,]$Nhh,na.rm=TRUE))
}

hh_elec<-NULL
hh_elec[""]<-"\u005cMyIndent    Has electricity (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  hh_elec[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$elec,na.rm=TRUE),3)*100)
}

hh_cement<-NULL
hh_cement[""]<-"\u005cMyIndent Has a cement floor (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  hh_cement[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$cement,na.rm=TRUE),3)*100)
}

hh_land<-NULL
hh_land[""]<-"\u005cMyIndent Acres of agricultural land owned"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  hh_land[grp]<-sprintf("%.2f",mean(enrol[ind,]$landacre,na.rm=TRUE))
}

dw_tubewell<-NULL
dw_tubewell[""]<-"\u005cMyIndent Tubewell primary water source (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  dw_tubewell[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$tubewell,na.rm=TRUE),3)*100)
}

dw_storewat<-NULL
dw_storewat[""]<-"\u005cMyIndent    Stored water observed at home (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  dw_storewat[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$storewat,na.rm=TRUE),3)*100)
}

san_odmen<-NULL
san_odmen[""]<- "\u005cMyIndentDbl    Adult men (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_odmen[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$odmen,na.rm=TRUE),3)*100)
}

san_odwom<-NULL
san_odwom[""]<- "\u005cMyIndentDbl    Adult women (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_odwom[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$odwom,na.rm=TRUE),3)*100)
}
san_odch815<-NULL
san_odch815[""]<- "\u005cMyIndentDbl    Children: 8-<15 years (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_odch815[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$odch815,na.rm=TRUE),3)*100)
}
san_odch38<-NULL 
san_odch38[""]<- "\u005cMyIndentDbl    Children: 3-<8 years (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_odch38[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$odch38,na.rm=TRUE),3)*100)
}
san_odchu3<-NULL
san_odchu3[""]<- "\u005cMyIndentDbl    Children: 0-<3 years (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_odchu3[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$odchu3,na.rm=TRUE),3)*100)
} 

san_latown <- NULL
san_latown[""] <- "\u005cMyIndentDbl    Owned (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_latown[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$latown,na.rm=TRUE),3)*100)
}
san_latslab <- NULL 
san_latslab[""] <- "\u005cMyIndentDbl    Concrete slab (\u005c%)" 
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_latslab[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$latslab,na.rm=TRUE),3)*100)
}
san_latseal  <- NULL
san_latseal[""]  <-  "\u005cMyIndentDbl    Functional water seal (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_latseal[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$latseal,na.rm=TRUE),3)*100)
}
san_latfeces  <- NULL
san_latfeces[""]  <-  "\u005cMyIndentDbl    Visible stool on slab or floor (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_latfeces[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$latfeces,na.rm=TRUE),3)*100)
}
san_potty  <- NULL
san_potty[""]  <-  "\u005cMyIndent    Owned a potty (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_potty[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$potty,na.rm=TRUE),3)*100)
}
san_humfeces  <- NULL
san_humfeces[""]  <-  "\u005cMyIndentDbl    House (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_humfeces[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$humfeces,na.rm=TRUE),3)*100)
}
san_humfecesch <- NULL
san_humfecesch[""] <-  "\u005cMyIndentDbl    Child's play area (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  san_humfecesch[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$humfecesch,na.rm=TRUE),3)*100)
}
hw_hwlatwat  <- NULL
hw_hwlatwat[""]  <-  "\u005cMyIndentDbl    Has water (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  hw_hwlatwat[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$hwlatwat,na.rm=TRUE),3)*100)
}
hw_hwlatsoap  <- NULL
hw_hwlatsoap[""]  <- "\u005cMyIndentDbl     Has soap (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  hw_hwlatsoap[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$hwlatsoap,na.rm=TRUE),3)*100)
}
hw_hwkitwat  <- NULL
hw_hwkitwat[""]  <- "\u005cMyIndentDbl    Has water (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  hw_hwkitwat[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$hwkitwat,na.rm=TRUE),3)*100)
}
hw_hwkitsoap <- NULL
hw_hwkitsoap[""] <- "\u005cMyIndentDbl    Has soap (\u005c%)"
for(grp in group[2:3]){
  ind<-enrol$status==grp
  hw_hwkitsoap[grp]<-sprintf("%.1f",round(mean(enrol[ind,]$hwkitsoap,na.rm=TRUE),3)*100)
}


glue_maternal<-c("\u005ctextbf{Maternal}","","")
glue_paternal<-c("\u005ctextbf{Paternal}","","")
glue_household<-c("\u005ctextbf{Household}","","")
glue_dw<-c("\u005ctextbf{Drinking Water}","","")
glue_san<-c("\u005ctextbf{Sanitation}","","")
glue_od<-c("\u005cMyIndent Daily defecation in the open","","")
glue_lat<-c("\u005cMyIndent Latrine","","")
glue_hfo<-c("\u005cMyIndent Human feces observed in the","","")
glue_hw<-c("\u005ctextbf{Handwashing}","","")
glue_steplat<-c("\u005cMyIndent Within 6 steps of latrine","","")
glue_stepkit<-c("\u005cMyIndent Within 6 steps of kitchen","","")

lost_balance<-rbind(
  n_households,
  glue_maternal,
  mom_age,
  mom_edu,
  glue_paternal,
  dad_edu,
  dad_agri,
  glue_household,
  hh_np,
  hh_elec,
  hh_cement,
  hh_land,
  glue_dw,
  dw_tubewell,
  dw_storewat,
  glue_san,
  glue_od,
  san_odmen,
  san_odwom,
  san_odch815,
  san_odch38,
  san_odchu3,
  glue_lat,
  san_latown,
  san_latslab,
  san_latseal,
  san_latfeces,
  san_potty,
  glue_hfo,
  san_humfeces,
  san_humfecesch,
  glue_hw,
  glue_steplat,
  hw_hwlatwat,
  hw_hwlatsoap,
  glue_stepkit,
  hw_hwkitwat,
  hw_hwkitsoap
)

save(lost_balance,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\lost-balance.Rdata")