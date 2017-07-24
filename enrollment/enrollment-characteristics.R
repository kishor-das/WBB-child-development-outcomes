#------------------------------------
# tab1-enrollment-characteristics.R
#
# Kishor Das (kishorisrt@gmail.com)
# 
# summary measures of enrollment
# characteristics by arm
#------------------------------------

#---------------------------------------
# input files :
#            washb-bangladesh-enrol.dta
#            washb-bangladesh-arm.dta
# output files:
#            enroll-balance.Rdata
#---------------------------------------

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
arm<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-arm.dta")

enrol<-merge(enrol,arm,by="dataid")

group<-c("","Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N")

#----------------------------------------------
# summary measures of enrolment characteristics
#----------------------------------------------

n_clust<-NULL
n_clust[""]<-"No. of clusters"
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  n_clust[arm]<-length(unique(enrol[ind,]$cluster))
}


n_households<-NULL
n_households[""]<-"No. of compounds"
for(arm in group[2:8]){
  n_households[arm]<-paste("(N = ",table(enrol$arm)[[arm]],")",sep="")
}

mom_age<-NULL
mom_age[""]<-"\u005cMyIndent    Age in year"
mom_age_mean<-NULL
mom_age_sd<-NULL
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  mom_age_mean[arm]<-dformat(mean(enrol[ind,]$momage,na.rm=TRUE),1)
  mom_age_sd[arm]<-dformat(sd(enrol[ind,]$momage,na.rm=TRUE),1)
  mom_age[arm]<- paste(mom_age_mean[arm]," $\u005cpm$ ",mom_age_sd[arm],sep="")  
}

mom_edu<-NULL
mom_edu[""]<-"\u005cMyIndent    Years of education"
mom_edu_mean<-NULL
mom_edu_sd<-NULL
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  mom_edu_mean[arm]<-dformat(mean(enrol[ind,]$momeduy,na.rm=TRUE),1)
  mom_edu_sd[arm]<-dformat(sd(enrol[ind,]$momeduy,na.rm=TRUE),1)
  mom_edu[arm]<- paste(mom_edu_mean[arm]," $\u005cpm$ ",mom_edu_sd[arm])
}

dad_edu<- NULL
dad_edu[""]<-"\u005cMyIndent    Years of education"
dad_edu_mean<- NULL
dad_edu_sd<- NULL
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  dad_edu_mean[arm]<-dformat(mean(enrol[ind,]$dadeduy,na.rm=TRUE),1)
  dad_edu_sd[arm]<-dformat(sd(enrol[ind,]$dadeduy,na.rm=TRUE),1)
  dad_edu[arm]<-paste(dad_edu_mean[arm]," $\u005cpm$ ",dad_edu_sd[arm])
}

dad_agri<-NULL
dad_agri[""]<-"\u005cMyIndent    Works in agriculture "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  dad_agrimean<-mean(enrol[ind,]$dadagri,na.rm=TRUE)*100
  dad_agri[arm]<-dformat(dad_agrimean,1)
}

hh_st<-NULL
hh_st[""]<-"\u005cMyIndent    Child stimulation $\u005cdagger$"
hh_st_mean<-NULL
hh_st_sd<-NULL
for(arm in group[2:8]){
  ind<-home$arm==arm
  hh_st_mean[arm]<-dformat(round(mean(home[ind,]$z_home,na.rm=TRUE),3),2)
  hh_st_sd[arm]<-dformat(round(sd(home[ind,]$z_home,na.rm=TRUE),3),2)
  hh_st[arm]<-paste(hh_st_mean[arm]," $\u005cpm$ ",hh_st_sd[arm])
}


hh_np<-NULL
hh_np[""]<-"\u005cMyIndent    Number of persons"
hh_np_mean<-NULL
hh_np_sd<-NULL
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  hh_np_mean[arm]<-dformat(mean(enrol[ind,]$Nhh,na.rm=TRUE),1)
  hh_np_sd[arm]<-dformat(sd(enrol[ind,]$Nhh,na.rm=TRUE),1)
  hh_np[arm]<- paste(hh_np_mean[arm]," $\u005cpm$ ",hh_np_sd[arm])
}

hh_elec<-NULL
hh_elec[""]<-"\u005cMyIndent    Has electricity "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  hh_elecmean<-mean(enrol[ind,]$elec,na.rm=TRUE)*100
  hh_elec[arm]<-dformat(hh_elecmean,1)
}

hh_cement<-NULL
hh_cement[""]<-"\u005cMyIndent Has a cement floor "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  hh_cementmean<-mean(enrol[ind,]$cement,na.rm=TRUE)*100
  hh_cement[arm]<-dformat(hh_cementmean,1)
}

hh_land<-NULL
hh_land[""]<-"\u005cMyIndent Acres of agricultural land owned"
hh_land_mean<-NULL
hh_land_sd<-NULL
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  hh_land_mean[arm]<-dformat(mean(enrol[ind,]$landacre,na.rm=TRUE),2)
  hh_land_sd[arm]<-dformat(sd(enrol[ind,]$landacre,na.rm=TRUE),2)
  hh_land[arm]<- paste(hh_land_mean[arm]," $\u005cpm$ ",hh_land_sd[arm])
}

dw_tubewell<-NULL
dw_tubewell[""]<-"\u005cMyIndent Tubewell primary water source "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  dw_tubewellmean<-mean(enrol[ind,]$tubewell,na.rm=TRUE)*100
  dw_tubewell[arm]<-dformat(dw_tubewellmean,1)
}

dw_storewat<-NULL
dw_storewat[""]<-"\u005cMyIndent    Stored water observed at home "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  dw_storewatmean<-mean(enrol[ind,]$storewat,na.rm=TRUE)*100
  dw_storewat[arm]<-dformat( dw_storewatmean,1)
}

san_odmen<-NULL
san_odmen[""]<- "\u005cMyIndentDbl    Adult men "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_odmenmean<-mean(enrol[ind,]$odmen,na.rm=TRUE)*100
  san_odmen[arm]<-dformat( san_odmenmean,1)
}

san_odwom<-NULL
san_odwom[""]<- "\u005cMyIndentDbl    Adult women "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_odwommean<-mean(enrol[ind,]$odwom,na.rm=TRUE)*100
  san_odwom[arm]<-dformat(san_odwommean ,1)
}
san_odch815<-NULL
san_odch815[""]<- "\u005cMyIndentDbl    Children: 8-<15 years "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_odch815mean<-mean(enrol[ind,]$odch815,na.rm=TRUE)*100
  san_odch815[arm]<-dformat(san_odch815mean ,1)
}
san_odch38<-NULL 
san_odch38[""]<- "\u005cMyIndentDbl    Children: 3-<8 years "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_odch38mean<-mean(enrol[ind,]$odch38,na.rm=TRUE)*100
  san_odch38[arm]<-dformat( san_odch38mean,1)
}
san_odchu3<-NULL
san_odchu3[""]<- "\u005cMyIndentDbl    Children: 0-<3 years "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_odchu3mean<-mean(enrol[ind,]$odchu3,na.rm=TRUE)*100
  san_odchu3[arm]<-dformat(san_odchu3mean ,1)
} 



san_latown <- NULL
san_latown[""] <- "\u005cMyIndentDbl    Owned "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_latownmean<-mean(enrol[ind,]$latown,na.rm=TRUE)*100
  san_latown[arm]<-dformat( san_latownmean,1)
}
san_latslab <- NULL 
san_latslab[""] <- "\u005cMyIndentDbl    Concrete slab " 
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_latslabmean<-mean(enrol[ind,]$latslab,na.rm=TRUE)*100
  san_latslab[arm]<-dformat(san_latslabmean ,1)
}
san_latseal  <- NULL
san_latseal[""]  <-  "\u005cMyIndentDbl    Functional water seal "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_latsealmean<-mean(enrol[ind,]$latseal,na.rm=TRUE)*100
  san_latseal[arm]<-dformat( san_latsealmean,1)
}
san_latfeces  <- NULL
san_latfeces[""]  <-  "\u005cMyIndentDbl    Visible stool on slab or floor "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_latfecesmean<-mean(enrol[ind,]$latfeces,na.rm=TRUE)*100
  san_latfeces[arm]<-dformat( san_latfecesmean,1)
}
san_potty  <- NULL
san_potty[""]  <-  "\u005cMyIndent    Owned a potty "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_pottymean<-mean(enrol[ind,]$potty,na.rm=TRUE)*100
  san_potty[arm]<-dformat( san_pottymean,1)
}
san_humfeces  <- NULL
san_humfeces[""]  <-  "\u005cMyIndentDbl    House "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_humfecesmean<-mean(enrol[ind,]$humfeces,na.rm=TRUE)*100
  san_humfeces[arm]<-dformat( san_humfecesmean,1)
}
san_humfecesch <- NULL
san_humfecesch[""] <-  "\u005cMyIndentDbl    Child's play area "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  san_humfeceschmean<-mean(enrol[ind,]$humfecesch,na.rm=TRUE)*100
  san_humfecesch[arm]<-dformat( san_humfeceschmean,1)
}
hw_hwlatwat  <- NULL
hw_hwlatwat[""]  <-  "\u005cMyIndentDbl    Has water "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  hw_hwlatwatmean<-mean(enrol[ind,]$hwlatwat,na.rm=TRUE)*100
  hw_hwlatwat[arm]<-dformat( hw_hwlatwatmean,1)
}
hw_hwlatsoap  <- NULL
hw_hwlatsoap[""]  <- "\u005cMyIndentDbl     Has soap "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  hw_hwlatsoapmean<-mean(enrol[ind,]$hwlatsoap,na.rm=TRUE)*100
  hw_hwlatsoap[arm]<-dformat( hw_hwlatsoapmean,1)
}
hw_hwkitwat  <- NULL
hw_hwkitwat[""]  <- "\u005cMyIndentDbl    Has water "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  hw_hwkitwatmean<-mean(enrol[ind,]$hwkitwat,na.rm=TRUE)*100
  hw_hwkitwat[arm]<-dformat( hw_hwkitwatmean,1)
}
hw_hwkitsoap <- NULL
hw_hwkitsoap[""] <- "\u005cMyIndentDbl    Has soap "
for(arm in group[2:8]){
  ind<-enrol$arm==arm
  hw_hwkitsoapmean<-mean(enrol[ind,]$hwkitsoap,na.rm=TRUE)*100
  hw_hwkitsoap[arm]<-dformat( hw_hwkitsoapmean,1)
}

#-------------------------------------------
# combining all the measures in a dataframe
#-------------------------------------------

glue_maternal<-c("\u005ctextbf{Maternal}","","","","","","","")
glue_paternal<-c("\u005ctextbf{Paternal}","","","","","","","")
glue_household<-c("\u005ctextbf{Household}","","","","","","","")
glue_dw<-c("\u005ctextbf{Drinking Water}","","","","","","","")
glue_san<-c("\u005ctextbf{Sanitation}","","","","","","","")
glue_od<-c("\u005cMyIndent Daily defecation in the open","","","","","","","")
glue_lat<-c("\u005cMyIndent Latrine","","","","","","","")
glue_hfo<-c("\u005cMyIndent Human feces observed in the","","","","","","","")
glue_hw<-c("\u005ctextbf{Handwashing}","","","","","","","")
glue_steplat<-c("\u005cMyIndent Within 6 steps of latrine","","","","","","","")
glue_stepkit<-c("\u005cMyIndent Within 6 steps of kitchen","","","","","","","")

enroll_balance<-rbind(
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

save(enroll_balance,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\enroll-balance.Rdata")
