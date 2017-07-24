#---------------------------
# tabs4-subgroup-maternalage.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# interventions on easq
# subgroup analysis by maternal age
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
#            washb-bangladesh-subgroupvar.dta
# output files:
#            sub-mage.Rdata
#---------------------------

#---------------------------
# preamble
#---------------------------
rm(list = ls())

library(foreign) # for read.dta() function
library(plyr)    # for rename() function 
library(washb)
## desired format
dformat<-function(x,d){
  x<-round(x,d)+0.000 # removing signed zero
  x<-formatC(x,format="f",digits = d)
  return(x)
}

#-------------------------------
# load the analysis dataset
#-------------------------------
easq<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-easq-year2.dta")
subvar<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-subgroupvar.dta")

easq<- subset(easq,
              select=c("dataid","childid","tchild","clusterid",
                       "block","arm","svydate","sex","dob","agedays",
                       "ageyears","agegroup","fracode","res","care","resage",
                       "z_com","z_motor","z_personal","z_combined" ))  

easq<- rename(easq,
              replace=c("z_com"="com",
                        "z_motor"="motor",
                        "z_personal"="personal",
                        "z_combined"="combined"
              )) 
easq<-merge(easq,subvar,by="dataid")

## desired format
dformat<-function(x,d){
  x<-round(x,d)+0.000 # removing signed zero
  x<-formatC(x,format="f",digits = d)
  return(x)
}
####

group<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N")
#-----------------------------------------
# analysis for easq-communication z-score
#-----------------------------------------
com_arm<-NULL
com_m21_n<-NULL
com_m21_mean<-NULL
com_m21_mdiff<-NULL
com_le20_n<-NULL
com_le20_mean<-NULL
com_le20_mdiff<-NULL
com_p<-NULL

com_arm<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH",
  "\u005cMyIndent N+WSH vs WSH",
  "\u005cMyIndent N+WSH vs N"
)

for(arm in group){
  ind_m21  <-easq$arm==arm & easq$catmomage=="M21"
  ind_le20  <-easq$arm==arm & easq$catmomage=="LE20"
  
  com_m21_n[arm]<-  dformat(sum(!is.na(easq[ind_m21,]$com)),0)
  mean_m21<-  mean(easq[ind_m21,]$com,na.rm =TRUE)  
  SD_m21 <-  sd(easq[ind_m21,]$com,na.rm =TRUE)  
  com_m21_mean[arm]<-paste(dformat(mean_m21,2)," (",dformat(SD_m21,2),")",sep="")
  
  com_le20_n[arm]<-dformat(sum(!is.na(easq[ind_le20,]$com)),0)  
  mean_le20 <-  mean(easq[ind_le20,]$com,na.rm =TRUE)  
  SD_le20   <-  sd(easq[ind_le20,]$com,na.rm =TRUE)
  com_le20_mean[arm]<-paste(dformat(mean_le20,2)," (",dformat(SD_le20,2),")",sep="")
}
com_m21_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_m21_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

com_le20_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_le20_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


com_m21_mdiff["Control"]<-""
com_le20_mdiff["Control"]<-""
com_p["Control"]<-""

for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("Control",arm))
  
  dif_m21<- dformat(reg$lincom$est[2],2)
  lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
  ucb_m21<- dformat(reg$lincom$est.ub[2],2)
  com_m21_mdiff[arm]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")
  
  dif_le20<- dformat(reg$lincom$est[1],2)
  lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
  ucb_le20<- dformat(reg$lincom$est.ub[1],2)
  com_le20_mdiff[arm]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")
  
  com_p[arm]<- dformat(reg$fit[paste("tr",arm,":VM21",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("WSH","WSH+N"))

dif_m21<- dformat(reg$lincom$est[2],2)
lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
ucb_m21<- dformat(reg$lincom$est.ub[2],2)
com_m21_mdiff["N+WSH vs WSH"]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")

dif_le20<- dformat(reg$lincom$est[1],2)
lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
ucb_le20<- dformat(reg$lincom$est.ub[1],2)
com_le20_mdiff["N+WSH vs WSH"]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")

com_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VM21","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("Nutrition","WSH+N"))

dif_m21<- dformat(reg$lincom$est[2],2)
lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
ucb_m21<- dformat(reg$lincom$est.ub[2],2)
com_m21_mdiff["N+WSH vs N"]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")

dif_le20<- dformat(reg$lincom$est[1],2)
lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
ucb_le20<- dformat(reg$lincom$est.ub[1],2)
com_le20_mdiff["N+WSH vs N"]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")

com_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VM21","Pr(>|z|)"],3)

#-----------------------------------------
# analysis for easq-gross-motor z-score
#-----------------------------------------

motor_arm<-NULL
motor_m21_n<-NULL
motor_m21_mean<-NULL
motor_m21_mdiff<-NULL
motor_le20_n<-NULL
motor_le20_mean<-NULL
motor_le20_mdiff<-NULL
motor_p<-NULL

motor_arm<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH",
  "\u005cMyIndent N+WSH vs WSH",
  "\u005cMyIndent N+WSH vs N"
)

for(arm in group){
  ind_m21  <-easq$arm==arm & easq$catmomage=="M21"
  ind_le20  <-easq$arm==arm & easq$catmomage=="LE20"
  
  motor_m21_n[arm]<-  dformat(sum(!is.na(easq[ind_m21,]$motor)),0)
  mean_m21<-  mean(easq[ind_m21,]$motor,na.rm =TRUE)  
  SD_m21 <-  sd(easq[ind_m21,]$motor,na.rm =TRUE)  
  motor_m21_mean[arm]<-paste(dformat(mean_m21,2)," (",dformat(SD_m21,2),")",sep="")
  
  motor_le20_n[arm]<-dformat(sum(!is.na(easq[ind_le20,]$motor)),0)  
  mean_le20 <-  mean(easq[ind_le20,]$motor,na.rm =TRUE)  
  SD_le20   <-  sd(easq[ind_le20,]$motor,na.rm =TRUE)
  motor_le20_mean[arm]<-paste(dformat(mean_le20,2)," (",dformat(SD_le20,2),")",sep="")
}
motor_m21_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
motor_m21_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

motor_le20_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
motor_le20_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


motor_m21_mdiff["Control"]<-""
motor_le20_mdiff["Control"]<-""
motor_p["Control"]<-""

for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("Control",arm))
  
  dif_m21<- dformat(reg$lincom$est[2],2)
  lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
  ucb_m21<- dformat(reg$lincom$est.ub[2],2)
  motor_m21_mdiff[arm]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")
  
  dif_le20<- dformat(reg$lincom$est[1],2)
  lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
  ucb_le20<- dformat(reg$lincom$est.ub[1],2)
  motor_le20_mdiff[arm]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")
  
  motor_p[arm]<- dformat(reg$fit[paste("tr",arm,":VM21",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("WSH","WSH+N"))

dif_m21<- dformat(reg$lincom$est[2],2)
lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
ucb_m21<- dformat(reg$lincom$est.ub[2],2)
motor_m21_mdiff["N+WSH vs WSH"]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")

dif_le20<- dformat(reg$lincom$est[1],2)
lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
ucb_le20<- dformat(reg$lincom$est.ub[1],2)
motor_le20_mdiff["N+WSH vs WSH"]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")

motor_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VM21","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("Nutrition","WSH+N"))

dif_m21<- dformat(reg$lincom$est[2],2)
lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
ucb_m21<- dformat(reg$lincom$est.ub[2],2)
motor_m21_mdiff["N+WSH vs N"]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")

dif_le20<- dformat(reg$lincom$est[1],2)
lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
ucb_le20<- dformat(reg$lincom$est.ub[1],2)
motor_le20_mdiff["N+WSH vs N"]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")

motor_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VM21","Pr(>|z|)"],3)

#-----------------------------------------
# analysis for easq-personal-social z-score
#-----------------------------------------
personal_arm<-NULL
personal_m21_n<-NULL
personal_m21_mean<-NULL
personal_m21_mdiff<-NULL
personal_le20_n<-NULL
personal_le20_mean<-NULL
personal_le20_mdiff<-NULL
personal_p<-NULL

personal_arm<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH",
  "\u005cMyIndent N+WSH vs WSH",
  "\u005cMyIndent N+WSH vs N"
)

for(arm in group){
  ind_m21  <-easq$arm==arm & easq$catmomage=="M21"
  ind_le20  <-easq$arm==arm & easq$catmomage=="LE20"
  
  personal_m21_n[arm]<-  dformat(sum(!is.na(easq[ind_m21,]$personal)),0)
  mean_m21<-  mean(easq[ind_m21,]$personal,na.rm =TRUE)  
  SD_m21 <-  sd(easq[ind_m21,]$personal,na.rm =TRUE)  
  personal_m21_mean[arm]<-paste(dformat(mean_m21,2)," (",dformat(SD_m21,2),")",sep="")
  
  personal_le20_n[arm]<-dformat(sum(!is.na(easq[ind_le20,]$personal)),0)  
  mean_le20 <-  mean(easq[ind_le20,]$personal,na.rm =TRUE)  
  SD_le20   <-  sd(easq[ind_le20,]$personal,na.rm =TRUE)
  personal_le20_mean[arm]<-paste(dformat(mean_le20,2)," (",dformat(SD_le20,2),")",sep="")
}
personal_m21_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
personal_m21_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

personal_le20_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
personal_le20_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


personal_m21_mdiff["Control"]<-""
personal_le20_mdiff["Control"]<-""
personal_p["Control"]<-""

for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("Control",arm))
  
  dif_m21<- dformat(reg$lincom$est[2],2)
  lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
  ucb_m21<- dformat(reg$lincom$est.ub[2],2)
  personal_m21_mdiff[arm]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")
  
  dif_le20<- dformat(reg$lincom$est[1],2)
  lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
  ucb_le20<- dformat(reg$lincom$est.ub[1],2)
  personal_le20_mdiff[arm]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")
  
  personal_p[arm]<- dformat(reg$fit[paste("tr",arm,":VM21",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("WSH","WSH+N"))

dif_m21<- dformat(reg$lincom$est[2],2)
lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
ucb_m21<- dformat(reg$lincom$est.ub[2],2)
personal_m21_mdiff["N+WSH vs WSH"]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")

dif_le20<- dformat(reg$lincom$est[1],2)
lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
ucb_le20<- dformat(reg$lincom$est.ub[1],2)
personal_le20_mdiff["N+WSH vs WSH"]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")

personal_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VM21","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("Nutrition","WSH+N"))

dif_m21<- dformat(reg$lincom$est[2],2)
lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
ucb_m21<- dformat(reg$lincom$est.ub[2],2)
personal_m21_mdiff["N+WSH vs N"]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")

dif_le20<- dformat(reg$lincom$est[1],2)
lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
ucb_le20<- dformat(reg$lincom$est.ub[1],2)
personal_le20_mdiff["N+WSH vs N"]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")

personal_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VM21","Pr(>|z|)"],3)

#-----------------------------------------
# analysis for easq-combined z-score
#-----------------------------------------
combined_arm<-NULL
combined_m21_n<-NULL
combined_m21_mean<-NULL
combined_m21_mdiff<-NULL
combined_le20_n<-NULL
combined_le20_mean<-NULL
combined_le20_mdiff<-NULL
combined_p<-NULL

combined_arm<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH",
  "\u005cMyIndent N+WSH vs WSH",
  "\u005cMyIndent N+WSH vs N"
)

for(arm in group){
  ind_m21  <-easq$arm==arm & easq$catmomage=="M21"
  ind_le20  <-easq$arm==arm & easq$catmomage=="LE20"
  
  combined_m21_n[arm]<-  dformat(sum(!is.na(easq[ind_m21,]$combined)),0)
  mean_m21<-  mean(easq[ind_m21,]$combined,na.rm =TRUE)  
  SD_m21 <-  sd(easq[ind_m21,]$combined,na.rm =TRUE)  
  combined_m21_mean[arm]<-paste(dformat(mean_m21,2)," (",dformat(SD_m21,2),")",sep="")
  
  combined_le20_n[arm]<-dformat(sum(!is.na(easq[ind_le20,]$combined)),0)  
  mean_le20 <-  mean(easq[ind_le20,]$combined,na.rm =TRUE)  
  SD_le20   <-  sd(easq[ind_le20,]$combined,na.rm =TRUE)
  combined_le20_mean[arm]<-paste(dformat(mean_le20,2)," (",dformat(SD_le20,2),")",sep="")
}
combined_m21_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_m21_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

combined_le20_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_le20_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


combined_m21_mdiff["Control"]<-""
combined_le20_mdiff["Control"]<-""
combined_p["Control"]<-""

for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("Control",arm))
  
  dif_m21<- dformat(reg$lincom$est[2],2)
  lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
  ucb_m21<- dformat(reg$lincom$est.ub[2],2)
  combined_m21_mdiff[arm]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")
  
  dif_le20<- dformat(reg$lincom$est[1],2)
  lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
  ucb_le20<- dformat(reg$lincom$est.ub[1],2)
  combined_le20_mdiff[arm]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")
  
  combined_p[arm]<- dformat(reg$fit[paste("tr",arm,":VM21",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("WSH","WSH+N"))

dif_m21<- dformat(reg$lincom$est[2],2)
lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
ucb_m21<- dformat(reg$lincom$est.ub[2],2)
combined_m21_mdiff["N+WSH vs WSH"]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")

dif_le20<- dformat(reg$lincom$est[1],2)
lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
ucb_le20<- dformat(reg$lincom$est.ub[1],2)
combined_le20_mdiff["N+WSH vs WSH"]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")

combined_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VM21","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomage"],V="catmomage",contrast=c("Nutrition","WSH+N"))

dif_m21<- dformat(reg$lincom$est[2],2)
lcb_m21<- dformat(reg$lincom$est.lb[2],2) 
ucb_m21<- dformat(reg$lincom$est.ub[2],2)
combined_m21_mdiff["N+WSH vs N"]<-paste(dif_m21," (",lcb_m21,", ",ucb_m21,")",sep="")

dif_le20<- dformat(reg$lincom$est[1],2)
lcb_le20<- dformat(reg$lincom$est.lb[1],2) 
ucb_le20<- dformat(reg$lincom$est.ub[1],2)
combined_le20_mdiff["N+WSH vs N"]<-paste(dif_le20," (",lcb_le20,", ",ucb_le20,")",sep="")

combined_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VM21","Pr(>|z|)"],3)

#----------------------------------------------------
# combining all the results into a single data frame
#----------------------------------------------------
com_df<- cbind(
  a=com_arm,
  b=com_m21_n,
  c=com_m21_mean,
  d=com_m21_mdiff,
  e=com_le20_n,
  f=com_le20_mean,
  g=com_le20_mdiff,
  h=com_p
)
motor_df<- cbind(
  a=motor_arm,
  b=motor_m21_n,
  c=motor_m21_mean,
  d=motor_m21_mdiff,
  e=motor_le20_n,
  f=motor_le20_mean,
  g=motor_le20_mdiff,
  h=motor_p
) 

personal_df<-  cbind(
  a=personal_arm,
  b=personal_m21_n,
  c=personal_m21_mean,
  d=personal_m21_mdiff,
  e=personal_le20_n,
  f=personal_le20_mean,
  g=personal_le20_mdiff,
  h=personal_p
)
combined_df<-  cbind(
  a=combined_arm,
  b=combined_m21_n,
  c=combined_m21_mean,
  d=combined_m21_mdiff,
  e=combined_le20_n,
  f=combined_le20_mean,
  g=combined_le20_mdiff,
  h=combined_p
)


com_glue<-      data.frame(a=c("\u005ctextbf{Communication z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
motor_glue<-      data.frame(a=c("\u005ctextbf{Gross Motor  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
personal_glue<-      data.frame(a=c("\u005ctextbf{Personal-social  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
combined_glue<- data.frame(a=c("\u005ctextbf{Combined z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))

sub_mage<-rbind(
  com_glue,
  com_df,
  motor_glue,
  motor_df,
  personal_glue,
  personal_df,
  combined_glue,
  combined_df  
)
save(sub_mage,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\sub-mage.Rdata")