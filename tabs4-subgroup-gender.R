#---------------------------
# tabs4-subgroup-gender.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# interventions on easq
# sub-group analysis by child sex
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
#            washb-bangladesh-subgroupvar.dta
# output files:
#            table-s4.Rdata
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


group<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N")

#-----------------------------------------
# analysis for easq-communication z-score
#-----------------------------------------
com_arm<-NULL
com_male_n<-NULL
com_male_mean<-NULL
com_male_mdiff<-NULL
com_female_n<-NULL
com_female_mean<-NULL
com_female_mdiff<-NULL
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
  ind_male  <-easq$arm==arm & easq$sex=="Male"
  ind_female<-easq$arm==arm & easq$sex=="Female"
    
  com_male_n[arm]<-  dformat(sum(!is.na(easq[ind_male,]$com)),0)
  mean_male<-  mean(easq[ind_male,]$com,na.rm =TRUE)
  SD_male <-  sd(easq[ind_male,]$com,na.rm =TRUE)
  com_male_mean[arm]<-paste(dformat(mean_male,2)," (",dformat(SD_male,2),")",sep="")
  
  com_female_n[arm]<-dformat(sum(!is.na(easq[ind_female,]$com)),0)
  mean_female <-  mean(easq[ind_female,]$com,na.rm =TRUE)
  SD_female   <-  sd(easq[ind_female,]$com,na.rm =TRUE)
  com_female_mean[arm]<-paste(dformat(mean_female,2)," (",dformat(SD_female,2),")",sep="")
}
com_male_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_male_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

com_female_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_female_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


com_male_mdiff["Control"]<-""
com_female_mdiff["Control"]<-""
com_p["Control"]<-""

for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("Control",arm))
  
  dif_male<- dformat(reg$lincom$est[2],2)
  lcb_male<- dformat(reg$lincom$est.lb[2],2) 
  ucb_male<- dformat(reg$lincom$est.ub[2],2)
  com_male_mdiff[arm]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")
  
  dif_female<- dformat(reg$lincom$est[1],2)
  lcb_female<- dformat(reg$lincom$est.lb[1],2) 
  ucb_female<- dformat(reg$lincom$est.ub[1],2)
  com_female_mdiff[arm]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")
  
  com_p[arm]<- dformat(reg$fit[paste("tr",arm,":VMale",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("WSH","WSH+N"))
  
  dif_male<- dformat(reg$lincom$est[2],2)
  lcb_male<- dformat(reg$lincom$est.lb[2],2) 
  ucb_male<- dformat(reg$lincom$est.ub[2],2)
  com_male_mdiff["N+WSH vs WSH"]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")
  
  dif_female<- dformat(reg$lincom$est[1],2)
  lcb_female<- dformat(reg$lincom$est.lb[1],2) 
  ucb_female<- dformat(reg$lincom$est.ub[1],2)
  com_female_mdiff["N+WSH vs WSH"]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")
  
  com_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VMale","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("Nutrition","WSH+N"))

dif_male<- dformat(reg$lincom$est[2],2)
lcb_male<- dformat(reg$lincom$est.lb[2],2) 
ucb_male<- dformat(reg$lincom$est.ub[2],2)
com_male_mdiff["N+WSH vs N"]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")

dif_female<- dformat(reg$lincom$est[1],2)
lcb_female<- dformat(reg$lincom$est.lb[1],2) 
ucb_female<- dformat(reg$lincom$est.ub[1],2)
com_female_mdiff["N+WSH vs N"]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")

com_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VMale","Pr(>|z|)"],3)

#-----------------------------------------
# analysis for easq-gross-motor z-score
#-----------------------------------------
mot_arm<-NULL
mot_male_n<-NULL
mot_male_mean<-NULL
mot_male_mdiff<-NULL
mot_female_n<-NULL
mot_female_mean<-NULL
mot_female_mdiff<-NULL
mot_p<-NULL

mot_arm<-c(
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
  ind_male  <-easq$arm==arm & easq$sex=="Male"
  ind_female<-easq$arm==arm & easq$sex=="Female"
  
  mot_male_n[arm]<-  dformat(sum(!is.na(easq[ind_male,]$motor)),0)
  mean_male<-  mean(easq[ind_male,]$motor,na.rm =TRUE)
  SD_male <-  sd(easq[ind_male,]$motor,na.rm =TRUE)
  mot_male_mean[arm]<-paste(dformat(mean_male,2)," (",dformat(SD_male,2),")",sep="")
  
  mot_female_n[arm]<-dformat(sum(!is.na(easq[ind_female,]$motor)),0)
  mean_female <-  mean(easq[ind_female,]$motor,na.rm =TRUE)
  SD_female   <-  sd(easq[ind_female,]$motor,na.rm =TRUE)
  mot_female_mean[arm]<-paste(dformat(mean_female,2)," (",dformat(SD_female,2),")",sep="")
}


mot_male_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_male_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

mot_female_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_female_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


mot_male_mdiff["Control"]<-""
mot_female_mdiff["Control"]<-""
mot_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("Control",arm))
  
  dif_male<- dformat(reg$lincom$est[2],2)
  lcb_male<- dformat(reg$lincom$est.lb[2],2) 
  ucb_male<- dformat(reg$lincom$est.ub[2],2)
  mot_male_mdiff[arm]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")
  
  dif_female<- dformat(reg$lincom$est[1],2)
  lcb_female<- dformat(reg$lincom$est.lb[1],2) 
  ucb_female<- dformat(reg$lincom$est.ub[1],2)
  mot_female_mdiff[arm]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")
  
  mot_p[arm]<- dformat(reg$fit[paste("tr",arm,":VMale",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("WSH","WSH+N"))

dif_male<- dformat(reg$lincom$est[2],2)
lcb_male<- dformat(reg$lincom$est.lb[2],2) 
ucb_male<- dformat(reg$lincom$est.ub[2],2)
mot_male_mdiff["N+WSH vs WSH"]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")

dif_female<- dformat(reg$lincom$est[1],2)
lcb_female<- dformat(reg$lincom$est.lb[1],2) 
ucb_female<- dformat(reg$lincom$est.ub[1],2)
mot_female_mdiff["N+WSH vs WSH"]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")

mot_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VMale","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("Nutrition","WSH+N"))

dif_male<- dformat(reg$lincom$est[2],2)
lcb_male<- dformat(reg$lincom$est.lb[2],2) 
ucb_male<- dformat(reg$lincom$est.ub[2],2)
mot_male_mdiff["N+WSH vs N"]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")

dif_female<- dformat(reg$lincom$est[1],2)
lcb_female<- dformat(reg$lincom$est.lb[1],2) 
ucb_female<- dformat(reg$lincom$est.ub[1],2)
mot_female_mdiff["N+WSH vs N"]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")

mot_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VMale","Pr(>|z|)"],3)
#-----------------------------------------
# analysis for easq-personal-social z-score
#-----------------------------------------
per_arm<-NULL
per_male_n<-NULL
per_male_mean<-NULL
per_male_mdiff<-NULL
per_female_n<-NULL
per_female_mean<-NULL
per_female_mdiff<-NULL
per_p<-NULL

per_arm<-c(
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
  ind_male  <-easq$arm==arm & easq$sex=="Male"
  ind_female<-easq$arm==arm & easq$sex=="Female"
  
  per_male_n[arm]<-  dformat(sum(!is.na(easq[ind_male,]$personal)),0)
  mean_male<-  mean(easq[ind_male,]$personal,na.rm =TRUE)
  SD_male <-  sd(easq[ind_male,]$personal,na.rm =TRUE)
  per_male_mean[arm]<-paste(dformat(mean_male,2)," (",dformat(SD_male,2),")",sep="")
  
  per_female_n[arm]<-dformat(sum(!is.na(easq[ind_female,]$personal)),0)
  mean_female <-  mean(easq[ind_female,]$personal,na.rm =TRUE)
  SD_female   <-  sd(easq[ind_female,]$personal,na.rm =TRUE)
  per_female_mean[arm]<-paste(dformat(mean_female,2)," (",dformat(SD_female,2),")",sep="")
}


per_male_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_male_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

per_female_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_female_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


per_male_mdiff["Control"]<-""
per_female_mdiff["Control"]<-""
per_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("Control",arm))
  
  dif_male<- dformat(reg$lincom$est[2],2)
  lcb_male<- dformat(reg$lincom$est.lb[2],2) 
  ucb_male<- dformat(reg$lincom$est.ub[2],2)
  per_male_mdiff[arm]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")
  
  dif_female<- dformat(reg$lincom$est[1],2)
  lcb_female<- dformat(reg$lincom$est.lb[1],2) 
  ucb_female<- dformat(reg$lincom$est.ub[1],2)
  per_female_mdiff[arm]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")
  
  per_p[arm]<- dformat(reg$fit[paste("tr",arm,":VMale",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("WSH","WSH+N"))

dif_male<- dformat(reg$lincom$est[2],2)
lcb_male<- dformat(reg$lincom$est.lb[2],2) 
ucb_male<- dformat(reg$lincom$est.ub[2],2)
per_male_mdiff["N+WSH vs WSH"]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")

dif_female<- dformat(reg$lincom$est[1],2)
lcb_female<- dformat(reg$lincom$est.lb[1],2) 
ucb_female<- dformat(reg$lincom$est.ub[1],2)
per_female_mdiff["N+WSH vs WSH"]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")

per_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VMale","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("Nutrition","WSH+N"))

dif_male<- dformat(reg$lincom$est[2],2)
lcb_male<- dformat(reg$lincom$est.lb[2],2) 
ucb_male<- dformat(reg$lincom$est.ub[2],2)
per_male_mdiff["N+WSH vs N"]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")

dif_female<- dformat(reg$lincom$est[1],2)
lcb_female<- dformat(reg$lincom$est.lb[1],2) 
ucb_female<- dformat(reg$lincom$est.ub[1],2)
per_female_mdiff["N+WSH vs N"]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")

per_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VMale","Pr(>|z|)"],3)

#-----------------------------------------
# analysis for easq-combined z-score
#-----------------------------------------
combined_arm<-NULL
combined_male_n<-NULL
combined_male_mean<-NULL
combined_male_mdiff<-NULL
combined_female_n<-NULL
combined_female_mean<-NULL
combined_female_mdiff<-NULL
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
  ind_male  <-easq$arm==arm & easq$sex=="Male"
  ind_female<-easq$arm==arm & easq$sex=="Female"
  
  combined_male_n[arm]<-  dformat(sum(!is.na(easq[ind_male,]$combined)),0)
  mean_male<-  mean(easq[ind_male,]$combined,na.rm =TRUE)
  SD_male <-  sd(easq[ind_male,]$combined,na.rm =TRUE)
  combined_male_mean[arm]<-paste(dformat(mean_male,2)," (",dformat(SD_male,2),")",sep="")
  
  combined_female_n[arm]<-dformat(sum(!is.na(easq[ind_female,]$combined)),0)
  mean_female <-  mean(easq[ind_female,]$combined,na.rm =TRUE)
  SD_female   <-  sd(easq[ind_female,]$combined,na.rm =TRUE)
  combined_female_mean[arm]<-paste(dformat(mean_female,2)," (",dformat(SD_female,2),")",sep="")
}

combined_male_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_male_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

combined_female_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_female_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


combined_male_mdiff["Control"]<-""
combined_female_mdiff["Control"]<-""
combined_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("Control",arm))
  
  dif_male<- dformat(reg$lincom$est[2],2)
  lcb_male<- dformat(reg$lincom$est.lb[2],2) 
  ucb_male<- dformat(reg$lincom$est.ub[2],2)
  combined_male_mdiff[arm]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")
  
  dif_female<- dformat(reg$lincom$est[1],2)
  lcb_female<- dformat(reg$lincom$est.lb[1],2) 
  ucb_female<- dformat(reg$lincom$est.ub[1],2)
  combined_female_mdiff[arm]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")
  
  combined_p[arm]<- dformat(reg$fit[paste("tr",arm,":VMale",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("WSH","WSH+N"))

dif_male<- dformat(reg$lincom$est[2],2)
lcb_male<- dformat(reg$lincom$est.lb[2],2) 
ucb_male<- dformat(reg$lincom$est.ub[2],2)
combined_male_mdiff["N+WSH vs WSH"]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")

dif_female<- dformat(reg$lincom$est[1],2)
lcb_female<- dformat(reg$lincom$est.lb[1],2) 
ucb_female<- dformat(reg$lincom$est.ub[1],2)
combined_female_mdiff["N+WSH vs WSH"]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")

combined_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VMale","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["sex"],V="sex",contrast=c("Nutrition","WSH+N"))

dif_male<- dformat(reg$lincom$est[2],2)
lcb_male<- dformat(reg$lincom$est.lb[2],2) 
ucb_male<- dformat(reg$lincom$est.ub[2],2)
combined_male_mdiff["N+WSH vs N"]<-paste(dif_male," (",lcb_male,", ",ucb_male,")",sep="")

dif_female<- dformat(reg$lincom$est[1],2)
lcb_female<- dformat(reg$lincom$est.lb[1],2) 
ucb_female<- dformat(reg$lincom$est.ub[1],2)
combined_female_mdiff["N+WSH vs N"]<-paste(dif_female," (",lcb_female,", ",ucb_female,")",sep="")

combined_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VMale","Pr(>|z|)"],3)

#----------------------------------------------------
# combining all the results into a single data frame
#----------------------------------------------------

com_df<- cbind(
  a=com_arm,
  b=com_male_n,
  c=com_male_mean,
  d=com_male_mdiff,
  e=com_female_n,
  f=com_female_mean,
  g=com_female_mdiff,
  h=com_p
  )
mot_df<- cbind(
  a=mot_arm,
  b=mot_male_n,
  c=mot_male_mean,
  d=mot_male_mdiff,
  e=mot_female_n,
  f=mot_female_mean,
  g=mot_female_mdiff,
  h=mot_p
) 
  
per_df<-  cbind(
  a=per_arm,
  b=per_male_n,
  c=per_male_mean,
  d=per_male_mdiff,
  e=per_female_n,
  f=per_female_mean,
  g=per_female_mdiff,
  h=per_p
)
combined_df<-  cbind(
  a=combined_arm,
  b=combined_male_n,
  c=combined_male_mean,
  d=combined_male_mdiff,
  e=combined_female_n,
  f=combined_female_mean,
  g=combined_female_mdiff,
  h=combined_p
)


com_glue<-      data.frame(a=c("\u005ctextbf{Communication z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
mot_glue<-      data.frame(a=c("\u005ctextbf{Gross Motor  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
per_glue<-      data.frame(a=c("\u005ctextbf{Personal-social  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
combined_glue<- data.frame(a=c("\u005ctextbf{Combined z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))

table_s4<-rbind(
  com_glue,
  com_df,
  mot_glue,
  mot_df,
  per_glue,
  per_df,
  combined_glue,
  combined_df  
  )
save(table_s4,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\table-s4.Rdata")