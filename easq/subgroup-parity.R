#---------------------------
# tabs4-subgroup-parity.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# interventions on easq
# subgroup analysis by parity
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
#            washb-bangladesh-subgroupvar.dta
# output files:
#            sub-parity.Rdata
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
com_multi_n<-NULL
com_multi_mean<-NULL
com_multi_mdiff<-NULL
com_nulli_n<-NULL
com_nulli_mean<-NULL
com_nulli_mdiff<-NULL
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
  ind_multi  <-easq$arm==arm & easq$parity=="multi"
  ind_nulli<-easq$arm==arm & easq$parity=="nulli"
  
  com_multi_n[arm]<-  dformat(sum(!is.na(easq[ind_multi,]$com)),0)
  mean_multi<-  mean(easq[ind_multi,]$com,na.rm =TRUE)
  SD_multi <-  sd(easq[ind_multi,]$com,na.rm =TRUE)
  com_multi_mean[arm]<-paste(dformat(mean_multi,2)," (",dformat(SD_multi,2),")",sep="")
  
  com_nulli_n[arm]<-dformat(sum(!is.na(easq[ind_nulli,]$com)),0)
  mean_nulli <-  mean(easq[ind_nulli,]$com,na.rm =TRUE)
  SD_nulli   <-  sd(easq[ind_nulli,]$com,na.rm =TRUE)
  com_nulli_mean[arm]<-paste(dformat(mean_nulli,2)," (",dformat(SD_nulli,2),")",sep="")
}
com_multi_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_multi_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

com_nulli_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_nulli_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


com_multi_mdiff["Control"]<-""
com_nulli_mdiff["Control"]<-""
com_p["Control"]<-""

for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("Control",arm))
  
  dif_multi<- dformat(reg$lincom$est[2],2)
  lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
  ucb_multi<- dformat(reg$lincom$est.ub[2],2)
  com_multi_mdiff[arm]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")
  
  dif_nulli<- dformat(reg$lincom$est[1],2)
  lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
  ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
  com_nulli_mdiff[arm]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")
  
  com_p[arm]<- dformat(reg$fit[paste("tr",arm,":Vmulti",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("WSH","WSH+N"))

dif_multi<- dformat(reg$lincom$est[2],2)
lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
ucb_multi<- dformat(reg$lincom$est.ub[2],2)
com_multi_mdiff["N+WSH vs WSH"]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")

dif_nulli<- dformat(reg$lincom$est[1],2)
lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
com_nulli_mdiff["N+WSH vs WSH"]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")

com_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:Vmulti","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("Nutrition","WSH+N"))

dif_multi<- dformat(reg$lincom$est[2],2)
lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
ucb_multi<- dformat(reg$lincom$est.ub[2],2)
com_multi_mdiff["N+WSH vs N"]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")

dif_nulli<- dformat(reg$lincom$est[1],2)
lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
com_nulli_mdiff["N+WSH vs N"]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")

com_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:Vmulti","Pr(>|z|)"],3)
#-----------------------------------------
# analysis for easq-gross-motor z-score
#-----------------------------------------
mot_arm<-NULL
mot_multi_n<-NULL
mot_multi_mean<-NULL
mot_multi_mdiff<-NULL
mot_nulli_n<-NULL
mot_nulli_mean<-NULL
mot_nulli_mdiff<-NULL
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
  ind_multi  <-easq$arm==arm & easq$parity=="multi"
  ind_nulli<-easq$arm==arm & easq$parity=="nulli"
  
  mot_multi_n[arm]<-  dformat(sum(!is.na(easq[ind_multi,]$motor)),0)
  mean_multi<-  mean(easq[ind_multi,]$motor,na.rm =TRUE)
  SD_multi <-  sd(easq[ind_multi,]$motor,na.rm =TRUE)
  mot_multi_mean[arm]<-paste(dformat(mean_multi,2)," (",dformat(SD_multi,2),")",sep="")
  
  mot_nulli_n[arm]<-dformat(sum(!is.na(easq[ind_nulli,]$motor)),0)
  mean_nulli <-  mean(easq[ind_nulli,]$motor,na.rm =TRUE)
  SD_nulli   <-  sd(easq[ind_nulli,]$motor,na.rm =TRUE)
  mot_nulli_mean[arm]<-paste(dformat(mean_nulli,2)," (",dformat(SD_nulli,2),")",sep="")
}


mot_multi_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_multi_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

mot_nulli_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_nulli_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


mot_multi_mdiff["Control"]<-""
mot_nulli_mdiff["Control"]<-""
mot_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("Control",arm))
  
  dif_multi<- dformat(reg$lincom$est[2],2)
  lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
  ucb_multi<- dformat(reg$lincom$est.ub[2],2)
  mot_multi_mdiff[arm]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")
  
  dif_nulli<- dformat(reg$lincom$est[1],2)
  lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
  ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
  mot_nulli_mdiff[arm]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")
  
  mot_p[arm]<- dformat(reg$fit[paste("tr",arm,":Vmulti",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("WSH","WSH+N"))

dif_multi<- dformat(reg$lincom$est[2],2)
lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
ucb_multi<- dformat(reg$lincom$est.ub[2],2)
mot_multi_mdiff["N+WSH vs WSH"]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")

dif_nulli<- dformat(reg$lincom$est[1],2)
lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
mot_nulli_mdiff["N+WSH vs WSH"]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")

mot_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:Vmulti","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("Nutrition","WSH+N"))

dif_multi<- dformat(reg$lincom$est[2],2)
lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
ucb_multi<- dformat(reg$lincom$est.ub[2],2)
mot_multi_mdiff["N+WSH vs N"]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")

dif_nulli<- dformat(reg$lincom$est[1],2)
lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
mot_nulli_mdiff["N+WSH vs N"]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")

mot_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:Vmulti","Pr(>|z|)"],3)

#-----------------------------------------
# analysis for easq-personal-social z-score
#-----------------------------------------
per_arm<-NULL
per_multi_n<-NULL
per_multi_mean<-NULL
per_multi_mdiff<-NULL
per_nulli_n<-NULL
per_nulli_mean<-NULL
per_nulli_mdiff<-NULL
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
  ind_multi  <-easq$arm==arm & easq$parity=="multi"
  ind_nulli<-easq$arm==arm & easq$parity=="nulli"
  
  per_multi_n[arm]<-  dformat(sum(!is.na(easq[ind_multi,]$personal)),0)
  mean_multi<-  mean(easq[ind_multi,]$personal,na.rm =TRUE)
  SD_multi <-  sd(easq[ind_multi,]$personal,na.rm =TRUE)
  per_multi_mean[arm]<-paste(dformat(mean_multi,2)," (",dformat(SD_multi,2),")",sep="")
  
  per_nulli_n[arm]<-dformat(sum(!is.na(easq[ind_nulli,]$personal)),0)
  mean_nulli <-  mean(easq[ind_nulli,]$personal,na.rm =TRUE)
  SD_nulli   <-  sd(easq[ind_nulli,]$personal,na.rm =TRUE)
  per_nulli_mean[arm]<-paste(dformat(mean_nulli,2)," (",dformat(SD_nulli,2),")",sep="")
}


per_multi_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_multi_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

per_nulli_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_nulli_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


per_multi_mdiff["Control"]<-""
per_nulli_mdiff["Control"]<-""
per_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("Control",arm))
  
  dif_multi<- dformat(reg$lincom$est[2],2)
  lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
  ucb_multi<- dformat(reg$lincom$est.ub[2],2)
  per_multi_mdiff[arm]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")
  
  dif_nulli<- dformat(reg$lincom$est[1],2)
  lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
  ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
  per_nulli_mdiff[arm]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")
  
  per_p[arm]<- dformat(reg$fit[paste("tr",arm,":Vmulti",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("WSH","WSH+N"))

dif_multi<- dformat(reg$lincom$est[2],2)
lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
ucb_multi<- dformat(reg$lincom$est.ub[2],2)
per_multi_mdiff["N+WSH vs WSH"]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")

dif_nulli<- dformat(reg$lincom$est[1],2)
lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
per_nulli_mdiff["N+WSH vs WSH"]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")

per_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:Vmulti","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("Nutrition","WSH+N"))

dif_multi<- dformat(reg$lincom$est[2],2)
lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
ucb_multi<- dformat(reg$lincom$est.ub[2],2)
per_multi_mdiff["N+WSH vs N"]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")

dif_nulli<- dformat(reg$lincom$est[1],2)
lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
per_nulli_mdiff["N+WSH vs N"]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")

per_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:Vmulti","Pr(>|z|)"],3)

#-----------------------------------------
# analysis for easq-combined z-score
#-----------------------------------------
combined_arm<-NULL
combined_multi_n<-NULL
combined_multi_mean<-NULL
combined_multi_mdiff<-NULL
combined_nulli_n<-NULL
combined_nulli_mean<-NULL
combined_nulli_mdiff<-NULL
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
  ind_multi  <-easq$arm==arm & easq$parity=="multi"
  ind_nulli<-easq$arm==arm & easq$parity=="nulli"
  
  combined_multi_n[arm]<-  dformat(sum(!is.na(easq[ind_multi,]$combined)),0)
  mean_multi<-  mean(easq[ind_multi,]$combined,na.rm =TRUE)
  SD_multi <-  sd(easq[ind_multi,]$combined,na.rm =TRUE)
  combined_multi_mean[arm]<-paste(dformat(mean_multi,2)," (",dformat(SD_multi,2),")",sep="")
  
  combined_nulli_n[arm]<-dformat(sum(!is.na(easq[ind_nulli,]$combined)),0)
  mean_nulli <-  mean(easq[ind_nulli,]$combined,na.rm =TRUE)
  SD_nulli   <-  sd(easq[ind_nulli,]$combined,na.rm =TRUE)
  combined_nulli_mean[arm]<-paste(dformat(mean_nulli,2)," (",dformat(SD_nulli,2),")",sep="")
}

combined_multi_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_multi_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

combined_nulli_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_nulli_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


combined_multi_mdiff["Control"]<-""
combined_nulli_mdiff["Control"]<-""
combined_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("Control",arm))
  
  dif_multi<- dformat(reg$lincom$est[2],2)
  lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
  ucb_multi<- dformat(reg$lincom$est.ub[2],2)
  combined_multi_mdiff[arm]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")
  
  dif_nulli<- dformat(reg$lincom$est[1],2)
  lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
  ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
  combined_nulli_mdiff[arm]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")
  
  combined_p[arm]<- dformat(reg$fit[paste("tr",arm,":Vmulti",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("WSH","WSH+N"))

dif_multi<- dformat(reg$lincom$est[2],2)
lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
ucb_multi<- dformat(reg$lincom$est.ub[2],2)
combined_multi_mdiff["N+WSH vs WSH"]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")

dif_nulli<- dformat(reg$lincom$est[1],2)
lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
combined_nulli_mdiff["N+WSH vs WSH"]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")

combined_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:Vmulti","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["parity"],V="parity",contrast=c("Nutrition","WSH+N"))

dif_multi<- dformat(reg$lincom$est[2],2)
lcb_multi<- dformat(reg$lincom$est.lb[2],2) 
ucb_multi<- dformat(reg$lincom$est.ub[2],2)
combined_multi_mdiff["N+WSH vs N"]<-paste(dif_multi," (",lcb_multi,", ",ucb_multi,")",sep="")

dif_nulli<- dformat(reg$lincom$est[1],2)
lcb_nulli<- dformat(reg$lincom$est.lb[1],2) 
ucb_nulli<- dformat(reg$lincom$est.ub[1],2)
combined_nulli_mdiff["N+WSH vs N"]<-paste(dif_nulli," (",lcb_nulli,", ",ucb_nulli,")",sep="")

combined_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:Vmulti","Pr(>|z|)"],3)

#----------------------------------------------------
# combining all the results into a single data frame
#----------------------------------------------------

com_df<- cbind(
  a=com_arm,
  b=com_multi_n,
  c=com_multi_mean,
  d=com_multi_mdiff,
  e=com_nulli_n,
  f=com_nulli_mean,
  g=com_nulli_mdiff,
  h=com_p
)
mot_df<- cbind(
  a=mot_arm,
  b=mot_multi_n,
  c=mot_multi_mean,
  d=mot_multi_mdiff,
  e=mot_nulli_n,
  f=mot_nulli_mean,
  g=mot_nulli_mdiff,
  h=mot_p
) 

per_df<-  cbind(
  a=per_arm,
  b=per_multi_n,
  c=per_multi_mean,
  d=per_multi_mdiff,
  e=per_nulli_n,
  f=per_nulli_mean,
  g=per_nulli_mdiff,
  h=per_p
)
combined_df<-  cbind(
  a=combined_arm,
  b=combined_multi_n,
  c=combined_multi_mean,
  d=combined_multi_mdiff,
  e=combined_nulli_n,
  f=combined_nulli_mean,
  g=combined_nulli_mdiff,
  h=combined_p
)


com_glue<-      data.frame(a=c("\u005ctextbf{Communication z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
mot_glue<-      data.frame(a=c("\u005ctextbf{Gross Motor  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
per_glue<-      data.frame(a=c("\u005ctextbf{Personal-social  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
combined_glue<- data.frame(a=c("\u005ctextbf{Combined z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))

sub_parity<-rbind(
  com_glue,
  com_df,
  mot_glue,
  mot_df,
  per_glue,
  per_df,
  combined_glue,
  combined_df  
)
save(sub_parity,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\sub-parity.Rdata")
