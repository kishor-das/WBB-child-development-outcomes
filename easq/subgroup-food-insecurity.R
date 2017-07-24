#---------------------------
# tabs4-subgroup-food-insecurity.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# interventions on easq
# subgroup analysis by household
# food insecurity
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
#            washb-bangladesh-subgroupvar.dta
# output files:
#            sub-food.Rdata
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
com_secure_n<-NULL
com_secure_mean<-NULL
com_secure_mdiff<-NULL
com_insecure_n<-NULL
com_insecure_mean<-NULL
com_insecure_mdiff<-NULL
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
  ind_secure  <-easq$arm==arm & easq$catfoodin=="secure"
  ind_insecure<-easq$arm==arm & easq$catfoodin=="insecure"
  
  com_secure_n[arm]<-  dformat(sum(!is.na(easq[ind_secure,]$com)),0)
  mean_secure<-  mean(easq[ind_secure,]$com,na.rm =TRUE)
  SD_secure <-  sd(easq[ind_secure,]$com,na.rm =TRUE)
  com_secure_mean[arm]<-paste(dformat(mean_secure,2)," (",dformat(SD_secure,2),")",sep="")
  
  com_insecure_n[arm]<-dformat(sum(!is.na(easq[ind_insecure,]$com)),0)
  mean_insecure <-  mean(easq[ind_insecure,]$com,na.rm =TRUE)
  SD_insecure   <-  sd(easq[ind_insecure,]$com,na.rm =TRUE)
  com_insecure_mean[arm]<-paste(dformat(mean_insecure,2)," (",dformat(SD_insecure,2),")",sep="")
}
com_secure_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_secure_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

com_insecure_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_insecure_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


com_secure_mdiff["Control"]<-""
com_insecure_mdiff["Control"]<-""
com_p["Control"]<-""

for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("Control",arm))
  
  dif_secure<- dformat(reg$lincom$est[1],2)
  lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
  ucb_secure<- dformat(reg$lincom$est.ub[1],2)
  com_secure_mdiff[arm]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")
  
  dif_insecure<- dformat(reg$lincom$est[2],2)
  lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
  ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
  com_insecure_mdiff[arm]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")
  
  com_p[arm]<- dformat(reg$fit[paste("tr",arm,":Vinsecure",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("WSH","WSH+N"))

dif_secure<- dformat(reg$lincom$est[1],2)
lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
ucb_secure<- dformat(reg$lincom$est.ub[1],2)
com_secure_mdiff["N+WSH vs WSH"]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")

dif_insecure<- dformat(reg$lincom$est[2],2)
lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
com_insecure_mdiff["N+WSH vs WSH"]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")

com_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:Vinsecure","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("Nutrition","WSH+N"))

dif_secure<- dformat(reg$lincom$est[1],2)
lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
ucb_secure<- dformat(reg$lincom$est.ub[1],2)
com_secure_mdiff["N+WSH vs N"]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")

dif_insecure<- dformat(reg$lincom$est[2],2)
lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
com_insecure_mdiff["N+WSH vs N"]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")

com_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:Vinsecure","Pr(>|z|)"],3)
#-----------------------------------------
# analysis for easq-gross-motor z-score
#-----------------------------------------
mot_arm<-NULL
mot_secure_n<-NULL
mot_secure_mean<-NULL
mot_secure_mdiff<-NULL
mot_insecure_n<-NULL
mot_insecure_mean<-NULL
mot_insecure_mdiff<-NULL
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
  ind_secure  <-easq$arm==arm & easq$catfoodin=="secure"
  ind_insecure<-easq$arm==arm & easq$catfoodin=="insecure"
  
  mot_secure_n[arm]<-  dformat(sum(!is.na(easq[ind_secure,]$motor)),0)
  mean_secure<-  mean(easq[ind_secure,]$motor,na.rm =TRUE)
  SD_secure <-  sd(easq[ind_secure,]$motor,na.rm =TRUE)
  mot_secure_mean[arm]<-paste(dformat(mean_secure,2)," (",dformat(SD_secure,2),")",sep="")
  
  mot_insecure_n[arm]<-dformat(sum(!is.na(easq[ind_insecure,]$motor)),0)
  mean_insecure <-  mean(easq[ind_insecure,]$motor,na.rm =TRUE)
  SD_insecure   <-  sd(easq[ind_insecure,]$motor,na.rm =TRUE)
  mot_insecure_mean[arm]<-paste(dformat(mean_insecure,2)," (",dformat(SD_insecure,2),")",sep="")
}


mot_secure_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_secure_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

mot_insecure_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_insecure_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


mot_secure_mdiff["Control"]<-""
mot_insecure_mdiff["Control"]<-""
mot_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("Control",arm))
  
  dif_secure<- dformat(reg$lincom$est[1],2)
  lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
  ucb_secure<- dformat(reg$lincom$est.ub[1],2)
  mot_secure_mdiff[arm]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")
  
  dif_insecure<- dformat(reg$lincom$est[2],2)
  lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
  ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
  mot_insecure_mdiff[arm]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")
  
  mot_p[arm]<- dformat(reg$fit[paste("tr",arm,":Vinsecure",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("WSH","WSH+N"))

dif_secure<- dformat(reg$lincom$est[1],2)
lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
ucb_secure<- dformat(reg$lincom$est.ub[1],2)
mot_secure_mdiff["N+WSH vs WSH"]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")

dif_insecure<- dformat(reg$lincom$est[2],2)
lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
mot_insecure_mdiff["N+WSH vs WSH"]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")

mot_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:Vinsecure","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("Nutrition","WSH+N"))

dif_secure<- dformat(reg$lincom$est[1],2)
lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
ucb_secure<- dformat(reg$lincom$est.ub[1],2)
mot_secure_mdiff["N+WSH vs N"]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")

dif_insecure<- dformat(reg$lincom$est[2],2)
lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
mot_insecure_mdiff["N+WSH vs N"]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")

mot_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:Vinsecure","Pr(>|z|)"],3)
#-----------------------------------------
# analysis for easq-persoanl-social z-score
#-----------------------------------------

per_arm<-NULL
per_secure_n<-NULL
per_secure_mean<-NULL
per_secure_mdiff<-NULL
per_insecure_n<-NULL
per_insecure_mean<-NULL
per_insecure_mdiff<-NULL
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
  ind_secure  <-easq$arm==arm & easq$catfoodin=="secure"
  ind_insecure<-easq$arm==arm & easq$catfoodin=="insecure"
  
  per_secure_n[arm]<-  dformat(sum(!is.na(easq[ind_secure,]$personal)),0)
  mean_secure<-  mean(easq[ind_secure,]$personal,na.rm =TRUE)
  SD_secure <-  sd(easq[ind_secure,]$personal,na.rm =TRUE)
  per_secure_mean[arm]<-paste(dformat(mean_secure,2)," (",dformat(SD_secure,2),")",sep="")
  
  per_insecure_n[arm]<-dformat(sum(!is.na(easq[ind_insecure,]$personal)),0)
  mean_insecure <-  mean(easq[ind_insecure,]$personal,na.rm =TRUE)
  SD_insecure   <-  sd(easq[ind_insecure,]$personal,na.rm =TRUE)
  per_insecure_mean[arm]<-paste(dformat(mean_insecure,2)," (",dformat(SD_insecure,2),")",sep="")
}


per_secure_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_secure_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

per_insecure_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_insecure_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


per_secure_mdiff["Control"]<-""
per_insecure_mdiff["Control"]<-""
per_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("Control",arm))
  
  dif_secure<- dformat(reg$lincom$est[1],2)
  lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
  ucb_secure<- dformat(reg$lincom$est.ub[1],2)
  per_secure_mdiff[arm]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")
  
  dif_insecure<- dformat(reg$lincom$est[2],2)
  lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
  ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
  per_insecure_mdiff[arm]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")
  
  per_p[arm]<- dformat(reg$fit[paste("tr",arm,":Vinsecure",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("WSH","WSH+N"))

dif_secure<- dformat(reg$lincom$est[1],2)
lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
ucb_secure<- dformat(reg$lincom$est.ub[1],2)
per_secure_mdiff["N+WSH vs WSH"]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")

dif_insecure<- dformat(reg$lincom$est[2],2)
lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
per_insecure_mdiff["N+WSH vs WSH"]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")

per_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:Vinsecure","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("Nutrition","WSH+N"))

dif_secure<- dformat(reg$lincom$est[1],2)
lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
ucb_secure<- dformat(reg$lincom$est.ub[1],2)
per_secure_mdiff["N+WSH vs N"]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")

dif_insecure<- dformat(reg$lincom$est[2],2)
lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
per_insecure_mdiff["N+WSH vs N"]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")

per_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:Vinsecure","Pr(>|z|)"],3)
#-----------------------------------------
# analysis for easq-combined z-score
#-----------------------------------------

combined_arm<-NULL
combined_secure_n<-NULL
combined_secure_mean<-NULL
combined_secure_mdiff<-NULL
combined_insecure_n<-NULL
combined_insecure_mean<-NULL
combined_insecure_mdiff<-NULL
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
  ind_secure  <-easq$arm==arm & easq$catfoodin=="secure"
  ind_insecure<-easq$arm==arm & easq$catfoodin=="insecure"
  
  combined_secure_n[arm]<-  dformat(sum(!is.na(easq[ind_secure,]$combined)),0)
  mean_secure<-  mean(easq[ind_secure,]$combined,na.rm =TRUE)
  SD_secure <-  sd(easq[ind_secure,]$combined,na.rm =TRUE)
  combined_secure_mean[arm]<-paste(dformat(mean_secure,2)," (",dformat(SD_secure,2),")",sep="")
  
  combined_insecure_n[arm]<-dformat(sum(!is.na(easq[ind_insecure,]$combined)),0)
  mean_insecure <-  mean(easq[ind_insecure,]$combined,na.rm =TRUE)
  SD_insecure   <-  sd(easq[ind_insecure,]$combined,na.rm =TRUE)
  combined_insecure_mean[arm]<-paste(dformat(mean_insecure,2)," (",dformat(SD_insecure,2),")",sep="")
}

combined_secure_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_secure_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

combined_insecure_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_insecure_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


combined_secure_mdiff["Control"]<-""
combined_insecure_mdiff["Control"]<-""
combined_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("Control",arm))
  
  dif_secure<- dformat(reg$lincom$est[1],2)
  lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
  ucb_secure<- dformat(reg$lincom$est.ub[1],2)
  combined_secure_mdiff[arm]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")
  
  dif_insecure<- dformat(reg$lincom$est[2],2)
  lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
  ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
  combined_insecure_mdiff[arm]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")
  
  combined_p[arm]<- dformat(reg$fit[paste("tr",arm,":Vinsecure",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("WSH","WSH+N"))

dif_secure<- dformat(reg$lincom$est[1],2)
lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
ucb_secure<- dformat(reg$lincom$est.ub[1],2)
combined_secure_mdiff["N+WSH vs WSH"]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")

dif_insecure<- dformat(reg$lincom$est[2],2)
lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
combined_insecure_mdiff["N+WSH vs WSH"]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")

combined_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:Vinsecure","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catfoodin"],V="catfoodin",contrast=c("Nutrition","WSH+N"))

dif_secure<- dformat(reg$lincom$est[1],2)
lcb_secure<- dformat(reg$lincom$est.lb[1],2) 
ucb_secure<- dformat(reg$lincom$est.ub[1],2)
combined_secure_mdiff["N+WSH vs N"]<-paste(dif_secure," (",lcb_secure,", ",ucb_secure,")",sep="")

dif_insecure<- dformat(reg$lincom$est[2],2)
lcb_insecure<- dformat(reg$lincom$est.lb[2],2) 
ucb_insecure<- dformat(reg$lincom$est.ub[2],2)
combined_insecure_mdiff["N+WSH vs N"]<-paste(dif_insecure," (",lcb_insecure,", ",ucb_insecure,")",sep="")

combined_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:Vinsecure","Pr(>|z|)"],3)

#----------------------------------------------------
# combining all the results into a single data frame
#----------------------------------------------------

com_df<- cbind(
  a=com_arm,
  b=com_secure_n,
  c=com_secure_mean,
  d=com_secure_mdiff,
  e=com_insecure_n,
  f=com_insecure_mean,
  g=com_insecure_mdiff,
  h=com_p
)
mot_df<- cbind(
  a=mot_arm,
  b=mot_secure_n,
  c=mot_secure_mean,
  d=mot_secure_mdiff,
  e=mot_insecure_n,
  f=mot_insecure_mean,
  g=mot_insecure_mdiff,
  h=mot_p
) 

per_df<-  cbind(
  a=per_arm,
  b=per_secure_n,
  c=per_secure_mean,
  d=per_secure_mdiff,
  e=per_insecure_n,
  f=per_insecure_mean,
  g=per_insecure_mdiff,
  h=per_p
)
combined_df<-  cbind(
  a=combined_arm,
  b=combined_secure_n,
  c=combined_secure_mean,
  d=combined_secure_mdiff,
  e=combined_insecure_n,
  f=combined_insecure_mean,
  g=combined_insecure_mdiff,
  h=combined_p
)


com_glue<-      data.frame(a=c("\u005ctextbf{Communication z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
mot_glue<-      data.frame(a=c("\u005ctextbf{Gross Motor  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
per_glue<-      data.frame(a=c("\u005ctextbf{Personal-social  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
combined_glue<- data.frame(a=c("\u005ctextbf{Combined z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))

sub_food<-rbind(
  com_glue,
  com_df,
  mot_glue,
  mot_df,
  per_glue,
  per_df,
  combined_glue,
  combined_df  
)
save(sub_food,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\sub-food.Rdata")