#---------------------------
# tabs4-subgroup-ses.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# interventions on easq
# subgroup analysis by socio-
# economic status
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
#            washb-bangladesh-subgroupvar.dta
# output files:
#            table-9s.Rdata
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
com_low_n<-NULL
com_low_mean<-NULL
com_low_mdiff<-NULL
com_high_n<-NULL
com_high_mean<-NULL
com_high_mdiff<-NULL
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
  ind_low  <-easq$arm==arm & easq$catasset=="Low"
  ind_high<-easq$arm==arm & easq$catasset=="High"
  
  com_low_n[arm]<-  dformat(sum(!is.na(easq[ind_low,]$com)),0)
  mean_low<-  mean(easq[ind_low,]$com,na.rm =TRUE)
  SD_low <-  sd(easq[ind_low,]$com,na.rm =TRUE)
  com_low_mean[arm]<-paste(dformat(mean_low,2)," (",dformat(SD_low,2),")",sep="")
  
  com_high_n[arm]<-dformat(sum(!is.na(easq[ind_high,]$com)),0)
  mean_high <-  mean(easq[ind_high,]$com,na.rm =TRUE)
  SD_high   <-  sd(easq[ind_high,]$com,na.rm =TRUE)
  com_high_mean[arm]<-paste(dformat(mean_high,2)," (",dformat(SD_high,2),")",sep="")
}
com_low_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_low_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

com_high_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_high_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


com_low_mdiff["Control"]<-""
com_high_mdiff["Control"]<-""
com_p["Control"]<-""

for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("Control",arm))
  
  dif_low<- dformat(reg$lincom$est[1],2)
  lcb_low<- dformat(reg$lincom$est.lb[1],2) 
  ucb_low<- dformat(reg$lincom$est.ub[1],2)
  com_low_mdiff[arm]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")
  
  dif_high<- dformat(reg$lincom$est[2],2)
  lcb_high<- dformat(reg$lincom$est.lb[2],2) 
  ucb_high<- dformat(reg$lincom$est.ub[2],2)
  com_high_mdiff[arm]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")
  
  com_p[arm]<- dformat(reg$fit[paste("tr",arm,":VHigh",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("WSH","WSH+N"))

dif_low<- dformat(reg$lincom$est[1],2)
lcb_low<- dformat(reg$lincom$est.lb[1],2) 
ucb_low<- dformat(reg$lincom$est.ub[1],2)
com_low_mdiff["N+WSH vs WSH"]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")

dif_high<- dformat(reg$lincom$est[2],2)
lcb_high<- dformat(reg$lincom$est.lb[2],2) 
ucb_high<- dformat(reg$lincom$est.ub[2],2)
com_high_mdiff["N+WSH vs WSH"]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")

com_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VHigh","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("Nutrition","WSH+N"))

dif_low<- dformat(reg$lincom$est[1],2)
lcb_low<- dformat(reg$lincom$est.lb[1],2) 
ucb_low<- dformat(reg$lincom$est.ub[1],2)
com_low_mdiff["N+WSH vs N"]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")

dif_high<- dformat(reg$lincom$est[2],2)
lcb_high<- dformat(reg$lincom$est.lb[2],2) 
ucb_high<- dformat(reg$lincom$est.ub[2],2)
com_high_mdiff["N+WSH vs N"]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")

com_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VHigh","Pr(>|z|)"],3)
#-----------------------------------------
# analysis for easq-gross-motor z-score
#-----------------------------------------

mot_arm<-NULL
mot_low_n<-NULL
mot_low_mean<-NULL
mot_low_mdiff<-NULL
mot_high_n<-NULL
mot_high_mean<-NULL
mot_high_mdiff<-NULL
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
  ind_low  <-easq$arm==arm & easq$catasset=="Low"
  ind_high<-easq$arm==arm & easq$catasset=="High"
  
  mot_low_n[arm]<-  dformat(sum(!is.na(easq[ind_low,]$motor)),0)
  mean_low<-  mean(easq[ind_low,]$motor,na.rm =TRUE)
  SD_low <-  sd(easq[ind_low,]$motor,na.rm =TRUE)
  mot_low_mean[arm]<-paste(dformat(mean_low,2)," (",dformat(SD_low,2),")",sep="")
  
  mot_high_n[arm]<-dformat(sum(!is.na(easq[ind_high,]$motor)),0)
  mean_high <-  mean(easq[ind_high,]$motor,na.rm =TRUE)
  SD_high   <-  sd(easq[ind_high,]$motor,na.rm =TRUE)
  mot_high_mean[arm]<-paste(dformat(mean_high,2)," (",dformat(SD_high,2),")",sep="")
}


mot_low_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_low_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

mot_high_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_high_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


mot_low_mdiff["Control"]<-""
mot_high_mdiff["Control"]<-""
mot_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("Control",arm))
  
  dif_low<- dformat(reg$lincom$est[1],2)
  lcb_low<- dformat(reg$lincom$est.lb[1],2) 
  ucb_low<- dformat(reg$lincom$est.ub[1],2)
  mot_low_mdiff[arm]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")
  
  dif_high<- dformat(reg$lincom$est[2],2)
  lcb_high<- dformat(reg$lincom$est.lb[2],2) 
  ucb_high<- dformat(reg$lincom$est.ub[2],2)
  mot_high_mdiff[arm]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")
  
  mot_p[arm]<- dformat(reg$fit[paste("tr",arm,":VHigh",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("WSH","WSH+N"))

dif_low<- dformat(reg$lincom$est[1],2)
lcb_low<- dformat(reg$lincom$est.lb[1],2) 
ucb_low<- dformat(reg$lincom$est.ub[1],2)
mot_low_mdiff["N+WSH vs WSH"]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")

dif_high<- dformat(reg$lincom$est[2],2)
lcb_high<- dformat(reg$lincom$est.lb[2],2) 
ucb_high<- dformat(reg$lincom$est.ub[2],2)
mot_high_mdiff["N+WSH vs WSH"]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")

mot_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VHigh","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("Nutrition","WSH+N"))

dif_low<- dformat(reg$lincom$est[1],2)
lcb_low<- dformat(reg$lincom$est.lb[1],2) 
ucb_low<- dformat(reg$lincom$est.ub[1],2)
mot_low_mdiff["N+WSH vs N"]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")

dif_high<- dformat(reg$lincom$est[2],2)
lcb_high<- dformat(reg$lincom$est.lb[2],2) 
ucb_high<- dformat(reg$lincom$est.ub[2],2)
mot_high_mdiff["N+WSH vs N"]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")

mot_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VHigh","Pr(>|z|)"],3)
#-----------------------------------------
# analysis for easq-personal-social z-score
#-----------------------------------------

per_arm<-NULL
per_low_n<-NULL
per_low_mean<-NULL
per_low_mdiff<-NULL
per_high_n<-NULL
per_high_mean<-NULL
per_high_mdiff<-NULL
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
  ind_low  <-easq$arm==arm & easq$catasset=="Low"
  ind_high<-easq$arm==arm & easq$catasset=="High"
  
  per_low_n[arm]<-  dformat(sum(!is.na(easq[ind_low,]$personal)),0)
  mean_low<-  mean(easq[ind_low,]$personal,na.rm =TRUE)
  SD_low <-  sd(easq[ind_low,]$personal,na.rm =TRUE)
  per_low_mean[arm]<-paste(dformat(mean_low,2)," (",dformat(SD_low,2),")",sep="")
  
  per_high_n[arm]<-dformat(sum(!is.na(easq[ind_high,]$personal)),0)
  mean_high <-  mean(easq[ind_high,]$personal,na.rm =TRUE)
  SD_high   <-  sd(easq[ind_high,]$personal,na.rm =TRUE)
  per_high_mean[arm]<-paste(dformat(mean_high,2)," (",dformat(SD_high,2),")",sep="")
}


per_low_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_low_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

per_high_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_high_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


per_low_mdiff["Control"]<-""
per_high_mdiff["Control"]<-""
per_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("Control",arm))
  
  dif_low<- dformat(reg$lincom$est[1],2)
  lcb_low<- dformat(reg$lincom$est.lb[1],2) 
  ucb_low<- dformat(reg$lincom$est.ub[1],2)
  per_low_mdiff[arm]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")
  
  dif_high<- dformat(reg$lincom$est[2],2)
  lcb_high<- dformat(reg$lincom$est.lb[2],2) 
  ucb_high<- dformat(reg$lincom$est.ub[2],2)
  per_high_mdiff[arm]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")
  
  per_p[arm]<- dformat(reg$fit[paste("tr",arm,":VHigh",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("WSH","WSH+N"))

dif_low<- dformat(reg$lincom$est[1],2)
lcb_low<- dformat(reg$lincom$est.lb[1],2) 
ucb_low<- dformat(reg$lincom$est.ub[1],2)
per_low_mdiff["N+WSH vs WSH"]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")

dif_high<- dformat(reg$lincom$est[2],2)
lcb_high<- dformat(reg$lincom$est.lb[2],2) 
ucb_high<- dformat(reg$lincom$est.ub[2],2)
per_high_mdiff["N+WSH vs WSH"]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")

per_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VHigh","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("Nutrition","WSH+N"))

dif_low<- dformat(reg$lincom$est[1],2)
lcb_low<- dformat(reg$lincom$est.lb[1],2) 
ucb_low<- dformat(reg$lincom$est.ub[1],2)
per_low_mdiff["N+WSH vs N"]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")

dif_high<- dformat(reg$lincom$est[2],2)
lcb_high<- dformat(reg$lincom$est.lb[2],2) 
ucb_high<- dformat(reg$lincom$est.ub[2],2)
per_high_mdiff["N+WSH vs N"]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")

per_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VHigh","Pr(>|z|)"],3)

#-----------------------------------------
# analysis for easq-combined z-score
#-----------------------------------------

combined_arm<-NULL
combined_low_n<-NULL
combined_low_mean<-NULL
combined_low_mdiff<-NULL
combined_high_n<-NULL
combined_high_mean<-NULL
combined_high_mdiff<-NULL
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
  ind_low  <-easq$arm==arm & easq$catasset=="Low"
  ind_high<-easq$arm==arm & easq$catasset=="High"
  
  combined_low_n[arm]<-  dformat(sum(!is.na(easq[ind_low,]$combined)),0)
  mean_low<-  mean(easq[ind_low,]$combined,na.rm =TRUE)
  SD_low <-  sd(easq[ind_low,]$combined,na.rm =TRUE)
  combined_low_mean[arm]<-paste(dformat(mean_low,2)," (",dformat(SD_low,2),")",sep="")
  
  combined_high_n[arm]<-dformat(sum(!is.na(easq[ind_high,]$combined)),0)
  mean_high <-  mean(easq[ind_high,]$combined,na.rm =TRUE)
  SD_high   <-  sd(easq[ind_high,]$combined,na.rm =TRUE)
  combined_high_mean[arm]<-paste(dformat(mean_high,2)," (",dformat(SD_high,2),")",sep="")
}

combined_low_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_low_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

combined_high_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_high_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


combined_low_mdiff["Control"]<-""
combined_high_mdiff["Control"]<-""
combined_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("Control",arm))
  
  dif_low<- dformat(reg$lincom$est[1],2)
  lcb_low<- dformat(reg$lincom$est.lb[1],2) 
  ucb_low<- dformat(reg$lincom$est.ub[1],2)
  combined_low_mdiff[arm]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")
  
  dif_high<- dformat(reg$lincom$est[2],2)
  lcb_high<- dformat(reg$lincom$est.lb[2],2) 
  ucb_high<- dformat(reg$lincom$est.ub[2],2)
  combined_high_mdiff[arm]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")
  
  combined_p[arm]<- dformat(reg$fit[paste("tr",arm,":VHigh",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("WSH","WSH+N"))

dif_low<- dformat(reg$lincom$est[1],2)
lcb_low<- dformat(reg$lincom$est.lb[1],2) 
ucb_low<- dformat(reg$lincom$est.ub[1],2)
combined_low_mdiff["N+WSH vs WSH"]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")

dif_high<- dformat(reg$lincom$est[2],2)
lcb_high<- dformat(reg$lincom$est.lb[2],2) 
ucb_high<- dformat(reg$lincom$est.ub[2],2)
combined_high_mdiff["N+WSH vs WSH"]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")

combined_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VHigh","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catasset"],V="catasset",contrast=c("Nutrition","WSH+N"))

dif_low<- dformat(reg$lincom$est[1],2)
lcb_low<- dformat(reg$lincom$est.lb[1],2) 
ucb_low<- dformat(reg$lincom$est.ub[1],2)
combined_low_mdiff["N+WSH vs N"]<-paste(dif_low," (",lcb_low,", ",ucb_low,")",sep="")

dif_high<- dformat(reg$lincom$est[2],2)
lcb_high<- dformat(reg$lincom$est.lb[2],2) 
ucb_high<- dformat(reg$lincom$est.ub[2],2)
combined_high_mdiff["N+WSH vs N"]<-paste(dif_high," (",lcb_high,", ",ucb_high,")",sep="")

combined_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VHigh","Pr(>|z|)"],3)

#----------------------------------------------------
# combining all the results into a single data frame
#----------------------------------------------------

com_df<- cbind(
  a=com_arm,
  b=com_low_n,
  c=com_low_mean,
  d=com_low_mdiff,
  e=com_high_n,
  f=com_high_mean,
  g=com_high_mdiff,
  h=com_p
)
mot_df<- cbind(
  a=mot_arm,
  b=mot_low_n,
  c=mot_low_mean,
  d=mot_low_mdiff,
  e=mot_high_n,
  f=mot_high_mean,
  g=mot_high_mdiff,
  h=mot_p
) 

per_df<-  cbind(
  a=per_arm,
  b=per_low_n,
  c=per_low_mean,
  d=per_low_mdiff,
  e=per_high_n,
  f=per_high_mean,
  g=per_high_mdiff,
  h=per_p
)
combined_df<-  cbind(
  a=combined_arm,
  b=combined_low_n,
  c=combined_low_mean,
  d=combined_low_mdiff,
  e=combined_high_n,
  f=combined_high_mean,
  g=combined_high_mdiff,
  h=combined_p
)


com_glue<-      data.frame(a=c("\u005ctextbf{Communication z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
mot_glue<-      data.frame(a=c("\u005ctextbf{Gross Motor  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
per_glue<-      data.frame(a=c("\u005ctextbf{Personal-social  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
combined_glue<- data.frame(a=c("\u005ctextbf{Combined z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))

table_s9<-rbind(
  com_glue,
  com_df,
  mot_glue,
  mot_df,
  per_glue,
  per_df,
  combined_glue,
  combined_df  
)
save(table_s9,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\table-s9.Rdata")