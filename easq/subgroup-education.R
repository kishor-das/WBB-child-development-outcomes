#-------------------------------------------
# tabs4-subgroup-education.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# interventions on easq
# subgroup analysis by maternal education
#-------------------------------------------

#-------------------------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
#            washb-bangladesh-subgroupvar.dta
# output files:
#            sbu-edu.Rdata
#-------------------------------------------

#-------------------------------------------
# preamble
#-------------------------------------------
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
com_le9_n<-NULL
com_le9_mean<-NULL
com_le9_mdiff<-NULL
com_m10_n<-NULL
com_m10_mean<-NULL
com_m10_mdiff<-NULL
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
  ind_le9  <-easq$arm==arm & easq$catmomedu=="LE9"
  ind_m10<-easq$arm==arm & easq$catmomedu=="M10"
  
  com_le9_n[arm]<-  dformat(sum(!is.na(easq[ind_le9,]$com)),0)
  mean_le9<-  mean(easq[ind_le9,]$com,na.rm =TRUE)
  SD_le9 <-  sd(easq[ind_le9,]$com,na.rm =TRUE)
  com_le9_mean[arm]<-paste(dformat(mean_le9,2)," (",dformat(SD_le9,2),")",sep="")
  
  com_m10_n[arm]<-dformat(sum(!is.na(easq[ind_m10,]$com)),0)
  mean_m10 <-  mean(easq[ind_m10,]$com,na.rm =TRUE)
  SD_m10   <-  sd(easq[ind_m10,]$com,na.rm =TRUE)
  com_m10_mean[arm]<-paste(dformat(mean_m10,2)," (",dformat(SD_m10,2),")",sep="")
}
com_le9_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_le9_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

com_m10_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
com_m10_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


com_le9_mdiff["Control"]<-""
com_m10_mdiff["Control"]<-""
com_p["Control"]<-""

for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("Control",arm))
  
  dif_le9<- dformat(reg$lincom$est[1],2)
  lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
  ucb_le9<- dformat(reg$lincom$est.ub[1],2)
  com_le9_mdiff[arm]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")
  
  dif_m10<- dformat(reg$lincom$est[2],2)
  lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
  ucb_m10<- dformat(reg$lincom$est.ub[2],2)
  com_m10_mdiff[arm]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")
  
  com_p[arm]<- dformat(reg$fit[paste("tr",arm,":VM10",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("WSH","WSH+N"))

dif_le9<- dformat(reg$lincom$est[1],2)
lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
ucb_le9<- dformat(reg$lincom$est.ub[1],2)
com_le9_mdiff["N+WSH vs WSH"]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")

dif_m10<- dformat(reg$lincom$est[2],2)
lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
ucb_m10<- dformat(reg$lincom$est.ub[2],2)
com_m10_mdiff["N+WSH vs WSH"]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")

com_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VM10","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$com,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("Nutrition","WSH+N"))

dif_le9<- dformat(reg$lincom$est[1],2)
lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
ucb_le9<- dformat(reg$lincom$est.ub[1],2)
com_le9_mdiff["N+WSH vs N"]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")

dif_m10<- dformat(reg$lincom$est[2],2)
lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
ucb_m10<- dformat(reg$lincom$est.ub[2],2)
com_m10_mdiff["N+WSH vs N"]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")

com_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VM10","Pr(>|z|)"],3)
#-----------------------------------------
# analysis for easq-gross-motor z-score
#-----------------------------------------
mot_arm<-NULL
mot_le9_n<-NULL
mot_le9_mean<-NULL
mot_le9_mdiff<-NULL
mot_m10_n<-NULL
mot_m10_mean<-NULL
mot_m10_mdiff<-NULL
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
  ind_le9  <-easq$arm==arm & easq$catmomedu=="LE9"
  ind_m10<-easq$arm==arm & easq$catmomedu=="M10"
  
  mot_le9_n[arm]<-  dformat(sum(!is.na(easq[ind_le9,]$motor)),0)
  mean_le9<-  mean(easq[ind_le9,]$motor,na.rm =TRUE)
  SD_le9 <-  sd(easq[ind_le9,]$motor,na.rm =TRUE)
  mot_le9_mean[arm]<-paste(dformat(mean_le9,2)," (",dformat(SD_le9,2),")",sep="")
  
  mot_m10_n[arm]<-dformat(sum(!is.na(easq[ind_m10,]$motor)),0)
  mean_m10 <-  mean(easq[ind_m10,]$motor,na.rm =TRUE)
  SD_m10   <-  sd(easq[ind_m10,]$motor,na.rm =TRUE)
  mot_m10_mean[arm]<-paste(dformat(mean_m10,2)," (",dformat(SD_m10,2),")",sep="")
}


mot_le9_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_le9_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

mot_m10_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
mot_m10_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


mot_le9_mdiff["Control"]<-""
mot_m10_mdiff["Control"]<-""
mot_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("Control",arm))
  
  dif_le9<- dformat(reg$lincom$est[1],2)
  lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
  ucb_le9<- dformat(reg$lincom$est.ub[1],2)
  mot_le9_mdiff[arm]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")
  
  dif_m10<- dformat(reg$lincom$est[2],2)
  lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
  ucb_m10<- dformat(reg$lincom$est.ub[2],2)
  mot_m10_mdiff[arm]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")
  
  mot_p[arm]<- dformat(reg$fit[paste("tr",arm,":VM10",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("WSH","WSH+N"))

dif_le9<- dformat(reg$lincom$est[1],2)
lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
ucb_le9<- dformat(reg$lincom$est.ub[1],2)
mot_le9_mdiff["N+WSH vs WSH"]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")

dif_m10<- dformat(reg$lincom$est[2],2)
lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
ucb_m10<- dformat(reg$lincom$est.ub[2],2)
mot_m10_mdiff["N+WSH vs WSH"]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")

mot_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VM10","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$motor,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("Nutrition","WSH+N"))

dif_le9<- dformat(reg$lincom$est[1],2)
lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
ucb_le9<- dformat(reg$lincom$est.ub[1],2)
mot_le9_mdiff["N+WSH vs N"]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")

dif_m10<- dformat(reg$lincom$est[2],2)
lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
ucb_m10<- dformat(reg$lincom$est.ub[2],2)
mot_m10_mdiff["N+WSH vs N"]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")

mot_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VM10","Pr(>|z|)"],3)
#-----------------------------------------
# analysis for easq-personal-social z-score
#-----------------------------------------

per_arm<-NULL
per_le9_n<-NULL
per_le9_mean<-NULL
per_le9_mdiff<-NULL
per_m10_n<-NULL
per_m10_mean<-NULL
per_m10_mdiff<-NULL
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
  ind_le9  <-easq$arm==arm & easq$catmomedu=="LE9"
  ind_m10<-easq$arm==arm & easq$catmomedu=="M10"
  
  per_le9_n[arm]<-  dformat(sum(!is.na(easq[ind_le9,]$personal)),0)
  mean_le9<-  mean(easq[ind_le9,]$personal,na.rm =TRUE)
  SD_le9 <-  sd(easq[ind_le9,]$personal,na.rm =TRUE)
  per_le9_mean[arm]<-paste(dformat(mean_le9,2)," (",dformat(SD_le9,2),")",sep="")
  
  per_m10_n[arm]<-dformat(sum(!is.na(easq[ind_m10,]$personal)),0)
  mean_m10 <-  mean(easq[ind_m10,]$personal,na.rm =TRUE)
  SD_m10   <-  sd(easq[ind_m10,]$personal,na.rm =TRUE)
  per_m10_mean[arm]<-paste(dformat(mean_m10,2)," (",dformat(SD_m10,2),")",sep="")
}


per_le9_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_le9_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

per_m10_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
per_m10_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


per_le9_mdiff["Control"]<-""
per_m10_mdiff["Control"]<-""
per_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("Control",arm))
  
  dif_le9<- dformat(reg$lincom$est[1],2)
  lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
  ucb_le9<- dformat(reg$lincom$est.ub[1],2)
  per_le9_mdiff[arm]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")
  
  dif_m10<- dformat(reg$lincom$est[2],2)
  lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
  ucb_m10<- dformat(reg$lincom$est.ub[2],2)
  per_m10_mdiff[arm]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")
  
  per_p[arm]<- dformat(reg$fit[paste("tr",arm,":VM10",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("WSH","WSH+N"))

dif_le9<- dformat(reg$lincom$est[1],2)
lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
ucb_le9<- dformat(reg$lincom$est.ub[1],2)
per_le9_mdiff["N+WSH vs WSH"]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")

dif_m10<- dformat(reg$lincom$est[2],2)
lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
ucb_m10<- dformat(reg$lincom$est.ub[2],2)
per_m10_mdiff["N+WSH vs WSH"]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")

per_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VM10","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$personal,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("Nutrition","WSH+N"))

dif_le9<- dformat(reg$lincom$est[1],2)
lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
ucb_le9<- dformat(reg$lincom$est.ub[1],2)
per_le9_mdiff["N+WSH vs N"]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")

dif_m10<- dformat(reg$lincom$est[2],2)
lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
ucb_m10<- dformat(reg$lincom$est.ub[2],2)
per_m10_mdiff["N+WSH vs N"]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")

per_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VM10","Pr(>|z|)"],3)

#-----------------------------------------
# analysis for easq-combined z-score
#-----------------------------------------
combined_arm<-NULL
combined_le9_n<-NULL
combined_le9_mean<-NULL
combined_le9_mdiff<-NULL
combined_m10_n<-NULL
combined_m10_mean<-NULL
combined_m10_mdiff<-NULL
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
  ind_le9  <-easq$arm==arm & easq$catmomedu=="LE9"
  ind_m10<-easq$arm==arm & easq$catmomedu=="M10"
  
  combined_le9_n[arm]<-  dformat(sum(!is.na(easq[ind_le9,]$combined)),0)
  mean_le9<-  mean(easq[ind_le9,]$combined,na.rm =TRUE)
  SD_le9 <-  sd(easq[ind_le9,]$combined,na.rm =TRUE)
  combined_le9_mean[arm]<-paste(dformat(mean_le9,2)," (",dformat(SD_le9,2),")",sep="")
  
  combined_m10_n[arm]<-dformat(sum(!is.na(easq[ind_m10,]$combined)),0)
  mean_m10 <-  mean(easq[ind_m10,]$combined,na.rm =TRUE)
  SD_m10   <-  sd(easq[ind_m10,]$combined,na.rm =TRUE)
  combined_m10_mean[arm]<-paste(dformat(mean_m10,2)," (",dformat(SD_m10,2),")",sep="")
}

combined_le9_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_le9_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""

combined_m10_n[c("N+WSH vs WSH","N+WSH vs N")]<-""
combined_m10_mean[c("N+WSH vs WSH","N+WSH vs N")]<-""


combined_le9_mdiff["Control"]<-""
combined_m10_mdiff["Control"]<-""
combined_p["Control"]<-""
#
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("Control",arm))
  
  dif_le9<- dformat(reg$lincom$est[1],2)
  lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
  ucb_le9<- dformat(reg$lincom$est.ub[1],2)
  combined_le9_mdiff[arm]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")
  
  dif_m10<- dformat(reg$lincom$est[2],2)
  lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
  ucb_m10<- dformat(reg$lincom$est.ub[2],2)
  combined_m10_mdiff[arm]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")
  
  combined_p[arm]<- dformat(reg$fit[paste("tr",arm,":VM10",sep=""),"Pr(>|z|)"],3)
} 

# row N+WSH vs WSH
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("WSH","WSH+N"))

dif_le9<- dformat(reg$lincom$est[1],2)
lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
ucb_le9<- dformat(reg$lincom$est.ub[1],2)
combined_le9_mdiff["N+WSH vs WSH"]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")

dif_m10<- dformat(reg$lincom$est[2],2)
lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
ucb_m10<- dformat(reg$lincom$est.ub[2],2)
combined_m10_mdiff["N+WSH vs WSH"]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")

combined_p["N+WSH vs WSH"]<- dformat(reg$fit["trWSH+N:VM10","Pr(>|z|)"],3)
# row N+WSH vs N
reg<- washb_glm(Y=easq$combined,tr=easq$arm,pair=easq$block,id=easq$block,W=easq["catmomedu"],V="catmomedu",contrast=c("Nutrition","WSH+N"))

dif_le9<- dformat(reg$lincom$est[1],2)
lcb_le9<- dformat(reg$lincom$est.lb[1],2) 
ucb_le9<- dformat(reg$lincom$est.ub[1],2)
combined_le9_mdiff["N+WSH vs N"]<-paste(dif_le9," (",lcb_le9,", ",ucb_le9,")",sep="")

dif_m10<- dformat(reg$lincom$est[2],2)
lcb_m10<- dformat(reg$lincom$est.lb[2],2) 
ucb_m10<- dformat(reg$lincom$est.ub[2],2)
combined_m10_mdiff["N+WSH vs N"]<-paste(dif_m10," (",lcb_m10,", ",ucb_m10,")",sep="")

combined_p["N+WSH vs N"]<- dformat(reg$fit["trWSH+N:VM10","Pr(>|z|)"],3)
#----------------------------------------------------
# combining all the results into a single data frame
#----------------------------------------------------

com_df<- cbind(
  a=com_arm,
  b=com_le9_n,
  c=com_le9_mean,
  d=com_le9_mdiff,
  e=com_m10_n,
  f=com_m10_mean,
  g=com_m10_mdiff,
  h=com_p
)
mot_df<- cbind(
  a=mot_arm,
  b=mot_le9_n,
  c=mot_le9_mean,
  d=mot_le9_mdiff,
  e=mot_m10_n,
  f=mot_m10_mean,
  g=mot_m10_mdiff,
  h=mot_p
) 

per_df<-  cbind(
  a=per_arm,
  b=per_le9_n,
  c=per_le9_mean,
  d=per_le9_mdiff,
  e=per_m10_n,
  f=per_m10_mean,
  g=per_m10_mdiff,
  h=per_p
)
combined_df<-  cbind(
  a=combined_arm,
  b=combined_le9_n,
  c=combined_le9_mean,
  d=combined_le9_mdiff,
  e=combined_m10_n,
  f=combined_m10_mean,
  g=combined_m10_mdiff,
  h=combined_p
)


com_glue<-      data.frame(a=c("\u005ctextbf{Communication z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
mot_glue<-      data.frame(a=c("\u005ctextbf{Gross Motor  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
per_glue<-      data.frame(a=c("\u005ctextbf{Personal-social  z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))
combined_glue<- data.frame(a=c("\u005ctextbf{Combined z-score}"),b=c(""),c=c(""),d=c(""),e=c(""),f=c(""),g=c(""),h=c(""))

sub_edu<-rbind(
  com_glue,
  com_df,
  mot_glue,
  mot_df,
  per_glue,
  per_df,
  combined_glue,
  combined_df  
)
save(sub_edu,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\sub-edu.Rdata")