#---------------------------
# tabs11-maternal-depression.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# interventions on maternal
# depression
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-momdepression-year1.dta
#            washb-bangladesh-momdepression-year2.dta
#            washb-bangladesh-home-year1.dta
#            washb-bangladesh-home-year2.dta
# output files:
#            home-dep.Rdata
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

depy1<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-momdepression-year1.dta")
depy2<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-momdepression-year2.dta")
homy1<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-home-year1.dta")
homy2<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-home-year2.dta")

depy1<- subset(depy1,select=c("dataid","childid","tchild","clusterid",
                              "block","arm","z_midline_depression"))
depy1<- rename(depy1, replace=c("z_midline_depression"="depy1"))

depy2<- subset(depy2,select=c("dataid","childid","tchild","clusterid",
                              "block","arm","z_endline_depression"))
depy2<- rename(depy2, replace=c("z_endline_depression"="depy2"))

homy1<- subset(homy1,select=c("dataid","childid","tchild","clusterid",
                            "block","arm","z_midline_stimulation"))
homy1<- rename(homy1, replace=c("z_midline_stimulation"="homy1"))
        

homy2<- subset(homy2,select=c("dataid","childid","tchild","clusterid",
                              "block","arm","z_endline_stimulation"))
homy2<- rename(homy2, replace=c("z_endline_stimulation"="homy2"))
        





group<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N")

#----------------------------------
# Year 1, maternal depression
#----------------------------------
#column 1
depy1_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
depy1_col_2<-NULL
for(arm in group){
  depy1_col_2[arm]<-dformat(sum(!is.na(depy1[depy1$arm==arm,]$depy1)),0)
  
}
#column 3
depy1_col_3<-NULL
for(arm in group){
  ind<-depy1$arm==arm
  mean<-  mean(depy1[ind,]$depy1,na.rm =TRUE)
  SD<-  sd(depy1[ind,]$depy1,na.rm =TRUE)
  depy1_col_3[arm]<-  paste(dformat(mean,2)," (",dformat(SD,2),")",sep="")
}
#column 4
depy1_col_4<-NULL
depy1_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=depy1$depy1,tr=depy1$arm,pair=depy1$block,id=depy1$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)
  depy1_col_4[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}

#----------------------------------
# Year 2, maternal depression
#----------------------------------
#column 1
depy2_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
depy2_col_2<-NULL
for(arm in group){
  depy2_col_2[arm]<-dformat(sum(!is.na(depy2[depy2$arm==arm,]$depy2)),0)
  
}
#column 3
depy2_col_3<-NULL
for(arm in group){
  ind<-depy2$arm==arm
  mean<-  mean(depy2[ind,]$depy2,na.rm =TRUE)
  SD<-  sd(depy2[ind,]$depy2,na.rm =TRUE)
  depy2_col_3[arm]<-  paste(dformat(mean,2)," (",dformat(SD,2),")",sep="")
}
#column 4
depy2_col_4<-NULL
depy2_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=depy2$depy2,tr=depy2$arm,pair=depy2$block,id=depy2$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)
  depy2_col_4[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}
#-----------------------------------------------------------
# year 1, home
#------------------------------------------------------------
#column 1
homy1_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
homy1_col_2<-NULL
for(arm in group){
  homy1_col_2[arm]<-dformat(sum(!is.na(homy1[homy1$arm==arm,]$homy1)),0)  
}
#column 3
homy1_col_3<-NULL
for(arm in group){
  ind<-homy1$arm==arm
  mean<-  mean(homy1[ind,]$homy1,na.rm =TRUE)
  SD<-  sd(homy1[ind,]$homy1,na.rm =TRUE)
  homy1_col_3[arm]<-  paste(dformat(mean,2)," (",dformat(SD,2),")",sep="")
}
#column 4
homy1_col_4<-NULL
homy1_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=homy1$homy1,tr=homy1$arm,pair=homy1$block,id=homy1$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)
  homy1_col_4[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}
#-----------------------------------------------------------
# year 2, home
#------------------------------------------------------------
#column 1
homy2_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
homy2_col_2<-NULL
for(arm in group){
  homy2_col_2[arm]<-dformat(sum(!is.na(homy2[homy2$arm==arm,]$homy2)),0)  
}
#column 3
homy2_col_3<-NULL
for(arm in group){
  ind<-homy2$arm==arm
  mean<-  mean(homy2[ind,]$homy2,na.rm =TRUE)
  SD<-  sd(homy2[ind,]$homy2,na.rm =TRUE)
  homy2_col_3[arm]<-  paste(dformat(mean,2)," (",dformat(SD,2),")",sep="")
}
#column 4
homy2_col_4<-NULL
homy2_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=homy2$homy2,tr=homy2$arm,pair=homy2$block,id=homy2$block,contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)
  homy2_col_4[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}



#-------------------------------------------
#combining results into a single data frame
#-------------------------------------------
depy1_df<-data.frame(a=depy1_col_1,b=depy1_col_2,c=depy1_col_3,d=depy1_col_4)
depy2_df<-data.frame(a=depy2_col_1,b=depy2_col_2,c=depy2_col_3,d=depy2_col_4)

homy1_df<-data.frame(a=homy1_col_1,b=homy1_col_2,c=homy1_col_3,d=homy1_col_4)
homy2_df<-data.frame(a=homy2_col_1,b=homy2_col_2,c=homy2_col_3,d=homy2_col_4)

home_dep<-rbind(depy1_df,depy2_df,homy1_df,homy2_df)

save(home_dep,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\home-dep.Rdata")
