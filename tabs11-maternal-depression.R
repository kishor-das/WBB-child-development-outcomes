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
# output files:
#            table-s11.Rdata
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
#diar<-read.dta("washb-bangladesh-diar.dta")

#ind_diary1<-(diar$childid=="T1" |diar$childid=="T2"  )& diar$svy==1 
#ind_diary2<-(diar$childid=="T1" |diar$childid=="T2"  )& diar$svy==2

#diary1<-diar[ind_diary1,]
#diary2<-diar[ind_diary2,]

depy1<- subset(depy1,select=c("dataid","childid","tchild","clusterid",
                              "block","arm","z_midline_depression"))
depy1<- rename(depy1, replace=c("z_midline_depression"="depy1"))

depy2<- subset(depy2,select=c("dataid","childid","tchild","clusterid",
                              "block","arm","z_endline_depression"))
depy2<- rename(depy2, replace=c("z_endline_depression"="depy2"))

#diary1<- subset(diary1,select=c("dataid","childid","tchild","clusterid","block","svy","diar7d"))
#diary1<- rename(diary1,replace=c("diar7d"="diar7dy1"))

#diary2<- subset(diary2,select=c("dataid","childid","tchild","clusterid","block","svy","diar7d"))
#diary2<- rename(diary2,replace=c("diar7d"="diar7dy2"))

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

#-------------------------------------------
#combining results into a single data frame
#-------------------------------------------
depy1_df<-data.frame(a=depy1_col_1,b=depy1_col_2,c=depy1_col_3,d=depy1_col_4)
depy2_df<-data.frame(a=depy2_col_1,b=depy2_col_2,c=depy2_col_3,d=depy2_col_4)

table_s11<-rbind(depy1_df,depy2_df)

save(table_s11,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\table-s11.Rdata")
