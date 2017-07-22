#---------------------------------------
# tabs2-who-motor-adjusted.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# interventions on who 6 motor milestones
# adjusted analysis
#--------------------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
#            washb-bangladesh-enrol.dta
# output files:
#            table-s2.Rdata
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
# source function to estimate hazard ratio
source("C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\analysis script\\functions\\washb_mmfunction.R")


#-------------------------------
# load the analysis dataset
#-------------------------------
whomm<-read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-motormile-year1.dta")
enrol<- read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-enrol.dta")

enrol<-subset(enrol,
              select=c("dataid","momage","momedu","momheight","hfiacat",
                       "Nlt18","Ncomp","watmin","elec","floor",
                       "walls","roof","asset_wardrobe","asset_table","asset_chair",
                       "asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike",
                       "asset_moto","asset_sewmach","asset_mobile"))
whomm_adj<-NULL
whomm_adj<-merge(enrol,whomm,by="dataid")

group<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N")
cov_adj<-c("agedays","month","fracode","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")
#-------------------------------
#Standing with assistance
#-------------------------------
#column 1
standsupp_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
standsupp_col_2<-NULL
for(arm in group){
  standsupp_col_2[arm]<-dformat(sum(!is.na(whomm_adj[whomm_adj$arm==arm,]$stand_supp)),0)
  
}
#column 3
standsupp_col_3<-NULL
standsupp_col_3["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- whomm_gam(Y=whomm_adj$stand_supp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Control",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)
  standsupp_col_3[arm]<-  paste(hr," (",lcb,", ",ucb,")",sep="")
}

#column 4
standsupp_col_4<-NULL
standsupp_col_4["Control"]<-     ""
standsupp_col_4["Water"]<-       ""
standsupp_col_4["Sanitation"]<-  ""
standsupp_col_4["Handwashing"]<- ""
standsupp_col_4["WSH"]<-         "Ref"
standsupp_col_4["Nutrition"]<-   ""
for(arm in group[7]){
  reg<-  whomm_gam(Y=whomm_adj$stand_supp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("WSH",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  standsupp_col_4[arm]<- paste(hr," (",lcb,", ",ucb,")",sep="") 
}
#column 5
standsupp_col_5<-NULL
standsupp_col_5["Control"]<-     ""
standsupp_col_5["Water"]<-       ""
standsupp_col_5["Sanitation"]<-  ""
standsupp_col_5["Handwashing"]<- ""
standsupp_col_5["WSH"]<-         ""
standsupp_col_5["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- whomm_gam(Y=whomm_adj$stand_supp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Nutrition",arm), W=whomm_adj[cov_adj], pval=0.1)
  dif<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  standsupp_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#------------------------------
#Hands and knees crawling
#------------------------------
# column 1
#column 1
handsknee_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
handsknee_col_2<-NULL
for(arm in group){
  handsknee_col_2[arm]<-dformat(sum(!is.na(whomm_adj[whomm_adj$arm==arm,]$crawl_nosupp)),0)
  
}
#column 3
handsknee_col_3<-NULL
handsknee_col_3["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- whomm_gam(Y=whomm_adj$crawl_nosupp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Control",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)
  handsknee_col_3[arm]<-  paste(hr," (",lcb,", ",ucb,")",sep="")
}
#column 4
handsknee_col_4<-NULL
handsknee_col_4["Control"]<-     ""
handsknee_col_4["Water"]<-       ""
handsknee_col_4["Sanitation"]<-  ""
handsknee_col_4["Handwashing"]<- ""
handsknee_col_4["WSH"]<-         "Ref"
handsknee_col_4["Nutrition"]<-   ""
for(arm in group[7]){
  reg<-  whomm_gam(Y=whomm_adj$crawl_nosupp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("WSH",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  handsknee_col_4[arm]<- paste(hr," (",lcb,", ",ucb,")",sep="") 
}
#column 5
handsknee_col_5<-NULL
handsknee_col_5["Control"]<-     ""
handsknee_col_5["Water"]<-       ""
handsknee_col_5["Sanitation"]<-  ""
handsknee_col_5["Handwashing"]<- ""
handsknee_col_5["WSH"]<-         ""
handsknee_col_5["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- whomm_gam(Y=whomm_adj$crawl_nosupp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Nutrition",arm), W=whomm_adj[cov_adj], pval=0.1)
  dif<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  handsknee_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#--------------------------
#Walking with assistance
#--------------------------
#column 1
walksupp_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
walksupp_col_2<-NULL
for(arm in group){
  walksupp_col_2[arm]<-dformat(sum(!is.na(whomm_adj[whomm_adj$arm==arm,]$walk_supp)),0)
  
}
#column 3
walksupp_col_3<-NULL
walksupp_col_3["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- whomm_gam(Y=whomm_adj$walk_supp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Control",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)
  walksupp_col_3[arm]<-  paste(hr," (",lcb,", ",ucb,")",sep="")
}
#column 4
walksupp_col_4<-NULL
walksupp_col_4["Control"]<-     ""
walksupp_col_4["Water"]<-       ""
walksupp_col_4["Sanitation"]<-  ""
walksupp_col_4["Handwashing"]<- ""
walksupp_col_4["WSH"]<-         "Ref"
walksupp_col_4["Nutrition"]<-   ""
for(arm in group[7]){
  reg<-  whomm_gam(Y=whomm_adj$walk_supp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("WSH",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  walksupp_col_4[arm]<- paste(hr," (",lcb,", ",ucb,")",sep="") 
}
#column 5
walksupp_col_5<-NULL
walksupp_col_5["Control"]<-     ""
walksupp_col_5["Water"]<-       ""
walksupp_col_5["Sanitation"]<-  ""
walksupp_col_5["Handwashing"]<- ""
walksupp_col_5["WSH"]<-         ""
walksupp_col_5["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- whomm_gam(Y=whomm_adj$walk_supp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Nutrition",arm), W=whomm_adj[cov_adj], pval=0.1)
  dif<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  walksupp_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#-----------------------
#Standing alone
#-----------------------
#column 1
standalone_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
standalone_col_2<-NULL
for(arm in group){
  standalone_col_2[arm]<-dformat(sum(!is.na(whomm_adj[whomm_adj$arm==arm,]$stand_nosupp)),0)
  
}
#column 3
standalone_col_3<-NULL
standalone_col_3["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- whomm_gam(Y=whomm_adj$stand_nosupp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Control",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)
  standalone_col_3[arm]<-  paste(hr," (",lcb,", ",ucb,")",sep="")
}
#column 4
standalone_col_4<-NULL
standalone_col_4["Control"]<-     ""
standalone_col_4["Water"]<-       ""
standalone_col_4["Sanitation"]<-  ""
standalone_col_4["Handwashing"]<- ""
standalone_col_4["WSH"]<-         "Ref"
standalone_col_4["Nutrition"]<-   ""
for(arm in group[7]){
  reg<-  whomm_gam(Y=whomm_adj$stand_nosupp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("WSH",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  standalone_col_4[arm]<- paste(hr," (",lcb,", ",ucb,")",sep="") 
}
#column 5
standalone_col_5<-NULL
standalone_col_5["Control"]<-     ""
standalone_col_5["Water"]<-       ""
standalone_col_5["Sanitation"]<-  ""
standalone_col_5["Handwashing"]<- ""
standalone_col_5["WSH"]<-         ""
standalone_col_5["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- whomm_gam(Y=whomm_adj$stand_nosupp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Nutrition",arm), W=whomm_adj[cov_adj], pval=0.1)
  dif<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  standalone_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#--------------------
#Walking alone
#--------------------
#column 1
walkalone_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
walkalone_col_2<-NULL
for(arm in group){
  walkalone_col_2[arm]<-dformat(sum(!is.na(whomm_adj[whomm_adj$arm==arm,]$walk_nosupp)),0)
  
}
#column 3
walkalone_col_3<-NULL
walkalone_col_3["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- whomm_gam(Y=whomm_adj$walk_nosupp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Control",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)
  walkalone_col_3[arm]<-  paste(hr," (",lcb,", ",ucb,")",sep="")
}
#column 4
walkalone_col_4<-NULL
walkalone_col_4["Control"]<-     ""
walkalone_col_4["Water"]<-       ""
walkalone_col_4["Sanitation"]<-  ""
walkalone_col_4["Handwashing"]<- ""
walkalone_col_4["WSH"]<-         "Ref"
walkalone_col_4["Nutrition"]<-   ""
for(arm in group[7]){
  reg<-  whomm_gam(Y=whomm_adj$walk_nosupp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("WSH",arm), W=whomm_adj[cov_adj], pval=0.1)
  hr<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  walkalone_col_4[arm]<- paste(hr," (",lcb,", ",ucb,")",sep="") 
}
#column 5
walkalone_col_5<-NULL
walkalone_col_5["Control"]<-     ""
walkalone_col_5["Water"]<-       ""
walkalone_col_5["Sanitation"]<-  ""
walkalone_col_5["Handwashing"]<- ""
walkalone_col_5["WSH"]<-         ""
walkalone_col_5["Nutrition"]<-   "Ref"
for(arm in group[7]){
  reg<- whomm_gam(Y=whomm_adj$walk_nosupp,childage=whomm_adj$agedays,tr=whomm_adj$arm,contrast=c("Nutrition",arm), W=whomm_adj[cov_adj], pval=0.1)
  dif<- dformat(reg[[1]][[1]],2)
  lcb<- dformat(reg[[1]][[3]],2) 
  ucb<- dformat(reg[[1]][[4]],2)  
  walkalone_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#----------------------------------------------------
# combining all the results into a single data frame
#----------------------------------------------------
standsupp_df<- data.frame(a=standsupp_col_1,  b=standsupp_col_2,   c=standsupp_col_3,   d=standsupp_col_4, e=standsupp_col_5)
handsknee_df<- data.frame(a=handsknee_col_1,  b=handsknee_col_2,   c=handsknee_col_3,   d=handsknee_col_4, e=handsknee_col_5)
walksupp_df<- data.frame(a=walksupp_col_1,    b=walksupp_col_2,    c=walksupp_col_3,    d=walksupp_col_4,  e=walksupp_col_5)
standalone_df<- data.frame(a=standalone_col_1,b=standalone_col_2,  c=standalone_col_3,  d=standalone_col_4,e=standalone_col_5)
walkalone_df<- data.frame(a=walkalone_col_1,  b=walkalone_col_2,   c=walkalone_col_3,   d=walkalone_col_4, e=walkalone_col_5)

head_df<-data.frame(a=c("", "Outcome, Arm"),b=c("","N"),c=c("Hazard Ratio","vs. Control (95% CI)"),d=c("Hazard Ratio","vs. WSH (95% CI)"),e=c("Hazard Ratio","vs. Nutrition (95% CI)"))
standsupp_glue<-  data.frame(a=c("\u005ctextbf{Standing with assistance} "),b=c(""),c=c(""),d=c(""),e=c(""))
handsknee_glue<-  data.frame(a=c("\u005ctextbf{Hands and knees crawling} "),b=c(""),c=c(""),d=c(""),e=c(""))
walksupp_glue<-   data.frame(a=c("\u005ctextbf{Walking with assistance} "), b=c(""),c=c(""),d=c(""),e=c(""))
standalone_glue<- data.frame(a=c("\u005ctextbf{Standing alone} "),          b=c(""),c=c(""),d=c(""),e=c(""))
walkalone_glue<-  data.frame(a=c("\u005ctextbf{Walking alone}"),            b=c(""),c=c(""),d=c(""),e=c(""))

table_s2 <- rbind(
  standsupp_glue,
  standsupp_df,
  handsknee_glue,
  handsknee_df,
  walksupp_glue,
  walksupp_df,
  standalone_glue,
  standalone_df,
  walkalone_glue,
  walkalone_df
)
save(table_s2,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\table-s2.Rdata")