#---------------------------
# tabs3-easq-adjusted.R
#
# Kishor Das (kishorisrt@gmail.com)
#
# estimate the effects of WASH
# intervention on easq
# adjusted analysis
#-----------------------------

#----------------------------
# input files :
#            washb-bangladesh-easq-year2.dta
#            washb-bangladesh-enrol.dta
# output files:
#            table-s3.Rdata
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



easq<-  read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-easq-year2.dta")
enrol<- read.dta("C:\\Users\\kdas\\Dropbox\\WASHB-Bangladesh-data\\2-child-development-outcomes-datasets\\washb-bangladesh-enrol.dta")

enrol<-subset(enrol,
              select=c("dataid","momage","momedu","momheight","hfiacat",
                       "Nlt18","Ncomp","watmin","elec","floor",
                       "walls","roof","asset_wardrobe","asset_table","asset_chair",
                       "asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike",
                       "asset_moto","asset_sewmach","asset_mobile"))

easq<- subset(easq,
              select=c("dataid","childid","tchild","clusterid",
                       "block","arm","svydate","month","sex","dob","agedays",
                       "ageyears","agegroup","fracode","res","care","resage",
                       "z_com","z_motor","z_personal","z_combined" ))  

easq<- rename(easq,
              replace=c("z_com"="com",
                        "z_motor"="motor",
                        "z_personal"="personal",
                        "z_combined"="combined"
              )) 
easq_adj<-NULL
easq_adj<-merge(enrol,easq,by="dataid")
group<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N")
# child age in days
cov_adj_1<-c("agedays")
cov_adj_2<-c("agedays","month","fracode","sex","momage","momedu","momheight","hfiacat","Nlt18","Ncomp","watmin","elec","floor","walls","roof","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_tv","asset_refrig","asset_bike","asset_moto","asset_sewmach","asset_mobile")

#----------------------------
#communication
#----------------------------
#column 1
com_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
com_col_2<-NULL
for(arm in group){
  com_col_2[arm]<-dformat(sum(!is.na(easq_adj[easq_adj$arm==arm,]$com)),0)
  
}
#column 3
com_col_3<-NULL
for(arm in group){
  ind<-easq_adj$arm==arm
  mean<-  dformat(mean(easq_adj[ind,]$com,na.rm =TRUE),2)
  SD<-  dformat(sd(easq_adj[ind,]$com,na.rm =TRUE),2)
  com_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
com_col_4<-NULL
com_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq_adj$com,tr=easq_adj$arm,pair=easq_adj$block,id=easq_adj$block,W=easq_adj[cov_adj_1],contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  com_col_4[arm]<-paste(dif," (",lcb,", ",ucb,")",sep="")  
}
#column 5
com_col_5<-NULL
com_col_5["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq_adj$com,tr=easq_adj$arm,pair=easq_adj$block,id=easq_adj$block,W=easq_adj[cov_adj_2],contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  com_col_5[arm]<-paste(dif," (",lcb,", ",ucb,")",sep="")  
}

#---------------------
#gross motor
#---------------------
#column 1
mot_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
mot_col_2<-NULL
for(arm in group){
  mot_col_2[arm]<-dformat(sum(!is.na(easq_adj[easq_adj$arm==arm,]$motor)),0)
  
}
#column 3
mot_col_3<-NULL
for(arm in group){
  ind<-easq_adj$arm==arm
  mean<-  dformat(mean(easq_adj[ind,]$motor,na.rm =TRUE),2)
  SD<-  dformat(sd(easq_adj[ind,]$motor,na.rm =TRUE),2)
  mot_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
mot_col_4<-NULL
mot_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq_adj$motor,tr=easq_adj$arm,pair=easq_adj$block,id=easq_adj$block,W=easq_adj[cov_adj_1],contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  mot_col_4[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="") 
}
#column 5
mot_col_5<-NULL
mot_col_5["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq_adj$motor,tr=easq_adj$arm,pair=easq_adj$block,id=easq_adj$block,W=easq_adj[cov_adj_2],contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  mot_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="") 
}

#--------------------------
#personal
#--------------------------
#column 1
per_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
per_col_2<-NULL
for(arm in group){
  per_col_2[arm]<-dformat(sum(!is.na(easq_adj[easq_adj$arm==arm,]$personal)),0)
  
}
#column 3
per_col_3<-NULL
for(arm in group){
  ind<-easq_adj$arm==arm
  mean<-  dformat(mean(easq_adj[ind,]$personal,na.rm =TRUE),2)
  SD<-  dformat(sd(easq_adj[ind,]$personal,na.rm =TRUE),2)
  per_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
per_col_4<-NULL
per_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq_adj$personal,tr=easq_adj$arm,pair=easq_adj$block,id=easq_adj$block,W=easq_adj[cov_adj_1],contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  per_col_4[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}
#column 5
per_col_5<-NULL
per_col_5["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq_adj$personal,tr=easq_adj$arm,pair=easq_adj$block,id=easq_adj$block,W=easq_adj[cov_adj_2],contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  per_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="") 
}

#----------------------
# Combined
#----------------------
#column 1
combined_col_1<-c(
  "\u005cMyIndent Control",
  "\u005cMyIndent Water",
  "\u005cMyIndent Sanitation",
  "\u005cMyIndent Handwashing",
  "\u005cMyIndent WSH",
  "\u005cMyIndent Nutrition",
  "\u005cMyIndent Nutrition+WSH"
)
#column 2
combined_col_2<-NULL
for(arm in group){
  combined_col_2[arm]<-dformat(sum(!is.na(easq_adj[easq_adj$arm==arm,]$combined)),0)
  
}
#column 3
combined_col_3<-NULL
for(arm in group){
  ind<-easq_adj$arm==arm
  mean<-  dformat(mean(easq_adj[ind,]$combined,na.rm =TRUE),2)
  SD<-  dformat(sd(easq_adj[ind,]$combined,na.rm =TRUE),2)
  combined_col_3[arm]<-  paste(mean," (",SD,")",sep="")
}
#column 4
combined_col_4<-NULL
combined_col_4["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq_adj$combined,tr=easq_adj$arm,pair=easq_adj$block,id=easq_adj$block,W=easq_adj[cov_adj_1],contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  combined_col_4[arm]<-  paste(dif," (",lcb,", ",ucb,")",sep="")
}
#column 5
combined_col_5<-NULL
combined_col_5["Control"]<-"Ref"
for(arm in group[2:7]){
  reg<- washb_glm(Y=easq_adj$combined,tr=easq_adj$arm,pair=easq_adj$block,id=easq_adj$block,W=easq_adj[cov_adj_2],contrast=c("Control",arm))
  dif<- dformat(reg$TR$Coef,2)
  lcb<- dformat(reg$TR$"2.5%",2) 
  ucb<- dformat(reg$TR$"97.5%",2)  
  combined_col_5[arm]<- paste(dif," (",lcb,", ",ucb,")",sep="") 
}

#---------------------------------------------------
# combining all the results into a single data frame
#---------------------------------------------------
com_df<-data.frame(a=com_col_1,b=com_col_2,c=com_col_3,d=com_col_4,e=com_col_5)
mot_df<- data.frame(a=mot_col_1,b=mot_col_2,c=mot_col_3,d=mot_col_4,e=mot_col_5)
per_df<-data.frame(a=per_col_1,b=per_col_2,c=per_col_3,d=per_col_4,e=per_col_5)
combined_df<-data.frame(a=combined_col_1,b=combined_col_2,c=combined_col_3,d=combined_col_4,e=combined_col_5)

#head_df<-data.frame(a=c("", "Outcome, Arm"),b=c("","N"),c=c("","Mean (SD)"),d=c("Mean Difference","vs. Control (95% CI)"),e=c("Mean Difference","vs. WSH (95% CI)"),f=c("Mean Difference","vs. Nutrition (95% CI)"))
com_glue<-data.frame(a=c("\u005ctextbf{Communication z score}"),b=c(""),c=c(""),d=c(""),e=c(""))
mot_glue<-data.frame(a=c("\u005ctextbf{Gross Motor z score}"),b=c(""),c=c(""),d=c(""),e=c(""))
per_glue<-data.frame(a=c("\u005ctextbf{Personal-social z score}"),b=c(""),c=c(""),d=c(""),e=c(""))
combined_glue<-data.frame(a=c("\u005ctextbf{Combined z score}"),b=c(""),c=c(""),d=c(""),e=c(""))

table_s3<-rbind(com_glue,com_df,mot_glue,mot_df,per_glue,per_df,combined_glue,combined_df)

save(table_s3,file="C:\\Users\\kdas\\Dropbox\\WASHB-cognitive-development-analysis\\results\\raw\\table-s3.Rdata")