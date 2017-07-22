plotDensity<-function(score,arm,block,color,pnum,contrast,ylab,xlab,rplot=T){
  require(washb)
  require(scales)
  # editing washb_ttest() for applying variable with missing values
  washb_ttest<-function (Y, tr, strat, contrast) 
  {
    ttdat <- data.frame(Y = Y[tr == contrast[1] | tr == contrast[2]], 
                        tr = tr[tr == contrast[1] | tr == contrast[2]], strat = strat[tr == 
                                                                                        contrast[1] | tr == contrast[2]])
    ttdat$tr <- factor(ttdat$tr, levels = contrast[1:2])
    blockmeans <- tapply(ttdat$Y, list(ttdat$strat, ttdat$tr), 
                         function(x) mean(x,na.rm=TRUE))
    t.est <- t.test(x = blockmeans[, 2], y = blockmeans[, 1], 
                    alternative = "two.sided", paired = TRUE, var.equal = FALSE, 
                    conf.level = 0.95)
    res <- c(t.est$estimate, t.est$conf.int[1], t.est$conf.int[2], 
             t.est$statistic, t.est$p.value)
    names(res) <- c("diff", "ci.lb", "ci.ub", "t-stat", "p")
    return(res)
  }
  ## desired format
  dformat<-function(x,d){
    x<-round(x,d)+0.000 # removing signed zero
    x<-formatC(x,format="f",digits = d)
    return(x)
  }
  
  # setting up plot dimension
  xmin<--1.8
  xmax<-1.8
  ymin<-0
  ymax<-.7
  # setting up gap between row and column
  #cexSize<-.3
  colDist<-.45
  rowDist<-.5
  # color settings
  fcol<-NULL
  fcol$r<-col2rgb(color[1])[1]
  fcol$g<-col2rgb(color[1])[2]
  fcol$b<-col2rgb(color[1])[3]
  scol<-NULL
  scol$r<-col2rgb(color[2])[1]
  scol$g<-col2rgb(color[2])[2]  
  scol$b<-col2rgb(color[2])[2]
  hfcol<-rgb(fcol$r/255,fcol$g/255,fcol$b/255)
  hscol<-rgb(scol$r/255,scol$g/255,scol$b/255)
  thfcol<-rgb(fcol$r/255,fcol$g/255,fcol$b/255,alpha=.5)
  thscol<-rgb(scol$r/255,scol$g/255,scol$b/255,alpha=.5)
  black<-rgb(0,0,0)
  # setting up graphical parameters: 
  #ps-point size, 
  #cex-character extension
  #mai- margin in inches
  #tck- length of tick
  #mgp- margin gape for axis title, labels, line
  par(ps = 6, 
      cex = 1, 
      cex.main = 1,
      cex.axis=1, 
      cex.lab=1, 
      cex.sub=1,
      mai=c(0.6,0.3,.4,0),
      tck=-0.02,
      mgp=c(.8,.3,0)
  )
  # a blank plot to set up the plot region, presence of axis label
  if(rplot){
    plot(c(xmin,xmax),c(ymin,ymax),type="n",axes=FALSE,bty="n",xlab="",ylab=ylab)
  }else{
    plot(c(xmin,xmax),c(ymin,ymax),type="n",axes=FALSE,bty="n",xlab="",ylab="")
  }
  # plot title
  mtext( pnum, side=3,  at=xmin-.6,adj=0,col="black",font=2,cex=2.2,line=2-.2)
  mtext( paste(contrast[2]," v. ",contrast[1]), side=3,adj=0,col="black", line=2-.2,cex=1.5)
  
  # table to print
  tab_1<-c("",contrast[1],contrast[2],"")
  #N_ref<-table(arm)[contrast[1]]
  #N_com<-table(arm)[contrast[2]]
  N_ref<-sum(!is.na(score[arm==contrast[1]]))
  N_com<-sum(!is.na(score[arm==contrast[2]]))
  
  tab_2<-c("N",N_ref,N_com,"")
  mean_ref<- dformat(mean(score[arm==contrast[1]], na.rm = TRUE),2)
  mean_com<- dformat(mean(score[arm==contrast[2]], na.rm = TRUE),2)
  tab_3<-c("Mean",mean_ref,mean_com,"")
  sd_ref<- dformat(sd(score[arm==contrast[1]], na.rm = TRUE),2)
  sd_com<- dformat(sd(score[arm==contrast[2]], na.rm = TRUE),2)
  tab_4<- c("SD",sd_ref,sd_com,"")
  ttest<-washb_ttest(Y=score,tr=arm,strat=block,contrast=contrast)
  reg <- washb_glm(Y=score,tr=arm,pair=block,id=block,contrast=contrast)
  
  diff_ci<- paste(dformat(reg$TR$Coef,2), " (",dformat(reg$TR$"2.5%",2),", ",dformat(reg$TR$"97.5%",2),")",sep="")
  diff_t<- paste("t-test p = ",sprintf("%.3f",ttest[5]),sep="")
  tab_5<- c("Diff. (95% CI)", "", diff_ci,diff_t)
  df<- data.frame(tab_1,tab_2,tab_3,tab_4,tab_5)
  df<-as.matrix(df)
  df_out<-capture.output(write.table(format(df, justify="right"),row.names=F,col.names=F,quote=F))
  col_1<-df[1,]
  col_2<-df[2,]
  col_3<-df[3,]
  col_4<-df[4,]
  mtext( col_1, side=3, line=2*rowDist,at=c(xmax-6*colDist,xmax-5*colDist,xmax-4*colDist,xmax-3*colDist,xmax),adj=1)
  mtext( col_2, side=3, line=rowDist,  at=c(xmax-6*colDist,xmax-5*colDist,xmax-4*colDist,xmax-3*colDist,xmax),adj=1,col=c(hfcol,hfcol,hfcol,hfcol,black))
  mtext( col_3, side=3, line=0,        at=c(xmax-6*colDist,xmax-5*colDist,xmax-4*colDist,xmax-3*colDist,xmax),adj=1,col=c(hscol,hscol,hscol,hscol,black))
  mtext( col_4, side=3, line=-rowDist, at=c(xmax-6*colDist,xmax-5*colDist,xmax-4*colDist,xmax-3*colDist,xmax),adj=1)
  #ploting axis
  axis(1,lwd =.4,mgp=c(-2,-.2,0),line=0)
  if(rplot){
    axis(2,col="white",col.tick="black",lwd =.4,las=1)
  }
  #plotting kernel density plot
  abline(h = 0, col = "gray60",lwd =.4)
  Y1<-score[arm==contrast[1]]
  Y2<-score[arm==contrast[2]]
  
  pmean_ref<- sprintf("%.2f",mean(Y1, na.rm = TRUE))
  pmean_com<- sprintf("%.2f",mean(Y2, na.rm = TRUE))
  #  score[arm==contrast[1]]
  #  score[arm==contrast[2]]
  points(density(Y1, na.rm = TRUE),lwd=1,main="",xlab="",type="l",lty="dashed",mgp=c(.7,.3,0),col=hfcol)
  points(density(Y2, na.rm = TRUE),type="l",lwd=1,main=NULL,mgp=c(.7,.3,0),col=hscol)
  points(as.numeric(pmean_ref),0-.015,pch=16,bg=alpha("gray60",.5),cex=.5,col=thfcol)
  points(as.numeric(pmean_com),0-.015,pch=16,bg=alpha("green",.5),cex=.5,col=thscol)
  segments(as.numeric(pmean_ref), 0, as.numeric(pmean_ref), .08, col= "gray60",lty="dashed",lwd=.4)
  segments(as.numeric(pmean_com), 0, as.numeric(pmean_com), .08, col= "gray60",lty="dashed",lwd=.4)
  text(mean(c(as.numeric(mean_com),as.numeric(mean_ref))),0.1,"Group Means",col="gray60",cex=.8)
  # plotting xlab in the righ place
  title(xlab=xlab,line=.5)
}
# End of function for kernel density plot