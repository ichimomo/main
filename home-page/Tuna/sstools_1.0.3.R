Beverton.holt <-
function(SSB,coef,st=TRUE,B0=60000){
  recruit <- 0
  if(st==FALSE){
    recruit <- coef[1]*SSB/(coef[2]+SSB)
  }
  else{
    coef.temp <- Beverton.holt.revised2(coef,B0=B0)
    #cat(coef.temp,"\n",coef,"\n")
    recruit <- coef.temp[1]*SSB/(coef.temp[2]+SSB)
  }
  recruit
}

Beverton.holt.ss2 <-
function(SSB,coef,B0=60000){
  4*coef[1]*coef[2]*SSB/(B0*(1-coef[1])+SSB*(5*coef[1]-1))
}

Lflattop <-
function(len,beta1,beta2,beta3,beta4,beta5,beta6){
  res <- rep(0,length(len))
  T1 <- min(len)+(1+exp(-beta1))^{-1}*(max(len)-min(len))
  T2 <- T1      +(1+exp(-beta2))^{-1}*(max(len)-T1)
  T3 <- (1+exp(-beta3))^{-1}
  T4 <- (1+exp(-beta4))^{-1}
  
  for(i in 1:length(len)){
    if(len[i]<T1){
      res[i] <- T3+((len[i]-min(len))/(T1-min(len)))^((1+beta5)*(1-T3))
    }
    else{
      if(len[i] <= T2){
        res[i] <- 1
      }
      else{
      res[i] <- 1+((len[i]-T2)/(max(len)-T2))^((1+beta6)*(T4-1))
      }}}
  list(res,T=c(T1,T2,T3,T4))
}

Llogistic <-
function(len,beta1,beta2){
  (1+exp(-log(19)*(len-beta1)/beta2))^{-1}
}

allplot.ss <-
function(repfile="Report.sso",outfile="out",is.plot.cohortSSB=TRUE,
                                      dev.type="pdf",len.residual.plot=FALSE,
                                      agecomp.on=FALSE,length.on=TRUE,aselect.on=F,select.on=TRUE,
                                      col.var=NULL,lty.var=NULL,
                                      lwd.var=NULL,true.value=NULL,
                                      repfile.legend=NULL,compfile="CompReport.sso",
                                      refpoint.year=NULL,  # If you want to calculate reference point, input years of reference F for selectivivty, such as 2004:2006.  This option needs optional functions distributed by Yukio Takeuchi (yukiot@fra.affrc.go.jp).  
                                      datfile=NULL#,plot.resid=F
                                      ){

  #!!!! Known bugs !!!!!
  # *** Not work if 1st option of F method (using Pope's approximation)
  #        * please tentatively replace 'F:' in my code with 'Hrate:' or 'Hrate:' with 'F:' in report file.  
  
  nline <- 0.5

  #-------------  error check if specified files exist ---
  tmp <- which(file.info(repfile)$size==0 | is.na(file.info(repfile)$size))
  if(length(tmp)>0){
    stop(message=paste("ERROR:: no file or no data in Repfile exits for ", repfile[tmp],".\n",sep=""))
  }

  if(length.on==TRUE){
    if(is.null(compfile) && is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
      stop(message="ERROR:: Please specify length composition file, named \"CompReport.SSO\" with argument of \"compfile=\"! for plotting length or age composition data with option of length.on=T")
    }
    if(is.ss3(repfile)){
      tmp <- which(file.info(compfile)$size==0 | is.na(file.info(compfile)$size))
      if(length(tmp)>0){
        stop(message=paste("ERROR:: no file or no data in CompReport exits for ", repfile[tmp],".\n",sep=""))
      }}
  }
  #------------- error check done

  if(is.list(repfile)){
    repfile <- unlist(repfile)
  }

  #------------ SET for ubnormal termination 
  tmpfunc <- function(){
    if(names(dev.cur())!="null device") dev.off()
#    par(org.par)
  }
  on.exit(tmpfunc())

  #------------ SET graphic parameters for multiplot
  if(is.null(lty.var)){
    if(length(repfile)>1) lty.var <- rep(1,length(repfile))
    else lty.var <- 1
  }
  if(is.null(col.var)){
    if(length(repfile)>1){
      col.var <- #c(1,good.colors())
        c(1:4,"#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3",
          "#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D",
          good.colors(),1:length(repfile))
    }
    else{
      col.var <- 1
    }
  }
  
  if(is.null(lwd.var)){
    if(length(repfile)>1) lwd.var <- rep(1,length(repfile))
    else lwd.var <- 1
  }

  if(!is.null(datfile)){
    fleet.name <- read.fleetname(datfile)
  }
  else{
    fleet.name <- NULL
  }

  if(dev.type=="ps") postscript(paste(outfile,".ps",sep=""),horizontal=FALSE,height=9,width=9)
  if(dev.type=="pdf") pdf(paste(outfile,".pdf",sep=""),height=11,width=9,paper="a4")
  if(dev.type=="x11") x11()

  #---------- SET graphical parameters
  org.par <- par()
  par(mgp=c(1.8,0.3,0),tck=0.03,font.main=1,cex.main=1,ps=11)
  multiplot <- length(repfile)>1

  #---------- READ report files
  cl0 <- count.fields(repfile[1],blank.lines.skip=FALSE)
  tb0 <- read.table(repfile[1],fill=TRUE,col.names=paste("V",1:max(cl0),sep=""),as.is=T,
                    blank.lines.skip=FALSE,colClasses=rep("character",max(cl0)))

  tmp <- getBabs.ss2(repfile[1],cl=cl0,tb=tb0)
  biom <- list(tmp[[1]])
  
  tmp <- getSPR.ss2(repfile[1],cl=cl0,tb=tb0)
  SPR <- list(tmp[[1]])

  tmp <- getCPUE.ss2(repfile[1],cl=cl0,tb=tb0)
  cpue <- list(tmp[[1]])

  tmp <- getNAA.ss2(repfile[1],cl=cl0,tb=tb0)
  naa <- list(tmp[[1]])

  tmp <- getCAA.ss2(repfile[1],cl=cl0,tb=tb0)
  caa <- as.list(numeric())
  caa[[1]] <- list(caa=tmp[[1]],caa.array=tmp[[3]])

  tmp <- getNMA.ss2(repfile[1],cl=cl0,tb=tb0)
  nma <- list(tmp[[1]])

  tmp <- getALK.ss2(repfile[1],cl=cl0,tb=tb0)
  alk <- list(tmp[[1]])

  tmp <- getSRfunc.ss2(repfile[1],cl=cl0,tb=tb0)
  SRfunc <- list(tmp[[1]])

  tmp <- getSRpara.ss2(repfile[1],cl=cl0,tb=tb0)
  SRpara <- list(tmp[[1]])  

  len.rep <- length(repfile)
  ptype <- "l"  
  if(multiplot){
    cl.tmp <- tb.tmp <- as.list(numeric())
    for(i in 2:len.rep){
      cl.tmp[[i]] <- count.fields(repfile[i],blank.lines.skip=FALSE)
      tb.tmp[[i]] <- read.table(repfile[i],fill=T,col.names=paste("V",1:max(cl.tmp[[i]]),sep=""),as.is=T,
                        blank.lines.skip=FALSE,colClasses=rep("character",max(cl.tmp[[i]])))[,1:2]
      biom[[i]] <- getBabs.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SPR[[i]] <- getSPR.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      cpue[[i]] <- getCPUE.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      alk[[i]] <- getALK.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      naa[[i]] <- getNAA.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      
      tmp <- getCAA.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])
      caa[[i]] <- list(caa=tmp[[1]],caa.array=tmp[[3]])
      
      nma[[i]] <- getNMA.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SRfunc[[i]] <- getSRfunc.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SRpara[[i]] <- getSRpara.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]      
      cat("Read",repfile[i],"file\n")


    }
  }
  else{
    tb.tmp <- list(tb0)
    cl.tmp <- list(cl0)    
  }
  #--------------- FINISH reading Report file

  ## Plot biomass, SSB and recruitment ##
  if(!multiplot){
    plotBSR(biom[[1]],NULL,
            true.value=true.value,what.plot=c(T,T,T),
          col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,nline=nline)
  }
  else{
    plotBSR(biom[[1]],biom[2:len.rep],
            true.value=true.value,what.plot=c(T,T,T),
          col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,nline=nline)    
  }

  if(is.null(repfile.legend)){
    legend("topright",legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)
  }
  else{
    legend("topright",legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)    
  }
  mtext(side=3,line=0.5,adj=0.1,"@ Biomass, SSB and Recruitment ",outer=T)
  cat("Plot of estimated biomass, SSB and Recruitments was done.\n")

  ## Show reference points (option)
  if(!is.null(refpoint.year)){
    par(mfrow=c(3,1))
    BRPs <-  try(calcBRP(repfile=repfile,year=refpoint.year[1]:refpoint.year[2],Fmed=T,tol=0.00001))
    if(class(BRPs)=="try-error"){
      cat("Sorry Reference points can't be calculated without optional functions of 'calcBRP'
           Skip this process..")      
    }
    else{
      par(xpd=T)
      if(is.matrix(BRPs)){
        b <- barplot(BRPs,col=1:nrow(BRPs),beside=T)
        for(i in 1:nrow(b)){
          text(b[i,],BRPs[i,],round(BRPs[i,],2),pos=4,srt=90)
        }
        nplot()
        legend("topright",fill=1:nrow(BRPs),legend=repfile,ncol=1)        
      }
      else{
        b <- barplot(BRPs)
        text(b,0.05,round(BRPs,2),pos=4,srt=90)
      }
      par(xpd=F)      
      write.csv(BRPs,file=paste2("BRPs",Sys.Date(),".csv"))
    }
  }
  else{
    BRPs <- NULL
  }
  
  ## Plot estimated (or fixed) growth curve -------------------------------------
  par(mfrow=c(3,1))
  a <- as.matrix(alk[[1]])
  x <- as.numeric(colnames(a)[-1])
  tmp <- a[,1]=="mean"
  tmp2 <- a[,1]=="sdsize"  
  plot(x,as.numeric(a[tmp,-1]),type="b",xlab="Age",ylab="Length (cm)",lwd=lwd.var[1])
  points(x,as.numeric(a[tmp,-1])-as.numeric(a[tmp2,-1]),type="l")
  points(x,as.numeric(a[tmp,-1])+as.numeric(a[tmp2,-1]),type="l")
  abline(h=seq(from=0,to=max(as.numeric(a[tmp,-1])),by=50),col="gray")
  title("Estimated growth curve (1st season in each age)",line=nline)
  if(multiplot){
    for(i in 2:len.rep){
      a <- as.matrix(alk[[i]])
      x <- as.numeric(colnames(a)[-1])
      tmp <- a[,1]=="mean"
      tmp2 <- a[,1]=="sdsize"        
      points(x,as.numeric(a[tmp,-1]),type="b",col=col.var[i],
             lty=lty.var[i],lwd=lwd.var[i])
      points(x,as.numeric(a[tmp,-1])-as.numeric(a[tmp2,-1]),type="l",
             col=col.var[i],lty=lty.var[i],lwd=lwd.var[i])
      points(x,as.numeric(a[tmp,-1])+as.numeric(a[tmp2,-1]),type="l",
             col=col.var[i],lty=lty.var[i],lwd=lwd.var[i])
      }}

  ## Plot SR curve 
  plotSRcurve(repfile=repfile[1],SRfunc=SRfunc[[1]],parameter=SRpara[[1]])
  if(multiplot){
    for(i in 2:len.rep){
      plotSRcurve(repfile=repfile[i],SRfunc=SRfunc[[i]],parameter=SRpara[[i]],
                  add=T,col.var=col.var[i])
    }
  }

  ##----------  Plot natural mortality --------------------
  x <- tapply(nma[[1]]$M,nma[[1]]$"Age_Mid",mean)
  plot(names(x),x,type="l",lwd=2,ylim=c(0,max(x)*1.1),xlab="Age",ylab="Natural mortality (/year)")
  if(multiplot){
    for(i in 2:len.rep){
      x <- tapply(nma[[i]]$M,nma[[i]]$"Age_Mid",mean)
      points(names(x),x,type="l",col=col.var[i],lwd=2)
    }
  }
  title("M at age",line=nline)  
  ##-------------------------------------------------------
  mtext(side=3,line=0.5,adj=0.1,"@ Growth curve, Spawner-recruitment and M ",outer=T)
  cat("Plot of growth curve was done.\n")  
  
  # legend
  if(multiplot){
#    nplot()
    if(is.null(repfile.legend)){
#      legend(1,10,legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)
      legend("topright",legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)      
    }
    else{
#      legend(1,10,legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)
      legend("topright",legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)      
    }
  }
  #---------------------------------------------------------------------------------
  
  ## Plot SSB by cohorts ##
  if(is.plot.cohortSSB==TRUE){
    par(mfcol=c(4,1))    
    for(i in 1:len.rep){
      plot.cohortSSB(repfile[i],naa=naa[[i]],nma=nma[[i]],title.text=paste("Numbers of spawners by year-class (",repfile[i],")"))
      par(las=1)
      if(i%%4==1) mtext(side=3,line=0.5,adj=0.1,"@ SSB by year-class",outer=T)    
    }
    if(i<4) mtext(side=3,line=0.5,adj=0.1,"@ SSB by year-class",outer=T)    
    cat("Plot of numbers by year class was done.\n")
  }
  
  
  ## Plot F and exploitation rates##
   par(mfrow=c(3,1),mar=c(4,6,3,0),oma=c(0,0,3,3))

  if(!multiplot){
    plot.data.frame(SPR[[1]],NULL,"Tot_Exploit",title="Total exploitation rates",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],NULL,"SPR",title="SPBfished/SPBzero",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],NULL,"Y/R",title="Y/R",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
  }
  else{
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"Tot_Exploit",title="Total exploitation rates",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"SPR",title="SPBfished/SPBzero",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"Y/R",title="Y/R",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)    
  }
  mtext(side=3,line=0.5,adj=0.1,"@ Total exploitation rates, SPR and Y/R",outer=T)      

  ## Plot F and exploitation rates (2): calculated from numbers and catch at age ##
  ## Read faa here
  ##---- !!!!!! imcomplete for SS3 !!!!!!!!!!!!!!!! --------------------
  par(mfrow=c(3,1),mar=c(3,3,2,1))  
  faa <- list(calFAA.ss(repfile=NULL,
                         datas=list(naa=naa[[1]],caa=caa[[1]],nma=nma[[1]],biom=biom[[1]]),is.plot=T))
  x <- rowtapply(apply(faa[[1]]$faa.array,c(1,2),sum))
  matplot(rownames(x),x,type="b",ylab="F",xlab="Year")
#  title(#side=3,line=0.5,adj=0.1,
#        paste2(" Average F at age (",repfile[1],") "))
#  mtext(side=3,line=0.5,adj=0.1,"@ F at age by year",outer=T)
  mtext(side=3,line=0.5,adj=0.1,
        paste2("F at age by year(",repfile[1],") "),outer=T)  
  
  if(multiplot){
    for(i in 2:len.rep){
      faa[[i]] <- calFAA.ss(repfile=NULL,
                             datas=list(naa=naa[[i]],caa=caa[[i]],nma=nma[[i]],biom=biom[[i]]),
                             is.plot=T)
      matplot(rowtapply(apply(faa[[i]]$faa.array,c(1,2),sum)),type="b",ylab="F",xlab="Year")      
      mtext(side=3,line=0.5,adj=0.1,
            paste2("F at age by year(",repfile[i],") "),outer=T)
#      mtext(side=3,line=0.5,adj=0.1,"@ F at age by year",outer=T)      
    }
  }

  # Fishing impacts by year
  nfleet <- dim(faa[[1]]$faa.array)[[3]]
  setncol(nfleet)
  nplot()
  #  browser()
  legend("topleft",legend=paste(1:nfleet,fleet.name,sep=":"),
         pch=1:nfleet,ncol=2,bty="n")
  years <- as.numeric(rownames(faa[[1]]$faa.array[,,1]))
  for(i in 1:dim(faa[[1]]$faa.array)[[2]]){
    x0 <- 0
    for(fleet in 1:nfleet){
      x <- tapply(faa[[1]]$faa.array[,i,fleet],floor(as.numeric(rownames(faa[[1]]$faa.array[,,fleet]))),sum)
      x0 <- max(x,x0,na.rm=T)
    }

    plot(names(x),rep(0,length(x)),ylim=c(0,x0),type="n",ylab="Continuous F",xlab="Year")
    title(paste("Age",dimnames(faa[[1]]$faa.array)[[2]][i]))
    for(fleet in 1:nfleet){
      tmp <- tapply(faa[[1]]$faa.array[,i,fleet],floor(as.numeric(rownames(faa[[1]]$faa.array[,,1]))),sum)
      if(sum(tmp,na.rm=T)>0){
        points(names(tmp),tmp,pch=fleet,type="b",cex=0.7)

        # multiplot
        if(multiplot==TRUE){
          for(k in 2:len.rep){
            tmp <- tapply(faa[[k]]$faa.array[,i,fleet],
                          floor(as.numeric(rownames(faa[[k]]$faa.array[,,1]))),sum)
            if(sum(tmp,na.rm=T)>0){
              points(names(tmp),tmp,pch=fleet,type="b",cex=0.7,col=col.var[k])
          }}
        }
        
      }
    }
    if(i%%10==9)   mtext(side=3,line=0.5,adj=0.1,"@ F by fleet, age and year ",outer=T)
  }

  cat("Plot of estimated F and exploitation rates was done.\n")
  ##-------------------------------------------------------------------------------  

  ## Plot total catch in weight##
  char.tmp <- numeric()
  for(i in 1:len.rep){
    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"retain(B)","ret_catch")
  }
  if(!multiplot){
    plotTotcatch(biom[[1]],NULL,
                 findseq=char.tmp,titlename="@ Total catch in weight (gray: observed, black: expected)",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=TRUE)
  }
  else{
    plotTotcatch(biom[[1]],biom[2:len.rep],
                 findseq=char.tmp,titlename="@ Total catch in weight (gray: observed, black: expected)",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=TRUE)    
  }
  cat("Plot of estimated total catches (in weight) was done.  \n")

  # total catch by number
  char.tmp <- numeric()
  for(i in 1:len.rep){
    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"retain(N)","ret_catch")
  }
  if(!multiplot){
    dummy.var <- NULL
  }
  else{
    dummy.var <- biom[2:len.rep]
  }
  plotTotcatch(biom[[1]],dummy.var,#NULL,
               findseq=char.tmp,titlename="@ Total catch in number (gray: observed, black: expected)",
               col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
               plot.obscatch=TRUE)
#  }
#  else{
#    plotTotcatch(biom[[1]],biom[2:len.rep],
#                 findseq=char.tmp,titlename="Total catch in number (gray: observed, black: expected)",
#                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
#                 plot.obscatch=T)    
#  }
  cat("Plot of estimated total catches (in number) was done.  \n")  

  ## Plot Exploitation rates by fleet  (average by season)##
  #  This find seq is depending on the option of F method.  
  #  for(i in 1:len.rep){  
  #    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"F:","Hrate")
  #  }
  if(!multiplot){
    plotTotcatch(biom[[1]],NULL,byyear=FALSE,
                 ylab="Exploitation rates or F",findseq=c("F:","Hrate"),FUN=mean,
                 titlename="@ Exploitation rates or F",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=F)
  }
  else{
    plotTotcatch(biom[[1]],biom[2:len.rep],byyear=FALSE,
                 ylab="Exploitation rates",findseq=c("F:","Hrate"),FUN=mean,
                 titlename="@ Exploitation rates",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=F)    
  }
#  mtext(side=3,line=0.5,adj=0.1,"Exploitation rates",outer=T)  
  cat("Plot of estimated exploitation rates was done.  \n")

  ### CPUE
  fleetn <- unique(cpue[[1]]$index)  
#  par(mfrow=c(length(fleetn),1),ps=16)

#  makedevice(filename="CPUE_fit",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#             new=T,append=T)
  nfile <- 1  
  setncol(length(fleetn))
  for(i in fleetn){
    cpue0 <- cpue[[1]]
    matplot(x <- cpue0$year[cpue0$index==i],
            y <- cbind(cpue0$obs[cpue0$index==i],cpue0$exp[cpue0$index==i]),
            pch=c(1,NA),col=c(1,1),type=c("b","l"),
            xlab="Year",ylab="CPUE",lty=c(1,1),lwd=lwd.var[1])
    title(paste("Fleet",i,":",fleet.name[i]),line=nline)    
#    legend(max(x),max(y),xjust=1,yjust=1,legend=c("Expected","Observed"),col=c(1,1),pch=c(NA,1),lty=c(1,1))

    ## For overlapped plots
    if(multiplot){
      for(j in 2:len.rep){
        cpue0 <- cpue[[j]]
        points(cpue0$year[cpue0$index==i],
               cpue0$exp[cpue0$index==i],type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
      }}
    ##
    
    if(length(fleetn)>10 && i%%10==0){
      mtext(side=3,line=nline,adj=0.1,"@ CPUE (line: expected, line+circle: observed)",outer=T)
      nfile <- nfile+1
#      makedevice(filename="CPUE_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                 new=T,append=T)
#      setncol(length(fleetn))
    }
  }
  
#  mtext(side=3,line=2,adj=0.3,"CPUE",outer=T)
  cat("Plot of CPUE (obs vs. est) was done.  \n")  

  # Selectivity
  if(select.on==TRUE){
#    makedevice(filename="Select",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#               new=T,append=T)
    nfile <- 1
    tmp <- getSelect.ss2(repfile[1],cl=cl0,tb=tb0)
    selects <- list(tmp[[1]])
    selects.target <- tmp[[2]]      

    if(multiplot){
      #    select.list <- as.list(rep(0,len.rep))    
      for(i in 2:len.rep){
        selects[[i]] <- getSelect.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      }
    }

    plotSelect(selects,multiplot=multiplot,ptype=ptype,col=col.var,lty=lty.var,lwd=lwd.var,nline=nline)
    cat("Plot of length selectivity was done.  \n")
  }

  # Age Selectivity
  if(aselect.on==TRUE){
#    makedevice(filename="ASelect",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#               new=T,append=T)
    nfile <- 1
    
    tmp <- getSelect.ss2(repfile[1],cl=cl0,tb=tb0,len=F)
    aselects <- list(tmp[[1]])
    aselects.target <- tmp[[2]]
    
    if(multiplot){
      for(i in 2:len.rep){
        aselects[[i]] <-
          getSelect.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])
      }
    }

#    par(oma=c(2,2,2,0))
    plotSelect(aselects,multiplot=multiplot,ptype=ptype,col=col.var,lty=lty.var,lwd=lwd.var,nline=nline)
    cat("Plot of age selectivity was done.  \n")
  }


  #-------- PLOT likelihodd if multiplot=T
  if(multiplot==TRUE){
    plotLL.ss(repfile,repfile.legend=repfile.legend)
    cat("Plot of likelihood was done.  \n")    
  }

  #!!!!!!!-------- if SS3, use function of plotlength.fit (preliminary setting)
  ## To show observed and expected size frequency
  if(length.on==TRUE | agecomp.on==TRUE){
    #--- if SS3 -> go to modified function
    if(is.ss3(repfile[1])){
      tb.tmp[[1]] <- tb0
      cl.tmp[[1]] <- cl0      
      plotlength.fit(repfile,compfile=compfile,tb=tb.tmp,cl=cl.tmp,len=!agecomp.on,
                     len.residual.plot=len.residual.plot)
      sum.length <- NA
    }
    else{  # reserve the program for SS2
      nfile <- 1
      tmp <- getAgecomp.ss2(repfile[1],cl=cl0,tb=tb0,len=!agecomp.on)
      comps <- list(list(tmp[[1]],tmp[[2]]))
      comps.target <- tmp[[3]]
      if(multiplot){
        #      b.list <- as.list(rep(0,len.rep))
        for(i in 2:len.rep){
          comps[[i]] <-
            getAgecomp.ss2(repfile[i],len=!agecomp.on)
        }
      }

      len.data <- comps[[1]][[1]]    
      fleet.row <- sort(unique(comps[[1]][[1]]$fleet))
      #  setncol(nfleet)
      setncol(length(fleet.row))

    # Pearson residual of length data:
    # !!!!!!!! Caution: when size data is too huge, this plot make the result file too heavy!!!!!!!!
      if(len.residual.plot==TRUE){
        browser()
        s <- 1
        for(i in fleet.row){
          tmp <- len.data[len.data$fleet==i,]
          plot(tmp$bin,y <- tmp$Pearson,ylim=c(-3,6),xlab="Length",ylab="Pearson residuals")
          title(main=paste("Fleet",i,":",fleet.name[i]),line=-1)
      #    sd.tmp <- (tmp$obs-tmp$exp)/sqrt(tmp$exp*(1-tmp$exp)*tmp$N)
      #    plot(tmp$bin,sd.tmp,
          if(!multiplot){
            x1 <- tapply(y,tmp$bin,median)
            x2 <- tapply(y,tmp$bin,mean)  
            abline(h=0,col="yellow")
            points(names(x1),x1,type="l",col="red")
            points(names(x2),x2,type="l",col="blue")
          }
          else{
            for(j in 2:len.rep){
              tmp <- comps[[j]][[1]][comps[[j]][[1]]$fleet==i,]
              points(tmp$bin,y <- tmp$Pearson,col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])          
            }
          }
        }
      }

      ###!!!  Not for USE now 
    if(0){
      for(i in fleet.row){
        plot(x <- len.data$exp[len.data$fleet==i & !is.na(len.data$obs)],y <- len.data$obs[len.data$fleet==i & !is.na(len.data$obs)],lwd=lwd.var[1],
             ylim=c(0,max(x,y)),xlim=c(0,max(x,y)),xlab="Expectet size freq",ylab="Observed size freq")
        title(main=paste("fleet ",i),line=nline)

        if(multiplot){
          for(j in 2:len.rep){
            tmp <- comps[[j]][[1]]
            points(x <- tmp$exp[tmp$fleet==i & !is.na(tmp$obs)],
                   y <- tmp$obs[tmp$fleet==i & !is.na(tmp$obs)]
                   ,type="p",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
          }}
        if(length(fleet.row)>10 && s%%10==0){
          mtext(side=3,line=0.5,adj=0.3,"Expected vs observed size composition",outer=T)
#          nfile <- nfile+1
#          makedevice(filename="length_cor",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                     new=T,append=T)
#          setncol(length(fleet.row))
        }
        s <- s+1
      }
      mtext(side=3,line=0.5,adj=0.3,"Expected vs observed size composition",outer=T)
      cat("Scatter plot of length frequency (obs vs est) was done.  \n")
    }
    

  ## Size frequency 2
  ##!!! TOO bad algrosm, which should be shoreter in future
    length.bin <- sort(unique(len.data$bin))
    sum.length <- list(array(0,dim=c(length(length.bin),length(fleet.row),2)))
    dimnames(sum.length[[1]]) <- list(length.bin,fleet.row,c("Obs","Exp"))
    
    if(multiplot){
#      sum.length <- as.list(rep(0,len.rep))
      for(i in 2:len.rep){
        sum.length[[i]] <- sum.length[[1]]
      }}
    
#    nfile <- 1
#    makedevice(filename="length_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=T,append=T,width=700)  
#    par(mfrow=c(5,5),mar=c(1.7,1.7,1.4,0.5),oma=c(0,0,3,3),ps=14)
    
#   browser()
    
    s <- 1
    row.name <- paste(floor(as.numeric(len.data$year)),len.data$season,len.data$fleet,sep="-")
    tmp2 <- unique(row.name)
    tmp.ref <- paste(floor(as.numeric(comps[[1]][[2]]$Year)),comps[[1]][[2]]$Seas,comps[[1]][[2]]$Index,sep="-")
    for(j in 1:length(fleet.row)){
      for(i in 1:length(tmp2)){
        if(sum(len.data$fleet[row.name==tmp2[i]]==fleet.row[j])){
          matplot(x <- len.data$bin[row.name==tmp2[i]],
                  y <- cbind(len.data$obs[row.name==tmp2[i]],
                             len.data$exp[row.name==tmp2[i]])*comps[[1]][[2]]$Nsamp[tmp.ref==tmp2[i]],
                  lwd=lwd.var[1],col=c("royalblue",1),type=c("b","l"),
                  pch=1:2,lty=1:1,cex=0.7,ylab="",xlab="")
        ## Sum up size data by fisheries
          y <- y[!is.na(y[,1]),]
          x <- x[!is.na(x)]
          sum.length[[1]][match(x,length.bin),j,] <- sum.length[[1]][match(x,length.bin),j,]+y
          ##
          title(main=paste(tmp2[i]),line=0.5)

          ## For multiple plots ##
          if(multiplot){
            for(k in 2:len.rep){
              b.tmp <- comps[[k]]
              row.name <- paste(floor(as.numeric(b.tmp[[1]]$year)),b.tmp[[1]]$season,b.tmp[[1]]$fleet,sep="-")
              tmp2 <- unique(row.name)
              tmp.ref <- paste(floor(as.numeric(b.tmp[[2]]$Year)),b.tmp[[2]]$Seas,b.tmp[[2]]$Index,sep="-")              
#              row.name2 <- paste(floor(as.numeric(b.tmp[[1]]$year)),b.tmp[[1]]$season,b.tmp[[1]]$fleet,sep="-")
#              tmp.ref2 <- paste(floor(as.numeric(b.tmp[[2]]$Year)),b.tmp[[2]]$Seas,b.tmp[[2]]$Index,sep="-")

              x <- b.tmp[[1]]$bin[row.name2==tmp2[i]]
              y <- cbind(b.tmp[[1]]$obs[row.name2==tmp2[i]],
                         b.tmp[[1]]$exp[row.name2==tmp2[i]]) * b.tmp[[2]]$Nsamp[tmp.ref2==tmp2[i]]
              points(x,y[,2],col=col.var[k],lty=lty.var[k],lwd=lwd.var[k],type="l")
              y <- y[!is.na(y[,1]),]
              x <- x[!is.na(x)]
              sum.length[[k]][match(x,length.bin),j,] <- sum.length[[k]][match(x,length.bin),j,]+y            
            }}
        ##
          
          if(s%%25==0){
            mtext(side=3,line=0.5,adj=0.3,
                  "Length fit (by each sampling, line: expected, line+circle: observed)",outer=T)
#            nfile <- nfile+1
#            makedevice(filename="length_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                       new=T,append=T,width=700)
#            par(mfrow=c(5,5),mar=c(1.7,1.7,1.4,0.5),oma=c(0,0,3,3),ps=14)#mgp=c(2,1,0),ps=14)   
          }
          s <- s+1                
        }}}
    
  cat("Plot of expected and observed length frequency by each observation was done.  \n")
#  par(mfrow=c(length(fleet.row),1))
    nfile <- 1
#    makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=T,append=T)    
      n <- dim(sum.length[[1]])[[2]]
    setncol(n)
    for(i in 1:n){
      matplot(x <- length.bin,y <- sum.length[[1]][,i,],type=c("b","l"),lty=1,
              pch=1,ylab="nsmple",col=c("black","royalblue"),lwd=lwd.var[1],xlab="Length (cm)")
      title(paste("Fleet",fleet.row[i],":",fleet.name[fleet.row[i]]),line=nline)

      if(multiplot){
        for(k in 2:len.rep){
          points(length.bin,sum.length[[k]][,i,2],
                 type="l",col=col.var[k+1],lty=lty.var[k],lwd=lwd.var[k])
          }}      
      
      if(n>10 && i%%10==0){
        mtext(side=3,line=0.5,adj=0.3,
              "Length fit (by fleet, line: expected, line+circle: observed)",outer=T)
#        nfile <- nfile+1
#        makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                   new=T,append=T)                
#        setncol(n)
      }}
    mtext(side=3,line=0.5,adj=0.3,"Length fit (by fleet, line: expected, line+circle: observed))",
          outer=T)
    cat("Plot of expected and observed length frequency by fleets was done.  \n")    

    # Bubble plot not for multiplot
    xrange <- range(len.data$year)
    yrange <- range(len.data$bin,na.rm=T)    
    max.res.fleet <- tapply(abs(len.data$Pearson),len.data$fleet,max,na.rm=T)
    par(mfrow=c(3,1))
    col.tmp <- c("black","black")
    col.tmp2 <- c(NA,"black")    
    for(j in 1:length(fleet.row)){
      with(len.data[len.data$fleet==j,],symbols(year,bin,circles=sqrt(abs(x <- Pearson))/8,
                          fg=col.tmp[(x<0)+1],bg=col.tmp2[(x<0)+1],lwd=0.5,
                          inches=FALSE,ylim=yrange,xlim=xrange))
#                          inches=max.res.fleet[names(max.res.fleet)==fleet.row[j]]/100))
      title(paste("Fleet",j,":",fleet.name[j]),line=nline)
      if(j%%3==0) mtext("Bubble plot of Pearson residuals, black(obs<exp), black (obs>exp)",outer=T)
    }
    }
  }
  if(names(dev.cur())!="null device") dev.off()

  if(dev.type!="x11"){
    cat("All plots were finished.  Open the file named ",paste(outfile,".",dev.type,sep=""),".  \n")
  }
  else{
    cat("All plots were finished.")
  }

#  par(org.par)
  invisible(list(biom=biom,SPR=SPR,naa=naa,faa=faa,cpue=cpue,
                 nma=nma,alk=alk,repfile=repfile,caa=caa,BRP=BRPs,
                 sum.length=ifelse(length.on==T,sum.length,NA)))
}

allplot.ss2 <-
function(repfile="Report.sso",outfile="out",is.plot.cohortSSB=TRUE,
                                      dev.type="pdf",len.residual.plot=FALSE,
                                      agecomp.on=FALSE,length.on=TRUE,aselect.on=F,select.on=TRUE,
                                      col.var=NULL,lty.var=NULL,
                                      lwd.var=NULL,true.value=NULL,
                                      repfile.legend=NULL,compfile="CompReport.sso",
                                      refpoint.year=NULL,  # If you want to calculate reference point, input years of reference F for selectivivty, such as 2004:2006.  This option needs optional functions distributed by Yukio Takeuchi (yukiot@fra.affrc.go.jp).  
                                      datfile=NULL#,plot.resid=F
                                      ){

  #!!!! Known bugs !!!!!
  # *** Not work if 1st option of F method (using Pope's approximation)
  #        * please tentatively replace 'F:' in my code with 'Hrate:' or 'Hrate:' with 'F:' in report file.  
  
  nline <- 0.5

  #-------------  error check if specified files exist ---
  tmp <- which(file.info(repfile)$size==0 | is.na(file.info(repfile)$size))
  if(length(tmp)>0){
    stop(message=paste("ERROR:: no file or no data in Repfile exits for ", repfile[tmp],".\n",sep=""))
  }

  if(length.on==TRUE){
    if(is.null(compfile) && is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
      stop(message="ERROR:: Please specify length composition file, named \"CompReport.SSO\" with argument of \"compfile=\"! for plotting length or age composition data with option of length.on=T")
    }
    if(is.ss3(repfile)){
      tmp <- which(file.info(compfile)$size==0 | is.na(file.info(compfile)$size))
      if(length(tmp)>0){
        stop(message=paste("ERROR:: no file or no data in CompReport exits for ", repfile[tmp],".\n",sep=""))
      }}
  }
  #------------- error check done

  if(is.list(repfile)){
    repfile <- unlist(repfile)
  }

  #------------ SET for ubnormal termination 
  tmpfunc <- function(){
    if(names(dev.cur())!="null device") dev.off()
#    par(org.par)
  }
  on.exit(tmpfunc())

  #------------ SET graphic parameters for multiplot
  if(is.null(lty.var)){
    if(length(repfile)>1) lty.var <- rep(1,length(repfile))
    else lty.var <- 1
  }
  if(is.null(col.var)){
    if(length(repfile)>1){
      col.var <- #c(1,good.colors())
        c(1:4,"#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3",
          "#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D",
          good.colors(),1:length(repfile))
    }
    else{
      col.var <- 1
    }
  }
  
  if(is.null(lwd.var)){
    if(length(repfile)>1) lwd.var <- rep(1,length(repfile))
    else lwd.var <- 1
  }

  if(!is.null(datfile)){
    fleet.name <- read.fleetname(datfile)
  }
  else{
    fleet.name <- NULL
  }

  if(dev.type=="ps") postscript(paste(outfile,".ps",sep=""),horizontal=FALSE,height=9,width=9)
  if(dev.type=="pdf") pdf(paste(outfile,".pdf",sep=""),height=11,width=9,paper="a4")
  if(dev.type=="x11") x11()

  #---------- SET graphical parameters
  org.par <- par()
  par(mgp=c(1.8,0.3,0),tck=0.03,font.main=1,cex.main=1,ps=11)
  multiplot <- length(repfile)>1

  #---------- READ report files
  cl0 <- count.fields(repfile[1],blank.lines.skip=FALSE)
  tb0 <- read.table(repfile[1],fill=TRUE,col.names=paste("V",1:max(cl0),sep=""),as.is=T,
                    blank.lines.skip=FALSE,colClasses=rep("character",max(cl0)))

  tmp <- getBabs.ss2(repfile[1],cl=cl0,tb=tb0)
  biom <- list(tmp[[1]])
  
  tmp <- getSPR.ss2(repfile[1],cl=cl0,tb=tb0)
  SPR <- list(tmp[[1]])

  tmp <- getCPUE.ss2(repfile[1],cl=cl0,tb=tb0)
  cpue <- list(tmp[[1]])

  tmp <- getNAA.ss2(repfile[1],cl=cl0,tb=tb0)
  naa <- list(tmp[[1]])

  tmp <- getCAA.ss2(repfile[1],cl=cl0,tb=tb0)
  caa <- as.list(numeric())
  caa[[1]] <- list(caa=tmp[[1]],caa.array=tmp[[3]])

  tmp <- getNMA.ss2(repfile[1],cl=cl0,tb=tb0)
  nma <- list(tmp[[1]])

  tmp <- getALK.ss2(repfile[1],cl=cl0,tb=tb0)
  alk <- list(tmp[[1]])

  tmp <- getSRfunc.ss2(repfile[1],cl=cl0,tb=tb0)
  SRfunc <- list(tmp[[1]])

  tmp <- getSRpara.ss2(repfile[1],cl=cl0,tb=tb0)
  SRpara <- list(tmp[[1]])  

  len.rep <- length(repfile)
  ptype <- "l"  
  if(multiplot){
    cl.tmp <- tb.tmp <- as.list(numeric())
    for(i in 2:len.rep){
      cl.tmp[[i]] <- count.fields(repfile[i],blank.lines.skip=FALSE)
      tb.tmp[[i]] <- read.table(repfile[i],fill=T,col.names=paste("V",1:max(cl.tmp[[i]]),sep=""),as.is=T,
                        blank.lines.skip=FALSE,colClasses=rep("character",max(cl.tmp[[i]])))[,1:2]
      biom[[i]] <- getBabs.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SPR[[i]] <- getSPR.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      cpue[[i]] <- getCPUE.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      alk[[i]] <- getALK.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      naa[[i]] <- getNAA.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      
      tmp <- getCAA.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])
      caa[[i]] <- list(caa=tmp[[1]],caa.array=tmp[[3]])
      
      nma[[i]] <- getNMA.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SRfunc[[i]] <- getSRfunc.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SRpara[[i]] <- getSRpara.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]      
      cat("Read",repfile[i],"file\n")


    }
  }
  else{
    tb.tmp <- list(tb0)
    cl.tmp <- list(cl0)    
  }
  #--------------- FINISH reading Report file

  ## Plot biomass, SSB and recruitment ##
  if(!multiplot){
    plotBSR(biom[[1]],NULL,
            true.value=true.value,what.plot=c(T,T,T),
          col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,nline=nline)
  }
  else{
    plotBSR(biom[[1]],biom[2:len.rep],
            true.value=true.value,what.plot=c(T,T,T),
          col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,nline=nline)    
  }

  if(is.null(repfile.legend)){
    legend("topright",legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)
  }
  else{
    legend("topright",legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)    
  }
  mtext(side=3,line=0.5,adj=0.1,"@ Biomass, SSB and Recruitment ",outer=T)
  cat("Plot of estimated biomass, SSB and Recruitments was done.\n")

  ## Show reference points (option)
  if(!is.null(refpoint.year)){
    par(mfrow=c(3,1))
    BRPs <-  try(calcBRP(repfile=repfile,year=refpoint.year[1]:refpoint.year[2],Fmed=T,tol=0.00001))
    if(class(BRPs)=="try-error"){
      cat("Sorry Reference points can't be calculated without optional functions of 'calcBRP'
           Skip this process..")      
    }
    else{
      par(xpd=T)
      if(is.matrix(BRPs)){
        b <- barplot(BRPs,col=1:nrow(BRPs),beside=T)
        for(i in 1:nrow(b)){
          text(b[i,],BRPs[i,],round(BRPs[i,],2),pos=4,srt=90)
        }
        nplot()
        legend("topright",fill=1:nrow(BRPs),legend=repfile,ncol=1)        
      }
      else{
        b <- barplot(BRPs)
        text(b,0.05,round(BRPs,2),pos=4,srt=90)
      }
      par(xpd=F)      
      write.csv(BRPs,file=paste2("BRPs",Sys.Date(),".csv"))
    }
  }
  else{
    BRPs <- NULL
  }
  
  ## Plot estimated (or fixed) growth curve -------------------------------------
  par(mfrow=c(3,1))
  a <- as.matrix(alk[[1]])
  x <- as.numeric(colnames(a)[-1])
  tmp <- a[,1]=="mean"
  tmp2 <- a[,1]=="sdsize"  
  plot(x,as.numeric(a[tmp,-1]),type="b",xlab="Age",ylab="Length (cm)",lwd=lwd.var[1])
  points(x,as.numeric(a[tmp,-1])-as.numeric(a[tmp2,-1]),type="l")
  points(x,as.numeric(a[tmp,-1])+as.numeric(a[tmp2,-1]),type="l")
  abline(h=seq(from=0,to=max(as.numeric(a[tmp,-1])),by=50),col="gray")
  title("Estimated growth curve (1st season in each age)",line=nline)
  if(multiplot){
    for(i in 2:len.rep){
      a <- as.matrix(alk[[i]])
      x <- as.numeric(colnames(a)[-1])
      tmp <- a[,1]=="mean"
      tmp2 <- a[,1]=="sdsize"        
      points(x,as.numeric(a[tmp,-1]),type="b",col=col.var[i],
             lty=lty.var[i],lwd=lwd.var[i])
      points(x,as.numeric(a[tmp,-1])-as.numeric(a[tmp2,-1]),type="l",
             col=col.var[i],lty=lty.var[i],lwd=lwd.var[i])
      points(x,as.numeric(a[tmp,-1])+as.numeric(a[tmp2,-1]),type="l",
             col=col.var[i],lty=lty.var[i],lwd=lwd.var[i])
      }}

  ## Plot SR curve 
  plotSRcurve(repfile=repfile[1],SRfunc=SRfunc[[1]],parameter=SRpara[[1]])
  if(multiplot){
    for(i in 2:len.rep){
      plotSRcurve(repfile=repfile[i],SRfunc=SRfunc[[i]],parameter=SRpara[[i]],
                  add=T,col.var=col.var[i])
    }
  }

  ##----------  Plot natural mortality --------------------
  x <- tapply(nma[[1]]$M,nma[[1]]$"Age_Mid",mean)
  plot(names(x),x,type="l",lwd=2,ylim=c(0,max(x)*1.1),xlab="Age",ylab="Natural mortality (/year)")
  if(multiplot){
    for(i in 2:len.rep){
      x <- tapply(nma[[i]]$M,nma[[i]]$"Age_Mid",mean)
      points(names(x),x,type="l",col=col.var[i],lwd=2)
    }
  }
  title("M at age",line=nline)  
  ##-------------------------------------------------------
  mtext(side=3,line=0.5,adj=0.1,"@ Growth curve, Spawner-recruitment and M ",outer=T)
  cat("Plot of growth curve was done.\n")  
  
  # legend
  if(multiplot){
#    nplot()
    if(is.null(repfile.legend)){
#      legend(1,10,legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)
      legend("topright",legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)      
    }
    else{
#      legend(1,10,legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)
      legend("topright",legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)      
    }
  }
  #---------------------------------------------------------------------------------
  
  ## Plot SSB by cohorts ##
  if(is.plot.cohortSSB==TRUE){
    par(mfcol=c(4,1))    
    for(i in 1:len.rep){
      plot.cohortSSB(repfile[i],naa=naa[[i]],nma=nma[[i]],title.text=paste("Numbers of spawners by year-class (",repfile[i],")"))
      par(las=1)
      if(i%%4==1) mtext(side=3,line=0.5,adj=0.1,"@ SSB by year-class",outer=T)    
    }
    if(i<4) mtext(side=3,line=0.5,adj=0.1,"@ SSB by year-class",outer=T)    
    cat("Plot of numbers by year class was done.\n")
  }
  
  
  ## Plot F and exploitation rates##
   par(mfrow=c(3,1),mar=c(4,6,3,0),oma=c(0,0,3,3))

  if(!multiplot){
    plot.data.frame(SPR[[1]],NULL,"Tot_Exploit",title="Total exploitation rates",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],NULL,"SPR",title="SPBfished/SPBzero",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],NULL,"Y/R",title="Y/R",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
  }
  else{
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"Tot_Exploit",title="Total exploitation rates",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"SPR",title="SPBfished/SPBzero",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"Y/R",title="Y/R",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)    
  }
  mtext(side=3,line=0.5,adj=0.1,"@ Total exploitation rates, SPR and Y/R",outer=T)      

  ## Plot F and exploitation rates (2): calculated from numbers and catch at age ##
  ## Read faa here
  ##---- !!!!!! imcomplete for SS3 !!!!!!!!!!!!!!!! --------------------
  par(mfrow=c(3,1),mar=c(3,3,2,1))  
  faa <- list(calFAA.ss(repfile=NULL,
                         datas=list(naa=naa[[1]],caa=caa[[1]],nma=nma[[1]],biom=biom[[1]]),is.plot=T))
  x <- rowtapply(apply(faa[[1]]$faa.array,c(1,2),sum))
  matplot(rownames(x),x,type="b",ylab="F",xlab="Year")
#  title(#side=3,line=0.5,adj=0.1,
#        paste2(" Average F at age (",repfile[1],") "))
#  mtext(side=3,line=0.5,adj=0.1,"@ F at age by year",outer=T)
  mtext(side=3,line=0.5,adj=0.1,
        paste2("F at age by year(",repfile[1],") "),outer=T)  
  
  if(multiplot){
    for(i in 2:len.rep){
      faa[[i]] <- calFAA.ss(repfile=NULL,
                             datas=list(naa=naa[[i]],caa=caa[[i]],nma=nma[[i]],biom=biom[[i]]),
                             is.plot=T)
      matplot(rowtapply(apply(faa[[i]]$faa.array,c(1,2),sum)),type="b",ylab="F",xlab="Year")      
      mtext(side=3,line=0.5,adj=0.1,
            paste2("F at age by year(",repfile[i],") "),outer=T)
#      mtext(side=3,line=0.5,adj=0.1,"@ F at age by year",outer=T)      
    }
  }

  # Fishing impacts by year
  nfleet <- dim(faa[[1]]$faa.array)[[3]]
  setncol(nfleet)
  nplot()
  #  browser()
  legend("topleft",legend=paste(1:nfleet,fleet.name,sep=":"),
         pch=1:nfleet,ncol=2,bty="n")
  years <- as.numeric(rownames(faa[[1]]$faa.array[,,1]))
  for(i in 1:dim(faa[[1]]$faa.array)[[2]]){
    x0 <- 0
    for(fleet in 1:nfleet){
      x <- tapply(faa[[1]]$faa.array[,i,fleet],floor(as.numeric(rownames(faa[[1]]$faa.array[,,fleet]))),sum)
      x0 <- max(x,x0,na.rm=T)
    }

    plot(names(x),rep(0,length(x)),ylim=c(0,x0),type="n",ylab="Continuous F",xlab="Year")
    title(paste("Age",dimnames(faa[[1]]$faa.array)[[2]][i]))
    for(fleet in 1:nfleet){
      tmp <- tapply(faa[[1]]$faa.array[,i,fleet],floor(as.numeric(rownames(faa[[1]]$faa.array[,,1]))),sum)
      if(sum(tmp,na.rm=T)>0){
        points(names(tmp),tmp,pch=fleet,type="b",cex=0.7)

        # multiplot
        if(multiplot==TRUE){
          for(k in 2:len.rep){
            tmp <- tapply(faa[[k]]$faa.array[,i,fleet],
                          floor(as.numeric(rownames(faa[[k]]$faa.array[,,1]))),sum)
            if(sum(tmp,na.rm=T)>0){
              points(names(tmp),tmp,pch=fleet,type="b",cex=0.7,col=col.var[k])
          }}
        }
        
      }
    }
    if(i%%10==9)   mtext(side=3,line=0.5,adj=0.1,"@ F by fleet, age and year ",outer=T)
  }

  cat("Plot of estimated F and exploitation rates was done.\n")
  ##-------------------------------------------------------------------------------  

  ## Plot total catch in weight##
  char.tmp <- numeric()
  for(i in 1:len.rep){
    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"retain(B)","ret_catch")
  }
  if(!multiplot){
    plotTotcatch(biom[[1]],NULL,
                 findseq=char.tmp,titlename="@ Total catch in weight (gray: observed, black: expected)",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=TRUE)
  }
  else{
    plotTotcatch(biom[[1]],biom[2:len.rep],
                 findseq=char.tmp,titlename="@ Total catch in weight (gray: observed, black: expected)",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=TRUE)    
  }
  cat("Plot of estimated total catches (in weight) was done.  \n")

  # total catch by number
  char.tmp <- numeric()
  for(i in 1:len.rep){
    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"retain(N)","ret_catch")
  }
  if(!multiplot){
    dummy.var <- NULL
  }
  else{
    dummy.var <- biom[2:len.rep]
  }
  plotTotcatch(biom[[1]],dummy.var,#NULL,
               findseq=char.tmp,titlename="@ Total catch in number (gray: observed, black: expected)",
               col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
               plot.obscatch=TRUE)
#  }
#  else{
#    plotTotcatch(biom[[1]],biom[2:len.rep],
#                 findseq=char.tmp,titlename="Total catch in number (gray: observed, black: expected)",
#                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
#                 plot.obscatch=T)    
#  }
  cat("Plot of estimated total catches (in number) was done.  \n")  

  ## Plot Exploitation rates by fleet  (average by season)##
  #  This find seq is depending on the option of F method.  
  #  for(i in 1:len.rep){  
  #    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"F:","Hrate")
  #  }
  if(!multiplot){
    plotTotcatch(biom[[1]],NULL,byyear=FALSE,
                 ylab="Exploitation rates or F",findseq=c("F:","Hrate"),FUN=mean,
                 titlename="@ Exploitation rates or F",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=F)
  }
  else{
    plotTotcatch(biom[[1]],biom[2:len.rep],byyear=FALSE,
                 ylab="Exploitation rates",findseq=c("F:","Hrate"),FUN=mean,
                 titlename="@ Exploitation rates",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=F)    
  }
#  mtext(side=3,line=0.5,adj=0.1,"Exploitation rates",outer=T)  
  cat("Plot of estimated exploitation rates was done.  \n")

  ### CPUE
  fleetn <- unique(cpue[[1]]$index)  
#  par(mfrow=c(length(fleetn),1),ps=16)

#  makedevice(filename="CPUE_fit",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#             new=T,append=T)
  nfile <- 1  
  setncol(length(fleetn))
  for(i in fleetn){
    cpue0 <- cpue[[1]]
    matplot(x <- cpue0$year[cpue0$index==i],
            y <- cbind(cpue0$obs[cpue0$index==i],cpue0$exp[cpue0$index==i]),
            pch=c(1,NA),col=c(1,1),type=c("b","l"),
            xlab="Year",ylab="CPUE",lty=c(1,1),lwd=lwd.var[1])
    title(paste("Fleet",i,":",fleet.name[i]),line=nline)    
#    legend(max(x),max(y),xjust=1,yjust=1,legend=c("Expected","Observed"),col=c(1,1),pch=c(NA,1),lty=c(1,1))

    ## For overlapped plots
    if(multiplot){
      for(j in 2:len.rep){
        cpue0 <- cpue[[j]]
        points(cpue0$year[cpue0$index==i],
               cpue0$exp[cpue0$index==i],type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
      }}
    ##
    
    if(length(fleetn)>10 && i%%10==0){
      mtext(side=3,line=nline,adj=0.1,"@ CPUE (line: expected, line+circle: observed)",outer=T)
      nfile <- nfile+1
#      makedevice(filename="CPUE_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                 new=T,append=T)
#      setncol(length(fleetn))
    }
  }
  
#  mtext(side=3,line=2,adj=0.3,"CPUE",outer=T)
  cat("Plot of CPUE (obs vs. est) was done.  \n")  

  # Selectivity
  if(select.on==TRUE){
#    makedevice(filename="Select",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#               new=T,append=T)
    nfile <- 1
    tmp <- getSelect.ss2(repfile[1],cl=cl0,tb=tb0)
    selects <- list(tmp[[1]])
    selects.target <- tmp[[2]]      

    if(multiplot){
      #    select.list <- as.list(rep(0,len.rep))    
      for(i in 2:len.rep){
        selects[[i]] <- getSelect.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      }
    }

    plotSelect(selects,multiplot=multiplot,ptype=ptype,col=col.var,lty=lty.var,lwd=lwd.var,nline=nline)
    cat("Plot of length selectivity was done.  \n")
  }

  # Age Selectivity
  if(aselect.on==TRUE){
#    makedevice(filename="ASelect",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#               new=T,append=T)
    nfile <- 1
    
    tmp <- getSelect.ss2(repfile[1],cl=cl0,tb=tb0,len=F)
    aselects <- list(tmp[[1]])
    aselects.target <- tmp[[2]]
    
    if(multiplot){
      for(i in 2:len.rep){
        aselects[[i]] <-
          getSelect.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])
      }
    }

#    par(oma=c(2,2,2,0))
    plotSelect(aselects,multiplot=multiplot,ptype=ptype,col=col.var,lty=lty.var,lwd=lwd.var,nline=nline)
    cat("Plot of age selectivity was done.  \n")
  }


  #-------- PLOT likelihodd if multiplot=T
  if(multiplot==TRUE){
    plotLL.ss(repfile,repfile.legend=repfile.legend)
    cat("Plot of likelihood was done.  \n")    
  }

  #!!!!!!!-------- if SS3, use function of plotlength.fit (preliminary setting)
  ## To show observed and expected size frequency
  if(length.on==TRUE | agecomp.on==TRUE){
    #--- if SS3 -> go to modified function
    if(is.ss3(repfile[1])){
      tb.tmp[[1]] <- tb0
      cl.tmp[[1]] <- cl0      
      plotlength.fit(repfile,compfile=compfile,tb=tb.tmp,cl=cl.tmp,len=!agecomp.on,
                     len.residual.plot=len.residual.plot)
      sum.length <- NA
    }
    else{  # reserve the program for SS2
      nfile <- 1
      tmp <- getAgecomp.ss2(repfile[1],cl=cl0,tb=tb0,len=!agecomp.on)
      comps <- list(list(tmp[[1]],tmp[[2]]))
      comps.target <- tmp[[3]]
      if(multiplot){
        #      b.list <- as.list(rep(0,len.rep))
        for(i in 2:len.rep){
          comps[[i]] <-
            getAgecomp.ss2(repfile[i],len=!agecomp.on)
        }
      }

      len.data <- comps[[1]][[1]]    
      fleet.row <- sort(unique(comps[[1]][[1]]$fleet))
      #  setncol(nfleet)
      setncol(length(fleet.row))

    # Pearson residual of length data:
    # !!!!!!!! Caution: when size data is too huge, this plot make the result file too heavy!!!!!!!!
      if(len.residual.plot==TRUE){
        browser()
        s <- 1
        for(i in fleet.row){
          tmp <- len.data[len.data$fleet==i,]
          plot(tmp$bin,y <- tmp$Pearson,ylim=c(-3,6),xlab="Length",ylab="Pearson residuals")
          title(main=paste("Fleet",i,":",fleet.name[i]),line=-1)
      #    sd.tmp <- (tmp$obs-tmp$exp)/sqrt(tmp$exp*(1-tmp$exp)*tmp$N)
      #    plot(tmp$bin,sd.tmp,
          if(!multiplot){
            x1 <- tapply(y,tmp$bin,median)
            x2 <- tapply(y,tmp$bin,mean)  
            abline(h=0,col="yellow")
            points(names(x1),x1,type="l",col="red")
            points(names(x2),x2,type="l",col="blue")
          }
          else{
            for(j in 2:len.rep){
              tmp <- comps[[j]][[1]][comps[[j]][[1]]$fleet==i,]
              points(tmp$bin,y <- tmp$Pearson,col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])          
            }
          }
        }
      }

      ###!!!  Not for USE now 
    if(0){
      for(i in fleet.row){
        plot(x <- len.data$exp[len.data$fleet==i & !is.na(len.data$obs)],y <- len.data$obs[len.data$fleet==i & !is.na(len.data$obs)],lwd=lwd.var[1],
             ylim=c(0,max(x,y)),xlim=c(0,max(x,y)),xlab="Expectet size freq",ylab="Observed size freq")
        title(main=paste("fleet ",i),line=nline)

        if(multiplot){
          for(j in 2:len.rep){
            tmp <- comps[[j]][[1]]
            points(x <- tmp$exp[tmp$fleet==i & !is.na(tmp$obs)],
                   y <- tmp$obs[tmp$fleet==i & !is.na(tmp$obs)]
                   ,type="p",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
          }}
        if(length(fleet.row)>10 && s%%10==0){
          mtext(side=3,line=0.5,adj=0.3,"Expected vs observed size composition",outer=T)
#          nfile <- nfile+1
#          makedevice(filename="length_cor",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                     new=T,append=T)
#          setncol(length(fleet.row))
        }
        s <- s+1
      }
      mtext(side=3,line=0.5,adj=0.3,"Expected vs observed size composition",outer=T)
      cat("Scatter plot of length frequency (obs vs est) was done.  \n")
    }
    

  ## Size frequency 2
  ##!!! TOO bad algrosm, which should be shoreter in future
    length.bin <- sort(unique(len.data$bin))
    sum.length <- list(array(0,dim=c(length(length.bin),length(fleet.row),2)))
    dimnames(sum.length[[1]]) <- list(length.bin,fleet.row,c("Obs","Exp"))
    
    if(multiplot){
#      sum.length <- as.list(rep(0,len.rep))
      for(i in 2:len.rep){
        sum.length[[i]] <- sum.length[[1]]
      }}
    
#    nfile <- 1
#    makedevice(filename="length_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=T,append=T,width=700)  
#    par(mfrow=c(5,5),mar=c(1.7,1.7,1.4,0.5),oma=c(0,0,3,3),ps=14)
    
#   browser()
    
    s <- 1
    row.name <- paste(floor(as.numeric(len.data$year)),len.data$season,len.data$fleet,sep="-")
    tmp2 <- unique(row.name)
    tmp.ref <- paste(floor(as.numeric(comps[[1]][[2]]$Year)),comps[[1]][[2]]$Seas,comps[[1]][[2]]$Index,sep="-")
    for(j in 1:length(fleet.row)){
      for(i in 1:length(tmp2)){
        if(sum(len.data$fleet[row.name==tmp2[i]]==fleet.row[j])){
          matplot(x <- len.data$bin[row.name==tmp2[i]],
                  y <- cbind(len.data$obs[row.name==tmp2[i]],
                             len.data$exp[row.name==tmp2[i]])*comps[[1]][[2]]$Nsamp[tmp.ref==tmp2[i]],
                  lwd=lwd.var[1],col=c("royalblue",1),type=c("b","l"),
                  pch=1:2,lty=1:1,cex=0.7,ylab="",xlab="")
        ## Sum up size data by fisheries
          y <- y[!is.na(y[,1]),]
          x <- x[!is.na(x)]
          sum.length[[1]][match(x,length.bin),j,] <- sum.length[[1]][match(x,length.bin),j,]+y
          ##
          title(main=paste(tmp2[i]),line=0.5)

          ## For multiple plots ##
          if(multiplot){
            for(k in 2:len.rep){
              b.tmp <- comps[[k]]
              row.name <- paste(floor(as.numeric(b.tmp[[1]]$year)),b.tmp[[1]]$season,b.tmp[[1]]$fleet,sep="-")
              tmp2 <- unique(row.name)
              tmp.ref <- paste(floor(as.numeric(b.tmp[[2]]$Year)),b.tmp[[2]]$Seas,b.tmp[[2]]$Index,sep="-")              
#              row.name2 <- paste(floor(as.numeric(b.tmp[[1]]$year)),b.tmp[[1]]$season,b.tmp[[1]]$fleet,sep="-")
#              tmp.ref2 <- paste(floor(as.numeric(b.tmp[[2]]$Year)),b.tmp[[2]]$Seas,b.tmp[[2]]$Index,sep="-")

              x <- b.tmp[[1]]$bin[row.name2==tmp2[i]]
              y <- cbind(b.tmp[[1]]$obs[row.name2==tmp2[i]],
                         b.tmp[[1]]$exp[row.name2==tmp2[i]]) * b.tmp[[2]]$Nsamp[tmp.ref2==tmp2[i]]
              points(x,y[,2],col=col.var[k],lty=lty.var[k],lwd=lwd.var[k],type="l")
              y <- y[!is.na(y[,1]),]
              x <- x[!is.na(x)]
              sum.length[[k]][match(x,length.bin),j,] <- sum.length[[k]][match(x,length.bin),j,]+y            
            }}
        ##
          
          if(s%%25==0){
            mtext(side=3,line=0.5,adj=0.3,
                  "Length fit (by each sampling, line: expected, line+circle: observed)",outer=T)
#            nfile <- nfile+1
#            makedevice(filename="length_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                       new=T,append=T,width=700)
#            par(mfrow=c(5,5),mar=c(1.7,1.7,1.4,0.5),oma=c(0,0,3,3),ps=14)#mgp=c(2,1,0),ps=14)   
          }
          s <- s+1                
        }}}
    
  cat("Plot of expected and observed length frequency by each observation was done.  \n")
#  par(mfrow=c(length(fleet.row),1))
    nfile <- 1
#    makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=T,append=T)    
      n <- dim(sum.length[[1]])[[2]]
    setncol(n)
    for(i in 1:n){
      matplot(x <- length.bin,y <- sum.length[[1]][,i,],type=c("b","l"),lty=1,
              pch=1,ylab="nsmple",col=c("black","royalblue"),lwd=lwd.var[1],xlab="Length (cm)")
      title(paste("Fleet",fleet.row[i],":",fleet.name[fleet.row[i]]),line=nline)

      if(multiplot){
        for(k in 2:len.rep){
          points(length.bin,sum.length[[k]][,i,2],
                 type="l",col=col.var[k+1],lty=lty.var[k],lwd=lwd.var[k])
          }}      
      
      if(n>10 && i%%10==0){
        mtext(side=3,line=0.5,adj=0.3,
              "Length fit (by fleet, line: expected, line+circle: observed)",outer=T)
#        nfile <- nfile+1
#        makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                   new=T,append=T)                
#        setncol(n)
      }}
    mtext(side=3,line=0.5,adj=0.3,"Length fit (by fleet, line: expected, line+circle: observed))",
          outer=T)
    cat("Plot of expected and observed length frequency by fleets was done.  \n")    

    # Bubble plot not for multiplot
    xrange <- range(len.data$year)
    yrange <- range(len.data$bin,na.rm=T)    
    max.res.fleet <- tapply(abs(len.data$Pearson),len.data$fleet,max,na.rm=T)
    par(mfrow=c(3,1))
    col.tmp <- c("black","black")
    col.tmp2 <- c(NA,"black")    
    for(j in 1:length(fleet.row)){
      with(len.data[len.data$fleet==j,],symbols(year,bin,circles=sqrt(abs(x <- Pearson))/8,
                          fg=col.tmp[(x<0)+1],bg=col.tmp2[(x<0)+1],lwd=0.5,
                          inches=FALSE,ylim=yrange,xlim=xrange))
#                          inches=max.res.fleet[names(max.res.fleet)==fleet.row[j]]/100))
      title(paste("Fleet",j,":",fleet.name[j]),line=nline)
      if(j%%3==0) mtext("Bubble plot of Pearson residuals, black(obs<exp), black (obs>exp)",outer=T)
    }
    }
  }
  if(names(dev.cur())!="null device") dev.off()

  if(dev.type!="x11"){
    cat("All plots were finished.  Open the file named ",paste(outfile,".",dev.type,sep=""),".  \n")
  }
  else{
    cat("All plots were finished.")
  }

#  par(org.par)
  invisible(list(biom=biom,SPR=SPR,naa=naa,faa=faa,cpue=cpue,
                 nma=nma,alk=alk,repfile=repfile,caa=caa,BRP=BRPs,
                 sum.length=ifelse(length.on==T,sum.length,NA)))
}

areadef.default <-
function(lon,lat,lats,lons,char=1){
  areadef <- as.numeric(lat>=lats[1] & lat<lats[2] & lon>=lons[1] & lon<lons[2])
  areadef <- ifelse(areadef==1,char,0)
  areadef
}

bind.list <-
function(reslist){
  reslist2 <- reslist[[1]]
  for(i in 1:(length(reslist)-1)){
    reslist2 <- rbind(reslist2,reslist[[i+1]])
  }
  reslist2
}

bind.list2 <-
function(reslist){
  reslist2 <- reslist[[1]]
  for(i in 1:(length(reslist)-1)){
    reslist2 <- rbind(reslist2,reslist[[i+1]])
  }
  reslist2
}

bindmat <-
function(mat,n,row=T){
  mat2 <- mat
  if(row==T){
    for(i in 1:(n-1))   mat2 <- rbind(mat2,mat)
  }
  else{
    for(i in 1:(n-1))   mat2 <- cbind(mat2,mat)
  }
  mat2
}

bindmat2 <-
function(mat,n){
  mat2 <- matrix(0,nrow(mat)*n,ncol(mat))
  s <- 1
  for(i in 1:nrow(mat)){
    for(j in 1:n){
      mat2[s,] <- mat[i,]
      s <- s+1
    }
  }
  mat2
}

branov.eq <-
function(x,n.age,naat.pred.vec,select.para,waa,nmaa,CHS){
	s <- 0
	for(i in c(1:n.age)){
		s <- s + (naat.pred.vec[i]*waa[i]*select.para[i]*x)/
			(select.para[i]*x+nmaa[i])*(1-exp(-select.para[i]*x-nmaa[i]))
		}
	(s/1000 - CHS)^2 # convert t. to kg by deviding s by 1000
}

branov.eq.pre <-
function(x,n.age,naat.pred.vec,select.para,waa,nmaa){
	s <- 0
	for(i in c(1:n.age)){
		s <- s + (naat.pred.vec[i]*waa[i]*select.para[i]*x)/
			(select.para[i]*x+nmaa[i])*(1-exp(-select.para[i]*x-nmaa[i]))
		}
	s/1000  # convert t. to kg by deviding s by 1000
}

branov.eq.pre2 <-
function(x,n.age,naat.pred.vec,select.para,waa,nmaa){
#        n.age <- 1:n.age
        tc <- (naat.pred.vec*waa*select.para*x)/
			(select.para*x+nmaa)*(1-exp(-select.para*x-nmaa))
		
        tc/1000
}

branov.eq.pre3 <-
function(x,n.age,naat.pred.vec,select.para,waa,nmaa,partial.catch){
#        n.age <- 1:n.age
  tc <- (naat.pred.vec*waa*partial.catch*x)/
			(select.para*x+nmaa)*(1-exp(-select.para*x-nmaa))
		
        tc/1000
}

calFAA.ss <-
function(repfile,datas=NULL,age.limit=NULL,namae=NULL,is.plot=F,
                       nline=-1,Fmulti=1,qt=4,naa.target=NULL){
  if(is.null(datas)){
    if(is.null(naa.target)){
      naa <- getNAA.ss2(repfile)
    }
    else{
      naa <- getNAA.ss2(repfile,target.line=naa.target)      
    }
    caa <- getCAA.ss2(repfile,target.line=naa[[2]]-10)
    nma <- getNMA.ss2(repfile,target.line=caa[[2]]-10,qt=qt)
    # In nma, M: natural mortality, Len_Mat: maturity rate, Wt_Beg: weight at age
    naa <- naa[[1]]
    caa <- list(caa=caa[[1]],caa.array=caa[[3]])
    nma <- nma[[1]]
  }
  else{
    naa <- datas$naa
    caa <- datas$caa
    nma <- datas$nma
  }

  naa$YQ <- as.numeric(naa$Year)+(as.numeric(naa$Seas)/qt)-1/qt
  totcatch <- apply(caa$caa.array,c(1,2),sum)

  if(is.null(datas)|is.null(datas$biom)){
    biom <- getBabs.ss2(repfile)[[1]]
  }
  else{
    biom <- datas$biom
  }

  is.catch <- substr(colnames(biom),1,9)=="ret_catch"
  if(sum(is.catch)==0) is.catch <- substr(colnames(biom),1,11)=="retain(B):_"
  if(sum(is.catch)==1){
    wtot.org <- biom[,is.catch]
  }
  else{
    wtot.org <- apply(biom[,is.catch],1,sum)  # total catch
  }
#  names(wtot.org) <- naa$YQ
  names(wtot.org) <- as.numeric(biom$year)+(as.numeric(biom$season)/qt)-1/qt

  faa <- faa.multi <- matrix(0,dim(totcatch)[[1]],dim(totcatch)[[2]],dimnames=dimnames(totcatch))
  faa.array <- faa.array.multi <-
    array(0,dim=dim(caa$caa.array),dimnames=dimnames(caa$caa.array))
  if(sum(colnames(nma)=="age")==0) nma$age <- nma$"Age"
  for(i in 1:nrow(faa)){
    for(j in 1:ncol(faa)){
      nage <- colnames(faa)[j]
      nyear <- rownames(faa)[i]
      if(totcatch[i,j]>0){
#        if(nyear=="2001.75" & nage==15) browser() 
        faa[i,j] <- solv.Feq(cvec=totcatch[i,j],
                             nvec=naa[naa$YQ==nyear,colnames(naa)==nage],
                             mvec=nma$M[nma$age==nage]/qt)
        faa.array[i,j,] <- faa[i,j]*caa$caa.array[i,j,]/sum(caa$caa.array[i,j,])
        
        if(Fmulti!=1){
          faa.multi[i,j] <- solv.Feq(cvec=totcatch[i,j]*Fmulti,
                                     nvec=naa[naa$YQ==nyear,colnames(naa)==nage],
                                     mvec=nma$M[nma$age==nage]/qt)
          faa.array.multi[i,j,] <- faa.multi[i,j]*caa$caa.array[i,j,]/sum(caa$caa.array[i,j,])
        }
      }
    }
  }
  if(Fmulti==1){
    dat <- list(naa=naa,caa=caa,caa.array=caa$caa.array,
                faa=faa,faa.array=faa.array,nma=nma,
                wtot=wtot.org,repfile=repfile)
  }
  else{
    dat <- list(naa=naa,caa=caa,caa.array=caa$caa.array,repfile=repfile,
                faa=faa.multi,faa.array=faa.array.multi,nma=nma,
                faa.org=faa,faa.array.org=faa.array,wtot=wtot.org)    
  }
  if(is.plot){
#    set.mypar()
#    par(mfrow=c(2,1),mar=c(3,3,1,1))
    if(is.null(age.limit)) age.limit <- 1:ncol(faa)
    if(is.null(namae)) namae <- 1:ncol(faa)  
#    plotFvalue(list("F at age table"=faa),age.limit=age.limit,namae=namae,cex=0.7,VPA=FALSE,locate="n")
    plotFvalue2(list("F at age table"=faa),
                year.limit=matrix(c(1952, 1959, 1960, 1969,
                  1970, 1979, 1980, 1989, 1990, 1999,2000,2006,2006,2008), 2, 7),VPA=FALSE)
    plotFvalue2(list("F at age table"=faa),
                year.limit=matrix(c(2003,2003.9,2004,2004.9,2005,2005.9,
                  2006,2006.9,2007,2007.9,2008,2008.9,2009,2009.9), 2, 7),
                VPA=FALSE)    
  }
  return(dat)
}

calFAA.ss2 <-
function(repfile,datas=NULL,age.limit=NULL,namae=NULL,is.plot=F,
                       nline=-1,Fmulti=1,qt=4,naa.target=NULL){
  if(is.null(datas)){
    if(is.null(naa.target)){
      naa <- getNAA.ss2(repfile)
    }
    else{
      naa <- getNAA.ss2(repfile,target.line=naa.target)      
    }
    caa <- getCAA.ss2(repfile,target.line=naa[[2]]-10)
    nma <- getNMA.ss2(repfile,target.line=caa[[2]]-10,qt=qt)
    # In nma, M: natural mortality, Len_Mat: maturity rate, Wt_Beg: weight at age
    naa <- naa[[1]]
    caa <- list(caa=caa[[1]],caa.array=caa[[3]])
    nma <- nma[[1]]
  }
  else{
    naa <- datas$naa
    caa <- datas$caa
    nma <- datas$nma
  }

  naa$YQ <- as.numeric(naa$Year)+(as.numeric(naa$Seas)/qt)-1/qt
  totcatch <- apply(caa$caa.array,c(1,2),sum)

  if(is.null(datas)|is.null(datas$biom)){
    biom <- getBabs.ss2(repfile)[[1]]
  }
  else{
    biom <- datas$biom
  }

  is.catch <- substr(colnames(biom),1,9)=="ret_catch"
  if(sum(is.catch)==0) is.catch <- substr(colnames(biom),1,11)=="retain(B):_"
  if(sum(is.catch)==1){
    wtot.org <- biom[,is.catch]
  }
  else{
    wtot.org <- apply(biom[,is.catch],1,sum)  # total catch
  }
#  names(wtot.org) <- naa$YQ
  names(wtot.org) <- as.numeric(biom$year)+(as.numeric(biom$season)/qt)-1/qt

  faa <- faa.multi <- matrix(0,dim(totcatch)[[1]],dim(totcatch)[[2]],dimnames=dimnames(totcatch))
  faa.array <- faa.array.multi <-
    array(0,dim=dim(caa$caa.array),dimnames=dimnames(caa$caa.array))
  if(sum(colnames(nma)=="age")==0) nma$age <- nma$"Age"
  for(i in 1:nrow(faa)){
    for(j in 1:ncol(faa)){
      nage <- colnames(faa)[j]
      nyear <- rownames(faa)[i]
      if(totcatch[i,j]>0){
#        if(nyear=="2001.75" & nage==15) browser() 
        faa[i,j] <- solv.Feq(cvec=totcatch[i,j],
                             nvec=naa[naa$YQ==nyear,colnames(naa)==nage],
                             mvec=nma$M[nma$age==nage]/qt)
        faa.array[i,j,] <- faa[i,j]*caa$caa.array[i,j,]/sum(caa$caa.array[i,j,])
        
        if(Fmulti!=1){
          faa.multi[i,j] <- solv.Feq(cvec=totcatch[i,j]*Fmulti,
                                     nvec=naa[naa$YQ==nyear,colnames(naa)==nage],
                                     mvec=nma$M[nma$age==nage]/qt)
          faa.array.multi[i,j,] <- faa.multi[i,j]*caa$caa.array[i,j,]/sum(caa$caa.array[i,j,])
        }
      }
    }
  }
  if(Fmulti==1){
    dat <- list(naa=naa,caa=caa,caa.array=caa$caa.array,
                faa=faa,faa.array=faa.array,nma=nma,
                wtot=wtot.org,repfile=repfile)
  }
  else{
    dat <- list(naa=naa,caa=caa,caa.array=caa$caa.array,repfile=repfile,
                faa=faa.multi,faa.array=faa.array.multi,nma=nma,
                faa.org=faa,faa.array.org=faa.array,wtot=wtot.org)    
  }
  if(is.plot){
#    set.mypar()
#    par(mfrow=c(2,1),mar=c(3,3,1,1))
    if(is.null(age.limit)) age.limit <- 1:ncol(faa)
    if(is.null(namae)) namae <- 1:ncol(faa)  
#    plotFvalue(list("F at age table"=faa),age.limit=age.limit,namae=namae,cex=0.7,VPA=FALSE,locate="n")
    plotFvalue2(list("F at age table"=faa),
                year.limit=matrix(c(1952, 1959, 1960, 1969,
                  1970, 1979, 1980, 1989, 1990, 1999,2000,2006,2006,2008), 2, 7),VPA=FALSE)
    plotFvalue2(list("F at age table"=faa),
                year.limit=matrix(c(2003,2003.9,2004,2004.9,2005,2005.9,
                  2006,2006.9,2007,2007.9,2008,2008.9,2009,2009.9), 2, 7),
                VPA=FALSE)    
  }
  return(dat)
}

calTotcatch.ALK <-
function(repfile,qt=4){
  caa <- getCAA.ss2(repfile)
  WatL <- getWatL.ss2(repfile)[[1]]  #
  ALK <- getALK.ss2(repfile=repfile,all=TRUE,qt=qt)[[1]]

  label <- as.numeric(unfactor(ALK[[1]][,1]))
  for(i in 1:qt){  # delete the label of first line
    ALK[[i]] <- ALK[[i]][!is.na(label),-1]
  }
  label <- label[!is.na(label)]

  # sort the bin by increasing
  for(i in 1:qt){
    ALK[[i]] <- ALK[[i]][order(label),]
    rownames(ALK[[i]]) <- label[order(label)]
  }
  label <- label[order(label)]

  wcaa.array <- caa$caa.array
  dc <- dim(wcaa.array)
  # check the number of bin with that of WatL
  if(sum(WatL$low != label)==0){
    for(i in 1:dc[3]){
      for(j in 1:dc[2]){
        for(k in 1:dc[1]){
#          browser()
#          cat(qtback(dimnames(wcaa.array)[[1]][k])," ")
          wcaa.array[k,j,i] <- sum(caa$caa.array[k,j,i] * 
            ALK[[qtback(dimnames(wcaa.array)[[1]][k])]][,j] * # catch at age (in number) by each bin
               WatL$Wt) # weight by bin
    }}}
  }
  else{
    cat("Length of label and WatL is different !!!!")
  }

  return(list(wcaa.array=wcaa.array,ALK=ALK,WatL=WatL))
}

calTotcatch.select <-
function(repfile,target.fleet=0,target.term=0,
                               future.res=NULL,
                               error.func="select",
                               len.sd=0,
                               # error.funcは、"multinom"、"select" または "both"
                               # len.sdは"select"または"both"のときのみ使う
                               # length.sample.size の1つ目は真の、2つ目はdatファイルのインプットのもの
                               length.sample.size=c(100,9)){
  
  # error function can be selected from "select" or "multinom"
#  biom <- getBabs.ss2(repfile)

  cl0 <- count.fields(repfile,blank.lines.skip=FALSE)
  tb0 <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl0),sep=""),as.is=T,
                    blank.lines.skip=FALSE,colClasses=rep("character",max(cl0)),
#                    nrows=length(cl0)/2)[,1:2]
                    )[,1:2]                      

  naa <- getNAA.ss2(repfile,cl=cl0,tb=tb0)[[1]]
  caa <- getCAA.ss2(repfile,cl=cl0,tb=tb0)
  select <- t(getSelect.ss2(repfile,cl=cl0,tb=tb0)[[1]])
  aselect <- t(getSelect.ss2(repfile,cl=cl0,tb=tb0,len=F)[[1]])
  alk <- getALK.ss2(repfile,all=TRUE,cl=cl0,tb=tb0)[[1]]
  WatL <- getWatL.ss2(repfile,cl=cl0,tb=tb0)[[1]]
  nma <- getNMA.ss2(repfile,cl=cl0,tb=tb0)[[1]]
  faa <- calFAA.ss2(repfile,datas=list(naa=naa,caa=caa,nma=nma))  
  
  tmp <- unlist(strsplit(rownames(aselect),"-"))
  label.aselect <- as.data.frame(t(matrix(tmp,3,length(tmp)/3)))
  tmp <- unlist(strsplit(rownames(select),"-"))
  label.select <- as.data.frame(t(matrix(tmp,3,length(tmp)/3)))

  colnames(label.select) <- colnames(label.aselect) <- c("fleet","year","Gmorph")
  label.select$fleet <- substr(as.character(label.select$fleet),2,3)
  label.select$year <- substr(as.character(label.select$year),2,5)
  label.aselect$fleet <- substr(as.character(label.aselect$fleet),2,3)
  label.aselect$year <- substr(as.character(label.aselect$year),2,5)    

  if(is.null(future.res)){
    years <- as.numeric(dimnames(caa$caa.array)[[1]])
  }
  else{
    years <- as.numeric(rownames(future.res$naa))
  }
  nfleet <- dim(caa$caa.array)[[3]]
  tmp.array <- caa$caa.array
  tmp.array[] <- 0
  wcaa.array <- caa$caa.array
  wcaa.array[] <- 0

  target.res <- list()  
  s <- 1

  nbins <- nrow(alk[[1]])-2

  for(i in 1:length(years)){
    qt <- qtback(years[i])

    if(1){ # <- Qを計算する場合は、全漁業込みのN_childarを使っているみたい。
      if(is.null(future.res)){        
        tmp <- naa$Year==floor(years[i]) & naa$Seas==qt
        tmp.faa <- as.numeric(faa$faa[rownames(faa$faa)==years[i],])
        # numbers at age by bin and age
        tmpline <- ifelse(is.ss3(repfile),11,4)
        naa.beg <- as.numeric(naa[tmp,tmpline:ncol(naa)])
        naa.mid <- naa.beg * exp(-0.5 * (nma$M[nma$Seas==qt]/max(nma$Seas)+tmp.faa))      
      }
      else{
        tmp <-  years==years[i]
        tmp.qt <- qtback(colnames(future.res$faa))==qt
        tmp.faa <- future.res$faa[tmp,tmp.qt]
        naa.mid <- future.res$naa[tmp,tmp.qt] * exp(-0.5 * (nma$M[nma$Seas==qt]/max(nma$Seas)+tmp.faa))      
      }

      if(0){ # ALKはmiddle of season かと思ったけど、違う
        alk[[max(nma$Seas)+1]] <- alk[[1]]
        alk.tmp <- (as.matrix(alk[[qt]][1:nbins,-1]) + as.matrix(alk[[qt+1]][1:nbins,-1]))/2
        n.ba <- sweep(alk.tmp,2,naa.mid,FUN="*")
      }
      if(1){
        n.ba <- sweep(as.matrix(alk[[qt]][1:nbins,-1]),2,naa.mid,FUN="*")
      }
        rownames(n.ba) <- alk[[1]][1:nbins,1]
      n.ba <- n.ba[nrow(n.ba):1,]
    }

    # by fleet
    for(j in 1:nfleet){

      if(0){ #----technicalマニュアルには、N_childerは漁業によって異なるとなっている
             # が、それは間違い
        if(is.null(future.res)){        
          tmp <- naa$Year==floor(years[i]) & naa$Seas==qt
          tmp.faa <- as.numeric(faa$faa.array[dimnames(faa$faa)[[1]]==years[i],,j])
          tmpline <- ifelse(is.ss3(repfile),11,4)
          naa.beg <- as.numeric(naa[tmp,tmpline:ncol(naa)])
          naa.mid <- naa.beg * exp(-0.5 * (nma$M[nma$Seas==qt]/max(nma$Seas)+tmp.faa))
        }
        else{
          tmp <-  years==years[i]
          tmp.qt <- qtback(colnames(future.res$faa))==qt
          tmp.faa <- future.res$faa[tmp,tmp.qt]
          naa.mid <- future.res$naa[tmp,tmp.qt] * exp(-0.5 * (nma$M[nma$Seas==qt]/max(nma$Seas)+tmp.faa))
        }
        n.ba <- sweep(as.matrix(alk[[qt]][1:nbins,-1]),2,naa.mid,FUN="*")        
        rownames(n.ba) <- alk[[1]][1:nbins,1]
        n.ba <- n.ba[nrow(n.ba):1,]
      }
      
      tmp <- label.aselect$fleet==j & label.aselect$year==max(naa$Year[naa$Per=="TIME"])#& label.aselect$year<=years[i] # !!!!!! made temporary change
      beta.a <- aselect[max(which(tmp)),]  # age selectivity

      tmp <- label.select$fleet==j & label.select$year==max(naa$Year[naa$Per=="TIME"])# & label.select$year<=years[i] !!!!!!!! temporary change 
      beta.l <- select[max(which(tmp)),]  # length selectivity
      n.b <- sweep(n.ba,2,beta.a,FUN="*") # age x length selctivity

      # vulnerable numbers (n.l) and weights (w.l) at age  by length
      n.l <- sweep(n.b,1,beta.l,FUN="*") 
      w.l <- sweep(n.l,1,WatL$Wt,FUN="*") 

      # vulnerable numbers at age (summed vuluneragble numbers at age by age)
      x <- apply(n.l,2,sum)

      #-------- Record vulnerable biomass
      if(j==1 && i==1){
        vna <- vba <- array(0,dim=c(length(x),nfleet,length(years)))
        dimnames(vna) <- dimnames(vba) <-list(1:length(x),1:nfleet,years)
      }
      vna[,j,i] <- x
      vba[,j,i] <- apply(w.l,2,sum)
      #-------- end

      # -------- Record expected length at age
      # expected length freq ==> 実際のcatch at ageに合わせたc.alとn.lがあるが、ここではn.lを使う
#      browser()
      if(error.func=="select"){
        obs.len.tmp <- apply(sweep(n.l,2,exp(rnorm(dim(n.l)[[2]],mean=0,sd=len.sd)),FUN="*"),1,sum)
      }
      else{
        if(error.func=="multinom"){
#          obs.len.tmp <- apply(sweep(n.l,2,exp(rnorm(dim(n.l)[[2]],mean=0,sd=len.sd)),FUN="*"),1,sum)
          obs.len.tmp <- apply(n.l,1,sum)
          obs.len.tmp <- rmultinom(1,length.sample.size[1],obs.len.tmp)
#          browser()
        }
        else{
          if(error.func=="both"){
            obs.len.tmp <- apply(sweep(n.l,2,exp(rnorm(dim(n.l)[[2]],mean=0,sd=len.sd)),FUN="*"),1,sum)
            obs.len.tmp <- rmultinom(1,length.sample.size[1],obs.len.tmp)
#            browser()
          }
          else{
            obs.len.tmp <- apply(n.l,1,sum)
          }
        }
      }
      
#      names(obs.len.tmp) <- names(n.b)
#      browser()
      obs.len.tmp <- c(floor(years[i]),qtback(years[i]),j,1,0,
                       length.sample.size[2],obs.len.tmp/sum(obs.len.tmp))
      
      if(j==1 && i==1){
        obs.len <- obs.len.tmp
      }
      else{
        obs.len <- rbind(obs.len,obs.len.tmp)
      }
      
      # actual catch at age (catch at age from repfile)
      if(is.null(future.res)){              
        x0 <- caa$caa.array[i,,j] # i <- year, j <- fleet
#--------- future projectionの場合は、必要ない      
#      else{
#        x0 <- future.res$pcaa[i,j,]
#      }

        tmp <- x0/x   
        tmp[tmp==Inf|is.nan(tmp)] <- 0
        # mutiply vulunerable number by exploitation rate -> catch at age and length
        # tmpは、c.alから計算されるcatch at ageがReport.ssoのcatch at ageと一致するように調整している。なので、全体の漁獲量がぴったり合う。
        # 本来なら、x0とxのcompositionは全く一致する必要がある。この処理は漁獲量を合わせるための小手先。      
        c.al <- sweep(n.l,2,tmp,FUN="*")

        #--------- 2kg以下を規制する等のシナリオのときに使う
        if(target.fleet==j & years[i] >= target.term[1]  & years[i] <= target.term[2]){
          target.res[[s]] <- c.al
          target.res[[s]] <- ifelse(is.nan(target.res[[s]]),0,target.res[[s]])
          names(target.res)[s] <- paste2("F",j,"Y",years[i])
          s <- s+1
        }

        wc.al <- sweep(c.al,1,WatL$Wt,FUN="*")
        wcaa.array[i,,j] <- apply(wc.al,2,sum) # catch weight at tf
        wcaa.array[i,is.nan(wcaa.array[i,,j]),j]  <- 0
        tmp.array[i,,j] <- tmp
      }
    }
  }

  dimnames(obs.len) <- NULL

  list(exp.rage=tmp.array,
       wcaa.array=wcaa.array,
       target.res=target.res,
       datas=list(naa=naa,caa=list(caa=caa$caa.array,caa.array=caa$caa.array),
         nma=nma,vna=vna,vba=vba,obs.len=obs.len))
}

calc.B0 <-
function(waa,nmaa,R.num) {
  sum(NperR(nmaa,rep(0,length(nmaa)),0)*waa)/1000 * R.num
}

calc.moon.age <-
function(yr,mon,day){
  library(date)
  time <- (as.numeric(mdy.date(mon,day,yr))-as.numeric(mdy.date(1,1,1970))) # dates from 1970/1/1
  cyc <- 29.530589
  jul <- 2440587.5 + time
  k <- floor((jul-2451550.09765) / cyc) # floor
  t <- k / 1236.85
  sg <- 2451550.09765 + cyc * k + 0.0001337 * t * t - 0.40720 * sin((201.5643 + 385.8169 * k)* pi/180) + 0.17241 * sin(( 2.5534 + 29.1054 * k)* pi/180)
  moon <- jul - sg
  moonage <- floor(moon*100)/100
  moonage
}

calc.pcaa <-
function(caa1,naa1,maa1){

  exp.rate <- pcaa <- caa1
  for(k in 1:dim(caa1)[3]){
    for(i in 1:dim(caa1)[1])
      for(j in 1:dim(caa1)[2]){
        if(caa1[i,j,k]!=0){
          exp.rate[i,j,k] <- caa1[i,j,k]/naa1[i,j]
          pcaa[i,j,k] <- calc.pcaa.tmp(exp.rate[i,j,k],maa1[j])
        }
        else{
          pcaa[i,j,k] <- 0
        }
      }
    cat(k," ")
  }
  pcaa
}

calc.pcaa.tmp <-
function(er1,maa1){
  tmpfunc <- function(er1,maa1,pf1){
    (er1-pf1/(pf1+maa1)*(1-exp(-pf1-maa1)))^2
  }
  optimize(tmpfunc,lower=0,upper=5,er1=er1,maa=maa1)$minimum
}

calc.total.catch <-
function(Fvec,Mvec,Nvec,Wvec){
  n <- length(Mvec)
  tc <- 0
  for(i in c(1:n)){
    tc <- tc + (Fvec[i]/(Fvec[i]+Mvec[i])*(1-exp(-Fvec[i]-Mvec[i])))*Nvec[i]*Wvec[i]/1000
  }
  tc
}

chline <-
function(char.tmp,n=10){
  res.char <- character()
  if(nchar(char.tmp)>n){
    for(i in 1:ceiling(nchar(char.tmp)/n)){
      if(i<ceiling(nchar(char.tmp)/n)){
        res.char <- paste2(res.char,substr(char.tmp,1+(i-1)*n,n*i),"\n")
      }
      else{
        res.char <- paste2(res.char,substr(char.tmp,1+(i-1)*n,n*i))        
      }
    }
  }
  else{
    res.char <- char.tmp
  }
  return(res.char)
}

constsmp <-
function(smp,dev=100,lim=NULL){
  if(!is.null(lim)){
    rep(lim,length(smp))
  }
  else{
    rep(round(mean(smp)/dev),length(smp))
  }
}

cor.dot <-
function(cor.res, filename="document/temp.dot",under.value=0.5){
  #  グラフの設定
cat("digraph \"foodweb.dhp\" { \n size=\"7,7\"; \n rankdir=\"LR\";\n node [fontname=\"Helvetica\" fontsize=14 shape=ellipse]; \n edge [fontname=\"Helvetica\" fontsize=12]; \n center=1;\n ",file=filename)

#cor.name <- as.character(cor.res[,1])
#cor.res <- cor.res[,-1]
#dimnames(cor.res) <- list(1:58,1:58)
n <- dim(cor.res)[1]
cat(n)
#cat(dimnames(cor.res))
#write.table(gyakulogistic(cor.res))
for(i in c(1:n)){
  cat(colnames(cor.res)[i],"\n",file=filename,append=TRUE)
  for(j in c(1:n)){
        if(abs(cor.res[i,j])>under.value && i > j )
        cat(colnames(cor.res)[i],"->",
            colnames(cor.res)[j],
            "[label=",
            round(cor.res[i,j]),
            "]; \n",file=filename,append=TRUE)
      }}
cat("}",file=filename,append=TRUE)
}

count.next.line <-
function(current.line,skip.line,table.property){
  k <- current.line+skip.line
  while(table.property[k]!=0 & k<=length(table.property)){
    k <- k+1
  }
  k-1
}

count.next.line2 <-
function(current.line,skip.line,table.property){
  k <- k0 <- current.line+skip.line
  k <- k+1
  while(table.property[k]==table.property[k0] & k<=length(table.property)){
    k <- k+1
  }
  k-1
}

custamize.LLrep <-
function(a,rm.zerolambda=TRUE,multilambda=TRUE){
  TLL <- sum(a[[1]][,2])
  names(TLL) <- "TLL"
  
  penalties <- a[[1]][c(7:13),2]
  names(penalties) <- a[[1]][c(7:13),1]

  if(rm.zerolambda==FALSE){
    tmp <- a[[2]]$"length_like"!=0
  }
  else{
    tmp <- a[[2]]$"length_like"!=0  & a[[2]]$"length_lambda"!=0
  }
  if(multilambda==TRUE){  
    length.LL <- a[[2]]$"length_like"[tmp] * a[[2]]$"length_lambda"[tmp]
  }
  else{
    length.LL <- a[[2]]$"length_like"[tmp]    
  }
  names(length.LL) <- paste2("Len_F",a[[2]]$"Fleet"[tmp])

  if(rm.zerolambda==FALSE){
    tmp <- a[[2]]$"surv_like"!=0
  }
  else{
    tmp <- a[[2]]$"surv_like"!=0  & a[[2]]$"surv_lambda"!=0    
  }
  
  if(multilambda==TRUE){
#    browser()
    surv.LL <- a[[2]]$"surv_like"[tmp]*a[[2]]$"surv_lambda"[tmp]    
  }
  else{
    surv.LL <- a[[2]]$"surv_like"[tmp]
  }
  names(surv.LL) <- paste2("Surv_F",a[[2]]$"Fleet"[tmp])  
  
  c(TLL,penalties,length.LL,surv.LL)
}

custamize.LLrep.ss3 <-
function(a,rm.zerolambda=FALSE,multilambda=TRUE){
  TLL <- as.numeric(as.character(a$TLL[2,2]))
  names(TLL) <- "TLL"
  
  penalties <- as.numeric(as.character(a[[1]][tmp <- c(4,3,7,9,11,12,8,10),2]))
  names(penalties) <- as.character(a[[1]][tmp,1])

  a0 <- as.data.frame(t(a[[2]]))

  if(rm.zerolambda==FALSE){
    tmp <- a0$"Length_like"!=0
  }
  else{
    tmp <- a0$"Length_like"!=0  & a0$"Length_lambda"!=0
  }
  if(multilambda==TRUE){  
    length.LL <- as.numeric(as.character(a0$"Length_like"[tmp])) * as.numeric(as.character(a0$"Length_lambda"[tmp]))
  }
  else{
    length.LL <- as.numeric(as.character(a0$"Length_like"[tmp]))
  }
  names(length.LL) <- paste2("Len_F",rownames(a0))

  if(rm.zerolambda==FALSE){
    tmp <- a0$"Surv_like"!=0
  }
  else{
    tmp <- a0$"Surv_like"!=0  & a0$"Surv_lambda"!=0    
  }
  
  if(multilambda==TRUE){
#    browser()
    surv.LL <- as.numeric(as.character(a0$"Surv_like"[tmp]))*as.numeric(as.character(a0$"Surv_lambda"[tmp]))    
  }
  else{
    surv.LL <- as.numeric(as.character(a0$"Surv_like"[tmp]))
  }
  names(surv.LL) <- paste2("Surv_F",rownames(a0))
  
  c(TLL,penalties,length.LL,surv.LL)
}

cutlimit <-
function(smp,lim=1000){
  smp[smp>lim] <- lim
  smp
}

do.bootstrap.ss3 <-
function(namesfile="starter.ss",control.boot="control_boot.SS",
                             ss3.arg="-nox -nohess",grad.criteria=0.1,nboot=300,
                             only.readdat=FALSE,use.parfile=TRUE,
                             is.plot=F,debug.mode=F,max.calc=5,additive.num=0,save.faa=FALSE){

  # (1) starter.ss2を読む
  # (2) ６行目の値をnbootに変更して、starter.ss2を書き直す
  # (3) 変更されたstarter.ss2が書き出され、一回SSが実行されブートストラップ用のdatファイルができる
  # (4) コントロールファイルが、ブートストラップ用に用意されていたもの(control.boot)と置き換わる
  # (3) (以下、ブートストラップ回分だけ繰り返す)
  #     SS2-nudata.datから一個分のdatファイルを切りだし、そのdatファイル(data-boot.ss)をもとに
  #     推定し、結果をss2boot-(i+additive.num).repにコピー
  
#  if(.Platform$OS.type=="unix") convert.filename()

  file.copy2(from=namesfile,to=paste2(namesfile,"_o"))  

  #--- when the program is terminated, the namesfile (or starter file)---XS
  #                                  is replaced with the original one

  exit.function <- function(){
    file.copy2(from=namesfile             ,to=paste2(namesfile,"_last"))
    file.copy2(from=paste2(namesfile,"_o"),to=namesfile)
    file.copy2(from="ss3-org.par"         ,to="ss3.par")    
    save(grad.rec,file="grad.rec.R")
  }

  on.exit(exit.function())

  #---------- Read starter file
  names.obj <- names.obj.o <- read.table(namesfile,as.is=T)
  if(is.null(nboot)){
    nboot <- as.numeric(names.obj[11,1])  # Number of boot straps
  }
  if(nboot==0) stop("Please input a figure more than 0 in the 11th line")
  
  grad.rec <- data.frame(grad=rep(0,nboot+1),ncalc=rep(0,nboot+1))
  grad.tmp <- 10
  
  if(only.readdat==FALSE){
    #---------- Write starter file
    names.obj[11,1] <- nboot
    names.obj[3,1] <- 0 # not use ss2.par
    write.table("#NuStarter.SS2",file=namesfile,row.names=F,col.names=F,quote=FALSE)      
    write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE,append=T)      
    #----------
  
    grad.tmp <- 10
    s <- 1
    #---------- Conduct 1 process for create dat files for bootstraps
    while(grad.tmp>grad.criteria && s<max.calc){ # 計算は５回まで
      if(s>1){ # ２回目からはss.parを読む
        names.obj[3,1] <- 1 
        write.table("#NuStarter.SS2",file=namesfile,row.names=F,col.names=F,quote=FALSE)      
        write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE,append=T)            
      }
      if(debug.mode==F){
        doss3(ss3.arg=ss3.arg) #!!
      }
      grad.tmp <- read.grad(parfile="ss3.par") #!!
      s <- s+1
    }
    grad.rec$grad[1] <- grad.tmp
    grad.rec$ncalc[1] <- s-1
  
    file.copy2(from="Report.sso",to="Report_org.sso")  #!!
    file.copy2(from="ss3.par",to="ss3-org.par")  #!!
    file.copy2(from="data.ss_new",to="data.ss_new_org")  #!!
    #  file.copy2(from=namesfile2,to=namesfile)
  }

  file.copy2(from="data.ss_new",to="data.ss_new_org")
  cf <- count.fields("data.ss_new",comment.char="")  #!!
  a <- read.table("data.ss_new",fill=T,col.names=paste("V",1:max(cf),sep=""),comment.char="") #!!
  
  cut.point <- c(2,which(a[,1]==999))

  #---------- modify starter.ss for bootstrapping
  names.obj[1,1] <- "data-boot.ss"
  names.obj[2,1] <- control.boot
  names.obj[3,1] <- as.numeric(use.parfile)
  names.obj[11,1] <- 0
  write.table("#NuStarter.SS2",file=namesfile,row.names=F,col.names=F,quote=FALSE)      
  write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE,append=T)
  #----------
  for(i in 1:nboot){
    write.table(a[(cut.point[i]+1):cut.point[i+1],],na="",
                file="data-boot.ss",                
                row.names=F,col.names=F,quote=FALSE)  # boot.datの更新
    file.copy2(from="ss3-org.par",to="ss3.par")                        
    file.copy2(from="data-boot.ss",to=paste("data-boot",toshi(i+additive.num),".ss",sep=""))
    
    s <- 1
    grad.tmp <- 10
    while(grad.tmp>grad.criteria && s<max.calc){ # 計算は５回まで
      if(debug.mode==F){
        doss3(ss3.arg=ss3.arg)
      }
      grad.tmp <- read.grad(parfile="ss3.par")
      s <- s+1      
    }
    grad.rec$grad[i+1] <- grad.tmp
    grad.rec$ncalc[i+1] <- s-1
      
    file.copy2(from="Report.sso",to=paste2("Report_b",toshi(i+additive.num),".sso"))
#    file.copy2(from="Forecast-report.SSO",to=paste2("Forecast-report_b",toshi(i+additive.num),".SSO"))

    file.copy2(from="CompReport.sso",to=paste("CompReport",toshi(i+additive.num),".sso",sep=""))    
    file.copy2(from="ss3.par",to=paste("ss3_b",toshi(i+additive.num),".par",sep=""))                                
  }

  ## plot of the results
  if(is.plot==T){
    tmp <- getBabs.ss2("ss3-org.rep")#,cl=cl0,tb=tb0)
    biom <- tmp[[1]]
    biom.target <- tmp[[2]]
    biom.list <- as.list(1:nboot)
    for(i in 1:nboot){
      biom.list[[i]] <- getBabs.ss2(paste2("Report",i+additive.num,".sso"),
                                    target.line=biom.target-10-nrow(biom))[[1]]
    }
    plotBSR(biom,biom.list)
  }
  save(grad.rec,file="grad.rec.R")

  if(save.faa==TRUE){
    for(i in 1:nboot){
      faa <- calFAA.ss2(paste2("Report_b",toshi(i+additive.num),".SSO"))
      save(faa,file=paste2("faa",toshi(i+additive.num),".R"))
    }
  }
  return(grad.rec)
}

doautorun <-
function(datfile=NULL,ctlfile=NULL,process.graph=TRUE,
                      repfile="ss2.rep",parfile="ss2.par",
                      namesfile=ifelse(vnumber<2,"SS2names.nam","starter.ss2"),
                      forecastfile="forecast.ss2",
                      max.grad=1e+20,max.calc=c(3,10),ask.repeat=FALSE,
                      initN.op=1,effN.op=2,store.repfile=TRUE,retro.year=NULL,
                      effN.arg=list(zero.intercept=TRUE,by.fleet=TRUE,conversion.criteria=0.1),
                      # effN.op==2のときの追加引数。
                      # intercepは、回帰するときの切片、by.fleetは、fleetごとに回帰するかどうか
                      # conversion.criteriaは、回帰の直線が1+-conversion.criteriaになったら
                      # 終了させる。
                      kai.lim=0.1,initN.lim=1000,how.many=1,vnumber=2,hess=TRUE,ss2.arg="",
                      ss2.arg1st="",
                      filename.stored=NULL,adjust.cpuesd=FALSE,#replace.CPUE=c(T,T,T,T,T,T),
                      exclude.effNfleet=NULL,debug.mode=FALSE){
  #-----------------------------------------
  # 2008/01/17
  # Add the new argument of 'exclude.effNfleet': to determine the number of fleet where
  #                                          replacement of effective sample size won't be
  #                                          conducted
  #   ex. exclude.effNfleet=8 or exclude.effNfleet=c(8,9) etc.
  #-----------------------------------------
  ss2.grad <- kai.tmp <- ss2.LL <- numeric()

#  com <- command.alias(.Platform$OS.type)
#  if(.Platform$OS.type=="unix") convert.filename()

  paste2 <- function(x,...){
    paste(x,sep="",...)
  }

  # remove old files (is it not needed?)
  #  if(file.exists("ss2.cor")) file.remove("ss2.cor")
  #  if(file.exists("ss2.std")) file.remove("ss2.std")
  file.copy2(from=namesfile,to=paste2(namesfile,"_o"))
            #  com$system(paste2(com$rm," ss2.cor"))
            #  com$system(paste2(com$rm," ss2.std"))
            #  com$system(paste2(com$cp," ",namesfile," ",namesfile,"_o"))  

  # when the program is terminated, the namesfile (or starter file)
  #                                  is replaced with the original one
  tmpfunc <- function(){
    #    com$system(paste2(com$cp," ",namesfile," ",namesfile,"_last"))
    #    com$system(paste2(com$cp," ",namesfile,"_o ",namesfile))
    file.copy2(from=namesfile,to=paste2(namesfile,"_last"))
    file.copy2(from=paste2(namesfile,"_o"),to=namesfile)
    if(names(dev.cur())!="null device") dev.off()
  }
  on.exit(tmpfunc())

#  on.exit(print("on.exit is working"),append=TRUE)
#  on.exit(print("on.exit is working 2 "),append=TRUE)

  names.obj <- names.obj.o <- read.table(namesfile,as.is=T)
  forecast.obj <- read.table(forecastfile,as.is=T)

# only for the setting of unix machine -> work not well.  pending.  
#  if(.Platform$OS.type=="unix"){
#    system(paste("nkf -d", names.obj[1,1],"> `echo", names.obj[1,1],"| tr '[A-Z]' '[a-z]'`"))
#    system(paste("nkf -d", names.obj[2,1],"> `echo", names.obj[2,1],"| tr '[A-Z]' '[a-z]'`"))        
#    names.obj[1,1] <- tolower(tolower(names.obj[1,1]))
#    names.obj[2,1] <- tolower(tolower(names.obj[2,1]))
#  }


  # forecastの設定はいじらないことにする
  doforecast <- ifelse(vnumber<2,forecast.obj[2,1],names.obj[22,1])
  if(vnumber<2){
#    forecast.obj[2,1] <- "0"
#    com$system(paste2(com$cp," ",forecastfile," ",forecastfile,"_o"))      
#    write.table(forecast.obj,file=forecastfile,row.names=F,col.names=F,quote=FALSE)    
  }
  else{
#    names.obj[22,1] <- "0"    
  }

  if(vnumber>=2) names.obj[16,1] <- ifelse(is.null(retro.year),names.obj[16,1],retro.year)

#  system("cp test.txt tmp.txt")
  
  write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE)
  min.grad <- ifelse(vnumber<2,as.numeric(names.obj[16,1]),as.numeric(names.obj[15,1]))

  # Replace initial sample size, according to the option of "initN"
  if(is.null(datfile)) datfile <- names.obj[1,1]
  if(is.null(ctlfile)) ctlfile <- names.obj[2,1]
  
  b <- replacedat.effN(0,datfile=datfile,outfile="tmp.dat")
  
  nsmp.res <- array(-100,dim=c(length(b[[1]]),max.calc[2]+1,8))
  dimnames(nsmp.res) <- list(paste("Obs",1:length(b[[1]]),sep=""),
                             paste("Cal",1:(max.calc[2]+1),sep=""),
                             c("Original","Replaced","Pred_from","Eff_op1","Eff_op2","Eff_op3","Nsamp","fleet"))
  nsmp.res[,,8] <- b$age.comp.obs$Fisheries
  nsmp.res[,1,1] <- b$oldsmp
  initN <- switch(initN.op,
                  b$oldsmp,
                  cutlimit(b$oldsmp,lim=initN.lim),
                  constsmp(b$oldsmp,lim=initN.lim),
                  b$oldsmp*initN.lim)
  nsmp.res[,1,2] <- initN
  b <- replacedat.effN(initN,datfile=datfile,outfile="ss2-init.dat")

  # Record SD of CPUE set by dat file
  sd.cpue <- tapply(as.numeric(b$cpue[,5]),factor(b$cpue[,3],levels=1:max(as.numeric(b$cpue[,3]))),mean)
  sd.cpue <- ifelse(is.na(sd.cpue),0,sd.cpue)

  sd.cpue.mat <- matrix(0,length(sd.cpue),max.calc[2])
  dimnames(sd.cpue.mat) <- list(1:length(sd.cpue),paste("Cal",1:max.calc[2],sep=""))#
  sd.cpue.mat[,1:ifelse(max.calc[2]<3,max.calc[2],3)] <- sd.cpue
  

  # Replace names.nam file for reading "ss2-init.dat" as dat file  
  names.obj[1,1] <- "ss2-init.dat"
  names.obj[2,1] <- ctlfile
  if(vnumber<2){
    names.obj[4,1] <- "0"
  }
  else{
    names.obj[3,1] <- "0"
  }

  write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE)

  #---------------------- Start 1st calculation ---------------------------
  n.ss2 <- 1
  if(ss2.arg1st=="") ss2.arg1st <- ss2.arg
  if(debug.mode==FALSE){
    doss2(how.many=how.many,ss2.arg=ss2.arg1st)
  }
  ss2.grad[n.ss2] <- read.grad(parfile) # Read the value of gradient from parfile
  ss2.LL[n.ss2] <- read.like(parfile) # Read the value of gradient from parfile  

  if(store.repfile==TRUE) mvfile(filename.stored,n.ss2)    

  cat("1st calculation is finished with maximum gradient component of ",ss2.grad[n.ss2],".\n",file="process_track.txt")
  
  #---------------------- Start 2nd calculation ---------------------------
  n.ss2 <- 2

  if(vnumber<2){
    names.obj[4,1] <- "1"
  }
  else{
    names.obj[3,1] <- "1"
  }

  write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE)
  if(debug.mode==FALSE){
    doss2(how.many=how.many,ss2.arg=ss2.arg)
  }
  nsmp.res[,2,1] <- initN
  nsmp.res[,2,2] <- initN    
  ss2.grad[n.ss2] <- read.grad(parfile) # Read the value of gradient from parfile
  ss2.LL[n.ss2] <- read.like(parfile) # Read the value of gradient from parfile    

  if(store.repfile==TRUE) mvfile(filename.stored,n.ss2)
  
  cat("2nd calculation is finished with maximum gradient component of ",ss2.grad[n.ss2],".\n",file="process_track.txt",append=T)
  
  over.g <- 1
  n.ss2 <- n.ss2+1

  # For making it faster to replace number of sampling when effN.op==3.
  if(effN.op==3){
    cl0 <- count.fields(repfile,blank.lines.skip=FALSE)    
    tb0 <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl0),sep=""),as.is=T,
                     blank.lines.skip=FALSE)[,1:2]
    target0 <- getAgecomp.ss2(repfile,tb=tb0,cl=cl0)[[3]]
  }

  #---------------------- Start 3rd- calculation ---------------------------  
  # Automatical repeating of SS2 until it is converged.  
  repeat{
    agecomp <- getAgecomp.ss2.1(repfile)[[1]]
    nsmp.res[,n.ss2,4] <- agecomp$effN
    nsmp.res[,n.ss2,6] <- ifelse(effN.op==3,
                                 lengthdist(getAgecomp.ss2(repfile))$label$effN2,
                                 -100)
    nsmp.res[,n.ss2,7] <- agecomp$Nsamp

    # if effN.opt==2
    if(effN.op==2){
      if(is.null(effN.arg$coef)){
        effN.arg$coef <- as.list(numeric())
      }              
      if(effN.arg$by.fleet==FALSE){
        if(effN.arg$zero.intercept==TRUE){
          tmp <- lm(agecomp$effN~agecomp$Nsamp+0)
        }
        else{
          tmp <- lm(agecomp$effN~agecomp$Nsamp)
        }
        nsmp.res[,n.ss2,5] <- tmp$fitted.values
        effN.arg$coef[[n.ss2]] <- tmp$coef
        cat("Multiplier to effN ",tmp$coef,".\n",file="process_track.txt",append=T)
      }
      else{
        fleets <- nsmp.res[,1,8]
        nfleets <- unique(fleets)
        effN.arg$coef[[n.ss2]] <- matrix(0,length(nfleets),2,dimnames=list(nfleets,c("bias","intercept")))
        for(i in 1:length(nfleets)){        
          if(effN.arg$zero.intercept==TRUE){
            tmp <- lm(agecomp$effN[fleets==nfleets[i]]~agecomp$Nsamp[fleets==nfleets[i]]+0)
            effN.arg$coef[[n.ss2]][i,1] <- tmp$coef
          }
          else{
            tmp <- lm(agecomp$effN[fleets==nfleets[i]]~agecomp$Nsamp[fleets==nfleets[i]])
            effN.arg$coef[[n.ss2]][i,] <- c(tmp$coef[2],tmp$coef[1])
          }
          nsmp.res[fleets==nfleets[i],n.ss2,5] <- tmp$fitted.values
        }
        cat("Multiplier to effN ",round(effN.arg$coef[[n.ss2]],4),".\n",
            file="process_track.txt",append=T)        
      }}
    
    # Get effective sample size, according to the option of "effN.op"
    if(n.ss2==3){
      effN <- switch(effN.op,
                     nsmp.res[,n.ss2,4],
#                     nsmp.res[,n.ss2,4],#??
                     nsmp.res[,n.ss2,5],                     
                     nsmp.res[,n.ss2,6])
      datfile2 <- "ss2-init.dat"
    }
    else{
      effN <- switch(effN.op,
                     nsmp.res[,n.ss2,4],
                     nsmp.res[,n.ss2,5],
                     nsmp.res[,n.ss2,6])
      datfile2 <- "ss2-new.dat"
    }

    # Replace control file to adjust SD of CPUE
    if(n.ss2>3 && adjust.cpuesd==TRUE){
      sd.offset <- replace.sd.offset(ctrfile=names.obj[2,1],repfile=repfile,
                                     newctl="control_new.ctl",def.sd=sd.cpue,vnumber=vnumber)
      names.obj[2,1] <- "control_new.ctl"
      sd.cpue.mat[,n.ss2] <- sd.offset
      write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE)          
    }
    
    # Replace datfile with effective sample size, and create new datfile of "SS2-new.dat"
    # add the arguments of 'exclude.effNfleet' (2008/01/17)
    b <- replacedat.effN(effN,datfile=datfile2,outfile="ss2-new.dat",exclude.effNfleet=exclude.effNfleet)
    nsmp.res[,n.ss2,1] <- b$oldsmp    # Record old sample size
    nsmp.res[,n.ss2,2] <- b$newsmp    # Record new sample size
    nsmp.res[,n.ss2,3] <- lm(b$newsmp~b$oldsmp)$fitted.values
    kai.tmp[n.ss2] <- sum((b$newsmp-nsmp.res[,n.ss2,3])^2/
                          nsmp.res[,n.ss2,3])/length(b$newsmp)
      
    # Rewrite SS2names.nam to read "ss2-new.dat" as a datfile
    names.obj[1,1] <- "ss2-new.dat"
    write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE)    

    # Conduct SS2 using "SS2-new.dat"
    if(debug.mode==FALSE){
      doss2(how.many=how.many,ss2.arg=ss2.arg)
    }
    ss2.grad[n.ss2] <- read.grad(parfile) # Record gradient value.
    ss2.LL[n.ss2] <- read.like(parfile) # Read the value of gradient from parfile      

    if(store.repfile==TRUE)   mvfile(filename.stored,n.ss2)

    cat(n.ss2,"th calculation is finished with maximum gradient component of ",ss2.grad[n.ss2],".\n",file="process_track.txt",append=T)    
    nearlast.like <- read.like(parfile)

    answer2 <- NULL  #  Y.T. 20080411
    #-------------- 計算おわり: 以下、収束条件の判定  --------------------#
    is.finished <- FALSE
    if(ask.repeat==TRUE && max.calc[2]==n.ss2 &&
       (ss2.grad[n.ss2]>min.grad | kai.tmp[n.ss2] > kai.lim)){
      cat(" ** Max grad: ",ss2.grad[n.ss2],"\n ** Sum of kai squared: ",kai.tmp[n.ss2],"\n")
      if(!is.null(effN.arg$coef)){
        cat(" ** multiplier for effective sample size: ",round(effN.arg$coef[[n.ss2]][,1],3),"\n")
      }
      cat("The model can't be judged to be converged. Do you want to replace effective sample size? Enter Yes (Y) or No (N)  ")
      answer1 <- readline()
     
      YorN <- substr(answer1,1,1)=="Y"|substr(answer1,1,1)=="y"
      if(!YorN){
        cat("Do you want to run ss2 without replacing effective sample size until converging? Enter Yes (Y) or No (N)  ")
        answer2 <- readline()
        YorN2 <- substr(answer2,1,1)=="Y"|substr(answer2,1,1)=="y"
        if(YorN2){
          is.finished <- TRUE
          cat("How many? Enter the number.")
          last.run.number <- floor(as.numeric(readline()))
          while(is.na(last.run.number)){
            cat("Please enter numerial value!")
            last.run.number <- floor(as.numeric(readline()))
          }          
        }
        else{
          NULL    
        }
      }
      else{
        cat("How many? Enter the number.")
        add.calc <- floor(as.numeric(readline()))
        while(is.na(add.calc)){
          cat("Please enter numerial value!")
          add.calc <- floor(as.numeric(readline()))
        }
        max.calc[2] <- max.calc[2] + add.calc
        tmp <- nsmp.res
        nsmp.res <- array(-100,dim=c(length(b[[1]]),max.calc[2]+1,8))
        dimnames(nsmp.res) <- list(paste("Obs",1:length(b[[1]]),sep=""),
                                   paste("Cal",1:(max.calc[2]+1),sep=""),
                                   c("Original","Replaced","Pred_from","Eff_op1",
                                     "Eff_op2","Eff_op3","Nsamp","fleet"))
        nsmp.res[,1:n.ss2,] <- tmp[,1:n.ss2,]
        sd.cpue.mat <- cbind(sd.cpue.mat,matrix(0,nrow(sd.cpue.mat),ncol(sd.cpue.mat)))
        colnames(sd.cpue.mat) <- paste("Cal",1:ncol(sd.cpue.mat))
      }
    }

#    answer2 <- NULL    Y.T. 20080411     

    #-------------- Finish SS2 run: judging conversion criteria below-----#        
    # If the gradient value is smaller than "min.grad", which is read from given "SS2names.nam",
    #  and kai squared value is smaller than kai.lim, the calculation will be finished.  
    if((ss2.grad[n.ss2]<min.grad && kai.tmp[n.ss2] < kai.lim)|max.calc[2]==n.ss2|is.finished==TRUE ){
      names(nsmp.res)[n.ss2] <- "Last_est"
      a <- 0
      last.like <- 0

      #--------------- calculate hessian matrix -----------------------#
      if(hess==TRUE){
        if(!is.null(answer2)){
          #            browser()
          cat("YorN2=",YorN2,"\n")                              
          if(YorN2){
            last.run.tmp <- 1
            while(ss2.grad[n.ss2]>min.grad && last.run.tmp<=last.run.number){
              curCond<-(ss2.grad[n.ss2]>min.grad && last.run.tmp<=last.run.number)
             cat("currentCondition is ",curCond," gradient is ",ss2.grad[n.ss2]," last.run.tmp is ",last.run.tmp,"\n",file="process_track.txt",append=T)
              n.ss2 <- n.ss2+1
              if(debug.mode==FALSE) doss2(ss2.arg=ss2.arg)
              last.run.tmp <- last.run.tmp + 1              
              ss2.grad[n.ss2] <- read.grad(parfile)
              cat("Calculation without replacing effective sample size is finished with maximum gradient component of ",ss2.grad[n.ss2],".\n",file="process_track.txt",append=T)            
          }}
        }
#        curCond<-(ss2.grad[n.ss2]>min.grad && last.run.tmp<=last.run.number)
#        cat("After while loop, currentCondition is ",curCond," gradient is ",ss2.grad[n.ss2]," last.run.tmp is ",last.run.tmp,"\n",file="process_track.txt",append=T)
        if(debug.mode==FALSE) doss2.withhess(ss2.arg=ss2.arg)
      }
      last.grad <- read.grad(parfile)
      #       last.like <- read.like(parfile)
     
      cat("Last run was conducted for estimating hessian matrix.  The last value of max gradient is ",read.grad(parfile),".\n",file="process_track.txt",append=T)
      
      if(!is.null(retro.year)) cat("CHECK: The number of retrospective year was externally replaced to",retro.year,"from",names.obj.o[16,1],"by this R function.\n")
#          a <- read.table("ss2.std",skip=1)
#    }


      #------------------ Terminal message ----------------------------#
      if(n.ss2 >= max.calc[2])
        cat("The last max grad is",last.grad,
            " and sum of kai squared is", kai.tmp[n.ss2],
            ", \n but the number of calculation exceeds",
            max.calc[2],
            "times. Then the calculation was forced to be finished.
            The calculation might not be converged. Please check the condition!\n")
      else
        cat("The gradient value is smaller than ",min.grad,
            " and kai squared statics also smaller than", kai.lim,
            " (or number of calculation exceeds max.calc[2]),",
            "then the calculation is finished successflly. Congraturation!\n")
      
      break
    }
    

    #----------------- Extraordinal temination when extremely large gradient --------------------#
    # If the gradient value is extremely large for max.calc[1] times, the calculation
    # will be forced to be finished.
    #--------------------------------------------------------------------------------------------#
    
    if(ss2.grad[n.ss2]>max.grad){
      over.g <- over.g+1
      if(over.g > max.calc[1]){
        cat("The gradient value is ",ss2.grad[n.ss2],", which exceeded ",max.grad," ",max.calc[1]," times. Because this calculation would not be converged, the calculation is forced to be stopped.\n")
        if(!is.null(retro.year)) cat("CHECK: The number of retrospective year was externally replaced from",retro.year,"from",names.obj.o[16,1],"by this R function.")
        break
      }}

    n.ss2 <- n.ss2+1
  }


  #------------------ Processing after calculation ---------------------------------------------#
  agecomp <- getAgecomp.ss2.1(repfile)[[1]]
  nsmp.res[,dim(nsmp.res)[[2]],4] <- agecomp$effN
  nsmp.res[,dim(nsmp.res)[[2]],6] <- ifelse(effN.op==3,lengthdist(getAgecomp.ss2(repfile))$label$effN2,-100)
  nsmp.res[,dim(nsmp.res)[[2]],5] <- lm(agecomp$effN~agecomp$Nsamp)$fitted.values    
  nsmp.res[,dim(nsmp.res)[[2]],7] <- agecomp$Nsamp      

  
  # Output results of gradient and effective sample size to "eff_smp_track.txt")
  write.table(cbind(ss2.grad,kai.tmp,ss2.LL),file="eff_smp_track.txt")
  for(i in 1:dim(nsmp.res)[[2]]){
    cat(i,"th calc:\n",file="eff_smp_track.txt",append=T)
    write.table(nsmp.res[,i,],file="eff_smp_track.txt",append=T,row.names=FALSE)
  }

  # Replace names.nam file to the original one.  
  #com$system(paste2(com$cp," ",namesfile," ",namesfile,"_last"))
  #com$system(paste2(com$cp," ",namesfile,"_o ",namesfile))
#  if(vnumber<2) com$system(paste2(com$cp," ",forecastfile,"_o ",forecastfile))

  if(!is.null(filename.stored)){
#    com$system(paste2(com$cp," ss2.rep ","ss2",filename.stored[2],".rep"))        
#    com$system(paste2(com$cp," ss2.par ","ss2",filename.stored[2],".par"))
#    com$system(paste2(com$cp," eff_smp_track.txt ","eff_smp_track",filename.stored[2],".txt"))
#    com$system(paste2(com$cp," process_track.txt ","process_track",filename.stored[2],".txt"))
    if(substr(filename.stored[1],
              nchar(filename.stored[1]),
              nchar(filename.stored[1]))!="/")
      filename.stored[1] <- paste2(filename.stored[1],"/")

    if(!file.exists(filename.stored[1])) dir.create(filename.stored[1])
    
    file.copy2(from="ss2.rep",
              to=paste2(filename.stored[1],"/ss2",filename.stored[2],".rep"))
    file.copy2(from="ss2.par",
              to=paste2(filename.stored[1],"/ss2",filename.stored[2],".par"))
    file.copy2(from="eff_smp_track.txt",
              to=paste2(filename.stored[1],"/eff_smp_track",filename.stored[2],".txt"))
    file.copy2(from="process_track.txt",
              to=paste2(filename.stored[1],"/process_track",filename.stored[2],".txt"))

#    com$system(paste2(com$mv," ss2",filename.stored[2],".rep ",filename.stored[1]))        
#    com$system(paste2(com$mv," ss2",filename.stored[2],".par ",filename.stored[1]))
#    com$system(paste2(com$mv," eff_smp_track",filename.stored[2],".txt ",filename.stored[1]))
#    com$system(paste2(com$mv," process_track",filename.stored[2],".txt ",filename.stored[1]))
#    com$system(paste2(com$mv," ss2",filename.stored[2],".rep ",filename.stored[1]))        
#    com$system(paste2(com$mv," ss2",filename.stored[2],".par ",filename.stored[1]))
#    com$system(paste2(com$mv," eff_smp_track",filename.stored[2],".txt ",filename.stored[1]))
#    com$system(paste2(com$mv," process_track",filename.stored[2],".txt ",filename.stored[1]))          
  }

  if(effN.op!=2){
    res <- list(grad=ss2.grad,nsmp.res=nsmp.res,
                sd.cpue.mat=sd.cpue.mat,LL=ss2.LL)
  }
  else{
    res <- list(grad=ss2.grad,nsmp.res=nsmp.res,
                sd.cpue.mat=sd.cpue.mat,LL=ss2.LL,effN.coef=effN.arg$coef)    
  }
  if(process.graph==TRUE && debug.mode==FALSE){
    plot.processgraph(res)
  }

  return(invisible(res))
}

doss3 <-
function(how.many=1,ss3.arg=""){
  for(i in 1:how.many){
    if(.Platform$OS.type=="unix"){
      system("cp ss3.par SS3.PAR")        
      system(paste("ss3 ",ss3.arg))
    }
    else{
      shell(paste("ss3.exe ",ss3.arg))
    }
  }
}

ex.mat <-
function(mattemp,rm.zero=FALSE){
  a1 <- rep(rownames(mattemp),ncol(mattemp))
  a2 <- rep(0,n <- ncol(mattemp)*nrow(mattemp))
  for(i in c(1:n)) a2[i] <- colnames(mattemp)[ceiling(i/nrow(mattemp))]
  a3 <- c(mattemp)

  res <- rbind(as.numeric(a1),as.numeric(a2),as.numeric(a3))
  
  if(rm.zero==TRUE) res <- res[,res[3,]!=0]
  res
}

ex.mat2 <-
function(mattemp,rm.zero=FALSE){
  a1 <- rep(rownames(mattemp),ncol(mattemp))
  a2 <- rep(0,n <- ncol(mattemp)*nrow(mattemp))
  for(i in c(1:n)) a2[i] <- colnames(mattemp)[ceiling(i/nrow(mattemp))]
  a3 <- c(mattemp)

  res <- data.frame(x=a1,y=a2,num=as.numeric(a3))
  
  if(rm.zero==TRUE) res <- res[res$num!=0,]
  res
}

expand.category <-
function(num,category,label){
  n <- sum(num)
  res <- data.frame(label=factor(NA,level=levels(label)),category=rep(0,n))
  s <- 0
  for(i in 1:length(num)){
    range <- (s+1):(s+num[i])
    res$label[range] <- label[i]
    res$category[range] <- category[i]
    s <- s+num[i]
  }
  res
}

expand.category0 <-
function(num,category,category.name){
  res <- data.frame(category=0,num=0)
  for(i in 1:length(num)){
    res <- rbind(res,data.frame(category=category.name[i],num=rep(category[i],num[i])))
  }
  res[-1,]
}

file.copy2 <-
function(from,to,...){
  file.copy(from=from,to=to,overwrite=T,...)
}

find.and.read.table <-
function(findseq,skipline,startpoint,gyou,comment.char="#",
			table.property,outfile,h=TRUE,is.ss2=FALSE,gyou.margin=0,...){
    
  
  for(i in c(startpoint:(length(table.property)-1))){
#    tmp <- read.table(outfile,skip=i-1,nrow=1)[1]
#    cat(i,":",as.character(tmp)," ")
    if(table.property[i]!=0 && read.table(outfile,skip=i-1,nrow=1,quote="")[1]==findseq){
      if(is.null(gyou)) gyou <- count.next.line(i,skipline,table.property)-i-skipline-gyou.margin
      a <- read.table(outfile,skip=i+skipline,header=FALSE,nrow=gyou,comment.char=comment.char,...)
      if(h==TRUE) {
        a.name <- read.table(outfile,skip=i+skipline-1,header=FALSE,nrow=1,colClasses="character")#as.is=TRUE)
        dimnames(a) <- list(a[,1],as.character(a.name))
      }
      if(is.ss2==FALSE){
        a <- a[,-1]
      }
      startpoint <-  i + gyou
      break
    }
  }
  list(a,startpoint)
}

find.and.read.table0 <-
function(findseq,skipline,startpoint,gyou,
			table.property,outfile,h=TRUE,is.ss2=FALSE,...){

  for(i in c(startpoint:(length(table.property)-1))){
#    tmp <- read.table(outfile,skip=i-1,nrow=1)[1]
#    cat(i,":",as.character(tmp)," ")
    if(table.property[i]!=0 && read.table(outfile,skip=i-1,nrow=1)[1]==findseq){
      if(is.null(gyou)) gyou <- count.next.line(i,skipline,table.property)-i-skipline
      a <- read.table(outfile,skip=i+skipline,header=FALSE,nrow=gyou,...)
      if(h==TRUE) {
        a.name <- read.table(outfile,skip=i+skipline-1,header=FALSE,nrow=1,colClasses="character")#as.is=TRUE)
        dimnames(a) <- list(a[,1],as.character(a.name))
      }
      if(is.ss2==FALSE){
        a <- a[,-1]
      }
      startpoint <-  i + gyou
      break
    }
  }
  list(a,startpoint)
}

find.and.read.table2 <-
function(findseq,skipline,gyou,comment.char="#",
			table.property,tb,outfile,h=TRUE,is.ss2=FALSE,gyou.margin=0,
                                target.line=NULL,...){
#  for(i in c(startpoint:(length(table.property)-1))){
#  if(table.property[i]!=0 && read.table(outfile,skip=i-1,nrow=1)[1]==findseq){

  if(is.null(target.line)){
#    tb <- read.table(outfile,fill=T,col.names=paste("V",1:max(table.property),sep=""),as.is=T,
#                     blank.lines.skip=FALSE)
    tmp <- 1:nrow(tb)
    target.line <- tmp[tb[,1]==findseq]
    if(length(target.line)==0) cat("Cannot find target line!!!")
  }
  if(is.null(gyou)) gyou <- count.next.line(target.line,skipline,table.property[1:nrow(tb)])-target.line-skipline-gyou.margin
  a <- read.table(outfile,skip=target.line+skipline,header=FALSE,
                  nrow=gyou,comment.char=comment.char,...)
                  
  if(h==TRUE) {
    a.name <- read.table(outfile,skip=target.line+skipline-1,header=FALSE,nrow=1,colClasses="character")#as.is=TRUE)
    dimnames(a) <- list(a[,1],as.character(a.name))
  }
  if(is.ss2==FALSE){
    a <- a[,-1]
  }
  list(a,target.line+gyou)
}

find.and.read.table3 <-
function(findseq,skipline,gyou,
			table.property,tb,outfile,h=TRUE,is.ss2=FALSE,
                                target.line=NULL,...){

#  for(i in c(startpoint:(length(table.property)-1))){
#  if(table.property[i]!=0 && read.table(outfile,skip=i-1,nrow=1)[1]==findseq){

  if(is.null(target.line)){
#    tb <- read.table(outfile,fill=T,col.names=paste("V",1:max(table.property),sep=""),as.is=T,
#                     blank.lines.skip=FALSE)
    tmp <- 1:nrow(tb)
    target.line <- tmp[tb[,1]==findseq]
  }
  res <- as.list(rep(0,length(target.line)+1))
  gyou0 <- rep(0,length(target.line))
  
  for(k in 1:length(target.line)){
    if(is.null(gyou)){
      gyou0[k] <- count.next.line(target.line[k],skipline+1,table.property[1:nrow(tb)])-target.line[k]-skipline
    }
    else{
      gyou0[k] <- gyou
    }

    a <- read.table(outfile,skip=target.line[k]+skipline,header=FALSE,nrow=gyou0[k],fill=T,col.names=c("V",1:max(cl)),...)
    a <- a[,apply(!is.na(a),2,sum)!=0]
    if(h==TRUE) {
      a.name <- read.table(outfile,skip=target.line[k]+skipline-1,header=FALSE,nrow=1,colClasses="character")#as.is=TRUE)
      dimnames(a) <- list(a[,1],as.character(a.name))
    }
    if(is.ss2==FALSE){
      a <- a[,-1]
    }
    res[[k]] <- a
  }
  res[[k+1]] <- target.line
  res
}

geomean <-
function(x)
{
  ifelse(all(x > 0), exp(mean(log(x))), NA)
}

get.tablename <-
function(x,FUN="names"){
  tmpfun <- get(FUN)
  xlabel <- tmpfun(x)
  return(as.numeric(as.character(xlabel)))
}

getALK.ss <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,#target.line=NULL,
                                    all=FALSE, qt=4){
  # 2008/4/1: modified to read all data by quarter with the option of (all=TRUE, and qt=4)
  
  vskipline <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile),3,6)
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
#  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  name.label <- find.and.read.table2("AGE_LENGTH_KEY",skipline=vskipline-1,gyou=1,
                                     table.property=cl,tb=tb,
                                     outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
  ALK <- find.and.read.table2("AGE_LENGTH_KEY",skipline=vskipline,gyou=NULL,tb=tb,
                              table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
                              target.line=name.label[[2]])
  ALK[[2]] <- ALK[[2]]
#  }
#  else{
#    name.label <- find.and.read.table("AGE_LENGTH_KEY",skipline=vskipline-1,gyou=1,
#                                       table.property=cl,startpoint=target.line-50,
#                                       outfile=repfile,h=FALSE,is.ss2=TRUE,
#                                      colClasses="character")  
#    ALK <- find.and.read.table("AGE_LENGTH_KEY",skipline=vskipline,gyou=NULL,
#                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
#                               startpoint=target.line-50)
#  }
  colnames(ALK[[1]]) <- as.character(name.label[[1]])  

  if(all==TRUE){
    tmp <- ALK[[1]]
    tmp.line <- ALK[[2]]-nrow(ALK[[1]])
    
    res <- list()
    res[[1]] <- tmp

    desc <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile),"SEASON:","Seas:")
    desc.line <- which(tb[,1]==desc)
      
    for(i in 1:length(desc.line)){
#      tmp <- find.and.read.table(desc,skipline=1,gyou=NULL,
#                                 table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
#                                 startpoint=tmp.line+1)
#      browser()
      tmp <- find.and.read.table2(desc,skipline=1,gyou=NULL,tb=tb,
                                 table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
                                  target.line=desc.line[i])
      
      res[[i]] <- tmp[[1]]
      dimnames(res[[i]]) <- dimnames(res[[1]])
      tmp.line <- tmp[[2]]-nrow(ALK[[1]])
    }
    ALK[[1]] <- res
  }

  list(ALK[[1]],ALK[[2]])
}

getALK.ss2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,#target.line=NULL,
                                    all=FALSE, qt=4){
  # 2008/4/1: modified to read all data by quarter with the option of (all=TRUE, and qt=4)
  
  vskipline <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile),3,6)
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
#  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  name.label <- find.and.read.table2("AGE_LENGTH_KEY",skipline=vskipline-1,gyou=1,
                                     table.property=cl,tb=tb,
                                     outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
  ALK <- find.and.read.table2("AGE_LENGTH_KEY",skipline=vskipline,gyou=NULL,tb=tb,
                              table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
                              target.line=name.label[[2]])
  ALK[[2]] <- ALK[[2]]
#  }
#  else{
#    name.label <- find.and.read.table("AGE_LENGTH_KEY",skipline=vskipline-1,gyou=1,
#                                       table.property=cl,startpoint=target.line-50,
#                                       outfile=repfile,h=FALSE,is.ss2=TRUE,
#                                      colClasses="character")  
#    ALK <- find.and.read.table("AGE_LENGTH_KEY",skipline=vskipline,gyou=NULL,
#                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
#                               startpoint=target.line-50)
#  }
  colnames(ALK[[1]]) <- as.character(name.label[[1]])  

  if(all==TRUE){
    tmp <- ALK[[1]]
    tmp.line <- ALK[[2]]-nrow(ALK[[1]])
    
    res <- list()
    res[[1]] <- tmp

    desc <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile),"SEASON:","Seas:")
    desc.line <- which(tb[,1]==desc)
      
    for(i in 1:length(desc.line)){
#      tmp <- find.and.read.table(desc,skipline=1,gyou=NULL,
#                                 table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
#                                 startpoint=tmp.line+1)
#      browser()
      tmp <- find.and.read.table2(desc,skipline=1,gyou=NULL,tb=tb,
                                 table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
                                  target.line=desc.line[i])
      
      res[[i]] <- tmp[[1]]
      dimnames(res[[i]]) <- dimnames(res[[1]])
      tmp.line <- tmp[[2]]-nrow(ALK[[1]])
    }
    ALK[[1]] <- res
  }

  list(ALK[[1]],ALK[[2]])
}

getAgecomp.ss.1 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,
                                                #target.line=NULL,
                                                len=TRUE){
  desc <- ifelse(len==TRUE,"FIT_LEN_COMPS","FIT_AGE_COMPS")
#  if(is.null(target.line)){
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  res <- find.and.read.table2(desc,skipline=1,gyou=NULL,
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
#}
#  else{
#    cl <- count.fields(repfile,blank.lines.skip=FALSE)
#    res <- find.and.read.table(desc,skipline=1,startpoint=target.line-50,gyou=NULL,
#                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)    
#  }
  if(len==TRUE){
    colnames(res[[1]]) <- c("Index","Year","Seas","Gender","Mkt","Nsamp","effN","Like")
  }
  else{
    colnames(res[[1]]) <- 
      c("Index","Year","Seas","Gender","Mkt","Ageerr","Lbin_lo","Lbin_hi","Nsamp","effN","Like")
  }
  res
}

getAgecomp.ss.2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,
                                                compfile=NULL){ #target.line=NULL,
  #----- preliminary coding for adjusting 2.00o
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  if((is.ss2.2(repfile) && substr(a[1,1],16,20)>"2.00o") || (is.ss3(repfile) && vnumber.ss3(repfile)<3.03)){
    #--  for version from 2.02o to 3.02
    name.tmp <- c("year","season","fleet","rep","pick_gender",
                  "kind","mkt","ageerr","gender","Lbin_lo","Lbin_hi",
                  "bin","obs","exp","Pearson","N","effN","like","Used")
  }
  else{
    if(is.ss3(repfile) & vnumber.ss3(repfile)>=3.03){
      #--  for newer than 3.03 (read compfile) 
      name.tmp <- c("year","season","fleet","rep","pick_gender","kind","Part","ageerr","gender","Lbin_lo","Lbin_hi","bin","obs","exp","Pearson","N","effN","Like","Cum_obs","Cum_exp","Used")
      if(is.null(compfile)){
        stop(message="This version is newer than 3.30.  Please specify length composition file, named \"CompReport.SSO\" with argument of \"compfile=\"!")
      }
      else{
        repfile <- compfile
        cl <- count.fields(repfile,blank.lines.skip=FALSE)
        tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)        
      }
    }
    else{
      #--  for version older than 2.02o 
      name.tmp <- c("year","season","fleet","rep","pick_gender","kind",
                    "mkt","ageerr","gender","Lbin_lo","Lbin_hi","bin",
                    "obs","exp","Pearson","N","effN","Used")
    }}
  type.tmp <- c("character",rep("numeric",4),"character",rep("numeric",length(name.tmp)-6))

  #----- preliminary coding for adjusting >3.00
  if(is.ss3(repfile)){
    gyou.margin <- 2
  }
  else{
    gyou.margin <- 0
  }

#  if(is.null(target.line)){
    if(is.null(cl)){
      cl <- count.fields(repfile,blank.lines.skip=FALSE)
    }
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    res <- find.and.read.table2("Composition_Database",skipline=1,
                                gyou=NULL,table.property=cl,tb=tb,gyou.margin=gyou.margin,
                                outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T,
                                colClasses=type.tmp,col.names=name.tmp)
#  }
#  else{
#    cl <- count.fields(repfile,blank.lines.skip=FALSE)  
#    res <- find.and.read.table("Composition_Database",skipline=1,startpoint=target.line-50,
#                               gyou=NULL,table.property=cl,gyou.margin=gyou.margin,
#                               outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T,
#                               colClasses=type.tmp,col.names=name.tmp)    
#  }
#  colnames(res[[1]]) <- name.tmp
  res
}

getAgecomp.ss <-
function(repfile="ss2.rep",tb=NULL,
                                            cl=NULL,len=T,compfile=NULL){#target.line=NULL,
#  composition.database <- getAgecomp.ss2.2(repfile,tb=tb,cl=cl,target.line=target.line[1],compfile=compfile)  
#  fit.len.comps <- getAgecomp.ss2.1(repfile,tb=tb,cl=cl,target.line=target.line[2],len=len)

  composition.database <- getAgecomp.ss2.2(repfile,tb=tb,cl=cl,compfile=compfile)  
  fit.len.comps <- getAgecomp.ss2.1(repfile,tb=tb,cl=cl,len=len)
  list(composition.database[[1]],fit.len.comps[[1]],
       c(composition.database[[2]],fit.len.comps[[2]]))
}

getAgecomp.ss2.1 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,
                                                #target.line=NULL,
                                                len=TRUE){
  desc <- ifelse(len==TRUE,"FIT_LEN_COMPS","FIT_AGE_COMPS")
#  if(is.null(target.line)){
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  res <- find.and.read.table2(desc,skipline=1,gyou=NULL,
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
#}
#  else{
#    cl <- count.fields(repfile,blank.lines.skip=FALSE)
#    res <- find.and.read.table(desc,skipline=1,startpoint=target.line-50,gyou=NULL,
#                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)    
#  }
  if(len==TRUE){
    colnames(res[[1]]) <- c("Index","Year","Seas","Gender","Mkt","Nsamp","effN","Like")
  }
  else{
    colnames(res[[1]]) <- 
      c("Index","Year","Seas","Gender","Mkt","Ageerr","Lbin_lo","Lbin_hi","Nsamp","effN","Like")
  }
  res
}

getAgecomp.ss2.2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,
                                                compfile=NULL){ #target.line=NULL,
  #----- preliminary coding for adjusting 2.00o
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  if((is.ss2.2(repfile) && substr(a[1,1],16,20)>"2.00o") || (is.ss3(repfile) && vnumber.ss3(repfile)<3.03)){
    #--  for version from 2.02o to 3.02
    name.tmp <- c("year","season","fleet","rep","pick_gender",
                  "kind","mkt","ageerr","gender","Lbin_lo","Lbin_hi",
                  "bin","obs","exp","Pearson","N","effN","like","Used")
  }
  else{
    if(is.ss3(repfile) & vnumber.ss3(repfile)>=3.03){
      #--  for newer than 3.03 (read compfile) 
      name.tmp <- c("year","season","fleet","rep","pick_gender","kind","Part","ageerr","gender","Lbin_lo","Lbin_hi","bin","obs","exp","Pearson","N","effN","Like","Cum_obs","Cum_exp","Used")
      if(is.null(compfile)){
        stop(message="This version is newer than 3.30.  Please specify length composition file, named \"CompReport.SSO\" with argument of \"compfile=\"!")
      }
      else{
        repfile <- compfile
        cl <- count.fields(repfile,blank.lines.skip=FALSE)
        tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)        
      }
    }
    else{
      #--  for version older than 2.02o 
      name.tmp <- c("year","season","fleet","rep","pick_gender","kind",
                    "mkt","ageerr","gender","Lbin_lo","Lbin_hi","bin",
                    "obs","exp","Pearson","N","effN","Used")
    }}
  type.tmp <- c("character",rep("numeric",4),"character",rep("numeric",length(name.tmp)-6))

  #----- preliminary coding for adjusting >3.00
  if(is.ss3(repfile)){
    gyou.margin <- 2
  }
  else{
    gyou.margin <- 0
  }

#  if(is.null(target.line)){
    if(is.null(cl)){
      cl <- count.fields(repfile,blank.lines.skip=FALSE)
    }
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    res <- find.and.read.table2("Composition_Database",skipline=1,
                                gyou=NULL,table.property=cl,tb=tb,gyou.margin=gyou.margin,
                                outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T,
                                colClasses=type.tmp,col.names=name.tmp)
#  }
#  else{
#    cl <- count.fields(repfile,blank.lines.skip=FALSE)  
#    res <- find.and.read.table("Composition_Database",skipline=1,startpoint=target.line-50,
#                               gyou=NULL,table.property=cl,gyou.margin=gyou.margin,
#                               outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T,
#                               colClasses=type.tmp,col.names=name.tmp)    
#  }
#  colnames(res[[1]]) <- name.tmp
  res
}

getAgecomp.ss2 <-
function(repfile="ss2.rep",tb=NULL,
                                            cl=NULL,len=T,compfile=NULL){#target.line=NULL,
#  composition.database <- getAgecomp.ss2.2(repfile,tb=tb,cl=cl,target.line=target.line[1],compfile=compfile)  
#  fit.len.comps <- getAgecomp.ss2.1(repfile,tb=tb,cl=cl,target.line=target.line[2],len=len)

  composition.database <- getAgecomp.ss2.2(repfile,tb=tb,cl=cl,compfile=compfile)  
  fit.len.comps <- getAgecomp.ss2.1(repfile,tb=tb,cl=cl,len=len)
  list(composition.database[[1]],fit.len.comps[[1]],
       c(composition.database[[2]],fit.len.comps[[2]]))
}

getBabs.ss <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2("TIME_SERIES",skipline=0,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table2("TIME_SERIES",skipline=1,gyou=NULL,comment.char="",
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  else{
    name.label <- find.and.read.table("TIME_SERIES",skipline=0,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table("TIME_SERIES",skipline=1,startpoint=target.line-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  colnames(res[[1]]) <- as.character(name.label[[1]])
  if(is.ss3(repfile)){
    tmp <- cbind(c("Yr","year"),
          c("Era","period"),
          c("Seas","season"),
          c("Bio_all","bio-all"),
          c("Bio_smry","bio-smry"),
          c("Recruit_0","recruit-0"))#,
#          c("enc(B):_1","enc_catch:_1"),
#          c("dead(B):_1","dead_catch:_1"),
#          c("retain(B):_1","ret_catch:_1"),
#          c("obs_cat:_1","obs_cat:_1"),
#          c("F:_1","Hrate-1"))
    colnames(res[[1]])[match(tmp[1,],colnames(res[[1]]))] <- tmp[2,]
    res[[1]]$"SpawnBio" <- as.numeric(as.character(res[[1]]$"SpawnBio"))    
  }
  res
}

getBabs.ss2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2("TIME_SERIES",skipline=0,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table2("TIME_SERIES",skipline=1,gyou=NULL,comment.char="",
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  else{
    name.label <- find.and.read.table("TIME_SERIES",skipline=0,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table("TIME_SERIES",skipline=1,startpoint=target.line-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  colnames(res[[1]]) <- as.character(name.label[[1]])
  if(is.ss3(repfile)){
    tmp <- cbind(c("Yr","year"),
          c("Era","period"),
          c("Seas","season"),
          c("Bio_all","bio-all"),
          c("Bio_smry","bio-smry"),
          c("Recruit_0","recruit-0"))#,
#          c("enc(B):_1","enc_catch:_1"),
#          c("dead(B):_1","dead_catch:_1"),
#          c("retain(B):_1","ret_catch:_1"),
#          c("obs_cat:_1","obs_cat:_1"),
#          c("F:_1","Hrate-1"))
    colnames(res[[1]])[match(tmp[1,],colnames(res[[1]]))] <- tmp[2,]
    res[[1]]$"SpawnBio" <- as.numeric(as.character(res[[1]]$"SpawnBio"))    
  }
  res
}

getCAA.ss <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "CATCH_AT_AGE"
  line.tmp <- ifelse(is.ss3(repfile),-1,0)
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=1+line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    caa <- find.and.read.table2(read.char,skipline=abs(line.tmp),gyou=NULL,fill=T,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="",colClasses="character")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=1+line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    caa <- find.and.read.table(read.char,skipline=abs(line.tmp),startpoint=name.label[[2]]-10,gyou=NULL,colClasses="character",
                               table.property=cl,comment.char="",fill=T,
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  colnames(caa[[1]]) <- as.character(name.label[[1]])  

  if(!is.ss3(repfile)){  # The case older than SS3
    elimit.line <- fleet <- area <- gmorph <- numeric()
    s <- 0
    for(i in 1:nrow(caa[[1]])){
      if(caa[[1]][i,1]=="fleet"){
        s <- s+1      
        elimit.line[s] <- i
      }
      fleet[i] <- caa[[1]][elimit.line[s],2]
      area[i] <- caa[[1]][elimit.line[s],4]
      gmorph[i] <- caa[[1]][elimit.line[s],6]        
    }
    YQ <- as.numeric(caa[[1]]$Year)+(as.numeric(caa[[1]]$Seas)/4)-0.25
    caa[[1]] <- cbind(fleet,area,gmorph,YQ,caa[[1]])
    caa[[1]] <- caa[[1]][c(-elimit.line,-(elimit.line+1)),]
  }
  else{
    caa[[1]]$Year <- caa[[1]]$Yr
    caa[[1]]$fleet <- caa[[1]]$Fleet
    caa[[1]]$YQ <- as.numeric(caa[[1]]$Year)+(as.numeric(caa[[1]]$Seas)/4)-0.25
  }

  age.cols <- !is.na(as.numeric(dimnames(caa[[1]])[[2]]))
  tmp <- !is.na(caa[[1]]$YQ)
  caa.array <- array(0,dim=c(length(unique(caa[[1]]$YQ[tmp])),
                         sum(age.cols),length(unique(caa[[1]]$fleet))))
  
  s <- 1
  #  browser()
  #  startcol <- ifelse(is.ss3(repfile),11,7)
  for(i in which(age.cols)){
    caa.array[,s,] <- tmp2 <- tapply(as.numeric(caa[[1]][tmp,i]),
                                    list(caa[[1]]$YQ[tmp],
                                         as.factor(unfactor(caa[[1]]$fleet[tmp]))),sum)
    s <- s+1
  }
  dimnames(caa.array) <- list(rownames(tmp2),colnames(caa[[1]])[age.cols],colnames(tmp2))    
  list(caa=caa[[1]],target.line=caa[[2]],caa.array=caa.array)
}

getCAA.ss2 <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "CATCH_AT_AGE"
  line.tmp <- ifelse(is.ss3(repfile),-1,0)
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=1+line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    caa <- find.and.read.table2(read.char,skipline=abs(line.tmp),gyou=NULL,fill=T,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="",colClasses="character")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=1+line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    caa <- find.and.read.table(read.char,skipline=abs(line.tmp),startpoint=name.label[[2]]-10,gyou=NULL,colClasses="character",
                               table.property=cl,comment.char="",fill=T,
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  colnames(caa[[1]]) <- as.character(name.label[[1]])  

  if(!is.ss3(repfile)){  # The case older than SS3
    elimit.line <- fleet <- area <- gmorph <- numeric()
    s <- 0
    for(i in 1:nrow(caa[[1]])){
      if(caa[[1]][i,1]=="fleet"){
        s <- s+1      
        elimit.line[s] <- i
      }
      fleet[i] <- caa[[1]][elimit.line[s],2]
      area[i] <- caa[[1]][elimit.line[s],4]
      gmorph[i] <- caa[[1]][elimit.line[s],6]        
    }
    YQ <- as.numeric(caa[[1]]$Year)+(as.numeric(caa[[1]]$Seas)/4)-0.25
    caa[[1]] <- cbind(fleet,area,gmorph,YQ,caa[[1]])
    caa[[1]] <- caa[[1]][c(-elimit.line,-(elimit.line+1)),]
  }
  else{
    caa[[1]]$Year <- caa[[1]]$Yr
    caa[[1]]$fleet <- caa[[1]]$Fleet
    caa[[1]]$YQ <- as.numeric(caa[[1]]$Year)+(as.numeric(caa[[1]]$Seas)/4)-0.25
  }

  age.cols <- !is.na(as.numeric(dimnames(caa[[1]])[[2]]))
  tmp <- !is.na(caa[[1]]$YQ)
  caa.array <- array(0,dim=c(length(unique(caa[[1]]$YQ[tmp])),
                         sum(age.cols),length(unique(caa[[1]]$fleet))))
  
  s <- 1
  #  browser()
  #  startcol <- ifelse(is.ss3(repfile),11,7)
  for(i in which(age.cols)){
    caa.array[,s,] <- tmp2 <- tapply(as.numeric(caa[[1]][tmp,i]),
                                    list(caa[[1]]$YQ[tmp],
                                         as.factor(unfactor(caa[[1]]$fleet[tmp]))),sum)
    s <- s+1
  }
  dimnames(caa.array) <- list(rownames(tmp2),colnames(caa[[1]])[age.cols],colnames(tmp2))    
  list(caa=caa[[1]],target.line=caa[[2]],caa.array=caa.array)
}

getCPUE.ss <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){

  vnumber2 <- is.ss2.2(repfile)
  vnumber3 <- is.ss3(repfile)

  index1.char <- ifelse(!vnumber2&!vnumber3,"index","INDEX_1")
  vskipline <- ifelse(!vnumber2&!vnumber3,0,1)
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  

  #  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  # read index_2
  res <- find.and.read.table2("INDEX_2",skipline=1,gyou=NULL,comment.char="",
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T)
  if(ncol(res[[1]])==10){
    colnames(res[[1]]) <- c("index","year","vuln_bio","obs","exp","eff_Q","SE","Dev","Like","Like+log(s)")
  }
  else{
    colnames(res[[1]]) <- c("index","year","Seas","vuln_bio","obs","exp","calc_Q","eff_Q","SE","Dev","Like","Like+log(s)")    
  }
  if(vnumber3){
    a <- strsplit(as.character(res[[1]]$index),"_")
    aa <- numeric()
    for(i in 1:length(a)){
      aa[i] <- a[[i]][1]
    }
#    res[[1]]$index <- as.numeric(t(as.matrix(as.data.frame(strsplit(as.character(res[[1]]$index),"_"))[1,])))
    res[[1]]$index <- as.numeric(aa)#as.numeric(t(as.matrix(as.data.frame()[1,])))      
  }
  #----- ad hoc solution to avoid the problems of "1.#QNAN"--------
  res[[1]]$exp <- as.numeric(as.character(res[[1]]$exp))
  #----------------------------------------------------------------  
  
  # read index_1  
  res[[3]] <- find.and.read.table(index1.char,skipline=vskipline,
                                  gyou=max(as.numeric(gsub("_","",substr(res[[1]][,1],1,2)))),
                                  startpoint=res[[2]]+3,
                                  comment.char="",
                                  table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)[[1]]      

  if(vnumber2){
    colnames(res[[3]]) <- c("Index","Do_Power","Power","Do_Env_var","Env_Link","Do_ExtraVar","Qtype ","Q","Num=0/Bio=1","Err_type","N","Npos","r.m.s.e.","mean_input_SE","mean_(Input+extra)_SE","pen_mean_Qdev","rmse_Qdev")
  }
  else{
    if(vnumber3){
      colnames(res[[3]]) <- c("Fleet","Do_Power","Power","Do_Env_var","Env_Link","Do_ExtraVar","Qtype","","Q","Num=0/Bio=1","Err_type","N","Npos","r.m.s.e.","mean_input_SE","Input+VarAdj","Input+VarAdj+extra","VarAdj","New_VarAdj","pen_mean_Qdev","rmse_Qdev")
    }
    else{
      colnames(res[[3]]) <- c("index","N","Nops","r.m.s.e.","mean_input_SE")         
    }}
  res
}

getCPUE.ss2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){

  vnumber2 <- is.ss2.2(repfile)
  vnumber3 <- is.ss3(repfile)

  index1.char <- ifelse(!vnumber2&!vnumber3,"index","INDEX_1")
  vskipline <- ifelse(!vnumber2&!vnumber3,0,1)
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  

  #  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  # read index_2
  res <- find.and.read.table2("INDEX_2",skipline=1,gyou=NULL,comment.char="",
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T)
  if(ncol(res[[1]])==10){
    colnames(res[[1]]) <- c("index","year","vuln_bio","obs","exp","eff_Q","SE","Dev","Like","Like+log(s)")
  }
  else{
    colnames(res[[1]]) <- c("index","year","Seas","vuln_bio","obs","exp","calc_Q","eff_Q","SE","Dev","Like","Like+log(s)")    
  }
  if(vnumber3){
    a <- strsplit(as.character(res[[1]]$index),"_")
    aa <- numeric()
    for(i in 1:length(a)){
      aa[i] <- a[[i]][1]
    }
#    res[[1]]$index <- as.numeric(t(as.matrix(as.data.frame(strsplit(as.character(res[[1]]$index),"_"))[1,])))
    res[[1]]$index <- as.numeric(aa)#as.numeric(t(as.matrix(as.data.frame()[1,])))      
  }
  #----- ad hoc solution to avoid the problems of "1.#QNAN"--------
  res[[1]]$exp <- as.numeric(as.character(res[[1]]$exp))
  #----------------------------------------------------------------  
  
  # read index_1  
  res[[3]] <- find.and.read.table(index1.char,skipline=vskipline,
                                  gyou=max(as.numeric(gsub("_","",substr(res[[1]][,1],1,2)))),
                                  startpoint=res[[2]]+3,
                                  comment.char="",
                                  table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)[[1]]      

  if(vnumber2){
    colnames(res[[3]]) <- c("Index","Do_Power","Power","Do_Env_var","Env_Link","Do_ExtraVar","Qtype ","Q","Num=0/Bio=1","Err_type","N","Npos","r.m.s.e.","mean_input_SE","mean_(Input+extra)_SE","pen_mean_Qdev","rmse_Qdev")
  }
  else{
    if(vnumber3){
      colnames(res[[3]]) <- c("Fleet","Do_Power","Power","Do_Env_var","Env_Link","Do_ExtraVar","Qtype","","Q","Num=0/Bio=1","Err_type","N","Npos","r.m.s.e.","mean_input_SE","Input+VarAdj","Input+VarAdj+extra","VarAdj","New_VarAdj","pen_mean_Qdev","rmse_Qdev")
    }
    else{
      colnames(res[[3]]) <- c("index","N","Nops","r.m.s.e.","mean_input_SE")         
    }}
  res
}

getEffsmp1 <-
function(repfile="ss2.rep"){
  pred.comp <- getAgecomp.ss2.1(repfile)
  pred.comp[[1]]$effN
}

getEffsmp2 <-
function(repfile="ss2.rep"){
  pred.comp <- getAgecomp.ss2.1(repfile)
  b <- lm(pred.comp[[1]]$effN~pred.comp[[1]]$Nsamp)
  b$fitted.values
}

getNAA.ss <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "NUMBERS_AT_AGE" #
  line.tmp <- ifelse(is.ss3(repfile),0,1)
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    naa <- find.and.read.table2(read.char,skipline=line.tmp+1,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    naa <- find.and.read.table(read.char,skipline=line.tmp+1,startpoint=name.label[[2]]-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
#  naa[[1]] <- lapply(naa[[1]],as.character)
#  naa[[1]] <- lapply(naa[[1]],as.numeric)
  naa[[1]] <- as.data.frame(naa[[1]])
#  res[[1]][res[[1]]=="+1.#IND"] <- Inf
#  res[[1]] <- as.data.frame(res[[1]])
  colnames(naa[[1]]) <- as.character(name.label[[1]])
  if(more.ss3.11(repfile)){
    colnames(naa[[1]])[7] <- "Year"
    colnames(naa[[1]])[11] <- "Per"    
  }
  else{
    if(is.ss3(repfile)){
      colnames(naa[[1]])[7] <- "Year"
      colnames(naa[[1]])[10] <- "Per"
    }}
  if(more.ss3.11(repfile)) naa[[1]] <- naa[[1]][naa[[1]]$"Beg/Mid"=="B",]
  naa
}

getNAA.ss2 <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "NUMBERS_AT_AGE" #
  line.tmp <- ifelse(is.ss3(repfile),0,1)
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    naa <- find.and.read.table2(read.char,skipline=line.tmp+1,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    naa <- find.and.read.table(read.char,skipline=line.tmp+1,startpoint=name.label[[2]]-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
#  naa[[1]] <- lapply(naa[[1]],as.character)
#  naa[[1]] <- lapply(naa[[1]],as.numeric)
  naa[[1]] <- as.data.frame(naa[[1]])
#  res[[1]][res[[1]]=="+1.#IND"] <- Inf
#  res[[1]] <- as.data.frame(res[[1]])
  colnames(naa[[1]]) <- as.character(name.label[[1]])
  if(more.ss3.11(repfile)){
    colnames(naa[[1]])[7] <- "Year"
    colnames(naa[[1]])[11] <- "Per"    
  }
  else{
    if(is.ss3(repfile)){
      colnames(naa[[1]])[7] <- "Year"
      colnames(naa[[1]])[10] <- "Per"
    }}
  if(more.ss3.11(repfile)) naa[[1]] <- naa[[1]][naa[[1]]$"Beg/Mid"=="B",]
  naa
}

getNMA.ss <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL,qt=4){
  read.char <- ifelse(is.ss3(repfile),"Biology_at_age","Season")
  line.tmp <- ifelse(is.ss3(repfile),1,0)
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=-1+line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    nma <- find.and.read.table2(read.char,skipline=0+line.tmp,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=-1+line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    nma <- find.and.read.table(read.char,skipline=0+line.tmp,startpoint=name.label[[2]]-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
#  nma[[1]] <- lapply(nma[[1]],as.character)
#  nma[[1]] <- lapply(nma[[1]],as.numeric)
  nma[[1]] <- as.data.frame(nma[[1]])
#  res[[1]][res[[1]]=="+1.#IND"] <- Inf
#  res[[1]] <- as.data.frame(res[[1]])
  colnames(nma[[1]]) <- as.character(name.label[[1]])

  if(qt>1 && !is.ss3(repfile)){
#    browser()
    tmp <- read.table(repfile,skip=nma[[2]]+1,nrow=nrow(nma[[1]])*(qt-1))
    colnames(tmp) <- colnames(nma[[1]])
    nma[[1]] <- rbind(nma[[1]],tmp)
  }
  nma
}

getNMA.ss2 <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL,qt=4){
  read.char <- ifelse(is.ss3(repfile),"Biology_at_age","Season")
  line.tmp <- ifelse(is.ss3(repfile),1,0)
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=-1+line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    nma <- find.and.read.table2(read.char,skipline=0+line.tmp,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=-1+line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    nma <- find.and.read.table(read.char,skipline=0+line.tmp,startpoint=name.label[[2]]-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
#  nma[[1]] <- lapply(nma[[1]],as.character)
#  nma[[1]] <- lapply(nma[[1]],as.numeric)
  nma[[1]] <- as.data.frame(nma[[1]])
#  res[[1]][res[[1]]=="+1.#IND"] <- Inf
#  res[[1]] <- as.data.frame(res[[1]])
  colnames(nma[[1]]) <- as.character(name.label[[1]])

  if(qt>1 && !is.ss3(repfile)){
#    browser()
    tmp <- read.table(repfile,skip=nma[[2]]+1,nrow=nrow(nma[[1]])*(qt-1))
    colnames(tmp) <- colnames(nma[[1]])
    nma[[1]] <- rbind(nma[[1]],tmp)
  }
  nma
}

getSPR.ss <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){
  if(!is.ss3(repfile)){
    read.char <- "SPR_series"
    line.tmp <- 0
  }
  else{
    read.char <- "SPR_series_uses_R0="
    line.tmp <- 4
  }
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table2(read.char,skipline=1+line.tmp,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table(read.char,skipline=1+line.tmp,startpoint=name.label[[2]]-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  res[[1]] <- lapply(res[[1]],as.character)
  res[[1]] <- lapply(res[[1]],as.numeric)
  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- as.character(name.label[[1]])
#  colnames(res[[1]])[2:which(colnames(res[[1]])=="Actual:")] <-
#    paste("R0-",colnames(res[[1]])[1:which(colnames(res[[1]])=="Actual:")],sep="")
#  browser()
  res
}

getSPR.ss2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){
  if(!is.ss3(repfile)){
    read.char <- "SPR_series"
    line.tmp <- 0
  }
  else{
    read.char <- "SPR_series_uses_R0="
    line.tmp <- 4
  }
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table2(read.char,skipline=1+line.tmp,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table(read.char,skipline=1+line.tmp,startpoint=name.label[[2]]-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  res[[1]] <- lapply(res[[1]],as.character)
  res[[1]] <- lapply(res[[1]],as.numeric)
  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- as.character(name.label[[1]])
#  colnames(res[[1]])[2:which(colnames(res[[1]])=="Actual:")] <-
#    paste("R0-",colnames(res[[1]])[1:which(colnames(res[[1]])=="Actual:")],sep="")
#  browser()
  res
}

getSRfunc.ss <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){
#  read.char <- "SPAWN_RECRUIT"
  read.char <- "S/Rcurve" 
#  line.tmp <- 6
  line.tmp <- -2  

  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
#  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  name.label <- find.and.read.table2(read.char,skipline=line.tmp,gyou=1,
                                     table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
  res <- find.and.read.table2(read.char,skipline=2+line.tmp,gyou=NULL,fill=T,
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
#}

  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- as.character(name.label[[1]])
  if(!is.ss3(repfile)) res[[1]]$"pred_recr" <- res[[1]]$"pred-recr"
#  browser()
  res
}

getSRfunc.ss2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){
#  read.char <- "SPAWN_RECRUIT"
  read.char <- "S/Rcurve" 
#  line.tmp <- 6
  line.tmp <- -2  

  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
#  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  name.label <- find.and.read.table2(read.char,skipline=line.tmp,gyou=1,
                                     table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
  res <- find.and.read.table2(read.char,skipline=2+line.tmp,gyou=NULL,fill=T,
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
#}

  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- as.character(name.label[[1]])
  if(!is.ss3(repfile)) res[[1]]$"pred_recr" <- res[[1]]$"pred-recr"
#  browser()
  res
}

getSRpara.ss <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL,qt=4){
  if(is.ss3(repfile)){
    read.char <- "PARAMETERS"
    line.tmp <- 2
    gyou.tmp <- NULL
  }
  else{
    read.char <- "SR_parms"
    line.tmp <- 0
    gyou.tmp <- 6
  }
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    SRpara <- find.and.read.table2(read.char,skipline=0+line.tmp,gyou=gyou.tmp,fill=T,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    SRpara <- find.and.read.table(read.char,skipline=0+line.tmp,startpoint=target.line,gyou=gyou.tmp,
                               table.property=cl,comment.char="",fill=T,
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  tmp <- as.data.frame(SRpara[[1]])
#  if(is.ss3(repfile)){
#    tmp <- tmp[,-1]
#  }  
  biom <- getBabs.ss2(repfile,cl=cl,tb=tb)
  if(is.ss3(repfile)){ # for version of > 3.00
    SRpara[[1]] <- list(logR0=tmp[tmp[,2]=="SR_R0",3],steepness=tmp[tmp[,2]=="SR_steep",3],
                        sigmaR=tmp[tmp[,2]=="SR_sigmaR",3],
                        envlink=tmp[tmp[,2]=="SR_envlink",3],
                        logR1=tmp[tmp[,2]=="SR_R1_offset",3],
                        Future=tmp[tmp[,2]=="SR_autocorr",3],  #???
                        SSB0=biom[[1]]$SpawnBio[!is.na(biom[[1]]$SpawnBio)&biom[[1]]$period=="VIRG"])
  }
  else{  # for version older than 3.00
    SRpara[[1]] <- list(logR0=tmp[1,2],steepness=tmp[2,2],sigmaR=tmp[3,2],
                        envlink=tmp[4,2],logR1=tmp[5,2],Future=tmp[6,2],
                        SSB0=biom[[1]]$SpawnBio[1])    
  }
  SRpara[[3]] <- tmp
  SRpara
}

getSRpara.ss2 <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL,qt=4){
  if(is.ss3(repfile)){
    read.char <- "PARAMETERS"
    line.tmp <- 2
    gyou.tmp <- NULL
  }
  else{
    read.char <- "SR_parms"
    line.tmp <- 0
    gyou.tmp <- 6
  }
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    SRpara <- find.and.read.table2(read.char,skipline=0+line.tmp,gyou=gyou.tmp,fill=T,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    SRpara <- find.and.read.table(read.char,skipline=0+line.tmp,startpoint=target.line,gyou=gyou.tmp,
                               table.property=cl,comment.char="",fill=T,
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  tmp <- as.data.frame(SRpara[[1]])
#  if(is.ss3(repfile)){
#    tmp <- tmp[,-1]
#  }  
  biom <- getBabs.ss2(repfile,cl=cl,tb=tb)
  if(is.ss3(repfile)){ # for version of > 3.00
    SRpara[[1]] <- list(logR0=tmp[tmp[,2]=="SR_R0",3],steepness=tmp[tmp[,2]=="SR_steep",3],
                        sigmaR=tmp[tmp[,2]=="SR_sigmaR",3],
                        envlink=tmp[tmp[,2]=="SR_envlink",3],
                        logR1=tmp[tmp[,2]=="SR_R1_offset",3],
                        Future=tmp[tmp[,2]=="SR_autocorr",3],  #???
                        SSB0=biom[[1]]$SpawnBio[!is.na(biom[[1]]$SpawnBio)&biom[[1]]$period=="VIRG"])
  }
  else{  # for version older than 3.00
    SRpara[[1]] <- list(logR0=tmp[1,2],steepness=tmp[2,2],sigmaR=tmp[3,2],
                        envlink=tmp[4,2],logR1=tmp[5,2],Future=tmp[6,2],
                        SSB0=biom[[1]]$SpawnBio[1])    
  }
  SRpara[[3]] <- tmp
  SRpara
}

getSelect.ss <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL,len=TRUE){
  if(is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
    vskipline <- 5
  }
  else{
    vskipline <- 0
  }

  if(len==TRUE){
    desc <- "LEN_SELEX"
  }
  else{
    desc <- "AGE_SELEX"
    if(is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
      vskipline <- 3
    }
  }
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(desc,skipline=0+vskipline,gyou=1,
                                       table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
    
    res <- find.and.read.table2(desc,skipline=1+vskipline,gyou=NULL,
                                table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  else{
#    tmp <- count.fields(repfile,blank.lines.skip=FALSE)
    name.label <- find.and.read.table(desc,skipline=0+vskipline,startpoint=target.line-10,gyou=1,
                                      table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)
    
    res <- find.and.read.table(desc,skipline=1+vskipline,startpoint=target.line-10,gyou=NULL,
                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)    
  }
  colnames(res[[1]]) <- unlist(lapply(name.label[[1]],as.character))#name.label[[1]]
#  rownames(res[[1]]) <- paste("F",res[[1]][,1],"-Y",res[[1]][,2],"-G",res[[1]][,3],sep="")
  if(is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
    if(len==FALSE){
      # Remove ?? lines at AGE_SELEX such as 'Asel2' and 'sel*wt, sel*ret*wt etc..',
      #    those lines are probably for the use of future projections
      res[[1]] <- res[[1]][substr(res[[1]]$factor,1,5)=="Asel",]
      # NOT include the column of 'Factor' for the function of 'calTotcatch.select' (2009/07/27), which might cuase error for other functions
      rownames(res[[1]]) <- paste("F",res[[1]]$fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")
      tmp <- -1:-7            
    }
    else{
#      rownames(res[[1]]) <- paste(res[[1]]$Factor,"-F",res[[1]]$Fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")
      res[[1]] <- res[[1]][substr(res[[1]]$Factor,1,4)=="Lsel",]      
      rownames(res[[1]]) <- paste("F",res[[1]]$Fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")
      tmp <- -1:-5            
    }
  }
  else{
    rownames(res[[1]]) <- paste("F",res[[1]][,1],"-Y",res[[1]][,2],"-G",res[[1]][,3],sep="")    
#    rownames(res[[1]]) <- paste("F",res[[1]]$Fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")    
    tmp <- -1:-4    
  }
  list(as.data.frame(t(res[[1]][,tmp])),res[[2]])
}

getSelect.ss2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL,len=TRUE){
  if(is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
    vskipline <- 5
  }
  else{
    vskipline <- 0
  }

  if(len==TRUE){
    desc <- "LEN_SELEX"
  }
  else{
    desc <- "AGE_SELEX"
    if(is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
      vskipline <- 3
    }
  }
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(desc,skipline=0+vskipline,gyou=1,
                                       table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
    
    res <- find.and.read.table2(desc,skipline=1+vskipline,gyou=NULL,
                                table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  else{
#    tmp <- count.fields(repfile,blank.lines.skip=FALSE)
    name.label <- find.and.read.table(desc,skipline=0+vskipline,startpoint=target.line-10,gyou=1,
                                      table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)
    
    res <- find.and.read.table(desc,skipline=1+vskipline,startpoint=target.line-10,gyou=NULL,
                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)    
  }
  colnames(res[[1]]) <- unlist(lapply(name.label[[1]],as.character))#name.label[[1]]
#  rownames(res[[1]]) <- paste("F",res[[1]][,1],"-Y",res[[1]][,2],"-G",res[[1]][,3],sep="")
  if(is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
    if(len==FALSE){
      # Remove ?? lines at AGE_SELEX such as 'Asel2' and 'sel*wt, sel*ret*wt etc..',
      #    those lines are probably for the use of future projections
      res[[1]] <- res[[1]][substr(res[[1]]$factor,1,5)=="Asel",]
      # NOT include the column of 'Factor' for the function of 'calTotcatch.select' (2009/07/27), which might cuase error for other functions
      rownames(res[[1]]) <- paste("F",res[[1]]$fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")
      tmp <- -1:-7            
    }
    else{
#      rownames(res[[1]]) <- paste(res[[1]]$Factor,"-F",res[[1]]$Fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")
      res[[1]] <- res[[1]][substr(res[[1]]$Factor,1,4)=="Lsel",]      
      rownames(res[[1]]) <- paste("F",res[[1]]$Fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")
      tmp <- -1:-5            
    }
  }
  else{
    rownames(res[[1]]) <- paste("F",res[[1]][,1],"-Y",res[[1]][,2],"-G",res[[1]][,3],sep="")    
#    rownames(res[[1]]) <- paste("F",res[[1]]$Fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")    
    tmp <- -1:-4    
  }
  list(as.data.frame(t(res[[1]][,tmp])),res[[2]])
}

getWatL.ss <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "BIOLOGY" #
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
#    name.label <- find.and.read.table2(read.char,skipline=0,gyou=1,
#                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,
#    is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table2(read.char,skipline=1,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
#    name.label <- find.and.read.table(read.char,skipline=0,startpoint=target.line-10,gyou=1,
#                                      table.property=cl,
#                                      outfile=repfile,h=FALSE,is.ss2=TRUE,
#                                      colClasses="character")  
    res <- find.and.read.table(read.char,skipline=1,startpoint=target.line-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }

  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- c("bin","low","mean_size","Wt","mat_len","mat_Wt")
  res
}

getWatL.ss2 <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "BIOLOGY" #
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
#    name.label <- find.and.read.table2(read.char,skipline=0,gyou=1,
#                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,
#    is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table2(read.char,skipline=1,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
#    name.label <- find.and.read.table(read.char,skipline=0,startpoint=target.line-10,gyou=1,
#                                      table.property=cl,
#                                      outfile=repfile,h=FALSE,is.ss2=TRUE,
#                                      colClasses="character")  
    res <- find.and.read.table(read.char,skipline=1,startpoint=target.line-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }

  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- c("bin","low","mean_size","Wt","mat_len","mat_Wt")
  res
}

getx <-
function(list){
  list$x
}

gety <-
function(list){
  list$x
}

good.colors <-
function(){
  tmp1 <- apply(col2rgb(colors()),2,max)<230
#  tmp1 <- TRUE
  tmp2 <- apply(col2rgb(colors()),2,sd)/apply(col2rgb(colors()),2,mean)>0.5
  col.tmp <- colors()[tmp1&tmp2&!is.na(tmp1)&!is.na(tmp2)]
  a <- col2rgb(col.tmp)
  col.tmp <- col.tmp[order(a[1,])]
  col.tmp <- matrix(col.tmp,3,floor(length(col.tmp)/2),byrow=T)
  dim(col.tmp) <- NULL
  col.tmp
}

int.dec <-
function(cate,pre.cate=NA,interactive.mode=FALSE){
  cate1 <- sort(unique(cate))
  n <- length(cate1)
  if(length(pre.cate)==1){
      cate.res <- rep(0,n)
      }
      else{
      cate.res <- pre.cate
  }
  if(interactive.mode==TRUE){
    cat(as.character(cate1),"\n")
    for(i in 1:n){
      cat(as.character(cate1[i]),": ", as.character(pre.cate[i])," ")
      tmp <- readline()
      if(tmp!="n"&tmp!="AN"){
        cate.res[i] <- tmp
      }
      else{
        if(tmp=="AN")
          break
      }
    }
    cat("Do new category?")
    tmp <- readline()
  }
  else{
    tmp <- "Y"
    if(tmp=="Y"){
      new.dat <- rep(0,length(cate))
      for(i in 1:n){
        new.dat[as.character(cate)==as.character(cate1)[i]] <-
          as.character(cate.res[i])
      }
    }
    else{
      new.dat <- NA
    }
  }
  return(list(category.list=cbind(as.character(cate1),cate.res),newdat=new.dat))
}

inv.logit <-
function(ip){
  exp(ip)/(1+exp(ip))
}

is.ss2.2 <-
function(repfile){
  vnumber <- read.table(repfile,nrow=1,colClasses="character",header=F)
  tmp <- as.numeric(substr(vnumber[1],16,19))>=2
  if(is.na(tmp)) tmp <- FALSE
  tmp
}

is.ss3 <-
function(repfile){
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  tmp <- as.numeric(substr(a[1,1],5,8))>3
  if(is.na(tmp)) tmp <- FALSE
  tmp
}

kai07 <-
function(L){ 1.7117 * 10^{-5} * L^{3.0382}}

kai07inv <-
function(W){
  exp((log(W)-log(1.7117*10^{-5}))/3.0382)
}

lengthdist <-
function(agecomp){
  fleet.row <- sort(unique(agecomp[[1]]$fleet))  
  length.bin <- sort(unique(agecomp[[1]]$bin))
  sum.length <- array(0,dim=c(length(length.bin),length(fleet.row),2))
  dimnames(sum.length) <- list(length.bin,fleet.row,c("Obs","Exp"))

#  par(mfrow=c(5,5),mar=c(2,2,0.5,0.5),oma=c(0,0,3,3),mgp=c(2,1,0),ps=14)    
  s <- 1
  tmp <- paste(floor(as.numeric(agecomp[[1]]$year)),agecomp[[1]]$season,agecomp[[1]]$fleet,sep="-")
  tmp2 <- unique(tmp)
  all.length <- array(0,dim=c(length(length.bin),length(tmp2),2))
  all.label <- data.frame(label=rep(0,length(tmp2)),effN=rep(0,length(tmp2)),effN2=rep(0,length(tmp2)))
  tmp.ref <- paste(floor(as.numeric(agecomp[[2]]$Year)),agecomp[[2]]$Seas,agecomp[[2]]$Index,sep="-")

#  for(j in 1:length(fleet.row)){
    for(i in 1:length(tmp2)){
#      if(sum(agecomp[[1]]$fleet[tmp==tmp2[i]]==fleet.row[j])){
        x <- agecomp[[1]]$bin[tmp==tmp2[i]]
        y <- cbind(agecomp[[1]]$obs[tmp==tmp2[i]],agecomp[[1]]$exp[tmp==tmp2[i]])*
          agecomp[[2]]$Nsamp[tmp.ref==tmp2[i]]
        ## Summation of the data
        y <- y[!is.na(y[,1]),]
        x <- x[!is.na(x)]
        all.length[match(x,length.bin),s,1] <- obsp <- y[,1]/sum(y[,1])# all.length-->composition data
        all.length[match(x,length.bin),s,2] <- expp <- y[,2]/sum(y[,2])
        all.label$effN[s] <- sum(expp*(1-expp))/sum((obsp-expp)^2)
        all.label$effN2[s] <- sum(obsp*(1-obsp))/sum((obsp-expp)^2)        
        all.label$label[s] <- tmp2[i]
        fleet.tmp <- mean(agecomp[[1]]$fleet[tmp==tmp2[i]])
        sum.length[match(x,length.bin),fleet.tmp,] <- sum.length[match(x,length.bin),fleet.tmp,]+y
        s <- s+1                
      }
#}}
  invisible(list(all.length=all.length,label=all.label,sum.length=sum.length))
}

make.partcatcharg <-
function(repfile=NULL,faa.res=NULL,yr.range=c(2002,2004),qt=4,gm=FALSE){

  as.numeric.terminalF <- function(x){
    terminal.F <- mean(x[,ncol(x)])
    res <- as.numeric(x)
    res[length(res)] <- terminal.F
    return(res)
  }

  if(is.null(faa.res)){
    a <- calFAA.ss2(repfile)
  }
  else{
    a <- faa.res
  }
#  tmp <- a$faa.array[as.numeric(rownames(a$faa))>=yr.range[1] &
#                     as.numeric(rownames(a$faa))<yr.range[2],,]
  tmp <- a$faa.array[floor(as.numeric(rownames(a$faa)))>=yr.range[1] &
                     floor(as.numeric(rownames(a$faa)))<=yr.range[2],,]  
  rownames(tmp) <- rep(1:qt,yr.range[2]-yr.range[1]+1)
  nfleet <- dim(tmp)[3]
  faa.yafq <- array(0,dim=c(dim(tmp)[1]/qt,dim(tmp)[2],dim(tmp)[3],qt)) #faa by year, age, fleet and quarter
  for(i in 1:qt){
    faa.yafq[,,,i] <- tmp[rownames(tmp)==i,,]  #=>最終的にはfaa.aqを作る
  }
  faa.yaq <- apply(faa.yafq,c(1,2,4),sum)
#  browser()
  if(gm==FALSE){
    faa.aq <- faa.aq.am <- apply(faa.yaq,c(2,3),mean)
  }else{
    faa.aq <- apply(faa.yaq,c(2,3),geomean)
    faa.aq <- faa.aq.gm <- ifelse(is.na(faa.aq),0,faa.aq)
  }

  faa.aq[nrow(faa.aq),qt] <- mean(faa.aq[nrow(faa.aq),])
  
  # calculate partial catch again
  faa.afq.tmp <- apply(faa.yafq,c(2:4),sum)
  faa.afq <- faa.afq.ratio <- array(0,dim=c(dim(tmp)[2],dim(tmp)[3],qt)) #faa by year, age, fleet and quarter
  for(j in 1:qt){
    for(i in 1:dim(faa.afq)[[1]]){
      if(i==dim(faa.afq)[[1]] & j==qt){
        # terminal ageのpartial Fの組成は、terminal ageの全四半期のものを使う
        x <- apply((faa.afq.tmp[i,,]/sum(faa.afq.tmp[i,,])),1,sum)
        faa.afq.ratio[i,,j] <- x/sum(x) 
      }
      else{
        faa.afq.ratio[i,,j] <- (faa.afq.tmp[i,,j]/sum(faa.afq.tmp[i,,j])) 
      }
      faa.afq[i,,j] <- faa.aq[i,j]*faa.afq.ratio[i,,j]                 
      faa.afq[i,,j] <- ifelse(is.nan(faa.afq[i,,j]),0,faa.afq[i,,j])              
    }}
  # aqの次元を1次元にする
  faa.qaf <- aperm(faa.afq,perm=c(3,1,2))
  faa.qaf.ratio <- aperm(faa.afq.ratio,perm=c(3,1,2))
#  browser()
  dim(faa.qaf) <-  c(dim(faa.qaf)[[1]]*dim(faa.qaf)[[2]],dim(faa.qaf)[[3]]) #<= tmp4
  dim(faa.qaf.ratio) <-  c(dim(faa.qaf.ratio)[[1]]*dim(faa.qaf.ratio)[[2]],dim(faa.qaf.ratio)[[3]])   #<= tmp3 （旧tmp3と少し違う、、）
  
  #2010.10.06 geometric meanのオプションを入れるため、大幅改訂==> ここから下は使わない
  if(0){
    tmp2 <- array(0,dim=c(qt,dim(tmp)[2],nfleet))
    for(i in 1:dim(tmp)[3]){
      tmp2[,,i] <- rowtapply(tmp[,,i])/(yr.range[2]-yr.range[1]+1) ## 年の次元を単純平均で減らす
    }
    tmp0 <- apply(tmp2,c(1,2),sum) # current F at age and season
    tmp3 <- tmp4 <- as.list(1:nfleet) # current F at age and season by fleet
    for(i in 1:nfleet){
      tmp3[[i]] <- tmp2[,,i]/tmp0
      tmp4[[i]] <- tmp2[,,i]
    }
    tmp3 <- as.data.frame(lapply(tmp3,as.numeric.terminalF)) # partila F by age (normazized)
    tmp4 <- as.data.frame(lapply(tmp4,as.numeric.terminalF)) # partila F by age (un-normalized)
    colnames(tmp3) <- colnames(tmp4) <-paste2("F",1:ncol(tmp4))
  }

  # weight at age
  tmp <- a$nma
  if(is.null(tmp$"age_Beg")){
    ages <- tmp$"Age_Beg"
  }  else{
    ages <- tmp$"age_Beg"
  }  
  waaf <- tmp[,substr(colnames(tmp),1,5)=="SelWt"][,1:nfleet]
  waaf <- waaf[order(ages),]
  rownames(waaf) <- rownames(faa.qaf) <- rownames(faa.qaf.ratio) <- sort(ages)

#  list(ratio=tmp3,waaf=waaf,Fmat=tmp4)  # arguments for partial catch
  return(list(ratio=faa.qaf.ratio,waaf=waaf,Fmat=faa.qaf))  # arguments for partial catch  )
}

make.pictexfig <-
function(picfile="test.tex",tempfile="template.tex",graphfile="test.ps"){
  system(paste("platex ",tempfile))

  tmp <- strsplit(tempfile,"\\.")
  system(paste("pdvips ",tmp[[1]][1],".dvi"," -o ", graphfile,sep=""))
  system(paste("ggv ",graphfile,"&"))
}

make.pointmap <-
function(x2,cex=1.5,limit=c(0,0.2,0.5,1,5,10)){
  library(RColorBrewer)
  col.tmp <- c(2,3,4,6,8,9)
  xtmp <- as.numeric(x2[3,]>limit[1])
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[2])  
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[3])
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[4])
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[5])
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[6] )
  tmp <- x2[3,]!=0 & !is.na(x2[3,]) & x2[3,]!=Inf
  points(x2[2,tmp]+0.5,x2[1,tmp]+0.5,col=brewer.pal(9,"Reds")[col.tmp[xtmp[tmp]]],pch=15,cex=cex)
  tmp <- x2[3,]==0 & !is.na(x2[3,]) & x2[3,]!=Inf
  points(x2[2,tmp]+0.5,x2[1,tmp]+0.5,pch=4,cex=cex,col="skyblue")
#  legend(360-150,50,col=brewer.pal(9,"Reds")[col.tmp],pch=15,cex=1,legend=paste(">",limit,sep=""))
}

makedevice <-
function(filename=NULL,dev.type="ps",filenum=NULL,htmlfile=NULL,
                       new=F,append=TRUE,tmp.par=par(),width=570,...){
  if(dev.type=="html"){
    if(new==T) dev.off()
    jpeg(file=n.tmp <- paste(filename,filenum,".jpeg",sep=""),
           quality=150,height=700,width=width,...)
#    par(tmp.par)
    cat(paste("<img src=\"./",n.tmp,"\" width=\"",width,"\" height=\"700\">\n",sep=""),file=htmlfile,append=append)
  }
  else{
 #  if(dev.type=="x11") locator(1)
  }
}

matplot2 <-
function(...){
  matplot(type="l",col="gray",lty=1,,...)
}

matslice <-
function(mat,num){
  tmp <- rep(0,nrow(mat))
  x <- ceiling(ncol(mat)/num)
  s <- 0
  for(i in 1:x){
    if(ncol(mat)<num){
      tmp <- cbind(tmp,apply(as.matrix(mat[,1:ncol(mat)]),1,sum))
    }
    else{
      tmp <- cbind(tmp,apply(as.matrix(mat[,1:num]),1,sum))
      mat <- as.matrix(mat[,-1:-num])
      s <- s+1
#      cat(num," ",s," ")
    }

  }
  tmp <- tmp[,-1]
  rownames(tmp) <- rownames(mat)
  colnames(tmp) <- num*1:ncol(tmp)
  tmp
}

mod2 <-
function(x,y){
  ifelse(x%%y==0,y,x%%y)
}

month2days <-
function(month,day){
  x <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  x1 <- numeric()
  if(length(month)==1) x1 <- sum(x[0:(month-1)])+day
  else{
    for(i in c(1:length(month))) x1[i] <- sum(x[0:(month[i]-1)])+day[i]
  }
  x1
}

more.ss3.11 <-
function(repfile){
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  tmp <- as.numeric(substr(a[1,1],5,8))>=3.11
  if(is.na(tmp)) tmp <- FALSE
  tmp
}

moving.mean <-
function(y,y.name=NULL,kukan=3){
  m <- (kukan-1)/2
  res <- res.name <- rep(0,length(y)-kukan+1)
  s <- 1
  for(i in (m+1):(length(y)-m)){
    res[s] <- mean(y[(i-m):(i+m)])
    if(!is.null(y.name)){
      res.name[s] <- y.name[i]
    }
    s <- s+1    
  }
  if(is.null(y.name)) res.name <- 1:length(res)
  list(x=as.numeric(res.name),y=res)
}

mvfile <-
function(filename.stored,n.ss2){
  if(.Platform$OS.type=="unix"){          
    system(paste("cp ss2.rep ","ss2",filename.stored[2],"-",n.ss2,".rep",sep=""))
    system(paste("mv ss2",filename.stored[2],"-",n.ss2,".rep ",filename.stored[1],sep=""))
    system(paste("cp ss2.par ","ss2",filename.stored[2],"-",n.ss2,".par",sep=""))
    system(paste("mv ss2",filename.stored[2],"-",n.ss2,".par ",filename.stored[1],sep=""))    
  }
  else{
    shell(paste("copy ss2.rep ","ss2",filename.stored[2],"-",n.ss2,".rep",sep=""))
    shell(paste("move ss2",filename.stored[2],"-",n.ss2,".rep ",filename.stored[1],sep=""))
    shell(paste("copy ss2.par ","ss2",filename.stored[2],"-",n.ss2,".par",sep=""))
    shell(paste("move ss2",filename.stored[2],"-",n.ss2,".par ",filename.stored[1],sep=""))    
  }
  
}

noninv.logit <-
function(ip){
  log(ip/(1-ip))
}

nplot <-
function(){
  plot(1:10,type="n",ylab="",xlab="",axes=F)
}

open.histdata <-
function(num,bin){
  res <- 1
  for(i in 1:length(num)){
    if(num[i]!=0){
      res <- c(res,rep(bin[i],num[i]))
    }
  }
  res[-1]
}

overplot.graph <-
function(mat,col.var=NULL,...){
  y <- apply(mat,1,sum)
  x <- as.numeric(names(y))

  if(length(x)==0) x <- 1:length(y)
  
  plot(x,y,type="n",ylim=c(0,max(y,na.rm=T)),...)

  if(is.null(col.var)) col.var <- gray(seq(from=0.1,to=0.9,length=ncol(mat)))

  mat <- cbind(0,mat)
  s <- rep(0,nrow(mat))
  
  for(i in 2:ncol(mat)){
    polygon(c(x,rev(x)),c(rev(s),s <- s+rev(mat[,i])),col=col.var[i])
  }
}

paste.new <-
function(vec){
  n <- length(vec)
  moji <- NULL
  for(i in c(1:n)) moji <- paste(moji,vec[i])
  moji
}

paste2 <-
function(x,...){
  paste(x,sep="",...)
}

pictex.template <-
function(picfile="test.tex",tempfile="template.tex"){
  cat("\\documentclass[a4paper]{jarticle}
\\usepackage{pictex}
\\usepackage{graphics}
\\begin{document}
\\input{",
      file=tempfile)
  cat(picfile,file=tempfile,append=T)
  cat("}
\\end{document}",file=tempfile,append=T)
}

plot.cohortSSB <-
function(repfile=NULL,naa=NULL,nma=NULL,age.specific.mat=TRUE,title.text=NULL){
  if(is.null(naa)|is.null(nma)){
    naa <- getNAA.ss2(repfile)[[1]]
    nma <- getNMA.ss2(repfile,qt=1)[[1]]
  }
  
  tmp <- nma$Seas==1
#  tmp <- nma$Season==1  
  {if(age.specific.mat==TRUE){
    maa <- nma$"Age_Mat"[tmp]  # maturity at age in using age specific maturity
  }
  else{
    maa <- nma$"Len_Mat"[tmp]  # maturity at age in using logistic
  }}

  if(!more.ss3.11(repfile)){
    ssb <- sweep(naa[tmp <- ((naa$Per=="TIME"|naa$Per=="FORE") & naa$Seas==1),which(names(naa)=="0"):ncol(naa)],2,maa,FUN="*")
  }
  else{
    ssb <- sweep(naa[tmp <- ((naa$Per=="TIME"|naa$Per=="FORE") &
         naa$Seas==1 & naa$"Beg/Mid"=="B"),which(names(naa)=="0"):ncol(naa)],2,maa,FUN="*")    
  }
  rownames(ssb) <- naa$"Year"[tmp]
  ssb <- t(as.matrix(ssb))

  col.mat <- ssb
  col.mat[] <- 0
  col.tmp <- c(terrain.colors(12)[tmp <- c(1,3,5,7,9,11,2,4,6,8,10,12)],rainbow(12)[tmp])
  #col.tmp <- c("#F7FCFD","#800026","#E5F5F9","#BD0026","#CCECE6","#E31A1C","#99D8C9","#FC4E2A","#66C2A4","#FD8D3C","#41AE76","#FEB24C","#238B45","#FED976","#006D2C","#FFEDA0","#00441B","#FFFFCC")
  for(i in -20:ncol(ssb)){
    s <- 0
    for(j in 1:nrow(ssb)){
      if(i+s<=ncol(ssb) && i+s>0){
        col.mat[j,i+s] <- col.tmp[i%%24+1]
      }
      s <- s+1        
    }
  }
  col.mat[col.mat==0] <- 1
#  set.mypar()

  #postscript("tmp.ps",horizontal=FALSE,height=9)
#  par(mfrow=c(2,1),mar=c(3,3,1,1))
  par(las=3)
  b0 <- barplot(ssb,col="gray")
  for(i in 1:ncol(ssb)){
    ssb0  <- ssb
    ssb0[,-i]  <- NA
    barplot(ssb0,add=TRUE,col=col.mat[,i],axes=FALSE,
            axisname=FALSE,ps=9)
    #  locator(1)
  }
  title(title.text)  
#  plot(1:10,type="n",axes=F,xlab="",ylab="")
  nplot()
  legend(1,10,fill=col.mat[1,],legend=colnames(col.mat),ncol=6,cex=0.8)
}

plot.data.frame <-
function(data,datalist=NULL,name,title.name,nline,col.var,lty.var,lwd.var,ptype){
  x <- data$Year
  y <- data[[name]]
  plot(x[y!=0],y[y!=0],ylab=title.name,ylim=c(0,max(y,na.rm=T)),
       xlab="Year",type=ptype,lwd=lwd.var[1])
  title(title.name,line=nline)
  if(!is.null(datalist)){
    for(i in 1:length(datalist)){
      x1 <- datalist[[i]]$Year
      y1 <- datalist[[i]][[name]]
      points(x1[y1!=0],y1[y1!=0],col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type="l")
      }}
}

plot.nofishBio <-
function(biom.mat,datfile=NULL){
  biom.mat <-  biom.mat[apply(is.na(biom.mat),1,sum)==0,]/1000
  plot(rownames(biom.mat),apply(biom.mat,1,max),type='n',xlab="Year",ylab="SSB(x1000)MT")
  yr <- as.numeric(rownames(biom.mat))
  n <- ncol(biom.mat)
  
  if(!is.null(datfile)){
    fleet.name <- read.fleetname(datfile)[1:(n-1)]
  }
  else{
    fleet.name <- paste2("F",1:(n-1))
  }

  colors <- rainbow(n)
  tmp <- cbind(0,biom.mat[,n],biom.mat[,1:(n-1)])
  for(i in 2:n){
    polygon(c(yr,rev(yr)),c(rep(tmp[,i-1]),rev(tmp[,i])),
            border="gray",col=colors[i],lwd=2)
  }
  legend("topright",max(biom.mat),fill=colors[2:n],legend=c("Current",fleet.name),ncol=2)
  invisible(biom.mat)
}

plot.processgraph <-
function(res,smp.lim=NULL,cpue.lim=NULL,
                              dev.type="pdf"){
  # USAGE:
  # res <- doautorun()
  # plot.processgraph(res$nsmp.res,res$sd.cpue.mat,smp.lim=200,cpue.lim=c(0,1))
  nsmp.res <- res$nsmp.res
  sd.cpue.mat <- res$sd.cpue.mat
  LL <- res$LL
  ncalc <- (dim(nsmp.res)[[2]])

  if(dev.type=="pdf"){
    tmpfunc <- function(){
      if(names(dev.cur())!="null device") dev.off()
    }
    on.exit(tmpfunc())
    pdf(file="process_track.pdf",paper="a4",height=9)
  }

  # effective sample size
  set.mypar()  
  par(mfrow=c(3,1),mar=c(3,3,1,1))
#  b <- nsmp.res[,3:ncalc,4]
  b <- nsmp.res[,3:ncalc,2]
   b <- as.data.frame(b)
   boxplot(b,ylab="Effective sample size",ylim=smp.lim)

  # CPUE plot
  matplot(t(sd.cpue.mat),type="b",col=1,lty=1,xlab="N of calculation",
          ylab="SD of CPUE",ylim=cpue.lim)

  # -log LL plot
  plot(LL,type="b",xlab="N of calculation",ylab="Negative log LL")
  

   # effective sample size by fleets
  fn <- sort(unique(nsmp.res[,1,8]))
  par(mfrow=c(5,2),mar=c(3,3,2,1))
  for(i in fn){
#     boxplot(as.data.frame(nsmp.res[nsmp.res[,1,8]==i,3:ncalc,4]),
    boxplot(as.data.frame(nsmp.res[nsmp.res[,1,8]==i,3:ncalc,2]),    # 4列目だと、effective sample sizeになっていまう。2列目にして、実際に置き換えた値を入れることにする
             ylab=paste("ESS by fleet"),ylim=smp.lim)
     title(paste("fleet",i),line=1)
   }

  if(dev.type=="pdf"){
    dev.off()
  }
}

plot.snail <-
function(repfile="ss2.rep",qt=4){
  faa <- calFAA.ss2(repfile,qt=qt,is.plot=F)
  tmp <- getSPR.ss2(repfile)[[1]]
#  ssb <- tmp$"Bio_Smry"
  ssb <- tmp$"SPR"  
  names(ssb) <- tmp$Year
  res <- list(faa=apply(rowtapply(faa$faa),1,mean),ssb=ssb)
  x <- res$faa[match(names(res$ssb),names(res$faa))]
  y <- res$ssb[match(names(res$faa),names(res$ssb))]
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  years <- as.numeric(names(x))
  pch.tmp <- floor((years-1900)/10)
  pch.tmp <- pch.tmp-min(pch.tmp)+1
  plot(x,y,type="o",xlab="Average F",ylab="SPR/SPR0",pch=pch.tmp,lwd=2)
#       col=rainbow(9)[pch.tmp])  
}

plotBSR <-
function(biom,biom.list=NULL,true.value=NULL,what.plot=c(T,T,T),
                    col.var=rep(1,1+length(biom.list)),lty.var=rep(1,1+length(biom.list)),
                    lwd.var=rep(1,1+length(biom.list)),ptype=c("l","b"),nline=-1.5,vnumber=2,
                    rev.setting=FALSE,ylim.factor=1,catch.plot=F,...){
 
  if(rev.setting==FALSE){
    par(mfrow=c(sum(what.plot),1),mar=c(4,4,2,0),oma=c(0,0,3,3),mgp=c(2,1,0))
  }
  islist <- is.list(biom.list)
#  if(islist){ #ptype <- "l"
#    
#  }
  x.adjust <- 0.25
  ylim.res <- rep(0,3)

  ## Total biomass
  tmp1 <- biom$period=="TIME"
  tmp2 <- biom$period=="FORE"
  tmp <- tmp1|tmp2
  vnumber <- 2
  if(sum(tmp)==0){ # for the version older than 2.00
    tmp <- biom$season!="E"
    vnumber <- 1
  }

  biom.summary <- matrix(0,sum(tmp),ifelse(islist,length(biom.list)+1,1))
  rownames(biom.summary) <- biom$year[tmp]+as.numeric(biom$season[tmp])/4-x.adjust
  if(islist){
    len.list <- length(biom.list)
  }
  
  if(what.plot[1]==T){
    x <- biom$year[tmp]+as.numeric(biom$season[tmp])/4-x.adjust
    y <- biom.summary[,1] <- biom$"bio-all"[tmp]
    plot(tmp <- x[y!=0],y[y!=0],ylab="Biomass",ylim=c(0,ylim.res[1] <- max(y,na.rm=T)*ylim.factor),
         xlab="Year",type=ptype[1],lwd=lwd.var[1],...)
    axis(at=floor(min(tmp*0.95)):floor(max(tmp*1.05)),labels=NA,side=1)
         
    if(vnumber>=2 & sum(tmp2)>0)
      points(biom$year[tmp2]+as.numeric(biom$season[tmp2])/4-x.adjust,biom$"bio-all"[tmp2],
             type=ptype[2],pch=3,cex=0.5)
    title("Total biomass",line=nline)
    
    if(islist){
      old.tmp <- tmp
      for(i in 1:len.list){
        if(vnumber<2){
          tmp <- biom.list[[i]]$season!="E"
        }
        else{
          tmp1 <- biom.list[[i]]$period=="TIME"
          tmp2 <- biom.list[[i]]$period=="FORE"
          tmp <- tmp1|tmp2    
        }
        x1 <- xl2 <- biom.list[[i]]$year[tmp]+as.numeric(biom.list[[i]]$season[tmp])/4-x.adjust
        xx.tmp <- match(xl2,rownames(biom.summary))
#        y1 <- biom.summary[remove.nadata(xx.tmp),i+1] <- biom.list[[i]]$"bio-all"[tmp&!is.na(xx.tmp)]
        y1 <- biom.summary[xx.tmp,i+1] <- biom.list[[i]]$"bio-all"[tmp]        
        points(x1[y1!=0],y1[y1!=0],col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[1])
        if(vnumber>=2 & sum(tmp2)>0)
          points(biom.list[[i]]$year[tmp2]+as.numeric(biom.list[[i]]$season[tmp2],
                                                      biom.list[[i]]$"bio-all"[tmp2])/4-x.adjust,pch=3,
                 col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[2],cex=0.5)
      }}

  # TRUE abundance in using the result of Operation Model
    if(!is.null(true.value)){
#      a <- readOM2(true.value,fish.year=0.5)
      points(true.value$year,true.value$biomass,col="plum1",lwd=2,type="l")    
    }
  }

  ## Spawning biomass
  if(vnumber<2){
    tmp <- biom$season!="E"
  }
  else{
    tmp1 <- biom$period=="TIME";tmp2 <- biom$period=="FORE"
    tmp <- tmp1|tmp2    
  }

  x.adjust <- 1
  ssb.summary <- matrix(0,sum(tmp),ifelse(islist,length(biom.list)+1,1))
  rownames(ssb.summary) <- biom$year[tmp]+as.numeric(biom$season[tmp])/4-x.adjust

  if(what.plot[2]==T){
#    x <- biom$year[tmp]+as.numeric(biom$season[tmp])/4-x.adjust
    x <- biom$year[tmp]
    y <- ssb.summary[,1] <- biom$"SpawnBio"[tmp]
    plot(tmp <- x[y!=0&!is.na(y)],y[y!=0&!is.na(y)],ylab="SSB",xlab="Year",type=ptype[1],lwd=lwd.var[1],ylim=c(0,ylim.res[2] <- max(y,na.rm=T)*ylim.factor),
         ...)
    axis(at=floor(min(tmp*0.95)):floor(max(tmp*1.05)),labels=NA,side=1)    
    title("Total SSB",line=nline)

    if(vnumber>=2 & sum(tmp2)>0)
#      points(biom$year[tmp2]+as.numeric(biom$season[tmp2])/4-x.adjust,biom$"SpawnBio"[tmp2],
      points(biom$year[tmp2],biom$"SpawnBio"[tmp2],      
             type=ptype[1],pch=3,cex=0.5)
  
    if(islist){
      for(i in 1:len.list){
        if(vnumber<2){
          tmp <- biom.list[[i]]$season!="E"
        }
        else{
          tmp1 <- biom.list[[i]]$period=="TIME";tmp2 <- biom.list[[i]]$period=="FORE"
          tmp <- tmp1|tmp2    
        }
     
#        xl <- biom.list[[i]]$year[tmp]+as.numeric(biom.list[[i]]$season[tmp])/4-x.adjust
        xl <- biom.list[[i]]$year[tmp]
        xl2 <- biom.list[[i]]$year[tmp]+as.numeric(biom.list[[i]]$season[tmp])/4-x.adjust
#        yl <- ssb.summary[,i+1] <- biom.list[[i]]$"SpawnBio"[tmp]
        xx.tmp <- match(xl2,rownames(ssb.summary))        
        yl <- ssb.summary[xx.tmp,i+1] <-
                                    biom.list[[i]]$"SpawnBio"[tmp]
        points(xl[yl!=0 & !is.na(yl)],yl[yl!=0 & !is.na(yl)],
               col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[1])
        if(vnumber>=2 & sum(tmp2)>0)
          points(biom.list[[i]]$year[tmp2]+as.numeric(biom.list[[i]]$season[tmp2])/4-x.adjust,
                 biom.list[[i]]$"SpawnBio"[tmp2],pch=3,
                 col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[1],cex=0.5)      
      }}

    #!!!! this algorism should be changed if the recruitment is occured at other than 1st quarter
    if(vnumber<2){
      tmp <- biom$season!="E"
    }
    else{
      tmp1 <- biom$period=="TIME";tmp2 <- biom$period=="FORE"
      tmp <- tmp1|tmp2    
    }
    if(!is.null(true.value)){
#      a <- readOM2(true.value,fish.year=0.5)
      points(true.value$year,true.value$ssb,col="plum1",lwd=2,type="l")    
    }    
  }

  tmp3 <- tmp&biom$"recruit-0">0
#  recruit.summary <- matrix(0,sum(tmp3),ifelse(islist,length(biom.list)+1,1))
#  rownames(recruit.summary) <- biom$year[tmp3]+as.numeric(biom$season[tmp3])/4-x.adjust
  
  x.adjust <- 0.25
  if(what.plot[3]==T){
    x <- biom$year[tmp3]+
                   as.numeric(biom$season[tmp3])/4-x.adjust
#    y <- recruit.summary[,1] <- biom$"recruit-0"[tmp3]
    y <- biom$"recruit-0"[tmp3]        

    ylim.res[3] <- max(y,na.rm=T)*ylim.factor
    plot(tmp <- x[y!=0],y[y!=0],ylab="Recruitment",xlab="Year",type=ptype[1],lwd=lwd.var[1],ylim=c(0,ylim.res[3]),
         ...)
    axis(at=floor(min(tmp*0.95)):floor(max(tmp*1.05)),labels=NA,side=1)    
    title("Recruitment",line=nline)
    if(vnumber>=2 & sum(tmp2)>0){
      tmp3 <- tmp2&biom$"recruit-0">0
      points(biom$year[tmp3]+as.numeric(biom$season[tmp3])/4-x.adjust,biom$"recruit-0"[tmp3],
             type=ptype[2],pch=3,cex=0.5)
    }
    if(islist){
      for(i in 1:len.list){
        if(vnumber<2){
          tmp <- biom.list[[i]]$season!="E"
        }
        else{
          tmp1 <- biom.list[[i]]$period=="TIME";tmp2 <- biom.list[[i]]$period=="FORE"
          tmp <- tmp1|tmp2    
        }
      
        tmp3 <- tmp&biom.list[[i]]$"recruit-0">0
        xl <- xl2 <- biom.list[[i]]$year[tmp3]+
          as.numeric(biom.list[[i]]$season[tmp3])/4-x.adjust
        xx.tmp <- match(xl2,rownames(ssb.summary))
#        yl <- recruit.summary[xx.tmp,i+1] <- biom.list[[i]]$"recruit-0"[tmp3]
        yl <- biom.list[[i]]$"recruit-0"[tmp3]        
        points(xl[yl!=0],yl[yl!=0],col=col.var[i+1],lty=lty.var[i+1],
               lwd=lwd.var[i+1],type=ptype[1])
        if(vnumber>=2 & sum(tmp2)>0){
          tmp3 <- tmp2&biom$"recruit-0">0          
          points(biom.list[[i]]$year[tmp3]+as.numeric(biom.list[[i]]$season[tmp3])/4-x.adjust,
                 biom.list[[i]]$"recruit-0"[tmp3],pch=3,
                 col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[2],cex=0.5)            
      }}}

    if(!is.null(true.value)){
#      a <- readOM2(true.value,fish.year=0.5)
#      points(a$recruit$x,a$recruit$y/1000,col="plum1",lwd=3,type="l")
      points(true.value$year,true.value$recruit,col="plum1",lwd=2,type="l")          
    }
  }

  invisible(list(biom=biom,biom.list=biom.list,biom.summary=biom.summary,
                 ssb.summary=ssb.summary,#recruit.summary=recruit.summary,
                 ylim.res=ylim.res))
}

plotFvalue2 <-
function(vpadata,
                        year.limit = matrix(c(1952, 1959,
                                 1960, 1969,
                                 1970, 1979,
                                 1980, 1989,
                                 1990, 1999,
                                 2001,2006),2,6),
                        locate="a",type="b",tuika=FALSE,VPA=TRUE,...
                          ){

  if(VPA==TRUE){
    temp <- vpadata$"F at age table"[,-ncol(vpadata$"F at age table")]
  }
  else{
    temp <- vpadata$"F at age table"
  }
  year.label <- as.numeric(rownames(temp))

#  if(range(year.limit) != range(year.label)){
#    stop("The dimension of 'year.limit' is invalid! Please input the vector with ", ncol(temp), " length.\n")
#  }  

  max.f.average <- 0
  leg.tmp <- FALSE
  for(i in c(1:ncol(year.limit))){
    if(sum(floor(year.label)>=year.limit[1,i] & floor(year.label)<=year.limit[2,i])!=0){
      leg.tmp[i] <- TRUE
      if(year.limit[1,i]==year.limit[2,i]){
        max.f.average <- max(c(max.f.average,
                              temp[floor(year.label) >= year.limit[1,i] &
                                                  floor(year.label) <= year.limit[2,i],]))
      }
      else{
        max.f.average <- max(c(max.f.average,
                              apply(as.matrix(temp[floor(year.label) >= year.limit[1,i] &
                                                  floor(year.label) <= year.limit[2,i],]),2,mean)))
      }
    }}
  
#  par(mar=c(4,4,1,1))
  if(tuika==FALSE){
    plot(1:ncol(temp),apply(temp,2,mean),type="n",xlab="Age",
       ylab="F",ylim=c(0,max.f.average),xaxt="n")
    abline(h=seq(from=0,to=3,by=0.5),col="gray")
  axis(1,at=1:ncol(temp),labels=colnames(temp))
  }

  for(i in c(1:ncol(year.limit))){
    temp2 <- temp[year.label >= year.limit[1,i] &
                                year.label <= year.limit[2,i],]
    if(!is.matrix(temp2)){
      points(c(1:ncol(temp)),
             temp2,pch=i,type=type,...)      
    }
    else{
      points(c(1:ncol(temp)),
           apply(as.matrix(temp2),2,mean)
           ,pch=i,type=type,...)
  }
  }

  if(locate!="n"){
                                        # 凡例
    if(locate=="a"){
      basyo <- c(ncol(temp),max.f.average*0.8)
    }
    else{
      if(is.vector(locate)){
        basyo <- locate
      }
      else{
        cat("Please click where you want to write the legend!\n")
        basyo <- locator(1)
      }}
#    browser()
    pch.tmp <- 1:ncol(year.limit)
    legend(basyo[1],basyo[2],legend=paste(year.limit[1,leg.tmp],
                               year.limit[2,leg.tmp],sep="-"),
           pch=pch.tmp[leg.tmp],#lty=c(1:nrow(year.limit)),
           bty="n",xjust=1,yjust=0.8)
    }
}

plotLL.ss <-
function(replist,repfile.legend=NULL,
                      is.plot=TRUE){
  rep.name <- character()
  nrep <- length(replist)
  for(i in 1:nrep){
    rep.name[i] <- chline(replist[i],n=35)
    tmp <- read.LLrep(replist[i])
    tmp <- custamize.LLrep.ss3(tmp)
    if(i==1){
      LLdata <- matrix(0,nrep,length(tmp))
      colnames(LLdata) <- names(tmp)
    }
    LLdata[i,] <- tmp
  }

  if(is.plot==TRUE){
    par.tmp <- par()
    nflame <- ifelse(nrep>5,4,5)
    layout(matrix(1:(nflame*2),nflame,2,byrow=T),
           width=c(3,1.7))
    nflame <- par()$mfcol[1]*par()$mfcol[2]
    par(las=1)
    s <- 1
    for(i in 1:ncol(LLdata)){
      if(!(all(LLdata[,i]==0)|all(is.na(LLdata[,i])))){
        if(s%%2==1){
          par(mar=c(4,15,1,1))
        }
        else{
          par(mar=c(4,0,1,1))
        }
        plot(LLdata[,i],
             x <- 1:nrep,type="n",yaxt="n",pch=20,
             xlab="",ylab="",ylim=c(0.5,max(x)+0.5))
        if(s%%nflame==0) mtext(side=3,outer=T,line=0.5,"@ Likelihood",adj=0.1)
        abline(h=x,col="gray")
        points(LLdata[,i],
               x <- 1:nrep,type="o",yaxt="n",pch=1,
               xlab="",ylab="",ylim=c(0.5,max(x)+0.5))

        if(s%%2==1){
          if(is.null(repfile.legend)){
            label.tmp <- rep.name
          }
          else{
            label.tmp <- repfile.legend
          }
          axis(at=x,labels=label.tmp,side=2)
        }
        title(colnames(LLdata)[i])
        s <- s+1
      }
    }
    par(par.tmp)
  }
  invisible(LLdata)
}

plotSRcurve <-
function(repfile=NULL,SRfunc=NULL,parameter=NULL,add=FALSE,col.var=1,plotVB=FALSE){
  if(is.null(SRfunc)&!is.null(repfile)){
    SRfunc <- getSRfunc.ss2(repfile)[[1]]
  }
  else{
    if(is.null(repfile)) cat("please enter file name of repfile or biomass object!!")
  }
  if(is.null(parameter)&!is.null(repfile)){
    parameter <- getSRpara.ss2(repfile)[[1]]
  }
  else{
    if(is.null(repfile)) cat("please enter file name of repfile or parameter object!!")
  }

  if(!plotVB){
    x.tmp <- SRfunc$year!="Virg"
    }
  else{
    x.tmp <- TRUE
  }

  if(sum(colnames(SRfunc)=="era")>0){
    pch.tmp <- as.numeric(SRfunc$era)
  }
  else{
    pch.tmp <- 1
  }
    #---------- plot
  if(add==FALSE){
    plot(x <- SRfunc$"spawn_bio"[x.tmp],y <- SRfunc$"pred_recr"[x.tmp],
         xlab="Spawning biomass",ylab="Recruitment",
         ylim=c(0,max(y,na.rm=T)),
         xlim=c(0,max(x,na.rm=T)),pch=pch.tmp[x.tmp])
    title("Spawner&Recruitment",line=1)
  }
  else{
    points(SRfunc$"spawn_bio"[x.tmp],SRfunc$"pred_recr"[x.tmp],col=col.var,
           pch=pch.tmp)    
  }
  points(x <- seq(from=0,to=max(SRfunc$"spawn_bio"[x.tmp],na.rm=T),length=100),
         y0 <- Beverton.holt.ss2(SSB=x,coef=c(parameter$steepness,exp(parameter$logR0),
                                   B0=SRfunc$SPBzero[1])),xlim=c(0,max(x)),
                           type="l",lwd=2,col="chartreuse3")

  text(rev(x)[1],rev(y0)[1],"R0")
  if(sum(colnames(SRfunc)=="era")>0){
    legend("topright",legend=unique(SRfunc$era[x.tmp]),pch=as.numeric(unique(SRfunc$era[x.tmp])))
  }  
  invisible(list(SRfunc=SRfunc,parameter=parameter))
}

plotSelect <-
function(selects,multiplot=FALSE,len.or.age="Length",ptype="l",col.var=1,lty.var=1,lwd.var=1,nline=1){
  # Only for allplot.ss2
  if(multiplot) len.rep <- length(selects)
  nfleet <- ncol(selects[[1]])
  setncol(nfleet)
  #  par(mfcol=c(ceiling(nfleet/2),2),ps=16,mar=c(4,4,1,1))
  # if the selectivity is competely same as the one plotted just before, the selectivity will not be plotted
  # This decision is done only in multiplot[[1]]
  old.selects <- 0
  title.tmp <- strsplit(colnames(selects[[1]]),"-")
  s <- 0
  ss <- numeric()
  for(i in 1:nfleet){
    selects0 <- selects[[1]]

    if(sum(old.selects!=selects0[,i])!=0 |
       ifelse(i==1,TRUE,title.tmp[[i]][1]!=title.tmp[[i-1]][1])){

      if(i>1){
        title(paste(title.tmp[[i-1]][1],"-",title.tmp[[ss[s]]][2],"to",title.tmp[[i-1]][2],"-",title.tmp[[i-1]][3]),line=nline)
      }
      
      plot(rownames(selects0),old.selects <- selects0[,i],type=ptype,xlab="",ylab="",lwd=lwd.var[1])
      if(s %% prod(par()$mfcol)==1)
        mtext(side=3,line=0.5,adj=0.1,paste("@ ",len.or.age,"selectivity"),outer=T)            
      s <- s+1
      ss[s] <- i

      if(multiplot){
        for(j in 2:len.rep){
          points(rownames(selects[[j]]),selects[[j]][,i],type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
        }}         

#      nfile <- 1
    }}
  title(paste(title.tmp[[i-1]][1],"-",title.tmp[[ss[s]]][2],"to",title.tmp[[i-1]][2],"-",title.tmp[[i-1]][3]),line=nline)  
  if(s < prod(par()$mfcol)) mtext(side=3,line=0.5,adj=0.1,paste("@ ",len.or.age,"selectivity"),outer=T)  
}

plotTotcatch <-
function(biom,biom.list=NULL,
                         col.var=rep(1,1+length(biom.list)),lty.var=rep(1,1+length(biom.list)),
                         lwd.var=rep(1,1+length(biom.list)),ptype="b",nline=-1,
                         byyear=TRUE,titlename="Total catch",Total.plot=FALSE,
                         ylab.tmp="Total catch",findseq="retain(B):_",FUN=sum,plot.obscatch=FALSE){
#  nfile <- 1
  islist <- is.list(biom.list)
  if(islist) ptype <- "l"

  totcatch <- 0

  target.cols <- as.list(numeric())
  target.cols[[1]] <- which(substr(names(biom),1,nchar(findseq[1]))==findseq[1])

  if(length(target.cols[[1]])==0 && length(findseq)>1){
    s <- 2
    while(length(target.cols[[1]])==0 && s<=length(findseq)){
      target.cols[[1]] <- which(substr(names(biom),1,nchar(findseq[s]))==findseq[s])
      s <- s+1
    }
  }

  res.summary <- list(numeric())
     
  if(islist){
#    for(i in 1:(length(findseq)-1)){
    for(i in 1:(length(biom.list))){    
#      target.cols[[i+1]] <- which(substr(names(biom.list[[i]]),1,nchar(findseq[i+1]))==findseq[i+1])
      target.cols[[i+1]] <- which(substr(names(biom.list[[i]]),1,nchar(findseq[1]))==findseq[1])      
      if(length(target.cols[[i+1]])==0 && length(findseq)>1){
        s <- 2
        while(length(target.cols[[i+1]])==0 && s<=length(findseq)){
          target.cols[[i+1]] <- which(substr(names(biom),1,nchar(findseq[s]))==findseq[s])
          s <- s+1
        }
      }
    }}
  
  ncatch <- length(target.cols[[1]])
  setncol(ncatch)
  s <- 1
  for(i in 1:ncatch){
#    if(vnumber>=2){ 
      tmp2.1 <- biom$period=="TIME"  & biom[,target.cols[[1]][i]]!="--"
      tmp2.2 <- biom$period=="FORE"  & biom[,target.cols[[1]][i]]!="--"
      tmp2 <- tmp2.1|tmp2.2          
#    }
#    else{
      if(sum(tmp2)==0){  # for the version older than 2.00
        tmp2 <- biom[,i]!="--" & biom$season!="E"
      }
    if(byyear==TRUE){
      y <- tapply(as.numeric(as.vector(biom[tmp2,target.cols[[1]][i]])),biom$year[tmp2],FUN)
      totcatch <- y+totcatch
      x.year <- unique(biom$year[tmp2])
      plot(x.year,y,
           type=ptype[1],lwd=lwd.var[1],ylab=ylab.tmp,xlab="Year",ylim=c(0,max(y)))

      # record plotted values
#      browser()
      if(i==1){
        res.summary[[1]] <- array(0,dim=c(length(x.year),ncatch,ifelse(plot.obscatch==TRUE,2,1)))
        dimnames(res.summary[[1]]) <- list(x.year,1:ncatch,NULL)
      }
      res.summary[[1]][,i,1] <- y
      
      if(plot.obscatch){  #
        plus.factor <- ifelse(substr(findseq[1],1,9)=="ret_catch",
                              1,ifelse(findseq[1]=="retain(N)",1,4))
        y <- tapply(as.numeric(as.vector(biom[tmp2,target.cols[[1]][i]+plus.factor])),
                                              biom$year[tmp2],sum)
        points(x.year,y,type="l",lwd=2,col="gray")
        res.summary[[1]][,i,2] <- y
        if(sum(tmp2.2)!=0){
          points(unique(biom$year[tmp2.2]),tapply(as.numeric(as.vector(biom[tmp2.2,target.cols[[1]][i]+1])),biom$year[tmp2.2],sum),
                 type="b",lwd=lwd.var[1],pch=3)        
        }
      }      
      title(names(biom)[target.cols[[1]][i]],line=nline)
    }
    else{
      y <- biom[tmp2,target.cols[[1]][i]]
      totcatch <- y+totcatch
      x.year <- biom$year[tmp2]
      plot(x.year,y,
           ylab=ylab.tmp,xlab="Year",type=ptype[1],lwd=lwd.var[1],ylim=c(0,max(max(y))))
      title(names(biom)[target.cols[[1]][i]],line=nline)

      # record plotted values
      if(i==1){
        res.summary[[1]] <- array(0,dim=c(length(x.year),ncatch,ifelse(plot.obscatch==TRUE,2,1)))
        dimnames(res.summary[[1]]) <- list(x.year,1:ncatch,NULL)
#        dimnames(res.summary[[1]])[[3]] <- 
      }
      res.summary[[1]][,i,1] <- y
      # done
      
      if(plot.obscatch){ # only for >2.00, plot observed catch
        y <- as.numeric(biom[tmp2,target.cols[[1]][i]+plus.factor])
        points(x.year,y,type="l",lwd=2,col="gray")
        res.summary[[1]][,i,2] <- y        
        if(sum(tmp2.2)!=0){
          points(biom$year[tmp2.2],as.numeric(biom[tmp2.2,target.cols[[1]][i]+1]),
                 type="b",lwd=lwd.var[1],pch=3)        
        }        
      }      
    }

      ## For overlapped plots
    if(islist){
      len.list <- length(biom.list)      
      for(j in 1:len.list){
          tmp2.1 <- biom.list[[j]]$period=="TIME"  & biom.list[[j]][,target.cols[[j+1]][i]]!="--"
          tmp2.2 <- biom.list[[j]]$period=="FORE"  & biom.list[[j]][,target.cols[[j+1]][i]]!="--"
          tmp2 <- tmp2.1|tmp2.2          
          if(sum(tmp2)==0){ # for the version older than 2.00
            tmp2 <- biom.list[[j]][,i]!="--" & biom.list[[j]]$season!="E"
          }
        if(byyear==TRUE){
          x.year <- unique(biom.list[[j]]$year[tmp2])
          y <- tapply(as.numeric(as.vector(biom.list[[j]][tmp2,target.cols[[j+1]][i]])),
                        biom.list[[j]]$year[tmp2],sum)
          points(x.year,y,type="l",
                 col=col.var[j+1],lty=lty.var[j+1],lwd=lwd.var[j+1])
        }
        else{
          points(x.year <- biom.list[[j]]$year[tmp2],
                 y <- as.numeric(biom.list[[j]][tmp2,target.cols[[j+1]][i]]),
                 type="l",col=col.var[j+1],lty=lty.var[j+1],lwd=lwd.var[j+1])          
        }
          
        # record plotted values
        if(i==1){
          res.summary[[j+1]] <- array(0,dim=c(length(x.year),ncatch,1))
          dimnames(res.summary[[j+1]]) <- list(x.year,1:ncatch,"exp")
        }
        res.summary[[j+1]][,i,1] <- y
          # done
        }}
      
      ## For title 
      if(ncatch>=10 && s%%10==0){
        mtext(titlename,side=3,line=0.5,adj=0.1,outer=T)
        #      nfile <- nfile+1
        #      makedevice(filename=titlename,dev.type=names(dev.cur()),filenum=nfile,
        #                 htmlfile=htmlfile,new=T,append=T)
        #      setncol(ncatch)
      }
      s <- s+1
    }
  invisible(list(totcatch=totcatch,res.summary=res.summary))
}

plotlength.fit <-
function(repfile,lty.var=rep(1,length(repfile)),col.var=1:length(repfile),
                           lwd.var=rep(1,length(repfile)),compfile=NULL,tb=NULL,cl=NULL,
                           len=FALSE,len.residual.plot=FALSE){
#  set.mypar()
  nrep <- length(repfile)
  multiplot <- nrep>1
#  comps.target <- c(100,100)
  comps <- as.list(1:nrep)
  len.rep <- nrep
  for(i in 1:nrep){
  #!!!!!!!!-------- belows are just only for SS3
    tmp <- getAgecomp.ss2(repfile[i],cl=cl[[i]],tb=tb[[i]],len=len,compfile=compfile[i])
#                          target.line=c(comps.target[1]-10,comps.target[1]-10))
    comps[[i]] <- list(tmp[[1]],tmp[[2]])
#    if(i>1) comps.target <- c(comps.target[1]-nrow(comps[[i]][[1]])-100,
#                            comps.target[2]-nrow(comps[[i]][[2]])-100)
  }

  len.data <- comps[[1]][[1]][comps[[1]][[1]]$kind==ifelse(len==TRUE,"LEN","AGE"),]
  fleet.row <- sort(unique(comps[[1]][[1]]$fleet))
  fleet.name <- 1:length(fleet.row)
 
  # Pearson residual of length data:
  # !!!!!!!! Caution: when size data is too huge, this plot make the result file too heavy!!!!!!!!
#  if(0){
  if(len.residual.plot==TRUE){
    #browser()
    par(mfrow=c(3,2))
    s <- 1
      for(i in fleet.row){
        tmp <- len.data[len.data$fleet==i,]
        plot(tmp$bin,y <- tmp$Pearson,ylim=c(-3,6),xlab="Length",ylab="Pearson residuals")
#        boxplot(y <- tmp$Pearson~tmp$bin,at=sort(unique(tmp$bin)),ylim=c(-3,6),xlab="Length",ylab="Pearson residuals")        
        title(main=paste("Fleet",i,":",fleet.name[i]),line=-1)
      #    sd.tmp <- (tmp$obs-tmp$exp)/sqrt(tmp$exp*(1-tmp$exp)*tmp$N)
      #    plot(tmp$bin,sd.tmp,
        if(!multiplot){
          x1 <- tapply(y,tmp$bin,median)
          x2 <- tapply(y,tmp$bin,mean)  
          abline(h=0,col="yellow")
          points(names(x1),x1,type="l",col="red")
          points(names(x2),x2,type="l",col="blue")
        }
        else{
          for(j in 2:len.rep){
            tmp <- comps[[j]][[1]][comps[[j]][[1]]$fleet==i,]
#            points(tmp$bin,y <- tmp$Pearson,col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
            points(tmp$bin,y <- tmp$Pearson)
          }
        }
      }
    }
#    }

    ###!!!!!!!!!!! NOT FOR USE currently !!!!!!!!!!!!!!!11
    if(0){
      for(i in fleet.row){
        plot(x <- len.data$exp[len.data$fleet==i & !is.na(len.data$obs)],y <- len.data$obs[len.data$fleet==i & !is.na(len.data$obs)],lwd=lwd.var[1],
             ylim=c(0,max(x,y)),xlim=c(0,max(x,y)),xlab="Expectet size freq",ylab="Observed size freq")
        title(main=paste("fleet ",i),line=nline)

        if(multiplot){
          for(j in 2:len.rep){
            tmp <- comps[[j]][[1]]
            points(x <- tmp$exp[tmp$fleet==i & !is.na(tmp$obs)],
                   y <- tmp$obs[tmp$fleet==i & !is.na(tmp$obs)]
                   ,type="p",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
          }}
        if(length(fleet.row)>10 && s%%10==0){
          mtext(side=3,line=0.5,adj=0.3,"Expected vs observed size composition",outer=T)
#          nfile <- nfile+1
#          makedevice(filename="length_cor",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                     new=T,append=T)
#          setncol(length(fleet.row))
        }
        s <- s+1
      }
      mtext(side=3,line=0.5,adj=0.3,"Expected vs observed size composition",outer=T)
      cat("Scatter plot of length frequency (obs vs est) was done.  \n")
    }

  ## Size frequency 2
  length.bin <- sort(unique(len.data$bin))
  sum.length <- list(array(0,dim=c(length(length.bin),length(fleet.row),2)))
  dimnames(sum.length[[1]]) <- list(length.bin,fleet.row,c("Obs","Exp"))
  if(multiplot){
    for(kk in 2:nrep){
      sum.length[[kk]] <- sum.length[[1]]
    }
  }
    
  s <- 1
  row.name <- paste("F:",len.data$fleet,"-Y:",floor(as.numeric(len.data$year)),
                    "-S:",len.data$season,sep="")
  original.uniquerow <- unique(row.name)
  tmp.ref <- paste("F:",comps[[1]][[2]]$Index,
                   "-Y:",floor(as.numeric(comps[[1]][[2]]$Year)),
                   "-S:",comps[[1]][[2]]$Seas,sep="")

  if(multiplot){
    row.name.list <- tmp.ref.list <- list()
    for(k in 2:nrep){
      b.tmp <- comps[[k]]#[comps[[k]][[1]]$kind==ifelse(len==TRUE,"LEN","AGE"),]
      b.tmp[[1]] <- b.tmp[[1]][b.tmp[[1]]$kind==ifelse(len==TRUE,"LEN","AGE"),]
      row.name.list[[k]] <- paste("F:",b.tmp[[1]]$fleet,
                                  "-Y:",floor(as.numeric(b.tmp[[1]]$year)),
                                  "-S:",len.data$season,sep="")
      tmp.ref.list[[k]] <- paste("F:",b.tmp[[2]]$Index,
                                 "-Y:",floor(as.numeric(b.tmp[[2]]$Year)),
                                 "-S:",b.tmp[[2]]$Seas,sep="")    
    }}
  
  par(mar=c(1.5,2,1,1),mfcol=c(8,4))  
  for(j in 1:length(fleet.row)){
    for(i in 1:length(original.uniquerow)){
      row.tmp <- row.name==original.uniquerow[i]
      if(sum(len.data$fleet[row.tmp]==fleet.row[j])){
        matplot(x <- len.data$bin[row.tmp],
                y <- cbind(len.data$obs[row.tmp],
                           len.data$exp[row.tmp])*comps[[1]][[2]]$Nsamp[tmp.ref==original.uniquerow[i]],
                col=c("royalblue",1),type=c("b","l"),                
                pch=1:2,lty=1:1,cex=0.7,ylab="",xlab="")
        ## Sum up size data by fisheries
        y <- y[!is.na(y[,1]),]
        x <- x[!is.na(x)]
        sum.length[[1]][match(x,length.bin),j,] <- sum.length[[1]][match(x,length.bin),j,]+y
        ##
        title(main=paste(original.uniquerow[i]),line=-0.7,adj=0.1,cex.main=0.7)
      
        ## For multiple plots ##
        if(multiplot){
            for(k in 2:nrep){
              row.tmp <- row.name.list[[k]]==original.uniquerow[i]
              x <- b.tmp[[1]]$bin[row.tmp]
              y <- cbind(b.tmp[[1]]$obs[row.tmp],
                         b.tmp[[1]]$exp[row.tmp]) *
                           b.tmp[[2]]$Nsamp[tmp.ref.list[[k]]==original.uniquerow[i]]
              points(x,y[,2],col=col.var[k],lty=lty.var[k],lwd=lwd.var[k],type="l")
              y <- y[!is.na(y[,1]),]
              x <- x[!is.na(x)]
              sum.length[[k]][match(x,length.bin),j,] <- sum.length[[k]][match(x,length.bin),j,]+y            
            }}
        if(s%%32==0){
          mtext(side=3,line=0.5,adj=0.1,
                "@ Length fit (by each sampling, line: expected, line+circle: observed)",outer=T)
        }
        s <- s+1                        
        
      }
        ##

    }}
  cat("Plot of expected and observed length frequency by each observation was done.  \n")
    
#  par(mfrow=c(length(fleet.row),1))
#    nfile <- 1
#    makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=T,append=T)    
    n <- dim(sum.length[[1]])[[2]]
    setncol(n)  
    for(i in 1:n){
      matplot(x <- length.bin,y <- sum.length[[1]][,i,],type=c("b","l"),lty=1,
#              pch=1,ylab="nsmple",col=c("black","royalblue"),lwd=lwd.var[1],xlab="Length (cm)")
              pch=1,ylab="nsmple",col=c("black","royalblue"),xlab="Length (cm)")              
#      title(paste("Fleet",fleet.row[i],":",fleet.name[fleet.row[i]]),line=nline)
      title(paste("Fleet",fleet.row[i]))
      
      if(multiplot){
          for(k in 2:nrep){
            points(length.bin,sum.length[[k]][,i,2],
                   type="l",col=col.var[k],lty=lty.var[k],lwd=lwd.var[k])
          }}
     
      
      if(n>10 && i%%32==0){
        mtext(side=3,line=0.5,adj=0.1,
              "@ Length fit (by fleet, line: expected, line+circle: observed)",outer=T)
#        nfile <- nfile+1
#        makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                   new=T,append=T)                
#        setncol(n)
      }}
    mtext(side=3,line=0.5,adj=0.3,"@ Length fit (by fleet, line: expected, line+circle: observed))",
          outer=T)
    cat("Plot of expected and observed length frequency by fleets was done.  \n")    

    # Bubble plot not for multiplot
    xrange <- range(len.data$year)
    yrange <- range(len.data$bin,na.rm=T)    
#    max.res.fleet <- tapply(abs(len.data$Pearson),len.data$fleet,max,na.rm=T)
    par(mfrow=c(3,1),mar=c(1.5,3,1.5,1))
    col.tmp <- c("black","black")
    col.tmp2 <- c(NA,"darkblue")
    ss  <- 1
    for(j in 1:length(fleet.row)){
      xxx <- len.data$fleet==fleet.row[j] & len.data$Pearson!=0 & !is.na(len.data$Pearson)
      if(sum(xxx)>0){
        year.range <- range(as.numeric(len.data[xxx,]$year))
        if(diff(year.range)<20){
          with(len.data[xxx,],symbols(as.numeric(year),bin,circles=sqrt(abs(x <- Pearson))/8,
                                      fg=col.tmp[(x<0)+1],bg=col.tmp2[(x<0)+1],lwd=0.5,
                                      inches=FALSE,ylim=yrange,xlim=year.range))#as.numeric(xrange)))
          title(paste("Fleet",fleet.row[j]))
          ss <- ss+1
        }
        else{
          ranges <- list(c(year.range[1],mean(year.range)),
                         c(mean(year.range),year.range[2]))
          for(kk in 1:2){
            with(len.data[xxx,],symbols(as.numeric(year),bin,circles=sqrt(abs(x <- Pearson))/8,
                                        fg=col.tmp[(x<0)+1],bg=col.tmp2[(x<0)+1],lwd=0.5,
                                        inches=FALSE,ylim=yrange,
                                        xlim=ranges[[kk]]))
            title(paste2("Fleet ",fleet.row[j]," (",ranges[[kk]][1],"-",ranges[[kk]][2],")"))
            ss <- ss+1            
          }
#          browser()
        }
      }
      if(ss%%3==0) mtext("@ Bubble plot of Pearson residuals, darkblue (obs<exp), white (obs>exp)",outer=T,adj=0.1)
    }
}

qtback <-
function(num){
 (as.numeric(num)-floor(as.numeric(num)))*4 + 1
}

read.LLrep <-
function(repfile){
  cl <- count.fields(repfile,blank.lines.skip=FALSE)
  TLL <- find.and.read.table("LIKELIHOOD",skipline=0,startpoint=0,
                             table.property=cl,comment.char="",fill=T,gyou=NULL,
                             outfile=repfile,h=FALSE,is.ss2=TRUE)

  if(!is.ss3(repfile)){
    LLs <- find.and.read.table("Forecast_Recruitment",skipline=2,startpoint=0,
                               table.property=cl,comment.char="",fill=T,gyou=NULL,
                               outfile=repfile,h=TRUE,is.ss2=TRUE)
  }
  else{
    LLs <- find.and.read.table("Fleet:",skipline=0,startpoint=0,
                               table.property=cl,comment.char="",fill=T,gyou=NULL,
                               outfile=repfile,h=TRUE,is.ss2=TRUE)    
  }

  list(TLL=TLL[[1]],LLs=LLs[[1]])
}

read.bootfiles <-
function(bootfiles,calc.PS=FALSE){
  nboot <- length(bootfiles)
  biom <- list()
  for(i in 1:nboot){
      biom[[i]] <- getBabs.ss2(bootfiles[i])[[1]]
      cat(i," ")
    }

  biom.mat <- matrix(0,nrow(biom[[1]]),nboot)
  for(i in 1:nboot){
    biom.mat[,i] <- biom[[i]]$SpawnBio
  }
  rownames(biom.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
  biom.mat <- as.data.frame(t(biom.mat))
  
  rec.mat <- matrix(0,nrow(biom[[1]]),nboot)
  for(i in 1:nboot){
    rec.mat[,i] <- biom[[i]]$"recruit-0"
  }
  rownames(rec.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
  rec.mat <- as.data.frame(t(rec.mat))

  # total catch # previous version
#  tc.mat <- matrix(0,nrow(biom[[1]]),nboot)
#  for(i in 1:nboot){
#    tmp <- apply(a[,substr(colnames(biom[[i]]),1,9)=="ret_catch"],1,sum)  # total catch
#    tc.mat[,i] <- tapply(tmp,biom[[i]]$year,sum)
#  }
#  rownames(tc.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
#  tc.mat <- as.data.frame(t(tc.mat))

  # Calculate population size
  if(calc.PS==FALSE){
    population.size <- NULL
  }
  else{
#    population.size <- rec.mat
#    population.size[] <- 0
    for(i in 1:nboot){
      a <- getNAA.ss2(bootfiles[i])[[1]]
      b <- getNMA.ss2(bootfiles[i])[[1]]
      x <- apply(sweep(a[,-1:-10],2,b$Age_Mat[b$Seas==4],FUN="*"),1,sum)
      if(i==1){
        population.size <- matrix(0,nboot,length(x))
        dimnames(population.size) <- list(1:nboot,a$Year+a$Seas/4-0.25)
      }
      population.size[i,] <- x
    }
  }
  
  return(list(biom.list=biom,rec.mat=rec.mat,biom.mat=biom.mat,population.size=population.size))
  
}

read.datfile <-
function(datfile="ss2.dat",outfile="ss_new.dat",
                                             cpue.replace=NULL,catch.replace=NULL,length.replace=NULL){
  
  # aに欠損値を含む、全てのデータが入った行列を入れる。今後はaの中身を変更していく
  cf <- count.fields(datfile,comment="",blank.lines.skip=T)
  a <- read.table(datfile,fill=T,col.names=paste("V",1:max(cf),sep=""),as.is=T)  
  a <- as.data.frame(a)

  nyear <- as.numeric(a[2,1])-as.numeric(a[1,1])+1
  y1 <- nyear*as.numeric(a[3,1])+18
  ncpue <- as.numeric(a[y1,1])
  cpue <- a[cpue.range <- ((y1+1):(y1+ncpue)),]
  ndiscard <- as.numeric(a[ncpue+y1+2,1])
  nmeanwt <- as.numeric(a[ncpue+y1+3+ndiscard,1])
  nobs <- as.numeric(a[y2 <- ncpue+y1+10+ndiscard+nmeanwt,1])
  length.label <- a[y2-1,]
  len.comp.obs <- a[length.range <- ((y2+1):(y2+nobs)),]
  catch <- a[catch.range <- 18:(y1-1),]
#  browser()

  # 後ろから順に置き換える
  if(!is.null(length.replace)){
    length.new <- matrix(NA,nrow(length.replace),ncol(length.replace))
    length.new[,1:ncol(length.replace)] <- as.matrix(length.replace)
    a <- rbind(a[1:(min(length.range)-1),],
               length.new,
               a[(max(length.range)+1):nrow(a),])
    a[y2,1] <- nrow(length.new)
  }

  if(!is.null(cpue.replace)){
    cpue.new <- matrix(NA,nrow(cpue.replace),ncol(cpue))
    cpue.new[,1:ncol(cpue.replace)] <- as.matrix(cpue.replace)
    a <- rbind(a[1:(min(cpue.range)-1),],
               cpue.new,
               a[(max(cpue.range)+1):nrow(a),])
    a[y1,1] <- nrow(cpue.new)
  }

  if(!is.null(catch.replace)){
    catch.new <- matrix(NA,nrow(catch.replace),ncol(catch))
    catch.new[,1:ncol(catch.replace)] <- as.matrix(catch.replace)
    a <- rbind(a[1:(min(catch.range)-1),],
               catch.new,
               a[(max(catch.range)+1):nrow(a),])
    a[17,1] <- nrow(catch.new)
    a[2,1] <- max(floor(as.numeric(rownames(catch.replace))))
  }

  cat("# This file is the dat file, in which some catch data were automatically replaced into zero\n",file=outfile,append=F)
  write.table(a,na="",file=outfile,row.names=F,col.names=F,append=T,quote=FALSE)
    
  # 古いサンプルサイズと新しいサンプルサイズを返す
  invisible(list(catch=catch,size=len.comp.obs,cpue=cpue,length.label=length.label[!is.na(length.label)]))
}

read.fleetname <-
function(datfile){
    cf <- count.fields(datfile,comment="",blank.lines.skip=T)
    a <- read.table(datfile,fill=T,col.names=paste("V",1:max(cf),sep=""),as.is=T)
#    fleet.name <- strsplit(a[8,1],"%")[[1]]
    fleet.name <- strsplit(a[9,1],"%")[[1]]    
    fleet.name
}

read.grad <-
function(parfile="ss2.par"){ # 
  a <- read.table(parfile,nrows=1,comment="")
  as.numeric(a[length(a)])
}

read.like <-
function(parfile="ss2.par"){ # 
  a <- read.table(parfile,nrows=1,comment="")
  as.numeric(a[11])
}

read.std <-
function(stdfile,foryear=NULL,esyear=NULL){
  b <- read.table(stdfile,fill=T)
  b <- b[-1,c(-1,-5)]
  colnames(b) <- c("label","estimates","std")
  b <- as.data.frame(b)
  b$estimates <- as.numeric(as.character(b$estimates))
  b$std <- as.numeric(as.character(b$std))

#  ssb <- fssb <- rec <- frec <- data.frame()

  tmp <- b$label=="spbio_std"

  if(is.null(esyear)) esyear <- 1:(sum(tmp)-2)
  ssb <- data.frame(x=esyear,
                    y=b$estimates[tmp][-1:-2],
                    sd=b$std[tmp][-1:-2])

  if(!is.null(foryear)){
    tmp <- b$label=="depletion"
    b0 <- b[tmp,][6:(6+foryear-1),]
    fssb <- data.frame(y=b0$estimates,
                      sd=b0$std,
                      x=(max(esyear)+1):(max(esyear)+foryear))
  }
  

  tmp <- b$label=="recr_std"
  rec <- data.frame(y=b$estimates[tmp][-1:-2],
                    x=esyear,
                    sd=b$std[tmp][-1:-2])

  if(!is.null(foryear)){
    tmp <- b$label=="depletion"
    b0 <- b[tmp,][(6+foryear):(6+foryear+foryear-1),]
    frec <- data.frame(y=b0$estimates,
                      sd=b0$std,
                      x=(max(esyear)+1):(max(esyear)+foryear))
  }

  if(is.null(foryear)){
    list(rec=rec,ssb=ssb)
  }
  else{
    list(rec=rec,frec=frec,ssb=ssb,fssb=fssb)    
  }
}

readLike <-
function(filename){
  ## Read likelihood from multiple files (only for SS2)
  like.b <- matrix(0,13,n <- length(filename))
  for(i in 1:n){
    like.b[,i] <- read.table(filename[i],skip=10,nrow=13)[,2]
  }
  like.b
}

readSS22 <-
function(repfile="ss2.rep",...){
  res <- getBabs.ss2(repfile,...)[[1]]

  y.name <- res$year[-1:-2]+as.numeric(res$season[-1:-2])/4
  tmp <- res$"recruit-0"[-1:-2]
  
  list(biomass=list(x=y.name,y=res$"bio-all"[-1:-2]),
       SSB=list(x=y.name,y=res$"SpawnBio"[-1:-2]),
       recruit=list(x=y.name[tmp!=0],y=1000*tmp[tmp!=0]))       
}

replace.sd.offset <-
function(ctrfile,repfile,newctl="control_new.ctl",def.sd=0.2,replace.sd=c(T,T,T,T,T,T),vnumber=1){
  b <-  getCPUE.ss2(repfile,target.line=11,vnumber=vnumber)[[3]]
  
  # 注！！controlファイルの置き換えは、ctrlファイルの行数を後ろからカウントして
  # 行っているので、"variance adjustment factors"の記述以降のファイルには、
  # コメントアウト以外の余計な改行を入れないようにしてください
  cl <- count.fields(ctrfile,comment="#",blank.lines.skip=T)
  a <- read.table(ctrfile,col.names=1:max(cl),fill=T)

  nline <- ifelse(vnumber<2,20,22)
#  a[nrow(a)-nline,1:length(def.sd)] <- b[,4]-def.sd
  a[nrow(a)-nline,1:length(def.sd)] <- b$r.m.s.e-def.sd  

  write.table(a,file=newctl,na="",row.names=F,col.names=F,quote=FALSE)

  return(b$r.m.s.e)
}

rowtapply <-
function(a0){
  yname <- floor(as.numeric(rownames(a0)))
  res <- matrix(0,length(unique(yname)),ncol(a0))
  for(i in 1:ncol(a0)){
    res[,i] <- tapply(a0[,i],yname,sum)
  }
  dimnames(res) <- list(unique(yname),colnames(a0))
  res
}

set.mypar <-
function(){
  par(mgp=c(1.3,0.3,0),tck=0.03,ps=10,cex.main=1,font.main=1,adj=0.5,oma=c(0,0,0,0))
}

setncol <-
function(n){
  if(n<7){
    par(mfrow=c(3,2))
  }
  else{
    if(n<11){
      par(mfrow=c(ceiling(n/2),2))
    }
    else{
      par(mfrow=c(7,3))
      par(mar=c(2,2,1.5,0),oma=c(0,0,3,3),ps=12,mgp=c(1.8,0.3,0))
    }
  }
}

shimose08 <-
function(t){
  lmax <- 233.6
  lmin <- 21.54
  Amax <- 15
  Amin <- 0
  K <- 0.195
  linf <- lmin+(lmax-lmin)/(1-exp(-K*(Amax-Amin)))
#  cat("L inf =",linf,"\n")

  linf + (lmin-linf) * exp((-K)*(t-Amin))
}

show.simpleres <-
function(repfiles,sp.season=4,
                           is.refpoint=FALSE){
  a <- LL <- spr <- as.list(rep(0,length(repfiles)))
  for(i in 1:length(a)){
    a[[i]] <- getBabs.ss2(repfiles[i])[[1]]
    if(is.ss3(repfiles[i])){
      LL[[i]] <- custamize.LLrep.ss3(read.LLrep(repfiles[i]))
    }
    else{
      LL[[i]] <- custamize.LLrep(read.LLrep(repfiles[i]))      
    }
    spr[[i]] <- getSPR.ss2(repfiles[i])[[1]]
  }

  res2 <- matrix(0,length(LL),length(LL[[1]]))
  colnames(res2) <- names(LL[[1]])
  res <- data.frame(Rave=rep(0,length(a)),Rsigma=rep(0,length(a)),R0=rep(0,length(a)),
                    Rini=rep(0,length(a)),
                    SSBstart=rep(0,length(a)),SSB0=rep(0,length(a)),SSBstart.SSB0=rep(0,length(a)),
                    SSB.end=rep(0,length(a)),
                    SPRstart=rep(0,length(a)),SPR.end=rep(0,length(a)),
                    Eq.catch=rep(0,length(a)))

  for(i in 1:length(a)){
    end.year <- max(a[[i]]$year[a[[i]]$period=="TIME"])
    start.year <- min(a[[i]]$year[a[[i]]$period=="TIME"])    
    res$Rave[i] <- mean(a[[i]]$recruit[a[[i]]$period=="TIME" & a[[i]]$season==1])
#    res$Rsigma[i] <- sd(log(a[[i]]$recruit[a[[i]]$period=="TIME" & a[[i]]$season==1]))
    res$Rsigma[i] <- sqrt(var(log(a[[i]]$recruit[tmp <- a[[i]]$period=="TIME" & a[[i]]$season==1]))*
                          (sum(tmp))/(sum(tmp)-1))
    res$R0[i] <- a[[i]]$recruit[a[[i]]$period=="VIRG" & a[[i]]$season==1][1]
    res$Rini[i] <- a[[i]]$recruit[a[[i]]$period=="INIT" & a[[i]]$season==1][1]    
  
    res$SSB0[i] <- a[[i]]$Spawn[a[[i]]$period=="VIRG" & a[[i]]$season==sp.season][1]
    res$SSBstart[i] <- a[[i]]$Spawn[a[[i]]$year==start.year & a[[i]]$season==sp.season]
    res$SSBstart.SSB0[i] <- res$SSBstart[i]/res$SSB0[i]
    res$SSB.end[i] <- a[[i]]$Spawn[a[[i]]$year==end.year & a[[i]]$season==sp.season]
    
    res$SPRstart[i] <- spr[[i]]$SPR[spr[[i]]$Year==start.year]
    res$SPR.end[i] <- spr[[i]]$SPR[spr[[i]]$Year==end.year-1]

    if(!is.ss3(repfiles[i])){
      res$Eq.catch[i] <- sum(a[[i]][a[[i]]$period=="INIT" & a[[i]]$season==1,
                                    substr(colnames(a[[i]]),1,5)=="ret_c"])
    }
    else{
      res$Eq.catch[i] <- sum(a[[i]][a[[i]]$period=="INIT" & a[[i]]$season==1,
                 substr(colnames(a[[i]]),1,9)=="retain(B)"])      
    }
    res2[i,] <- LL[[i]]
  }

  res <- cbind(res,res2)
  
  rownames(res) <- repfiles

  if(is.refpoint==TRUE && is.ss3(repfiles[1])){
    f.opt <- numeric()
#    source("~/R/takeuchi_code/ss3util_20090619.r")
#    source("~/R/takeuchi_code/SS3ypr_20090619.r")
    for(i in 1:length(repfiles)){
      aaa <- testAll(repfiles[i])
      f.opt[i] <- 1/aaa$fmult[max(aaa$ypr)==aaa$ypr]
      if(i==1){
        YPR <- aaa$ypr
      }
      else{
        YPR <- cbind(YPR,aaa$ypr)
      }
    }
    res <- cbind(res,f.opt)
    names(res)[length(res)] <- "Fmax/Fcur"
    if(is.refpoint){
      colnames(YPR) <- repfiles
    }
    rownames(YPR) <- aaa$fmult
  }
  if(is.refpoint==TRUE){
    return(list(res,YPR))
  }
  else{
    return(res)    
  }
}

solv.Feq <-
function(cvec,nvec,mvec){

  Fres <- rep(0,length(cvec))
 # cat(nvec," ")
  for(i in 1:length(cvec)){
    F0 <- cvec[i]/nvec[i]
    F1 <- cvec[i]*(F0+mvec[i])/nvec[i]/(1-exp(-F0-mvec[i]))
    if(round(cvec[i],6)<round(nvec[i],6)){
      while(abs(F0-F1)>0.0001 ){
        F0 <- F1
        F1 <- cvec[i]*(F0+mvec[i])/nvec[i]/(1-exp(-F0-mvec[i]))
        if(F0-F1==-Inf) cat("\n",cvec[i]," ",nvec[i]," \n")
      }
      Fres[i] <- F1
    }
    else{
      Fres[i] <- 10
      cat("Warning: catch exceeded tot_num at: ",i," ",
          round(cvec[i],6)," ",round(nvec[i],6),"\n")
      }
  }
  Fres
}

solve.baranov <-
function(nage,naat.pre,select.para,waa,nmaa,CHS){
.C("slvbaranov",
arg1=as.integer(nage),
arg2=as.double(naat.pre),
arg3=as.double(select.para),
arg4=as.double(waa),
arg5=as.double(nmaa),
arg6=as.double(CHS),
arg7=double(1))$arg7
}

sweep2 <-
function(x){
  sweep(x,2,apply(x,2,sum),FUN="/")
}

tlist <-
function(list.x){
  n <- length(list.x)
  list.x2 <- list.x
  for(i in c(1:n)){
    list.x2[[n-i+1]] <- list.x[[i]]
  }
  list.x2
}

to.num <-
function(x){
  as.numeric(as.character(x))
}

to.quarter <-
function(month){
  ceiling(month/3)
}

to.quarterbase <-
function(outdata,qt=4,age.specific.mat=TRUE){ 
  # bological parameter
  if(is.null(outdata$nma$"age_Beg")){
    ages <- outdata$nma$"Age_Beg"
  }
  else{
    ages <- outdata$nma$"age_Beg"
  }
  tmp <- order(ages)
  nmaa <- outdata$nma$M[tmp]  # F at age
  caa.array <- outdata$caa.array
  waa <- outdata$nma$"Wt_Beg"[tmp] # weight at age ( in calculating SSB, the model uses Wt_Beg)
  waa.mid <- outdata$nma$"Wt_Mid"[tmp] # another weight at age 2008/3/31
  if(age.specific.mat==TRUE){
    maa <- outdata$nma$"Age_Mat"[tmp]  # maturity at age if specific maturity at age
  }
  else{
    maa <- outdata$nma$"Len_Mat"[tmp]  # maturity at age if logistic maturity at age
  }
  ages <- sort(ages)
  names(maa) <- names(waa) <- names(nmaa) <- ages

  # estimated Numbers at age
  naat <- outdata$naa  # numbers at age estimated in VPA -> to covert quarter base
  measured.year <- range(as.numeric(naat$YQ[naat$Per=="TIME"]))
  qt.tmp <- naat$Seas
  rownames(naat) <- naat$YQ
#  naat <- naat[,-c(1:3,ncol(naat))]
  naat <- naat[,!is.na(as.numeric(colnames(naat)))]  
  naat <- naat*1000
  faat <- outdata$faa  # estimated fishing mortality

  #------- # total catch by year
  n.age <- length(nmaa)   # number of age
  
  # expand to quarter (naat,faat -> naat.qt, faat.qt)
  naat.qt <- matrix(0,nrow(naat),ncol(naat)*qt,
                               dimnames=list(rownames(naat),ages))
  faat.qt <- matrix(0,nrow(faat),ncol(faat)*qt,
                               dimnames=list(rownames(faat),ages))

  #expand to quarter (caa.array -> caa.array.qt)
  caa.array.qt <- wcaa.array.qt <-
    array(0,dim=c(dim(caa.array)[[1]],dim(caa.array)[[2]]*qt,dim(caa.array)[[3]]),
                        dimnames=list(dimnames(caa.array)[[1]],ages,dimnames(caa.array)[[3]]))  
  
  for(i in 1:nrow(naat)){
    for(a in 1:ncol(naat)){
      naat.qt[i,qt.tmp[i]+(a-1)*qt] <- naat[i,a]
    }}
  
  for(i in 1:nrow(faat)){
    for(a in 1:ncol(faat)){
      faat.qt[i,qt.tmp[i]+(a-1)*qt] <- faat[i,a]
    }
    faat.qt[i,ncol(faat.qt)] <- faat[i,a] # F in plus group
  }


  # expand caa to quarter
  for(f in 1:dim(caa.array)[[3]]){
    for(i in 1:dim(caa.array)[[1]]){
      for(a in 1:(dim(caa.array)[[2]]-1)){
        caa.array.qt[i,qt.tmp[i]+(a-1)*qt,f] <- caa.array[i,a,f]
      }
   }}

  # total catch (waa at begging)
  tc.beg <- tc.mid <- tc.alk <-
    matrix(0,dim(wcaa.array.qt)[[1]],dim(wcaa.array.qt)[[3]],
           dimnames=list(dimnames(wcaa.array.qt)[[1]],dimnames(wcaa.array.qt)[[3]]))
  for(i in 1:dim(wcaa.array.qt)[[3]]){
    wcaa.array.qt[,,i] <- sweep(caa.array.qt[,,i],2,waa,FUN="*")
    tc.beg[,i] <- as.matrix(apply(wcaa.array.qt[,,i],1,sum))
  }

  # total catch (waa at middle)
  for(i in 1:dim(wcaa.array.qt)[[3]]){
    tmp <- sweep(caa.array.qt[,,i],2,waa.mid,FUN="*")
    tc.mid[,i] <- as.matrix(apply(tmp,1,sum))
  }

  ## total catch from ALK
  #tc.alk <- calTotcatch.ALK(repfile)
  #tc.alk <- as.matrix(apply(tc.alk$wcaa.array,c(1,3),sum))

  outdata <- list("Numbers at age table"=naat.qt,
                  "F at age table"=faat.qt,
                  "Natural mortality at age"=nmaa,
                  "Weight at age (kg)"=waa,
                  "Maturity at age"=maa,
                  "Total catch"=t(as.matrix(t(outdata$wtot))),
                  "Age and year"=matrix(c(range(ages),
                    measured.year),2,2,byrow=TRUE),
                  tc.beg=tc.beg,
                  tc.mid=tc.mid,
#                  tc.alk=tc.alk,
                  waa.beg=waa,
                  waa.mid=waa.mid)
}

to.yrmonday <-
function(xyz,split="/"){
  a <- strsplit(as.character(xyz),split=split)
  tmpfunc <- function(vec,n){vec[n]}
  x <- lapply(a,tmpfunc,1)
  y <- lapply(a,tmpfunc,2)
  z <- lapply(a,tmpfunc,3)

  data.frame(year=unfactor(x),month=unfactor(y),day=unfactor(z))

}

toshi <-
function(x){
  if(length(x)==1){
    x1 <- numeric()
    if(x<0) x1 <- x
      else{
          if(floor(log10(x))==0) x1 <- paste("00",x,sep="")
          if(floor(log10(x))==1) x1 <- paste("0",x,sep="")
          if(floor(log10(x))==2) x1 <- x
        }
        x1
  }
  else{
    x1 <- rep(0,length(x))
    for(i in c(1:length(x1))){
        if(x[i]<0) x1[i] <- x[i]
           else{ 
              if(floor(log10(x[i]))==0) x1[i] <- paste("00",x[i],sep="")
              if(floor(log10(x[i]))==1) x1[i] <- paste("0",x[i],sep="")
              if(floor(log10(x[i]))==2) x1[i] <- x[i]
               }   
      }
        x1
  }
}

toshi2 <-
function(x){  
  if(length(x)==1){
    x1 <- numeric()
    if(x<0) x1 <- x
      else{
          if(floor(log10(x))==0) x1 <- paste("000",x,sep="")
          if(floor(log10(x))==1) x1 <- paste("00",x,sep="")
          if(floor(log10(x))==2) x1 <- paste("0",x,sep="")          
          if(floor(log10(x))==3) x1 <- x
          if(x==0) x1 <- "0000"                        
        }
        x1
  }
  else{
    x1 <- rep(0,length(x))
    for(i in c(1:length(x1))){
        if(x[i]<0) x1[i] <- x[i]
           else{ 
              if(floor(log10(x[i]))==0) x1[i] <- paste("000",x[i],sep="")
              if(floor(log10(x[i]))==1) x1[i] <- paste("00",x[i],sep="")
              if(floor(log10(x[i]))==2) x1[i] <- paste("0",x[i],sep="")              
              if(floor(log10(x[i]))==3) x1[i] <- x[i]
              if(x[i]==0) x1[i] <- "0000"              
               }   
      }
        x1
  }
}

toshi3 <-
function(x){
  if(length(x)==1){
    x1 <- numeric()
    if(x<0) x1 <- x
      else{
          if(floor(log10(x))==0) x1 <- paste("0",x,sep="")
          if(floor(log10(x))==1) x1 <- x
        }
        x1
  }
  else{
    x1 <- rep(0,length(x))
    for(i in c(1:length(x1))){
        if(x[i]<0) x1[i] <- x[i]
           else{ 
              if(floor(log10(x[i]))==0) x1[i] <- paste("0",x[i],sep="")
              if(floor(log10(x[i]))==1) x1[i] <- x[i]
               }   
      }
        x1
  }
}

try.nofisheffects <-
function(namesfile=ifelse(vnumber<2,"SS2names.nam","starter.ss2"),vnumber=2,debug.mode=FALSE,is.plot=T){
  
#  com <- command.alias(.Platform$OS.type)
#  com$system(paste2(com$cp," ",namesfile," ",namesfile,"_o"))
#  com$system(paste2(com$cp," ss2.par ss2_old.par"))  
#  on.exit(com$system(paste2(com$cp," ",namesfile," ",namesfile,"_last")))
#  on.exit(com$system(paste2(com$cp," ",namesfile,"_o ",namesfile)),append=TRUE)

  file.copy2(from=namesfile,to=paste2(namesfile,"_o"))
  file.copy2(from="ss2.par",to="ss2_old.par")
  
  on.exit(file.copy2(from=namesfile,to=paste2(namesfile,"_last")))
  on.exit(file.copy2(from=paste2(namesfile,"_o"),to=namesfile),append=TRUE)  

  names.obj <- names.obj.o <- read.table(namesfile,as.is=T)

  # First fun with full catch
  if(debug.mode==FALSE){
    if(vnumber<3){
      doss2()
    }
    else{
      doss3()
    }
  }
#  com$system(paste2(com$cp," ss2.rep ss2_org.rep"))
  if(vnumber<3){
    file.copy2(from="ss2.rep",to="ss2_org.rep")
  }
  else{
    file.copy2(from="report.sso",to="ss2_org.rep")    
  }

  a <- replacedat.catch(names.obj[1,1],outfile="tmp.dat",zero.fleets=NULL)
  nfish <- as.numeric(a$nfish)
  outname <- paste2("minusF1to",1:nfish)
  
  for(i in 1:nfish){
    browser()
    a <- replacedat.catch(names.obj[1,1],outfile=paste2(outname[i],".dat"),zero.fleets=1:i)      
    names.obj[1,1] <- paste2(outname[i],".dat")
    write.table("# Nustarter.SS2 \n",file=namesfile,row.names=F,col.names=F,quote=FALSE)
    write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE,append=T)
    if(debug.mode==FALSE){
      if(vnumber<3){
        doss2()
      }
      else{
        doss3()
      }
      }
#    com$system(paste2(com$cp," ss2.rep ",outname[i],".rep"))
#    com$system(paste2(com$cp," ss2_old.par ss2.par"))
    if(vnumber<3){
      file.copy2(from="ss2.rep",to=paste2(outname[i],".rep"))
    }
    else{
      file.copy2(from="report.sso",to=paste2(outname[i],".rep"))      
    }
    file.copy2(from="ss2_old.par",to="ss2.par")          
  }

  biom <- as.list(1:(nfish+1))
 # getBabs
  for(i in 1:nfish){
    biom[[i]]  <- getBabs.ss2(paste2(outname[i],".rep"))[[1]]
  }
  biom[[nfish+1]] <- getBabs.ss2("ss2_org.rep")[[1]]

  biom.mat <- matrix(0,nrow(biom[[1]]),nfish+1)
  qt <- max(biom[[1]]$season)
  dimnames(biom.mat) <- list(biom[[1]]$year+biom[[1]]$season/qt-1/qt,c(outname,"Org"))
  for(i in 1:nfish){
    biom.mat[,i] <- biom[[i]]$SpawnBio
  }
  biom.mat[,nfish+1] <- biom[[nfish+1]]$SpawnBio  
#  plotBSR(biom[[nfish]],biom[c(1:(nfish-1),nfish+1)])

  if(is.plot==TRUE){
    plot.nofishBio(biom.mat,datfile=names.obj[1,1])
  }

  invisible(list(biom.list=biom,biom.mat=biom.mat))
}

unfactor <-
function (x)
{
    ch <- as.character(x)
    opt <- options(warn = -1)
    num <- as.numeric(ch)
    options(opt)
    if (any(is.na(num)))
        ch
    else num
}

update.number <-
function(n1,n0,a,m,f){
  for(i in 2:(a-1)){
    n1[i] <- n0[i-1]*exp(-f[i-1]-m[i-1])
  }
  n1[a] <- n0[a-1]*exp(-f[a-1]-m[a-1]) + n0[a]*exp(-f[a]-m[a])
  n1
}

vnumber.ss3 <-
function(repfile){
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  tmp <- as.numeric(substr(a[1,1],5,8))
  if(is.na(tmp)) tmp <- FALSE
  tmp
}

watanabe06 <-
function(L){
  8.7 * 10^{-5} * L^{2.67}
}

write.parameter <-
function(zahyou,para.list,kiri=1,moji="-"){
  n <- length(para.list)
  moji <- NULL
  for(i in c(1:n)) {
    ifelse(length(para.list[[i]])==1,
    moji <- paste(moji, names(para.list)[i],": ",paste(para.list[[i]]),"\n"),
           moji <- paste(moji, names(para.list)[i],": ",
                         paste(round(min(para.list[[i]]),kiri)),moji,
                         paste(round(max(para.list[[i]]),kiri)),"\n"))
  }
  text(zahyou[1],zahyou[2],as.character(moji),pos=4)
}

write.parameter.d <-
function(zahyou,para.list,kiri=1,moji="-"){
  n <- length(para.list)
  moji <- NULL
  for(i in c(1:n)) {
#    ifelse(length(para.list[[i]])==1,
    moji <- paste(moji, names(para.list)[i],": ",
                  paste.new(round(para.list[[i]][1:length(para.list[[i]])],kiri)),"\n")
  }
#           moji <- paste(moji, names(para.list)[i],": ",
#                         paste(round(min(para.list[[i]]),kiri)),moji,
#                         paste(round(max(para.list[[i]]),kiri)),"\n"))
#  }
  text(zahyou[1],zahyou[2],as.character(moji),pos=4)
}

y.per.recruits <-
function(maa,waa,saa,fs){
  wcs <- rep(0,length(fs))
  for(j in c(1:length(fs))) wcs[j] <- y.per.recruit(maa,waa,saa,fs[j])

  wcs
}

yukinawa.yabuta <-
function(age){
  len <- 320.5*(1-exp(-0.1035*age-0.0728))
  len
}

