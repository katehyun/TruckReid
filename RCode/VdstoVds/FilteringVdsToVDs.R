install.packages("stringr")
rm(list=ls())
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
load("./ProcessedData/Oct02/IrvineLoadinOct02")
load("./ProcessedData/Mar20/IrvineLoadinMar20")


######## check duration and filter out (When WIM data comes in, step-by-step filtering approches needed) #######
### Time Window ###

# buffertimewindow=0.35; # 0.3 min (VDS-VDS case)
buffertimewindow=0.3; # 0.3 min (WIM-WIM case)
bufferduration = 0.2; # 0.2 min
buffernumpnt = 500


### input file - Oct 02
vdsheader = Irvine.VDSOct02Header
vdsheader_new <-subset(vdsheader, v8 > 300) # collect only data points > 300 (truck)
wimheader = Irvine.WIMOct02Header
wimheader_new <-subset(wimheader, v8 > 300)

wimsig = Irvine.WIMOct02sig
wimsig_IM=subset(wimsig, select=c(id,mag,wimsigid)) 

wimheader_ID=subset(wimheader_new, select=c(wimsigid)) 
wimheader_ID=t(wimheader_ID) # transpose

### input file - Mar 20
vdsheader = Irvine.VDSMar20Header
vdsheader_new <-subset(vdsheader, v8 > 300) # collect only data points > 300 (truck)
vdsheader_new <-subset(vdsheader_new, utc > 1363800600000)
vdsheader_new <-subset(vdsheader_new, utc < 1363831200000)
wimheader = Irvine.WIMMar20Header
wimheader_new <-subset(wimheader, v8 > 300)
wimheader_new <-subset(wimheader_new, utc > 1363800600000)
wimheader_new <-subset(wimheader_new, utc < 1363831200000)

wimsig = Irvine.WIMMar20sig
wimsig_IM=subset(wimsig, select=c(id,mag,wimsigid)) 

wimheader_ID=subset(wimheader_new, select=c(wimsigid)) 
wimheader_ID=t(wimheader_ID) # transpose




# look traffic condition
# set buffer

settime <- matrix(nrow=length(wimheader_ID), ncol=1)
setduration<- matrix(nrow=length(wimheader_ID), ncol=1)
setnumpnt<- matrix(nrow=length(wimheader_ID), ncol=1)


lb <- matrix(nrow=length(wimheader_ID), ncol=1)
ld <- matrix(nrow=length(wimheader_ID), ncol=1)
ud <- matrix(nrow=length(wimheader_ID), ncol=1)
lp <- matrix(nrow=length(wimheader_ID), ncol=1)
up <- matrix(nrow=length(wimheader_ID), ncol=1)



for (j in 1: length(wimheader_ID)){
  settime[j] <- as.numeric(wimheader_new[j,12])
  lb[j] <- settime[j] - buffertimewindow * 60000  
}

for (j in 1: length(wimheader_ID)){
  setduration[j] <- as.numeric(wimheader_new[j,7])
  ld[j] <- setduration[j] - bufferduration  
  ud[j] <- setduration[j] + bufferduration  
}

for (j in 1: length(wimheader_ID)){
  setnumpnt[j] <- as.numeric(wimheader_new[j,8])
  lp[j] <- setnumpnt[j] - buffernumpnt  
  up[j] <- setnumpnt[j] + buffernumpnt  
}




### time window - TIME & DURATION 
vdssiglist <- list()

for (j in 1: length(wimheader_ID)){ 
  
  vdssiglist[j] <- list(subset(vdsheader_new$vdssigid,  vdsheader_new$utc > lb[j] &  vdsheader_new$utc <= settime[j]
                               & vdsheader_new[,7] > ld[j] & vdsheader_new[,7] < ud[j]
                               & vdsheader_new[,8] > lp[j] & vdsheader_new[,8] < up[j]
                             ))
}



### input files - wim

num <- 1000
no_round <- 1000




# find index for potential matching
wimidx <- match ( (wimheader_ID),wimsig_IM[,3] )
wimidx <- wimidx[!is.na(wimidx)]
wimtarget <- wimsig_IM[wimidx,3]


# find index for potential matching

wimindex <- c()
wimobjout <- c()

for (w in 1: length(wimidx)){
#for (w in 1: 2){

  wimindex <- wimidx[w]
  inwimsig <- wimsig_IM[wimindex+1,]
  wimindex <- wimindex+1

    while (wimsig_IM[wimindex+1,1] < 100){
      inwimsig <- rbind(inwimsig,wimsig_IM[wimindex+1,])
      wimindex <- wimindex+1
    }

  inwimsig <- f.normalization(inwimsig)
  splinewim <- f.interpolation(inwimsig,num,no_round)
  colnames(splinewim) <- c("outwimtime", "outwimmag")
    #write.table(inDownsig, "./ProcessedData/TestCode/inDownsig.txt", sep="\t")
    #write.table(splineDown, "./ProcessedData/TestCode/splineDown.txt", sep="\t")

  wimobj <- c(splinewim[,2])
  wimobj <- t(wimobj)
  wimobjout <-rbind(wimobjout,  wimobj)
}

### input files -vds


vdssig = Irvine.VDSOct02sig # Oct 02
vdssig_IM=subset(vdssig, select=c(id,mag,vdssigid))

vdssig = Irvine.VDSMar20sig # Mar 20
vdssig_IM=subset(vdssig, select=c(id,mag,vdssigid))

for (i in 1:length(vdsheader_new)){
  vdsidx <- match ( vdsheader_new$vdssigid, vdssig_IM$vdssigid ) 
}

vdsheaderID <-vdssig_IM[vdsidx,3]


vdsindex <- c()
vdsobjout <- c()
for (w in 1: length(vdsidx)){
  
 # for (w in 1: 2){
    vdsindex <- vdsidx[w]
    invdssig <- vdssig_IM[vdsindex+1,]
    vdsindex <- vdsindex+1
    
    while (vdssig_IM[vdsindex+1,1] < 100){
      invdssig <- rbind(invdssig,vdssig_IM[vdsindex+1,])
      vdsindex <- vdsindex+1
    }
    
    invdssig <- f.normalization(invdssig)
    splinevds <- f.interpolation(invdssig,num,no_round)
    colnames(splinevds) <- c("outvdstime", "outvdsmag")
    
    
    vdsobj <- c(splinevds[,2])
    vdsobj <- t(vdsobj)
    vdsobjout <-rbind(vdsobjout,  vdsobj)
  }

save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Oct02/06302014.RData") # for Oct 02
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Mar20/07272014Mar20.RData")  # for Mar 20

#####################################################################end




