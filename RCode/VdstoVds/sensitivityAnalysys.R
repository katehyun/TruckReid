rm(list=ls())
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
load("./ProcessedData/Mar20/IrvineLoadinMar20")
load("./ProcessedData/Mar20/wimobjoutMar20.RData")
#load function book


# buffertimewindow=0.5; # 0.5 min = 30 second
#  buffertimewindow=1;  # 1 min 
# buffertimewindow=10;  # 10 min 
buffertimewindow=30;  # 30 min 

bufferduration = 0.15; # 0.2 min
buffernumpnt = 500

### input file - Mar 20
vdsheader = Irvine.VDSMar20Header
vdsheader_new <-subset(vdsheader, v8 > 300) # collect only data points > 300 (truck)

vdsheader_new <-subset(vdsheader_new, utc > 1363809300000) # for 1 min
vdsheader_new <-subset(vdsheader_new, utc > 1363809000000) # for 10 min
vdsheader_new <-subset(vdsheader_new, utc > 1363807800000) # for 30 min

vdsheader_new <-subset(vdsheader_new, utc < 1363813200000)

wimheader = Irvine.WIMMar20Header
wimheader_new <-subset(wimheader, v8 > 300)

wimheader_new <-subset(wimheader_new, utc > 1363800600000)
wimheader_new <-subset(wimheader_new, utc < 1363831200000)

wimheader_startidx <- which.max(wimheader_new$utc[wimheader_new$utc < 1363809600000])
wimheader_endidx <- which.max(wimheader_new$utc[wimheader_new$utc < 1363813200000])

wimheader_new <-subset(wimheader_new, utc > 1363809600000)
wimheader_new <-subset(wimheader_new, utc < 1363813200000)

wimsig = Irvine.WIMMar20sig
wimsig_IM=subset(wimsig, select=c(id,mag,wimsigid)) 

wimheader_ID=subset(wimheader_new, select=c(wimsigid)) 
wimheader_ID=t(wimheader_ID) # transpose


# find index for potential matching
wimidx <- match ( (wimheader_ID),wimsig_IM[,3] )
wimidx <- wimidx[!is.na(wimidx)]

wimtarget <- wimsig_IM[wimidx,3]


# look traffic condition
######################### need to coding

# set buffer

settime <- matrix(nrow=nrow(vdsheader_new), ncol=1)
setduration<- matrix(nrow=nrow(vdsheader_new), ncol=1)
setnumpnt<- matrix(nrow=nrow(vdsheader_new), ncol=1)

lb <- matrix(nrow=nrow(vdsheader_new), ncol=1)

ld <- matrix(nrow=nrow(vdsheader_new), ncol=1)
ud <- matrix(nrow=nrow(vdsheader_new), ncol=1)
lp <- matrix(nrow=nrow(vdsheader_new), ncol=1)
up <- matrix(nrow=nrow(vdsheader_new), ncol=1)

lenvds=nrow(vdsheader_new)
lenwim=nrow(wimheader_new)

for (j in 1: lenwim){
  settime[j] <- as.numeric(wimheader_new[j,12])
  lb[j] <- settime[j] - buffertimewindow * 60000  
  
}

for (j in 1: lenwim){
  setduration[j] <- as.numeric(wimheader_new[j,7])
  ld[j] <- setduration[j] - bufferduration  
  ud[j] <- setduration[j] + bufferduration  
}

for (j in 1: lenwim){
  setnumpnt[j] <- as.numeric(wimheader_new[j,8])
  lp[j] <- setnumpnt[j] - buffernumpnt  
  up[j] <- setnumpnt[j] + buffernumpnt  
}

### time window - TIME & DURATION 
vdssiglist <- list()

for (j in 1: lenwim){ 
  
  vdssiglist[j] <- list(subset(vdsheader_new$vdssigid,  vdsheader_new$utc > lb[j] & vdsheader_new$utc <= settime[j] 
                               & vdsheader_new$v7 > ld[j] & vdsheader_new$v7 < ud[j]
                               & vdsheader_new$v8 > lp[j] & vdsheader_new$v8 < up[j]))
}

vdssig = Irvine.VDSMar20sig # Mar 20
vdssig_IM=subset(vdssig, select=c(id,mag,vdssigid))

# check!
vdsidx <- list()
for (i in 1:length(vdssiglist)){
  vdsidx[i] <- list(match ( vdssiglist[[i]], vdssig_IM$vdssigid ))
}


#wimobjout <- wimobjout[(wimheader_startidx+1):wimheader_endidx,]
#save(wimobjout, file="./ProcessedData/SensitivityAnalysis/wimobjout_tw.RData")

#save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/SensitivityAnalysis/Mar20tw1.RData")  # for 1 min
#save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/SensitivityAnalysis/Mar20tw10.RData")  # for 10 min
 save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/SensitivityAnalysis/Mar20tw30.RData")  # for 30 min
