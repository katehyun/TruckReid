install.packages("stringr")
rm(list=ls())
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine") 
load("./ProcessedData/Oct02/IrvineLoadinOct02")
load("./ProcessedData/Mar20/IrvineLoadinMar20")
load("./SOLCLoadinJan0910")

######## check duration and filter out (When WIM data comes in, step-by-step filtering approches needed) #######
### Time Window ###

# buffertimewindow=0.35; # 0.3 min (VDS-VDS case)
buffertimewindow=60; # 0.3 min (WIM-WIM case)
bufferduration = 0.2; # 0.2 min
buffernumpnt = 500
bufferlen = 3
bufferaspacing12 = 2
bufferaspacing23 = 1
bufferaspacing34 = 3
bufferaspacing45 = 2

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

### input file - Jan 0910

SOJan_v1 <- read.table("./RawData/SOJan/SOJan_v1.txt",fill=T)
LCJan_v1 <- read.table("./RawData/LCJan/LCJan_v1.txt",fill=T)

vdsheader = SO.Jan0910Header
vdsheader[,14] <- SOJan_v1[,6][match( vdsheader$wimsigid, SOJan_v1[,3])] # FHWA class
vdsheader[,15] <- SOJan_v1[,8][match( vdsheader$wimsigid, SOJan_v1[,3])] # length
vdsheader[,16] <- SOJan_v1[,9][match( vdsheader$wimsigid, SOJan_v1[,3])] # axle spacing 1-2
vdsheader[,17] <- SOJan_v1[,10][match( vdsheader$wimsigid, SOJan_v1[,3])] # axle spacing 2-3
vdsheader[,18] <- SOJan_v1[,11][match( vdsheader$wimsigid, SOJan_v1[,3])] # axle spacing 3-4
vdsheader[,19] <- SOJan_v1[,12][match( vdsheader$wimsigid, SOJan_v1[,3])] # axle spacing 4-5


vdsheader_new <-subset(vdsheader, vdsheader[,14] > 3)
vdsheader_new <-subset(vdsheader_new, vdsheader_new[,14] < 15)

wimheader = LC.Jan0910Header
wimheader[,14] <- LCJan_v1[,6][match( wimheader$wimsigid, LCJan_v1[,3])]
wimheader[,15] <- LCJan_v1[,8][match( wimheader$wimsigid, LCJan_v1[,3])]
wimheader[,16] <- LCJan_v1[,9][match( wimheader$wimsigid, LCJan_v1[,3])]
wimheader[,17] <- LCJan_v1[,10][match( wimheader$wimsigid, LCJan_v1[,3])]
wimheader[,18] <- LCJan_v1[,11][match( wimheader$wimsigid, LCJan_v1[,3])]
wimheader[,19] <- LCJan_v1[,12][match( wimheader$wimsigid, LCJan_v1[,3])]
wimheader_new <-subset(wimheader, wimheader[,14] > 3)
wimheader_new <-subset(wimheader_new, wimheader_new[,14] < 15)

#wimheader_new2 <-subset (wimheader_new,wimheader_new$wimsigid %in% matching_SOLC$LC)
#wimheader_new <-subset (wimheader_new,wimheader_new$wimsigid %in% matching_SOLC$LC)

wimsig = LC.Jan0910sig
wimsig_IM=subset(wimsig, select=c(id,mag,wimsigid)) 

wimheader_ID=(wimheader_new$wimsigid)



# find index for potential matching
wimidx <- match ( (wimheader_ID),wimsig_IM[,3] )
wimidx <- wimidx[!is.na(wimidx)]
wimtarget <- wimsig_IM[wimidx,3]


# look traffic condition
# set buffer

settime <- matrix(nrow=length(wimheader_ID), ncol=1)
setduration<- matrix(nrow=length(wimheader_ID), ncol=1)
setnumpnt<- matrix(nrow=length(wimheader_ID), ncol=1)
setlen<- matrix(nrow=length(wimheader_ID), ncol=1)
setaspacing12 <- matrix(nrow=length(wimheader_ID), ncol=1)
setaspacing23 <- matrix(nrow=length(wimheader_ID), ncol=1)
setaspacing34 <- matrix(nrow=length(wimheader_ID), ncol=1)
setaspacing45 <- matrix(nrow=length(wimheader_ID), ncol=1)

lb <- matrix(nrow=length(wimheader_ID), ncol=1)
ld <- matrix(nrow=length(wimheader_ID), ncol=1)
ud <- matrix(nrow=length(wimheader_ID), ncol=1)
lp <- matrix(nrow=length(wimheader_ID), ncol=1)
up <- matrix(nrow=length(wimheader_ID), ncol=1)
ul <- matrix(nrow=length(wimheader_ID), ncol=1)
ll <- matrix(nrow=length(wimheader_ID), ncol=1)

la12 <- matrix(nrow=length(wimheader_ID), ncol=1)
ua12 <- matrix(nrow=length(wimheader_ID), ncol=1)
la23 <- matrix(nrow=length(wimheader_ID), ncol=1)
ua23 <- matrix(nrow=length(wimheader_ID), ncol=1)
la34 <- matrix(nrow=length(wimheader_ID), ncol=1)
ua34 <- matrix(nrow=length(wimheader_ID), ncol=1)
la45 <- matrix(nrow=length(wimheader_ID), ncol=1)
ua45 <- matrix(nrow=length(wimheader_ID), ncol=1)


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

for (j in 1: length(wimheader_ID)){
  setlen[j] <- as.numeric(wimheader_new[j,15])
  ll[j] <- setlen[j] - bufferlen  
  ul[j] <- setlen[j] + bufferlen  
}

for (j in 1: length(wimheader_ID)){
  setaspacing12[j] <- as.numeric(wimheader_new[j,16])
  setaspacing23[j] <- as.numeric(wimheader_new[j,17])
  setaspacing34[j] <- as.numeric(wimheader_new[j,18])
  setaspacing45[j] <- as.numeric(wimheader_new[j,19])
  la12[j] <- setaspacing12[j] - bufferaspacing12  
  ua12[j] <- setaspacing12[j] + bufferaspacing12
  la23[j] <- setaspacing23[j] - bufferaspacing23  
  ua23[j] <- setaspacing23[j] + bufferaspacing23 
  la34[j] <- setaspacing34[j] - bufferaspacing34 
  ua34[j] <- setaspacing34[j] + bufferaspacing34  
  la45[j] <- setaspacing45[j] - bufferaspacing45  
  ua45[j] <- setaspacing45[j] + bufferaspacing45  
}



### time window - TIME & DURATION 
vdssiglist <- list()

for (j in 1: length(wimheader_ID)){ 
  
  vdssiglist[j] <- list(subset(vdsheader_new$wimsigid,  vdsheader_new$utc > lb[j] &  vdsheader_new$utc <= settime[j]
                               & vdsheader_new[,7] > ld[j] & vdsheader_new[,7] < ud[j]
                               & vdsheader_new[,8] > lp[j] & vdsheader_new[,8] < up[j]
                               & vdsheader_new[,15] > ll[j] & vdsheader_new[,15] < ul[j]
                               & vdsheader_new[,14] == wimheader_new[j,14] 
                               & vdsheader_new[,16] > la12[j] & vdsheader_new[,16] < ua12[j]
                                & vdsheader_new[,17] > la23[j] & vdsheader_new[,17] < ua23[j]
                                & vdsheader_new[,18] > la34[j] & vdsheader_new[,18] < ua34[j]
                                & vdsheader_new[,19] > la45[j] & vdsheader_new[,19] < ua45[j]
))
}

### input files - vds

vdssig = Irvine.VDSOct02sig # Oct 02
vdssig_IM=subset(vdssig, select=c(id,mag,vdssigid))
vdssig = Irvine.VDSMar20sig # Mar 20
vdssig_IM=subset(vdssig, select=c(id,mag,vdssigid))
vdssig = SO.Jan0910sig # Mar 20
vdssig_IM=subset(vdssig, select=c(id,mag,wimsigid))

# sig idx
vdsidx <- list()
for (i in 1:length(vdssiglist)){
  vdsidx[i] <- list(match ( vdssiglist[[i]], vdssig_IM$wimsigid )) # or vdssigid
}



# number of max col for list
a<-c()
for (i in 1:length(vdsidx)){
  a[i]<- (length(vdsidx[[i]]))
  nloop=a[which.max(a)]
}



save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Oct02/06302014.RData") # for Oct 02
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Mar20/07082014Mar20.RData")  # for Mar 20
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/07162014Jan0910.RData")  # for Jan 0910
#####################################################################end




