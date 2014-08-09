#install.packages("stringr")
rm(list=ls())
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 

load("./ProcessedData/Jan0910/SOLCLoadinJan0910")
# load functionbook2
######## check duration and filter out (When WIM data comes in, step-by-step filtering approches needed) #######
### Time Window ###

buffertimewindow=60; # min (WIM-WIM case)
bufferduration = 0.4; # 0.2 min
buffernumpnt = 800
bufferlen = 12
bufferaspacing12 = 8
bufferaspacing23 = 5
bufferaspacing34 = 5
bufferaspacing45 = 5
buffergvw = 40

# bufferaweightl1 = 3
# bufferaweightr1 = 3
# bufferaweightl2 = 3
# bufferaweightr2 = 3
# bufferaweightl3 = 3
# bufferaweightr3 = 3
# bufferaweightl4 = 3
# bufferaweightr4 = 3
# bufferaweightl5 = 3
# bufferaweightr5 = 3


### input file - Jan 0910

SOJan_v1 <- read.table("./RawData/SOJan/SOJan_v1.txt",fill=T)
LCJan_v1 <- read.table("./RawData/LCJan/LCJan_v1.txt",fill=T)

Upheader = SO.Jan0910Header
Upheader[,14] <- SOJan_v1[,6][match( Upheader$sigid, SOJan_v1[,3])] # FHWA class
Upheader[,15] <- SOJan_v1[,8][match( Upheader$sigid, SOJan_v1[,3])] # length
Upheader[,16] <- SOJan_v1[,9][match( Upheader$sigid, SOJan_v1[,3])] # gvw

Upheader[,17] <- SOJan_v1[,10][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 1-2
Upheader[,18] <- SOJan_v1[,11][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 2-3
Upheader[,19] <- SOJan_v1[,12][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 3-4
Upheader[,20] <- SOJan_v1[,13][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 4-5

Upheader[,21] <- SOJan_v1[,14][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 1 left
Upheader[,22] <- SOJan_v1[,15][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 1 right
Upheader[,23] <- SOJan_v1[,16][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 2 left
Upheader[,24] <- SOJan_v1[,17][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 2 right
Upheader[,25] <- SOJan_v1[,18][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 3 left
Upheader[,26] <- SOJan_v1[,19][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 3 right
Upheader[,27] <- SOJan_v1[,20][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 4 left
Upheader[,28] <- SOJan_v1[,21][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 4 right
Upheader[,29] <- SOJan_v1[,22][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 5 left
Upheader[,30] <- SOJan_v1[,23][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 5 right

Upheader_new <-subset(Upheader, Upheader[,8] > 100)
Upheader_new <-subset(Upheader_new, Upheader_new[,14] > 3)
Upheader_new <-subset(Upheader_new, Upheader_new[,14] < 15)

Downheader = LC.Jan0910Header
Downheader[,14] <- LCJan_v1[,6][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,15] <- LCJan_v1[,8][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,16] <- LCJan_v1[,9][match( Downheader$sigid, LCJan_v1[,3])]

Downheader[,17] <- LCJan_v1[,10][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,18] <- LCJan_v1[,11][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,19] <- LCJan_v1[,12][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,20] <- LCJan_v1[,13][match( Downheader$sigid, LCJan_v1[,3])]


Downheader[,21] <- LCJan_v1[,14][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,22] <- LCJan_v1[,15][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,23] <- LCJan_v1[,16][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,24] <- LCJan_v1[,17][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,25] <- LCJan_v1[,18][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,26] <- LCJan_v1[,19][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,27] <- LCJan_v1[,20][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,28] <- LCJan_v1[,21][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,29] <- LCJan_v1[,22][match( Downheader$sigid, LCJan_v1[,3])]
Downheader[,30] <- LCJan_v1[,23][match( Downheader$sigid, LCJan_v1[,3])]

Downheader_new <-subset(Downheader, Downheader[,14] > 3)
Downheader_new <-subset(Downheader_new, Downheader_new[,14] < 15)


Downsig = LC.Jan0910sig
Downsig_IM=subset(Downsig, select=c(id,mag,sigid)) 

Downheader_ID=(Downheader_new$sigid)




# look traffic condition
# set buffer

settime <- matrix(nrow=length(Downheader_ID), ncol=1)
setduration<- matrix(nrow=length(Downheader_ID), ncol=1)
setnumpnt<- matrix(nrow=length(Downheader_ID), ncol=1)
setlen<- matrix(nrow=length(Downheader_ID), ncol=1)
setgvw<- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing12 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing23 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing34 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing45 <- matrix(nrow=length(Downheader_ID), ncol=1)
# 
# setaweightl1 <- matrix(nrow=length(Downheader_ID), ncol=1)
# setaweightr1 <- matrix(nrow=length(Downheader_ID), ncol=1)
# setaweightl2 <- matrix(nrow=length(Downheader_ID), ncol=1)
# setaweightr2 <- matrix(nrow=length(Downheader_ID), ncol=1)
# setaweightl3 <- matrix(nrow=length(Downheader_ID), ncol=1)
# setaweightr3 <- matrix(nrow=length(Downheader_ID), ncol=1)
# setaweightl4 <- matrix(nrow=length(Downheader_ID), ncol=1)
# setaweightr4 <- matrix(nrow=length(Downheader_ID), ncol=1)
# setaweightl5 <- matrix(nrow=length(Downheader_ID), ncol=1)
# setaweightr5 <- matrix(nrow=length(Downheader_ID), ncol=1)


lb <- matrix(nrow=length(Downheader_ID), ncol=1)
ld <- matrix(nrow=length(Downheader_ID), ncol=1)
ud <- matrix(nrow=length(Downheader_ID), ncol=1)
lp <- matrix(nrow=length(Downheader_ID), ncol=1)
up <- matrix(nrow=length(Downheader_ID), ncol=1)
ul <- matrix(nrow=length(Downheader_ID), ncol=1)
ll <- matrix(nrow=length(Downheader_ID), ncol=1)
ug <- matrix(nrow=length(Downheader_ID), ncol=1)
lg <- matrix(nrow=length(Downheader_ID), ncol=1)

la12 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua12 <- matrix(nrow=length(Downheader_ID), ncol=1)
la23 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua23 <- matrix(nrow=length(Downheader_ID), ncol=1)
la34 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua34 <- matrix(nrow=length(Downheader_ID), ncol=1)
la45 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua45 <- matrix(nrow=length(Downheader_ID), ncol=1)
# 
# lwl1 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwl1 <- matrix(nrow=length(Downheader_ID), ncol=1)
# lwr1 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwr1 <- matrix(nrow=length(Downheader_ID), ncol=1)
# lwl2 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwl2 <- matrix(nrow=length(Downheader_ID), ncol=1)
# lwr2 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwr2 <- matrix(nrow=length(Downheader_ID), ncol=1)
# lwl3 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwl3 <- matrix(nrow=length(Downheader_ID), ncol=1)
# lwr3 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwr3 <- matrix(nrow=length(Downheader_ID), ncol=1)
# lwl4 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwl4 <- matrix(nrow=length(Downheader_ID), ncol=1)
# lwr4 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwr4 <- matrix(nrow=length(Downheader_ID), ncol=1)
# lwl5 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwl5 <- matrix(nrow=length(Downheader_ID), ncol=1)
# lwr5 <- matrix(nrow=length(Downheader_ID), ncol=1)
# uwr5 <- matrix(nrow=length(Downheader_ID), ncol=1)


for (j in 1: length(Downheader_ID)){
  settime[j] <- as.numeric(Downheader_new[j,12])
  lb[j] <- settime[j] - buffertimewindow * 60000  
}

for (j in 1: length(Downheader_ID)){
  setduration[j] <- as.numeric(Downheader_new[j,7])
  ld[j] <- setduration[j] - bufferduration  
  ud[j] <- setduration[j] + bufferduration  
}

for (j in 1: length(Downheader_ID)){
  setnumpnt[j] <- as.numeric(Downheader_new[j,8])
  lp[j] <- setnumpnt[j] - buffernumpnt  
  up[j] <- setnumpnt[j] + buffernumpnt  
}

for (j in 1: length(Downheader_ID)){
  setlen[j] <- as.numeric(Downheader_new[j,15])
  ll[j] <- setlen[j] - bufferlen  
  ul[j] <- setlen[j] + bufferlen  
}

for (j in 1: length(Downheader_ID)){
  setgvw[j] <- as.numeric(Downheader_new[j,16])
  lg[j] <- setgvw[j] - buffergvw  
  ug[j] <- setgvw[j] + buffergvw 
}

for (j in 1: length(Downheader_ID)){
  setaspacing12[j] <- as.numeric(Downheader_new[j,17])
  setaspacing23[j] <- as.numeric(Downheader_new[j,18])
  setaspacing34[j] <- as.numeric(Downheader_new[j,19])
  setaspacing45[j] <- as.numeric(Downheader_new[j,20])
  la12[j] <- setaspacing12[j] - bufferaspacing12  
  ua12[j] <- setaspacing12[j] + bufferaspacing12
  la23[j] <- setaspacing23[j] - bufferaspacing23  
  ua23[j] <- setaspacing23[j] + bufferaspacing23 
  la34[j] <- setaspacing34[j] - bufferaspacing34 
  ua34[j] <- setaspacing34[j] + bufferaspacing34  
  la45[j] <- setaspacing45[j] - bufferaspacing45  
  ua45[j] <- setaspacing45[j] + bufferaspacing45  
}

# for (j in 1: length(Downheader_ID)){
#   setaweightl1[j] <- as.numeric(Downheader_new[j,21])
#   setaweightr1[j] <- as.numeric(Downheader_new[j,22])
#   setaweightl2[j] <- as.numeric(Downheader_new[j,23])
#   setaweightr2[j] <- as.numeric(Downheader_new[j,24])
#   setaweightl3[j] <- as.numeric(Downheader_new[j,25])
#   setaweightr3[j] <- as.numeric(Downheader_new[j,26])
#   setaweightl4[j] <- as.numeric(Downheader_new[j,27])
#   setaweightr4[j] <- as.numeric(Downheader_new[j,28])
#   setaweightl5[j] <- as.numeric(Downheader_new[j,29])
#   setaweightr5[j] <- as.numeric(Downheader_new[j,30])
  
#   lwl1[j] <- setaweightl1[j] - bufferaweightl1 
#   uwl1[j] <- setaweightl1[j] + bufferaweightl1
#   lwl2[j] <- setaweightl2[j] - bufferaweightl2 
#   uwl2[j] <- setaweightl2[j] + bufferaweightl2
#   lwl3[j] <- setaweightl3[j] - bufferaweightl3 
#   uwl3[j] <- setaweightl3[j] + bufferaweightl3
#   lwl4[j] <- setaweightl4[j] - bufferaweightl4 
#   uwl4[j] <- setaweightl4[j] + bufferaweightl4
#   lwl5[j] <- setaweightl5[j] - bufferaweightl5 
#   uwl5[j] <- setaweightl5[j] + bufferaweightl5
#   
#   lwr1[j] <- setaweightr1[j] - bufferaweightr1 
#   uwr1[j] <- setaweightr1[j] + bufferaweightr1
#   lwr2[j] <- setaweightr2[j] - bufferaweightr2 
#   uwr2[j] <- setaweightr2[j] + bufferaweightr2
#   lwr3[j] <- setaweightr3[j] - bufferaweightr3 
#   uwr3[j] <- setaweightr3[j] + bufferaweightr3
#   lwr4[j] <- setaweightr4[j] - bufferaweightr4 
#   uwr4[j] <- setaweightr4[j] + bufferaweightr4
#   lwr5[j] <- setaweightr5[j] - bufferaweightr5 
#   uwr5[j] <- setaweightr5[j] + bufferaweightr5
 
# }


### time window - TIME & DURATION 
Upsiglist <- list()

for (j in 1: length(Downheader_ID)){ 
  
  Upsiglist[j] <- list(subset(Upheader_new$sigid,  Upheader_new$utc > lb[j] &  Upheader_new$utc <= settime[j]
                              & Upheader_new[,7] > ld[j] & Upheader_new[,7] < ud[j]
                              & Upheader_new[,8] > lp[j] & Upheader_new[,8] < up[j]
                              & Upheader_new[,14] == Downheader_new[j,14] 
                              & Upheader_new[,15] > ll[j] & Upheader_new[,15] < ul[j]
                              & Upheader_new[,16] > lg[j] & Upheader_new[,16] < ug[j]
                              
                              & Upheader_new[,17] > la12[j] & Upheader_new[,17] < ua12[j]
                              & Upheader_new[,18] > la23[j] & Upheader_new[,18] < ua23[j]
                              & Upheader_new[,19] > la34[j] & Upheader_new[,19] < ua34[j]
                              & Upheader_new[,20] > la45[j] & Upheader_new[,20] < ua45[j]
#                               
#                               & Upheader_new[,21] > lwl1[j] & Upheader_new[,21] < uwl1[j]
#                               & Upheader_new[,22] > lwr1[j] & Upheader_new[,22] < uwr1[j]
#                               & Upheader_new[,23] > lwl2[j] & Upheader_new[,23] < uwl2[j]
#                               & Upheader_new[,24] > lwr2[j] & Upheader_new[,24] < uwr2[j]
#                               & Upheader_new[,25] > lwl3[j] & Upheader_new[,25] < uwl3[j]
#                               & Upheader_new[,26] > lwr3[j] & Upheader_new[,26] < uwr3[j]
#                               & Upheader_new[,27] > lwl4[j] & Upheader_new[,27] < uwl4[j]
#                               & Upheader_new[,28] > lwr4[j] & Upheader_new[,28] < uwr4[j]
#                               & Upheader_new[,29] > lwl5[j] & Upheader_new[,29] < uwl5[j]
#                               & Upheader_new[,30] > lwr5[j] & Upheader_new[,30] < uwr5[j]
  ))
}

### input files - DOWN

num <- 1000
no_round <- 1000

# find index for potential matching
Downidx <- match ( (Downheader_ID),Downsig_IM[,3] )
Downidx <- Downidx[!is.na(Downidx)]


Downindex <- c()
Downobjout <- c()

for (w in 1: length(Downidx)){
# w=1
  Downindex <- Downidx[w]
  inDownsig <- Downsig_IM[Downindex+1,]
  Downindex <- Downindex+1
  
  while (Downsig_IM[Downindex+1,1] < 100){
    inDownsig <- rbind(inDownsig,Downsig_IM[Downindex+1,])
    Downindex <- Downindex+1
  }
  
  inDownsig <- f.normalization(inDownsig)
  splineDown <- f.interpolation(inDownsig,num,no_round)
  colnames(splineDown) <- c("outDowntime", "outDownmag")
  #write.table(inDownsig, "./ProcessedData/TestCode/inDownsig.txt", sep="\t")
  #write.table(splineDown, "./ProcessedData/TestCode/splineDown.txt", sep="\t")
  
  Downobj <- c(splineDown[,2])
  Downobj <- t(Downobj)
  Downobjout <-rbind(Downobjout,  Downobj)
}

### input files - UP
Upsig = SO.Jan0910sig # Mar 20
Upsig_IM=subset(Upsig, select=c(id,mag,sigid))


for (i in 1:length(Upheader_new)){
  Upidx <- match ( Upheader_new$sigid, Upsig_IM$sigid ) 
}

UpheaderID <-Upsig_IM[Upidx,3]


Upindex <- c()
Upobjout <- c()


for (w in 1: length(Upidx)){
# for (w in 1: 20){
  Upindex <- Upidx[w]
  inUpsig <- Upsig_IM[Upindex+1,]
  Upindex <- Upindex+1
  
  while (Upsig_IM[Upindex+1,1] < 100){
    inUpsig <- rbind(inUpsig,Upsig_IM[Upindex+1,])
    Upindex <- Upindex+1
  }
  
  inUpsig <- f.normalization(inUpsig)
  splineUp <- f.interpolation(inUpsig,num,no_round)
  colnames(splineUp) <- c("outUptime", "outUpmag")
  
  
  Upobj <- c(splineUp[,2])
  Upobj <- t(Upobj)
  Upobjout <-rbind(Upobjout,  Upobj)
}



save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/08062014Jan0910.RData")  # for Jan 0910


save(Upobjout, file="./ProcessedData/Jan0910/Upobjout.RData")
save(Downobjout, file="./ProcessedData/Jan0910/Downobjout.RData")
#####################################################################end
# remove files

rm(la12, la23, la34, la45, lb, ld, lg, ll, lp, lwl1, lwl2, lwl3, lwl4, lwl5, lwr1, lwr2, lwr3, lwr4, lwr5,
   ua12, ua23, ua34, ua45, ud, ug, ul, up, uw1l, uw1r, uw2l, uw2r, uw3l, uw3r, uw4l, uw4r, uw5l, uw5r, 
   uwl1, uwl2, uwl3, uwl4, uwl5, uwr1, uwr2, uwr3, uwr4, uwr5)

rm(setaspacing12, setaspacing23, setaspacing34, setaspacing45, setaweightl1, setaweightl2, setaweightl3,
   setaweightl4, setaweightl5, setaweightr1, setaweightr2, setaweightr3, setaweightr4, setaweightr5,
   setduration, setgvw, setnumpnt, settime, setlen)

rm(LC.Jan09ML3Header1, LC.Jan09ML3Header2, LC.Jan09ML3sig1, LC.Jan09ML3sig2, 
   LC.Jan09ML4Header1, LC.Jan09ML4Header2, LC.Jan09ML4sig1, LC.Jan09ML4sig2, 
   LC.Jan10ML3Header1, LC.Jan10ML3Header2, LC.Jan10ML3Header3, LC.Jan10ML3Header4,
   LC.Jan10ML3sig1, LC.Jan10ML3sig2, LC.Jan10ML3sig3, LC.Jan10ML3sig4,
   LC.Jan10ML4Header1, LC.Jan10ML4Header2, LC.Jan10ML4Header3, LC.Jan10ML4Header4,
   LC.Jan10ML4sig1, LC.Jan10ML4sig2, LC.Jan10ML4sig3, LC.Jan10ML4sig4)

rm(SO.Jan09ML3Header1, SO.Jan09ML3sig1, SO.Jan09ML4Header1, SO.Jan09ML4sig1,
   SO.Jan10ML3Header1, SO.Jan10ML3sig1, SO.Jan10ML3Header2, SO.Jan10ML3sig2, 
   SO.Jan10ML4Header1, SO.Jan10ML4sig1, SO.Jan10ML4Header2, SO.Jan10ML4sig2)

rm(bufferaspacing12, bufferaspacing23, bufferaspacing34, bufferaspacing45,
   bufferaweightl1,bufferaweightl2,bufferaweightl3,bufferaweightl4,bufferaweightl5,
   bufferaweightr1,bufferaweightr2,bufferaweightr3,bufferaweightr4,bufferaweightr5,
   bufferduration, buffergvw, bufferlen, buffernumpnt, buffertimewindow)

rm(x,y,i,j, len, splineUp, inUpsig, inDownsig)
