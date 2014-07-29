#utils:::menuInstallPkgs() 
#library(gtools)
#library(plyr)
rm(list=ls())
# load functionbook2



load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/07162014Jan0910.RData") 

setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine") 





### Input ready 
num=100
no_round = 100

Downobj <- c()
Downobjout <- c()

matching <-format(matching, scientific = FALSE) 
base_magdif <- c()
magdif <- c()
magdif2 <- c()

non_ss<- list()
candi_1 <- list()
candi_2 <- list()
candi_3 <- list()
candi_4 <- list()
candi_5 <- list()
candi_6 <- list()
candi_7 <- list()
candi_8 <- list()

a_basemagdif <- list()
rank1 <- list()
rank2 <- list()
rank3 <- list()
rank4 <- list()
rank5 <- list()
rank6 <- list()
rank7 <- list()
rank8 <- list()

#Down

for (w in 1:length(Downidx)){
  #for (w in 1:2){  # for testing
  w=1
  
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
  
  Downobj <- c(splineDown[,2])
  Downobj <- t(Downobj)
  Downobjout <-rbind(Downobjout,  Downobj)
  
  if (length(Upidx[[w]]) < 1 ) { 
    
    
    
    a_basemagdif[w] <- list(c(99999))
    
    
  }
  
  
  else {
    
    
    #Up 
    
    magdif2 <- c()
    base_magdif <- c()
    
    for (q in 1: length(Upidx[[w]])){
      
      
      if (is.na(Upidx[[w]][q])) { 
        magdif2[q] <- NA
        non_ss[q] <- NA
      }
      
      
      else {
        
        Upindex <- Upidx[[w]][q]
        inUpsig <- Upsig_IM[Upindex+1,]
        Upindex <- Upindex +1
        
        while (Upsig_IM[Upindex+1,1] < 100){
          inUpsig <- rbind(inUpsig,Upsig_IM[Upindex+1,])
          Upindex <- Upindex+1
        }
        
        
        inUpsig <- f.normalization(inUpsig)
        
        splineUp <- f.interpolation(inUpsig,num,no_round)
        colnames(splineUp) <- c("outUptime", "outUpmag")
        
        
        base_magdif[q] = sum (abs (splineDown - splineUp))
        non_ss[q] <- list(splineUp)
        rank_base=rank(base_magdif)
        
        rank1= non_ss[which(rank_base %in% c(1))]
        rank2= non_ss[which(rank_base %in% c(2))]
        rank3= non_ss[which(rank_base %in% c(3))]
        rank4= non_ss[which(rank_base %in% c(4))]
        rank5= non_ss[which(rank_base %in% c(5))]
        rank6= non_ss[which(rank_base %in% c(6))]
        rank7= non_ss[which(rank_base %in% c(7))]
        rank8= non_ss[which(rank_base %in% c(8))]
        
      }
    }
    
    a_basemagdif[w] <- list(base_magdif)
    
    
    
    # candidate 
    
    candi_1[w] <- list(rank1)
    candi_2[w] <- list(rank2)
    candi_3[w] <- list(rank3)
    candi_4[w] <- list(rank4)
    candi_5[w] <- list(rank5)
    candi_6[w] <- list(rank6)
    candi_7[w] <- list(rank7)
    candi_8[w] <- list(rank8)
  }
  
}



#save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/non_ss_shiftandstretch_Jan0910_2.RData")

#save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/Mar20tw1_nonss.RData")
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/Mar20tw10_nonss_1.RData")
#save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/Mar20tw30_nonss_1.RData")
##############################################################end
