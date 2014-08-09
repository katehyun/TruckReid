#utils:::menuInstallPkgs() 
#library(gtools)
#library(plyr)
rm(list=ls())
# load functionbook2




load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/Mar20tw1.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/Mar20tw10.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/Mar20tw30.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/07162014Jan0910.RData") 

setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine") 





### Input ready 
num=100
no_round = 100

wimobj <- c()
wimobjout <- c()

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

#wim

for (w in 1:length(wimidx)){
#for (w in 1:2){  # for testing
  w=1
  
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
  
  wimobj <- c(splinewim[,2])
  wimobj <- t(wimobj)
  wimobjout <-rbind(wimobjout,  wimobj)
  
  if (length(vdsidx[[w]]) < 1 ) { 
    
    
   
    a_basemagdif[w] <- list(c(99999))
    

  }
  
  
  else {
    
    
    #vds 
    
    magdif2 <- c()
    base_magdif <- c()
    
    for (q in 1: length(vdsidx[[w]])){
          
    
      if (is.na(vdsidx[[w]][q])) { 
        magdif2[q] <- NA
        non_ss[q] <- NA
      }
      
      
      else {
        
        vdsindex <- vdsidx[[w]][q]
        invdssig <- vdssig_IM[vdsindex+1,]
        vdsindex <- vdsindex +1
        
        while (vdssig_IM[vdsindex+1,1] < 100){
          invdssig <- rbind(invdssig,vdssig_IM[vdsindex+1,])
          vdsindex <- vdsindex+1
        }
        
        
        invdssig <- f.normalization(invdssig)
        
        splinevds <- f.interpolation(invdssig,num,no_round)
        colnames(splinevds) <- c("outvdstime", "outvdsmag")
        
        
        base_magdif[q] = sum (abs (splinewim - splinevds))
        non_ss[q] <- list(splinevds)
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

