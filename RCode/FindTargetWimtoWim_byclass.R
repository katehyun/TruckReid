#utils:::menuInstallPkgs() 
#rm(list=ls())
# load functonbook2
library(pnn)
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/shiftandstretch_Jan0910.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData ")
rm (rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8 )
rm(swift, stret, ss, splineDown, splineUp, minstretmagdif, minswiftmagdif, magdif, candi_magdif)
############################################################# do not run when loading RData
### target 1 & 2 :  base and after shift and stretch
# min magdif
min_a_magdif<-vector()
for (i in 1: length(a_magdif)){
  min_a_magdif[i] <- min(a_magdif[[i]])
}

min_a_basemagdif<-vector()
for (i in 1: length(a_basemagdif)){
  min_a_basemagdif[i] <- min(a_basemagdif[[i]])
}


idx_basemagdif <- lapply(a_basemagdif,which.min)
idx_magdif <- lapply(a_magdif,which.min)

base_Upid <- c()
base_Upid_after <- c()

j=1

for (i in 1:length(idx_basemagdif)){
  a <- unlist(idx_basemagdif[i])
  
  if (length(Upsiglist[[j]]) == 0 ){
    
    base_Upid[i] <-999
    j <- j+1}
  
  else {
    
    base_Upid[i] <- Upsiglist[[j]][a]
    j <- j+1
  }
  
}

a_Upid <- c()
base_Upid <- c()
a_Upid_after <- c()

j <- 1
for (i in 1:length(a_magdif)){
  
  a <- unlist(idx_magdif[i])
  b <- unlist(idx_magdif[i])
  
  if (length(Upsiglist[[j]]) == 0 ){
    
    a_Upid[i] <-999
    base_Upid[i] <-999
    j <- j+1}
  
  else {
    
    a_Upid[i] <- Upsiglist[[j]][a]
    base_Upid[i] <- Upsiglist[[j]][b]
    j <- j+1
  }
  
}


# Jan 0910
p <- 7
rm(Target_baseanalysis_Jan0910_table, Result_NN, Result)
Target_baseanalysis_Jan0910_obj2 <- rep(NA, length(a_Upid))

Downtarget <- Downheader_ID


SOLCAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/LCJan/LCJan_v1.txt", fill=T)
SOLCFHWAClass <- SOLCAllFHWAClass[,6] [match (Downtarget, SOLCAllFHWAClass[,3])] 

Downtarget=Downtarget[1:length(candi_1)]
Target_baseanalysis_Jan0910_obj  <- (matching$SO[ match ( Downtarget,  matching$LC )])
Target_baseanalysis_Jan0910_obj2 <- Target_baseanalysis_Jan0910_obj

Target_baseanalysis_Jan0910_obj2[is.na ( Target_baseanalysis_Jan0910_obj2)]  <- c(999)



Target_baseanalysis_Jan0910_table <- cbind(SOLCFHWAClass,min_a_basemagdif,min_a_magdif, 
                                            Downtarget,  Target_baseanalysis_Jan0910_obj, 
                                           Target_baseanalysis_Jan0910_obj2, base_Upid, a_Upid )

# start from here  - by Class
threshold_NN<- seq(from = 20, to = 80, by = 1) 
Result_NN <- data.frame()


Class <- sort(unique(Target_baseanalysis_Jan0910_table[,1]))
p <- 9

# by class


for (z in 1: length(Class)){

  setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result/") 
  TargetTable <- subset(Target_baseanalysis_Jan0910_table, Target_baseanalysis_Jan0910_table[,1] == Class[z])
  classresult <- f.ResultNN (threshold_NN,  TargetTable, p )
  assign(paste("Result_NN",Class[z],sep=""), classresult$resultnn)
  assign(paste("TargetTable",Class[z], sep=""),classresult$tt) 
  write.table(TargetTable, paste("TargetTable",Class[z], (".txt"), sep=""), sep="\t")
  write.table(classresult$resultnn, paste("Result_NN",Class[z], (".txt"), sep=""), sep="\t")
}


# save(Target_baseanalysis_Jan0910_table, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Target_base_Jan0910.RData")
write.table(Target_baseanalysis_Jan0910_table, "./Target_base_Jan0910.txt", sep="\t")


utils::View(Result_NN9)