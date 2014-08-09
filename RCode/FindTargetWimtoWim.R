utils:::menuInstallPkgs() 
rm(list=ls())
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

# second min
second_min_a_magdif <- c()
for (i in 1: length(a_magdif)){
  n <- length(a_magdif[[i]])
  if (n > 1) {
    second_min_a_magdif[i] <- sort(a_magdif[[i]], partial=n-1)[n-1]
  }
  else {
    second_min_a_magdif[i] <- c(999)
  }
}

second_min_a_basemagdif <- c()
for (i in 1: length(a_basemagdif)){
  n <- length(a_basemagdif[[i]])
  if (n > 1) {
    second_min_a_basemagdif[i] <- sort(a_basemagdif[[i]],partial=n-1)[n-1]
  }
  else {
    second_min_a_basemagdif[i] <- c(999)
  }
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
p <- 10
rm(Target_baseanalysis_Jan0910_table, Result_NN, Result)
Target_baseanalysis_Jan0910_obj2 <- rep(NA, length(a_Upid))

Downtarget <- Downheader_ID


SOLCAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/LCJan/LCJan_v1.txt", fill=T)
SOLCFHWAClass <- SOLCAllFHWAClass[,6] [match (Downtarget, SOLCAllFHWAClass[,3])] 

Downtarget=Downtarget[1:length(candi_1)]
Target_baseanalysis_Jan0910_obj  <- (matching$SO[ match ( Downtarget,  matching$LC )])
Target_baseanalysis_Jan0910_obj2 <- Target_baseanalysis_Jan0910_obj

Target_baseanalysis_Jan0910_obj2[is.na ( Target_baseanalysis_Jan0910_obj2)]  <- c(999)
  
  
  
Target_baseanalysis_Jan0910_table <- cbind(SOLCFHWAClass,min_a_basemagdif,min_a_magdif, second_min_a_basemagdif,
                                           second_min_a_magdif,  Downtarget,  Target_baseanalysis_Jan0910_obj, 
                                           Target_baseanalysis_Jan0910_obj2, base_Upid )
                                          
threshold_NN <- c(99999, 20, 30, 40, 50, 60, 70, 80) 
Result_NN <- data.frame()


for (j in 1: length(threshold_NN)) {
  
  for (i in 1:length(a_magdif)){
    if (min_a_magdif[i] < threshold_NN[j]){
      a_Upid_after[i] <- a_Upid[i]
    }
    else {
      a_Upid_after[i] <- c(999)
    }
 }
  
  Target_baseanalysis_Jan0910_table <- cbind ( Target_baseanalysis_Jan0910_table, a_Upid_after )
  
  
  # result
#   missing_nonthreshold <- length(subset(Target_baseanalysis_Jan0910_table[,6], a_Upid_after !=Target_baseanalysis_Jan0910_obj))
#   matching_nonthreshold <- length(subset(Target_baseanalysis_Jan0910_table[,6], a_Upid_after ==Target_baseanalysis_Jan0910_obj))
#   cmr_matching_nonthreshold <- matching_nonthreshold / (matching_nonthreshold+missing_nonthreshold  )
   
  missing_obj <- length (Target_baseanalysis_Jan0910_obj[is.na(Target_baseanalysis_Jan0910_obj)]) 
  matching_obj <- length(Target_baseanalysis_Jan0910_obj) - missing_obj
  
#   matching_NN_nonss <-table(Target_baseanalysis_Jan0910_obj == base_Upid)["TRUE"]
#   missing_NN_nonss <- table(base_Upid == c(999))["TRUE"]
  
  matching_NN <-table(Target_baseanalysis_Jan0910_obj == a_Upid_after)["TRUE"]
  missing_NN <- table(a_Upid_after == c(999))["TRUE"]
  
#   cmr_basematching <- 1 - (abs (matching_NN_nonss - matching_obj )/ matching_obj)
#   cmr_basemissing <-  1 - (abs (missing_NN_nonss - missing_obj) / missing_obj)
 
 
#   cmr_matching <- 1 - (abs (matching_NN - matching_obj )/ matching_obj)
#   cmr_missing <-  1 - (abs(missing_NN-missing_obj) / missing_obj)


  CMVeh <-  matching_NN
  CVeh <- matching_obj[1]
  MVeh <- sum(   (as.numeric( Target_baseanalysis_Jan0910_table[,p])) > 1000 )  
                          
 
  SIMR <- CMVeh / CVeh
  SCMR <- CMVeh / MVeh
  
  MMVeh <- length(  subset(Target_baseanalysis_Jan0910_table[,1], as.numeric( Target_baseanalysis_Jan0910_table[,8] ) 
                           !=  as.numeric( Target_baseanalysis_Jan0910_table[,p])   ))
  
  p <- p+1
  Veh <- length(Target_baseanalysis_Jan0910_table[,1])
  SER <- MMVeh / Veh
  
  Result <- data.frame(threshold_NN[j], matching_obj[1], missing_obj[1], 

                       matching_NN[[1]],  missing_NN[[1]],
                      CMVeh, CVeh, MVeh, SIMR, SCMR, MMVeh, Veh, SER )
  
  Result_NN <- rbind(Result_NN, Result)
}


colnames(Result_NN) <- c("Threhsold", "matching_obj", "missing_obj", 
                         "matching_NN", "missing_NN", 
                         "CMVeh", "CVeh", "MVeh", "SIMR", "SCMR", "MMVeh", "Veh", "SER")



utils::View(Result_NN)



save(Target_baseanalysis_Jan0910_table, file="./ProcessedData/Jan0910/Target_base_Jan0910.RData")
write.table(Target_baseanalysis_Jan0910_table, "./ProcessedData/Jan0910/Target_base_Jan0910.txt", sep="\t")
write.table(Result_NN, "./ProcessedData/Jan0910/Result_NN.txt", sep="\t")

### target 3 : PNN
############################################################################## start here when loading RData
### AFTER LOADING START HERE!!!
# run pnn
Target_pnnanalysis_Jan0910_resultpnn  <- table(c(0,0)) # Mar20
Target_pnnanalysis_Jan0910_resultpnn2  <- table(c(0,0)) # Mar20


Downpnn <- list()
Downpnn2 <- list()

candi1 <- c()
candi2 <- c()
candi3 <- c()
candi4 <- c()
candi5 <- c()
candi6 <- c()
candi7 <- c()
candi8 <- c()

pnn_md <- data.frame()
pnn_md2 <- data.frame()

find_pnnindex <- list()
find_pnn_order <- list()
len_find_pnn <- c()
pnn_Upid <- c()
pnn_Upid_after <- c()
len1 <- c()

find_pnnindex2 <- list()
find_pnn_order2 <- list()
len_find_pnn2 <- c()
pnn_Upid2 <- c()
pnn_Upid2_after <- c()
pnn_Upid_after2 <- c()
len2 <- c()

### choose params!
seqlevel <- 20
sigma <- 1

### find index for pnn

f.pnn <- function ( nn, Downobjout1){
  
  cat <- list()
  prob <- list()
  category <- list()
  probs <- list()
  
  candi_guess <- list(guess(nn, Downobjout1)) 
  cat <- candi_guess[[1]][1]
  prob <- candi_guess[[1]][2]
  
  
  return (list(category <- cat , probs <- prob))
}



for (w in 1: length(a_magdif)){
  
  
  find_pnnindex[w] <- list( a_magdif[[w]] )
  len_find_pnn[w] <- length(find_pnnindex[[w]])
  find_pnn_order[[w]] <-  rank(a_magdif[[w]])
  
  min_a_magdif = min(a_magdif[[w]])
  find_pnnindex2[w] <- list( which( a_magdif[[w]] < min_a_magdif * 2 ))
  len_find_pnn2[w] <- length(find_pnnindex2[[w]])
  find_pnn_order2[[w]] <-  rank(a_magdif[[w]])
  
}


Up_obj_matrix <- seq( from = 1, to = 20 )

Downobjout1 <- c()

for (w in 1: length(candi_1)){ 
  # for (w in 1: 2){ 
  Up_obj <- c()
  objout <- c()
  
  
  len1 <- len_find_pnn[w]
  
  candi1 <- candi_1[w]
  candi2 <- candi_2[w]
  candi3 <- candi_3[w]
  candi4 <- candi_4[w]
  candi5 <- candi_5[w]
  candi6 <- candi_6[w]
  candi7 <- candi_7[w]
  candi8 <- candi_8[w]
  
  Downobjout1 <- Downobjout[w,]
  Downobjout1 <- Downobjout1[seq(1, length(Downobjout1), seqlevel)]
  
  if (len1 > 1) {
    
    objout <- f.findpnn(seqlevel, len1, candi1, candi2, candi3, candi4, candi5, candi6, candi7, candi8 )
    row.names(objout) <- NULL
    
    if (len1 > 8 ){
      len1 <- 8
    }
    
    Up_obj <-(Up_obj_matrix[1:len1])
    pnn_md <- data.frame(Up_obj, objout )
    nn <- learn( pnn_md, category.column=1)
    nn <- smooth(nn, sigma)
    
    
    Downpnn[[w]] <- f.pnn(nn, Downobjout1)
  }
  
  if (len1 <= 1) {
    
    Downpnn[[w]] <- c(999)
  }
}

for (w in 1: length(candi_1)){ 
  # for (w in 1: 2){ 
  Up_obj2 <- c()
  objout2 <- c()
  
  
  len2 <- len_find_pnn2[w]
  
  candi1 <- candi_1[w]
  candi2 <- candi_2[w]
  candi3 <- candi_3[w]
  candi4 <- candi_4[w]
  candi5 <- candi_5[w]
  candi6 <- candi_6[w]
  candi7 <- candi_7[w]
  candi8 <- candi_8[w]
  
  Downobjout1 <- Downobjout[w,]
  Downobjout1 <- Downobjout1[seq(1, length(Downobjout1), seqlevel)]
  
  if (len2 > 1) {
    
    
    objout2 <- f.findpnn(seqlevel, len2, candi1, candi2, candi3, candi4, candi5, candi6, candi7, candi8 )
    row.names(objout2) <- NULL
    
    
    if (len2 > 8 ){
      len2 <- 8
    }
    
    Up_obj2 <-(Up_obj_matrix[1:len2])
    pnn_md2 <- data.frame(Up_obj2, objout2 )
    nn2 <- learn( pnn_md2, category.column=1)
    nn2 <- smooth(nn2, sigma)
    
    Downpnn2[[w]] <- f.pnn(nn2, Downobjout1)
  }
  
  if (len2 <= 1) {
    
    Downpnn2[[w]] <- c(999)
  }
  
}

### only pnn

for (w in 1: length(a_magdif)){
  #   for (w in 1: 2){
  if (as.numeric (Downpnn[[w]][[1]][[1]]) < 100 ) {
    pnn_Upid[w] <- Upsiglist[[w]] [which ( as.numeric( Downpnn[[w]][[1]][[1]] )  == find_pnn_order[[w]])]
  }
  
  else {
    pnn_Upid[w] <- base_Upid[w]
  }
  
}

max_pnn_prob <- vector()
for (i in 1: length(Downpnn)){
  if (length(Downpnn[[i]]) > 1) {
    max_pnn_prob[i] <- max(Downpnn[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob[i] <- 999
  }
  
}

# second max 
second_max_pnn_prob<-c()
for (i in 1: length(Downpnn)){
  n <- length(Downpnn[[i]])
  if (length(Downpnn[[i]]) > 1) {
    second_max_pnn_prob[i] <- sort(Downpnn[[i]][[2]][[1]],partial=n-1)[n-1]
  }
  else {
    second_max_pnn_prob[i] <- c(999)
  }
}



### first ss and second pnn
for (w in 1: length(candi_1)){
  #  for (w in 1: 2){
  if (as.numeric (Downpnn2[[w]][[1]][[1]] < 998 )) {
    
    pnn_Upid2[w] <- Upsiglist[[w]] [which ( as.numeric( Downpnn2[[w]][[1]][[1]] )  == find_pnn_order2[[w]])]
  }
  
  else {
    pnn_Upid2[w] <- base_Upid[w]
    
  }
  
}


max_pnn_prob2 <- vector()
for (i in 1: length(Downpnn2)){
  if (length(Downpnn2[[i]]) > 1) {
    max_pnn_prob2[i] <- max(Downpnn2[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob2[i] <- 999
  }
  
}

# second max 
second_max_pnn_prob2 <- vector()
for (i in 1: length(Downpnn2)){
  n <- length(Downpnn2[[i]])
  if (length(Downpnn2[[i]]) > 1) {
    second_max_pnn_prob2[i] <- sort(Downpnn2[[i]][[2]][[1]],partial=n-1)[n-1]
  }
  else {
    second_max_pnn_prob2[i] <- c(999)
  }
}




# Jan 0910
rm(Result_PNN, Result, Target_pnnanalysis_Jan0910_table )
SOLCAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/LCJan/LCJan_v1.txt", fill=T)
SOLCFHWAClass <- SOLCAllFHWAClass[,6] [match (Downtarget, SOLCAllFHWAClass[,3])] 


Target_pnnanalysis_Jan0910_obj  <-Target_baseanalysis_Jan0910_obj 

Target_pnnanalysis_Jan0910_table <- cbind(SOLCFHWAClass, min_a_magdif, second_min_a_magdif, max_pnn_prob, max_pnn_prob2, 
                                          second_max_pnn_prob, Downtarget, 
                                          Target_pnnanalysis_Jan0910_obj, pnn_Upid, pnn_Upid2 )
                                         


threshold_PNN <- c(0, 0.2,0.3, 0.4, 0.5, 0.6, 0.7)
Result_PNN <- data.frame()


for (j in 1: length(threshold_PNN)) {
  
  for (i in 1:length(max_pnn_prob)){
    
    if (max_pnn_prob[i] != c(999)){
      
        
        if (max_pnn_prob[i] >= threshold_PNN[j]){
          pnn_Upid_after[i] <- pnn_Upid[i]
        }
        else {
          pnn_Upid_after[i] <- c(999)
        }
    }
    
    else {
      pnn_Upid_after[i] <- c(999)
    }
    
  }
  
  
  for (k in 1:length(max_pnn_prob2)){
    
    if (max_pnn_prob2[k] != c(999)){
                  
          if (max_pnn_prob2[k] > threshold_PNN[j]){
            pnn_Upid_after2[k] <- pnn_Upid2[k]
          }
          else {
            pnn_Upid_after2[k] <- c(999)
          }
    }
    else{
            
            if (max_pnn_prob[k] != c(999) ){
                pnn_Upid_after2[k] <- pnn_Upid[k]
            }
            
            else {
              pnn_Upid_after2[k] <- c(999)
            }
    }
  }
  
  Target_pnnanalysis_Jan0910_table <- cbind ( Target_pnnanalysis_Jan0910_table, pnn_Upid_after, pnn_Upid_after2 )
  
  
  # result
#   missing_nonthreshold <- length(subset(Target_pnnanalysis_Jan0910_table[,7], pnn_Upid !=Target_pnnanalysis_Jan0910_obj))
#   matching_nonthreshold <- length(subset(Target_pnnanalysis_Jan0910_table[,7], pnn_Upid ==Target_pnnanalysis_Jan0910_obj))
#   cmr_matching_nonthreshold <- matching_nonthreshold / (matching_nonthreshold+missing_nonthreshold  )
  
  missing_obj <- length (Target_pnnanalysis_Jan0910_obj[is.na(Target_pnnanalysis_Jan0910_obj)]) 
  matching_obj <- length(Target_pnnanalysis_Jan0910_obj) - missing_obj
  
  matching_PNN0 <-table(Target_pnnanalysis_Jan0910_obj == pnn_Upid)["TRUE"]
  missing_PNN0 <- table(pnn_Upid_after == c(999))["TRUE"]

  matching_PNN <-table(Target_pnnanalysis_Jan0910_obj == pnn_Upid_after)["TRUE"]
  missing_PNN <- table(pnn_Upid_after == c(999))["TRUE"]
  
  matching_PNN2 <-table(Target_pnnanalysis_Jan0910_obj == pnn_Upid_after2)["TRUE"]
  missing_PNN2 <- table(pnn_Upid_after2 == c(999))["TRUE"]
  
  cmr_matching <- 1 - (abs (matching_PNN - matching_obj )/ matching_obj)
  cmr_missing <-  1 - (abs(missing_PNN - missing_obj) / missing_obj)
  
  cmr_matching2 <- 1 - (abs (matching_PNN2 - matching_obj )/ matching_obj)
  cmr_missing2 <-  1 - (abs(missing_PNN2 - missing_obj) / missing_obj)
  
  Result <- data.frame(threshold_PNN[j],  matching_obj[1], missing_obj[1],matching_PNN0[1],  missing_PNN0[1],
                       matching_PNN[1],  missing_PNN[1], matching_PNN2[1],
                       missing_PNN2[1], cmr_matching[1], cmr_missing[1], cmr_matching2[1], cmr_missing2[1])
                        
  Result_PNN <- rbind(Result_PNN, Result)
}



colnames(Result_PNN) <- c("threshold", "matching_obj", "missing_obj",   "matching_PNN_B", "missing_PNN_B", "matching_PNN", "missing_PNN",
                          "matching_PNN2", "missing_PNN2", "cmr_matching", "cmr_missing", "cmr_matching2", "cmr_missing2")

                        
utils::View(Result_PNN)

save(Target_pnnanalysis_Jan0910_table, file="./ProcessedData/Jan0910/Target_pnn_Jan0910.RData")
write.table(Target_pnnanalysis_Jan0910_table, "./ProcessedData/Jan0910/Target_pnn_Jan0910.txt", sep="\t")
write.table(Result_PNN, "./ProcessedData/Jan0910/Result_PNN.txt", sep="\t")

#####################################################################################end

write.table(Upheader_new, "./ProcessedData/Jan0910/Upheader_new.txt", sep="\t")
write.table(Downheader_new, "./ProcessedData/Jan0910/Downheader_new.txt", sep="\t")

