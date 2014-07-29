utils:::menuInstallPkgs() 
rm(list=ls())
# load functonbook2
library(pnn)
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine") 


# load MAr20 tw
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/Mar20tw1_nonss.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/Mar20tw10_nonss.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/Mar20tw30_nonss.RData")


load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/non_ss_shiftandstretch_Jan0910_2.RData")
rm (SO.Jan09ML3sig1, SO.Jan09ML4sig1, SO.Jan10ML4sig2, )
############################################################# do not run when loading RData
### target 1 & 2 :  base and after shift and stretch
# min magdif
min_a_basemagdif<-vector()
for (i in 1: length(a_basemagdif)){
  min_a_basemagdif[i] <- min(a_basemagdif[[i]])
}

idx_basemagdif <- c()

a <- c()

idx_basemagdif <- lapply(a_basemagdif,which.min)

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


# Mar 20
rm(Target_baseanalysis_Mar20_table)
Downtarget=Downtarget[1:length(candi_1)]

Target_baseanalysis_Mar20 <- (cbind(Downtarget, base_Upid))
colnames(Target_baseanalysis_Mar20) <- c("Downid", "baseUpid")
Target_baseanalysis_Mar20_obj  <- (matching_Mar20$sigid  [ match ( Downtarget,matching_Mar20$Downsigid )])
Target_baseanalysis_Mar20_table <- cbind(Target_baseanalysis_Mar20, Target_baseanalysis_Mar20_obj)

Target_baseanalysis_Mar20_table<- cbind(Target_baseanalysis_Mar20_table, min_a_basemagdif )

IrvineAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/RawData/IrvineAllFHWAClass.txt", fill=T)
IrvineMarFHWAClass <- IrvineAllFHWAClass[,6] [match (Downtarget, IrvineAllFHWAClass[,3])] 
Target_baseanalysis_Mar20_table<- cbind(Target_baseanalysis_Mar20_table, IrvineMarFHWAClass)

Target_baseanalysis_Mar20_resultbase <- table(Target_baseanalysis_Mar20_obj == base_Upid)


utils::View(Target_baseanalysis_Mar20_resultbase)


save(Target_baseanalysis_Mar20_table, file="./ProcessedData/SensitivityAnalysis/Target_base_tw10_1_Mar20.RData")
write.table(Target_baseanalysis_Mar20_table, "./ProcessedData/SensitivityAnalysis/Target_base_tw10_1_Mar20.txt", sep="\t")

# Jan 0910

rm(Target_baseanalysis_Jan0910_table, Result_NN)

SOLCAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/RawData/LCJan/LCJan_v1.txt", fill=T)
SOLCFHWAClass <- SOLCAllFHWAClass[,6] [match (Downtarget, SOLCAllFHWAClass[,3])] 
Downtarget=Downtarget[1:length(candi_1)]
Target_baseanalysis_Jan0910_obj  <- (matching$SO[ match ( Downtarget, matching$LC )])

Target_baseanalysis_Jan0910_table <- cbind(SOLCFHWAClass,min_a_basemagdif, Downtarget,  Target_baseanalysis_Jan0910_obj,
                                           base_Upid )
threshold_NN <- c(30, 40, 50, 60, 70, 99999) 
Result_NN <- data.frame()


for (j in 1: length(threshold_NN)) {
  
  for (i in 1:length(idx_basemagdif)){
    if (min_a_basemagdif[i] < threhold_NN[j]){
      base_Upid_after[i] <- base_Upid[i]
    }
    else {
      base_Upid_after[i] <- 999
    }
  }
  
  Target_baseanalysis_Jan0910_table <- cbind ( Target_baseanalysis_Jan0910_table,base_Upid_after )
  
  
  # result
  missing_nonthreshold <- length(subset(Target_baseanalysis_Jan0910_table[,3], base_Upid !=Target_baseanalysis_Jan0910_obj))
  matching_nonthreshold <- length(subset(Target_baseanalysis_Jan0910_table[,3], base_Upid ==Target_baseanalysis_Jan0910_obj))
  cmr_matching_nonthreshold <- matching_nonthreshold / (matching_nonthreshold+missing_nonthreshold  )
  
  missing_obj <- length (Target_baseanalysis_Jan0910_obj[is.na(Target_baseanalysis_Jan0910_obj)]) 
  matching_obj <- length(Target_baseanalysis_Jan0910_obj) - missing_obj
  
  matching_NN_nonss <-table(Target_baseanalysis_Jan0910_obj == base_Upid_after)["TRUE"]
  missing_NN_nonss <- table(base_Upid_after == c(999))["TRUE"]
  
  cmr_matching <- 1 - (abs (matching_NN_nonss - matching_obj )/ matching_obj)
  cmr_missing <-  1 - (abs(missing_NN_nonss-missing_obj) / missing_obj)
  
  Result <- data.frame(matching_nonthreshold[1], missing_nonthreshold[1], cmr_matching_nonthreshold[1],
                       matching_obj[1], missing_obj[1], matching_NN_nonss[[1]],  missing_NN_nonss[[1]],
                       cmr_matching[[1]], cmr_missing[[1]])
  
  Result_NN <- rbind(Result_NN, Result)
}


colnames(Result_NN) <- c("matching_nonthreshold", "missing_nonthreshold", "cmr_matching_nonthreshold",
                         "matching_obj", "missing_obj", "matching_NN_nonss", "missing_NN_nonss",
                         "cmr_matching", "cmr_missing")
utils::View(Result_NN)

save(Target_baseanalysis_Jan0910_table, file="./ProcessedData/Jan0910/Target_base_nonss_Jan0910.RData")
write.table(Target_baseanalysis_Jan0910_table, "./ProcessedData/Jan0910/Target_base_nonss_Jan0910.txt", sep="\t")
write.table(Result_NN, "./ProcessedData/Jan0910/Result_NN.txt", sep="\t")

### target 3 : PNN
############################################################################## start here when loading RData
### AFTER LOADING START HERE!!!
# run pnn
Target_pnnanalysis_Mar20_resultpnn  <- table(c(0,0)) # Mar20
Target_pnnanalysis_Mar20_resultpnn2  <- table(c(0,0)) # Mar20


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



for (w in 1: length(candi_1)){
  #   for (w in 1: 2){
  
  find_pnnindex[w] <- list( a_basemagdif[[w]] )
  len_find_pnn[w] <- length(find_pnnindex[[w]])
  find_pnn_order[[w]] <-  rank(a_basemagdif[[w]])
  
  min_a_magdif = min(a_basemagdif[[w]])
  find_pnnindex2[w] <- list( which( a_basemagdif[[w]] < min_a_magdif * 1.5 ))
  len_find_pnn2[w] <- length(find_pnnindex2[[w]])
  find_pnn_order2[[w]] <-  rank(a_basemagdif[[w]])
  
}


Up_obj_matrix <- seq( from = 1, to = 20 )

Downobjout1 <- c()

for (w in 1: length(candi_1)){ 
  # for (w in 1: 2){ 
  Up_obj <- c()
  Upobjout <- c()
  
  
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
    
    Upobjout <- f.findpnn(seqlevel, len1, candi1, candi2, candi3, candi4, candi5, candi6, candi7, candi8 )
    row.names(Upobjout) <- NULL
    
    if (len1 > 8 ){
      len1 <- 8
    }
    
    Up_obj <-(Up_obj_matrix[1:len1])
    pnn_md <- data.frame(Up_obj, Upobjout )
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
  Upobjout2 <- c()
  
  
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
    
    
    Upobjout2 <- f.findpnn(seqlevel, len2, candi1, candi2, candi3, candi4, candi5, candi6, candi7, candi8 )
    row.names(Upobjout2) <- NULL
    
    
    if (len2 > 8 ){
      len2 <- 8
    }
    
    Up_obj2 <-(Up_obj_matrix[1:len2])
    pnn_md2 <- data.frame(Up_obj2, Upobjout2 )
    nn2 <- learn( pnn_md2, category.column=1)
    nn2 <- smooth(nn2, sigma)
    
    Downpnn2[[w]] <- f.pnn(nn2, Downobjout1)
  }
  
  if (len2 <= 1) {
    
    Downpnn2[[w]] <- c(999)
  }
  
}

### only pnn

for (w in 1: length(candi_1)){
  #   for (w in 1: 2){
  if (as.numeric (Downpnn[[w]][[1]][[1]]) < 100 ) {
    pnn_Upid[w] <- Upsiglist[[w]] [which ( as.numeric( Downpnn[[w]][[1]][[1]] )  == find_pnn_order[[w]])]
  }
  
  else {
    pnn_Upid[w] <- base_Upid
  }
  
}

# Mar20
rm(Target_pnnanalysis_Mar20,Target_pnnanalysis_Mar20_table )
Target_pnnanalysis_Mar20 <- (cbind(Downtarget[1:length(candi_1)], base_Upid[1:length(candi_1)], pnn_Upid[1:length(candi_1)]))
colnames(Target_pnnanalysis_Mar20) <- c("Downid", "baseUpid",  "pnnUpid")
Target_pnnanalysis_Mar20 <- cbind(Target_pnnanalysis_Mar20, IrvineOctFHWAClass)

Target_pnnanalysis_Mar20_resultpnn <- table(Target_baseanalysis_Mar20_obj  == pnn_Upid)
Target_pnnanalysis_Mar20 <- cbind(Target_pnnanalysis_Mar20, Target_baseanalysis_Mar20_obj)


max_pnn_prob <- vector()
for (i in 1: length(Downpnn)){
  if (length(Downpnn[[i]]) > 1) {
    max_pnn_prob[i] <- max(Downpnn[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob[i] <- 999
  }
  
}

Target_pnnanalysis_Mar20_table<- cbind(Target_pnnanalysis_Mar20, max_pnn_prob)

utils::View(Target_pnnanalysis_Mar20_resultpnn)

save(Target_pnnanalysis_Mar20_table, file="./ProcessedData/SensitivityAnalysis/Target_pnn_tw10_1_Mar20.RData")
write.table(Target_pnnanalysis_Mar20_table, "./ProcessedData/SensitivityAnalysis/Target_pnn_tw10_1_Mar20.txt", sep="\t")

# Jan 0910

SOLCAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/RawData/LCJan/LCJan_v1.txt", fill=T)
SOLCFHWAClass <- SOLCAllFHWAClass[,6] [match (Downtarget, SOLCAllFHWAClass[,3])] 
Downtarget=Downtarget[1:length(candi_1)]
Target_pnnanalysis_Jan0910_obj  <- (matching$SO[ match ( Downtarget, matching$LC )])

Target_pnnanalysis_Jan0910_table <- (cbind(Downtarget[1:length(candi_1)], base_Upid[1:length(candi_1)], pnn_Upid[1:length(candi_1)]))
colnames(Target_pnnanalysis_Jan0910) <- c("Downid", "baseUpid",  "pnnUpid")
Target_pnnanalysis_Jan0910_table <- cbind(SOLCFHWAClass, max_pnn_prob, Downtarget,  Target_pnnanalysis_Jan0910_obj,
                                          pnn_Upid )


threhold_PNN <- c(0.4, 0.5, 0.6, 0.7, 0)



Target_pnnanalysis_Jan0910_resultpnn_before <- table(Target_baseanalysis_Jan0910_obj  == pnn_Upid)


max_pnn_prob <- vector()
for (i in 1: length(Downpnn)){
  if (length(Downpnn[[i]]) > 1) {
    max_pnn_prob[i] <- max(Downpnn[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob[i] <- 999
  }
}

Target_pnnanalysis_Jan0910_table<- cbind(Target_pnnanalysis_Jan0910_table, max_pnn_prob)

utils::View(Target_pnnanalysis_Jan0910_resultpnn_before)

for (i in 1:length(Downpnn)){
  
  if (max_pnn_prob[i] == 999){
    pnn_Upid_after[i] <- c(999)
  }
  
  else{
    
    if (max_pnn_prob[i] > threhold_PNN){
      pnn_Upid_after[i] <- pnn_Upid[i]
    }
    else {
      pnn_Upid_after[i] <- c(999)
    }
  }
}

Target_pnnanalysis_Jan0910_table<- cbind(Target_pnnanalysis_Jan0910_table, pnn_Upid_after)
Target_pnnanalysis_Jan0910_resultpnn_after <- table(Target_baseanalysis_Jan0910_obj == pnn_Upid_after)

utils::View(Target_pnnanalysis_Jan0910_resultpnn_before)
utils::View(Target_pnnanalysis_Jan0910_resultpnn_after)

save(Target_pnnanalysis_Jan0910_table, file="./ProcessedData/Jan0910/Target_pnn_nonss_Jan0910.RData")
write.table(Target_pnnanalysis_Jan0910_table, "./Results/Jan0910/Target_pnn_nonss_Jan0910.txt", sep="\t")



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
# Mar20
rm(Target_pnnanalysis2_Mar20)
Target_pnnanalysis2_Mar20 <- (cbind(Downtarget[1:length(candi_1)], base_Upid[1:length(candi_1)], pnn_Upid[1:length(candi_1)], pnn_Upid2[1:length(candi_1)]))
colnames(Target_pnnanalysis2_Mar20) <- c("Downid", "baseUpid", "pnnUpid", "pnnUpid2")
Target_pnnanalysis2_Mar20<- cbind(Target_pnnanalysis2_Mar20, IrvineMarFHWAClass)

Target_pnnanalysis2_Mar20_resultpnn2 <- table(Target_baseanalysis_Mar20_obj  == pnn_Upid2)
Target_pnnanalysis2_Mar20_table2 <- cbind(Target_pnnanalysis2_Mar20, Target_baseanalysis_Mar20_obj)

max_pnn_prob2 <- vector()
for (i in 1: length(Downpnn2)){
  if (length(Downpnn2[[i]]) > 1) {
    max_pnn_prob2[i] <- max(Downpnn2[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob2[i] <- 999
  }
  
}

Target_pnnanalysis2_Mar20_table2 <- cbind(Target_pnnanalysis2_Mar20_table2, max_pnn_prob2)

utils::View(Target_pnnanalysis2_Mar20_resultpnn2)

save(Target_pnnanalysis2_Mar20_table2, file="./ProcessedData/SensitivityAnalysis/Target_pnn2_tw10_1_Mar20.RData")
write.table(Target_pnnanalysis2_Mar20_table2, "./ProcessedData/SensitivityAnalysis/Target_pnn2_tw10_1_Mar20.txt", sep="\t")

# Jan 0910
threhold_NN <- 20 # 30, 40, 50, 60, 70 
threhold_PNN2 <- 0.4 # 0.5, 0.6, 0.7

Target_pnnanalysis2_Jan0910 <- (cbind(Downtarget[1:length(candi_1)], base_Upid[1:length(candi_1)],  pnn_Upid[1:length(candi_1)], pnn_Upid2[1:length(candi_1)]))
colnames(Target_pnnanalysis2_Jan0910) <- c("Downid", "baseUpid", "pnnUpid", "pnnUpid2")
Target_pnnanalysis2_Jan0910<- cbind(Target_pnnanalysis2_Jan0910, SOLCFHWAClass)

Target_pnnanalysis2_Jan0910_resultpnn2_before <- table(Target_baseanalysis_Jan0910_obj  == pnn_Upid2)
Target_pnnanalysis2_Jan0910_table2 <- cbind(Target_pnnanalysis2_Jan0910, Target_baseanalysis_Jan0910_obj)

max_pnn_prob2 <- vector()
for (i in 1: length(Downpnn2)){
  
  if (length(Downpnn2[[i]]) > 1) {
    max_pnn_prob2[i] <- max(Downpnn2[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob2[i] <- 999
  }
  
}

Target_pnnanalysis2_Jan0910_table2 <- cbind(Target_pnnanalysis2_Jan0910_table2, max_pnn_prob2)


pnn_Upid_after2 <-c()

for (i in 1:length(Downpnn2)){
  if (max_pnn_prob2[i] <- 999){
    pnn_Upid_after2[i] <- c(999)
  }
  else {
    
    if (min_a_basemagdif[i] < threhold_NN) {
      
      if (max_pnn_prob2[i] > threhold_PNN2){
        pnn_Upid_after2[i] <- pnn_Upid2[i]
      }
      else {
        pnn_Upid_after2[i] <- c(999)
      }
    }
    else{
      pnn_Upid_after2[i] <- c(999)
    }
  }
}

Target_pnnanalysis2_Jan0910_table2<- cbind(Target_pnnanalysis2_Jan0910_table2, pnn_Upid_after2)
Target_pnnanalysis2_Jan0910_resultpnn2_after <- table(Target_baseanalysis_Jan0910_obj == pnn_Upid_after2)

utils::View(Target_pnnanalysis2_Jan0910_resultpnn2_before)
utils::View(Target_pnnanalysis2_Jan0910_resultpnn2_after)



save(Target_pnnanalysis2_Jan0910_table2, file="./ProcessedData/Jan0910/Target_pnn2_nonss_Jan0910.RData")
write.table(Target_pnnanalysis2_Jan0910_table2, "./ProcessedData/Jan0910/Target_pnn2_nonss_Jan0910.txt", sep="\t")



# save
save.image("./ProcessedData/SensitivityAnalysis/FindTarget_tw30_1_Mar20.RData") # sensitivity

save.image("./ProcessedData/Jan0910/FindTarget_Jan0910_nonss.RData") # Jan
####################################################################################################### end



##########################
## find threshold
min_a_magdif<-vector()
for (i in 1: length(a_magdif)){
  min_a_magdif[i] <- min(a_magdif[[i]])
}
View(min_a_magdif)

max_pnn_prob <- vector()
for (i in 1: length(Downpnn)){
  if (length(Downpnn[[i]]) > 1) {
    max_pnn_prob[i] <- max(Downpnn[[i]][[2]][[1]])
  }
}

View(max_pnn_prob)
