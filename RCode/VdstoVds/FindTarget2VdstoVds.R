
utils:::menuInstallPkgs() 
rm(list=ls())
# load functonbook2
library(pnn)
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine") 


# load Oct 02
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Oct02/shiftandstretch_Oct02.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Oct02/wimobjout1_Oct02.RData")
wimobjout<-wimobjout1

# load Mar 20
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Mar20/shiftandstretch_Mar20.RData")

#load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Mar20/Target_base_Mar20.RData")
#load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Mar20/wimobjout_Mar20.RData")
#load("./ wimobjout1_Mar20.RData")

# load Jan0910
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/shiftandstretch_Jan0910.RData")
laod("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/wimobjout_Jan0910.RData")

# seqlevel50 <- 50
# seqlevel10 <- 10
# seqlevel5 <- 5
# seqlevel2 <- 2
# seqlevel1 <- 1
############################################################# do not run when loading RData
### target 1 & 2 :  base and after shift and stretch
# min magdif
min_a_magdif<-vector()
for (i in 1: length(a_magdif)){
  min_a_magdif[i] <- min(a_magdif[[i]])
}

# second min
for (i in 1: length(a_magdif)){
  n <- length(a_magdif[i])
  if n > 1 {
    second_min_a_magdif[i] <- sort(a_magdif,partial=n-1)[n-1]
  }
  else {
    second_min_a_magdif[i] <- c(999)
  }
}

min_a_basemagdif<-vector()
for (i in 1: length(a_magdif)){
  min_a_basemagdif[i] <- min(a_basemagdif[[i]])
}

for (i in 1: length(a_basemagdif)){
  n <- length(a_basemagdif[i])
  if n > 1 {
    second_min_a_basemagdif[i] <- sort(a_basemagdif,partial=n-1)[n-1]
  }
  else {
    second_min_a_basemagdif[i] <- c(999)
  }
}


idx_basemagdif <- c()
idx_ssmagdif <- c()
a <- c()
b <- c()

idx_basemagdif <- lapply(a_basemagdif,which.min)
idx_ssmagdif <- lapply(a_magdif,which.min)

base_vdsid <- c()
ss_vdsid <- c()
j=1

for (i in 1:length(idx_basemagdif)){
  a <- unlist (idx_basemagdif[i])
  b <- unlist (idx_ssmagdif[i])
  if (length(a) > 0 ){
    base_vdsid[i] <- vdssiglist[[j]][a]
    ss_vdsid[i] <- vdssiglist[[j]][b]
    j <- j+1
  }
  else {
    base_vdsid[i] <- c(999)
    ss_vdsid[i] <- c(999)
    j <- j+1
  }
}


# Oct 02
rm(Target_baseanalysis_Oct02_table)
wimtarget=wimtarget[1:length(candi_1)]

Target_baseanalysis_Oct02 <- (cbind(wimtarget, base_vdsid, ss_vdsid))
colnames(Target_baseanalysis_Oct02) <- c("wimid", "basevdsid", "ssvdsid")
Target_baseanalysis_Oct02_obj  <- (matching_Oct02$sigid  [ match ( wimtarget,matching_Oct02$wimsigid )])
Target_baseanalysis_Oct02_table <- cbind(Target_baseanalysis_Oct02, Target_baseanalysis_Oct02_obj)

Target_baseanalysis_Oct02_table<- cbind(Target_baseanalysis_Oct02_table, min_a_basemagdif, min_a_magdif, second_min_a_magdif, second_min_a_basemagdif )

IrvineAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/RawData/IrvineAllFHWAClass.txt", fill=T)
IrvineOctFHWAClass <- IrvineAllFHWAClass[,6] [match (wimtarget, IrvineAllFHWAClass[,3])] 
Target_baseanalysis_Oct02_table<- cbind(Target_baseanalysis_Oct02_table, IrvineOctFHWAClass)

Target_baseanalysis_Oct02_resultbase <- table(Target_baseanalysis_Oct02_obj == base_vdsid)
Target_baseanalysis_Oct02_resultss <- table(Target_baseanalysis_Oct02_obj == ss_vdsid)


utils::View(Target_baseanalysis_Oct02_resultbase)
utils::View(Target_baseanalysis_Oct02_resultss)

save(Target_baseanalysis_Oct02_table, file="./ProcessedData/Oct02/Target_base_Oct02.RData")
write.table(Target_baseanalysis_Oct02_table, "./ProcessedData/Oct02/Target_base_Oct02.txt", sep="\t")

# Mar 20
rm(Target_baseanalysis_Mar20_table)
wimtarget=wimtarget[1:length(candi_1)]

Target_baseanalysis_Mar20 <- (cbind(wimtarget, base_vdsid, ss_vdsid))
colnames(Target_baseanalysis_Mar20) <- c("wimid", "basevdsid", "ssvdsid")
Target_baseanalysis_Mar20_obj  <- (matching_Mar20$sigid  [ match ( wimtarget,matching_Mar20$wimsigid )])
Target_baseanalysis_Mar20_table <- cbind(Target_baseanalysis_Mar20, Target_baseanalysis_Mar20_obj)

Target_baseanalysis_Mar20_table<- cbind(Target_baseanalysis_Mar20_table, min_a_basemagdif, min_a_magdif, second_min_a_magdif, second_min_a_basemagdif )

IrvineAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/RawData/IrvineAllFHWAClass.txt", fill=T)
IrvineMarFHWAClass <- IrvineAllFHWAClass[,6] [match (wimtarget, IrvineAllFHWAClass[,3])] 
Target_baseanalysis_Mar20_table<- cbind(Target_baseanalysis_Mar20_table, IrvineMarFHWAClass)

Target_baseanalysis_Mar20_resultbase <- table(Target_baseanalysis_Mar20_obj == base_vdsid)
Target_baseanalysis_Mar20_resultss <- table(Target_baseanalysis_Mar20_obj == ss_vdsid)

utils::View(Target_baseanalysis_Mar20_resultbase)
utils::View(Target_baseanalysis_Mar20_resultss)

save(Target_baseanalysis_Mar20_table, file="./ProcessedData/Mar20/Target_base_Mar20.RData")
write.table(Target_baseanalysis_Mar20_table, "./ProcessedData/Mar20/Target_base_Mar20.txt", sep="\t")

# Jan 0910
wimtarget=wimtarget[1:length(candi_1)]

Target_baseanalysis_Jan0910 <- (cbind(wimtarget, base_vdsid, ss_vdsid))
colnames(Target_baseanalysis_Jan0910 ) <- c("wimid", "basevdsid", "ssvdsid")
Target_baseanalysis_Jan0910_obj  <- (matching$SO[ match ( wimtarget, matching$LC )])

Target_baseanalysis_Jan0910_table <- cbind(Target_baseanalysis_Jan0910 , Target_baseanalysis_Jan0910_obj)

Target_baseanalysis_Jan0910_resultbase <- table(Target_baseanalysis_Jan0910_obj == base_vdsid)
Target_baseanalysis_Jan0910_resultss <- table(Target_baseanalysis_Jan0910_obj == ss_vdsid)

utils::View(Target_baseanalysis_Jan0910_resultbase)
utils::View(Target_baseanalysis_Jan0910_resultss)

save(Target_baseanalysis_Jan0910_table, file="./ProcessedData/Jan0910/Target_base_Jan0910.RData")
write.table(Target_baseanalysis_Mar20_table, "./ProcessedData/Jan0910/Target_base_Jan0910 .txt", sep="\t")

### target 3 : PNN
############################################################################## start here when loading RData
### AFTER LOADING START HERE!!!
# run pnn
Target_pnnanalysis_Oct02_resultpnn  <- table(c(0,0)) # Oct 02
Target_pnnanalysis_Oct02_resultpnn2  <- table(c(0,0)) # Oct 02
Target_pnnanalysis_Mar20_resultpnn  <- table(c(0,0)) # Mar 20
Target_pnnanalysis_Mar20_resultpnn2  <- table(c(0,0)) # Mar 20 v2
Target_pnnanalysis_Jan0910_resultpnn  <- table(c(0,0)) # Jan0910
Target_pnnanalysis_Jan0910_resultpnn2  <- table(c(0,0)) # Jan0910 v2

wimpnn <- list()
wimpnn2 <- list()

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
pnn_vdsid <- c()
len1 <- c()

find_pnnindex2 <- list()
find_pnn_order2 <- list()
len_find_pnn2 <- c()
pnn_vdsid2 <- c()
len2 <- c()

### choose params!
seqlevel <- 20
sigma <- 1

### find index for pnn

f.pnn <- function ( nn, wimobjout1){
  
  cat <- list()
  prob <- list()
  category <- list()
  probs <- list()
  
  candi_guess <- list(guess(nn, wimobjout1)) 
  cat <- candi_guess[[1]][1]
  prob <- candi_guess[[1]][2]
  
  
  return (list(category <- cat , probs <- prob))
}



 for (w in 1: length(candi_1)){
#   for (w in 1: 2){

  find_pnnindex[w] <- list( a_magdif[[w]] )
  len_find_pnn[w] <- length(find_pnnindex[[w]])
  find_pnn_order[[w]] <-  rank(a_magdif[[w]])
  
  min_a_magdif = min(a_magdif[[w]])
  find_pnnindex2[w] <- list( which( a_magdif[[w]] < min_a_magdif * 1.5 ))
  len_find_pnn2[w] <- length(find_pnnindex2[[w]])
  find_pnn_order2[[w]] <-  rank(a_magdif[[w]])
  
}


vds_obj_matrix <- seq( from = 1, to = 20 )

wimobjout1 <- c()

for (w in 1: length(candi_1)){ 
# for (w in 1: 2){ 
  vds_obj <- c()
  vdsobjout <- c()
 
  
  len1 <- len_find_pnn[w]
 
  candi1 <- candi_1[w]
  candi2 <- candi_2[w]
  candi3 <- candi_3[w]
  candi4 <- candi_4[w]
  candi5 <- candi_5[w]
  candi6 <- candi_6[w]
  candi7 <- candi_7[w]
  candi8 <- candi_8[w]
  
  wimobjout1 <- wimobjout[w,]
  wimobjout1 <- wimobjout1[seq(1, length(wimobjout1), seqlevel)]
 
  if (len1 > 1) {
    
   vdsobjout <- f.findpnn(seqlevel, len1, candi1, candi2, candi3, candi4, candi5, candi6, candi7, candi8 )
   row.names(vdsobjout) <- NULL
  
        if (len1 > 8 ){
          len1 <- 8
        }
    
    vds_obj <-(vds_obj_matrix[1:len1])
    pnn_md <- data.frame(vds_obj, vdsobjout )
     nn <- learn( pnn_md, category.column=1)
     nn <- smooth(nn, sigma)
    
  
    wimpnn[[w]] <- f.pnn(nn, wimobjout1)
  }
  
  if (len1 <= 1) {
    
    wimpnn[[w]] <- c(999)
  }
}

for (w in 1: length(candi_1)){ 
  # for (w in 1: 2){ 
  vds_obj2 <- c()
  vdsobjout2 <- c()
  

  len2 <- len_find_pnn2[w]
  
  candi1 <- candi_1[w]
  candi2 <- candi_2[w]
  candi3 <- candi_3[w]
  candi4 <- candi_4[w]
  candi5 <- candi_5[w]
  candi6 <- candi_6[w]
  candi7 <- candi_7[w]
  candi8 <- candi_8[w]
  
  wimobjout1 <- wimobjout[w,]
  wimobjout1 <- wimobjout1[seq(1, length(wimobjout1), seqlevel)]

  if (len2 > 1) {
    
    
     vdsobjout2 <- f.findpnn(seqlevel, len2, candi1, candi2, candi3, candi4, candi5, candi6, candi7, candi8 )
     row.names(vdsobjout2) <- NULL
     
      
       if (len2 > 8 ){
         len2 <- 8
       }
     
     vds_obj2 <-(vds_obj_matrix[1:len2])
     pnn_md2 <- data.frame(vds_obj2, vdsobjout2 )
     nn2 <- learn( pnn_md2, category.column=1)
     nn2 <- smooth(nn2, sigma)

     wimpnn2[[w]] <- f.pnn(nn2, wimobjout1)
   }

   if (len2 <= 1) {
   
      wimpnn2[[w]] <- c(999)
    }

}
### only pnn

 for (w in 1: length(candi_1)){
#   for (w in 1: 2){
    if (as.numeric (wimpnn[[w]][[1]][[1]]) < 100 ) {
       pnn_vdsid[w] <- vdssiglist[[w]] [which ( as.numeric( wimpnn[[w]][[1]][[1]] )  == find_pnn_order[[w]])]
    }
    
    else {
      pnn_vdsid[w] <- base_vdsid[w]
    }
    
}

# Oct 02
rm(Target_pnnanalysis_Oct02,Target_pnnanalysis_Oct02_table )
Target_pnnanalysis_Oct02 <- (cbind(wimtarget[1:length(candi_1)], base_vdsid[1:length(candi_1)],ss_vdsid[1:length(candi_1)], pnn_vdsid[1:length(candi_1)]))
colnames(Target_pnnanalysis_Oct02) <- c("wimid", "basevdsid", "ssvdsid", "pnnvdsid")
Target_pnnanalysis_Oct02 <- cbind(Target_pnnanalysis_Oct02, IrvineOctFHWAClass)

Target_pnnanalysis_Oct02_resultpnn <- table(Target_baseanalysis_Oct02_obj  == pnn_vdsid)
Target_pnnanalysis_Oct02 <- cbind(Target_pnnanalysis_Oct02, Target_baseanalysis_Oct02_obj)


max_pnn_prob <- vector()
for (i in 1: length(wimpnn)){
  if (length(wimpnn[[i]]) > 1) {
    max_pnn_prob[i] <- max(wimpnn[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob[i] <- 999
  }
  
}


# second max 
for (i in 1: length(wimpnn)){
  n <- length(wimpnn)
  if (length(wimpnn[[i]]) > 1) {
    second_max_pnn_prob[i] <- sort(wimpnn[[i]][[2]][[1]],partial=n-1)[n-1]
  }
  else {
    second_max_pnn_prob[i] <- c(999)
  }
}

Target_pnnanalysis_Oct02_table<- cbind(Target_pnnanalysis_Oct02, max_pnn_prob, second_max_pnn_prob)

utils::View(Target_pnnanalysis_Oct02_resultpnn)

save(Target_pnnanalysis_Oct02_table, file="./ProcessedData/Oct02/Target_pnn_Oct02.RData")
#save.image("./ProcessedData/Oct02/FindTarget_Oct02.RData")
write.table(Target_pnnanalysis_Oct02_table, "./ProcessedData/Oct02/Target_pnn_Oct02.txt", sep="\t")

# Mar 20
rm(Target_pnnanalysis_Mar20)
Target_pnnanalysis_Mar20 <- (cbind(wimtarget[1:length(candi_1)], base_vdsid[1:length(candi_1)],ss_vdsid[1:length(candi_1)], pnn_vdsid[1:length(candi_1)]))
colnames(Target_pnnanalysis_Mar20) <- c("wimid", "basevdsid", "ssvdsid", "pnnvdsid")

Target_pnnanalysis_Mar20_resultpnn <- table(Target_baseanalysis_Mar20_obj  == pnn_vdsid)
Target_pnnanalysis_Mar20_table <- cbind(Target_pnnanalysis_Mar20, Target_baseanalysis_Mar20_obj)
Target_pnnanalysis_Mar20_table<- cbind(Target_pnnanalysis_Mar20_table, IrvineMarFHWAClass)

max_pnn_prob <- vector()
for (i in 1: length(wimpnn)){
  if (length(wimpnn[[i]]) > 1) {
    max_pnn_prob[i] <- max(wimpnn[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob[i] <- 999
  }
    
}

# second max 
for (i in 1: length(wimpnn)){
  n <- length(wimpnn)
  if (length(wimpnn[[i]]) > 1) {
    second_max_pnn_prob[i] <- sort(wimpnn[[i]][[2]][[1]],partial=n-1)[n-1]
  }
  else {
    second_max_pnn_prob[i] <- c(999)
  }
}

Target_pnnanalysis_Mar20_table <- cbind(Target_pnnanalysis_Mar20_table, max_pnn_prob, second_max_pnn_prob)

utils::View(Target_pnnanalysis_Mar20_resultpnn)

save(Target_pnnanalysis_Mar20_table, file="./ProcessedData/Mar20/Target_pnn_Mar20.RData")

write.table(Target_pnnanalysis_Mar20_table, "./ProcessedData/Mar20/Target_pnn_Mar20.txt", sep="\t")

# Jan 0910
Target_pnnanalysis_Jan0910 <- (cbind(wimtarget[1:length(candi_1)], base_vdsid[1:length(candi_1)],ss_vdsid[1:length(candi_1)], pnn_vdsid[1:length(candi_1)]))
colnames(Target_pnnanalysis_Jan0910) <- c("wimid", "basevdsid", "ssvdsid", "pnnvdsid")

Target_pnnanalysis_Jan0910_resultpnn <- table(Target_baseanalysis_Jan0910_obj  == pnn_vdsid)
Target_pnnanalysis_Jan0910_table <- cbind(Target_pnnanalysis_Jan0910, Target_baseanalysis_Jan0910_obj)


utils::View(Target_pnnanalysis_Jan0910_resultpnn)

save(Target_pnnanalysis_Jan0910_table, file="./ProcessedData/Jan0910/Target_pnn_Jan0910.RData")
#save.image("./ProcessedData/Jan0910/FindTarget_Jan0910.RData")
write.table(Target_pnnanalysis_Jan0910_table, "./Results/Jan0910/Target_pnn_Jan0910.txt", sep="\t")

### first ss and second pnn
for (w in 1: length(candi_1)){
#  for (w in 1: 2){
   if (as.numeric (wimpnn2[[w]][[1]][[1]] < 998 )) {
     
       pnn_vdsid2[w] <- vdssiglist[[w]] [which ( as.numeric( wimpnn2[[w]][[1]][[1]] )  == find_pnn_order2[[w]])]
   }
   
   else {
     pnn_vdsid2[w] <- base_vdsid[w]
     
   }
     
}
# OCt 02
rm(Target_pnnanalysis2_Oct02)
Target_pnnanalysis2_Oct02 <- (cbind(wimtarget[1:length(candi_1)], base_vdsid[1:length(candi_1)], ss_vdsid[1:length(candi_1)], pnn_vdsid[1:length(candi_1)], pnn_vdsid2[1:length(candi_1)]))
colnames(Target_pnnanalysis2_Oct02) <- c("wimid", "basevdsid", "ssvdsid", "pnnvdsid", "pnnvdsid2")
Target_pnnanalysis2_Oct02<- cbind(Target_pnnanalysis2_Oct02, IrvineOctFHWAClass)

Target_pnnanalysis2_Oct02_resultpnn2 <- table(Target_baseanalysis_Oct02_obj  == pnn_vdsid2)

Target_pnnanalysis2_Oct02_table2 <- cbind(Target_pnnanalysis2_Oct02, Target_baseanalysis_Oct02_obj)

max_pnn_prob2 <- vector()
for (i in 1: length(wimpnn2)){
  if (length(wimpnn2[[i]]) > 1) {
    max_pnn_prob2[i] <- max(wimpnn2[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob2[i] <- 999
  }
  
}

# second max 
for (i in 1: length(wimpnn2)){
  n <- length(wimpnn2)
  if (length(wimpnn2[[i]]) > 1) {
    second_max_pnn_prob2[i] <- sort(wimpnn2[[i]][[2]][[1]],partial=n-1)[n-1]
  }
  else {
    second_max_pnn_prob2[i] <- c(999)
  }
}

Target_pnnanalysis2_Oct02_table2 <- cbind(Target_pnnanalysis2_Oct02_table2, max_pnn_prob2, second_max_pnn_prob2)

utils::View(Target_pnnanalysis2_Oct02_resultpnn2)

save(Target_pnnanalysis2_Oct02_table2, file="./ProcessedData/Oct02/Target_pnn2_Oct02.RData")

write.table(Target_pnnanalysis2_Oct02_table2, "./ProcessedData/Oct02/Target_pnn2_Oct02.txt", sep="\t")

# Mar 20
rm(Target_pnnanalysis2_Mar20)
Target_pnnanalysis2_Mar20 <- (cbind(wimtarget[1:length(candi_1)], base_vdsid[1:length(candi_1)], ss_vdsid[1:length(candi_1)], pnn_vdsid[1:length(candi_1)], pnn_vdsid2[1:length(candi_1)]))
colnames(Target_pnnanalysis2_Mar20) <- c("wimid", "basevdsid", "ssvdsid", "pnnvdsid", "pnnvdsid2")
Target_pnnanalysis2_Mar20 <- cbind(Target_pnnanalysis2_Mar20, IrvineMarFHWAClass)

Target_pnnanalysis2_Mar20_resultpnn2 <- table(Target_baseanalysis_Mar20_obj  == pnn_vdsid2)
Target_pnnanalysis2_Mar20_table2 <- cbind(Target_pnnanalysis2_Mar20, Target_baseanalysis_Mar20_obj)

max_pnn_prob2 <- vector()
for (i in 1: length(wimpnn2)){
  if (length(wimpnn2[[i]]) > 1) {
    max_pnn_prob2[i] <- max(wimpnn2[[i]][[2]][[1]])
  }
  else {
    max_pnn_prob2[i] <- 999
  }
  
}

# second max 
for (i in 1: length(wimpnn2)){
  n <- length(wimpnn2)
  if (length(wimpnn2[[i]]) > 1) {
    second_max_pnn_prob2[i] <- sort(wimpnn2[[i]][[2]][[1]],partial=n-1)[n-1]
  }
  else {
    second_max_pnn_prob2[i] <- c(999)
  }
}

Target_pnnanalysis2_Mar20_table2 <- cbind(Target_pnnanalysis2_Mar20_table2, max_pnn_prob2, second_max_pnn_prob2)
utils::View(Target_pnnanalysis2_Mar20_resultpnn2)

save(Target_pnnanalysis2_Mar20_table2, file="./ProcessedData/Mar20/Target_pnn2_Mar20.RData")

write.table(Target_pnnanalysis2_Mar20_table2, "./ProcessedData/Mar20/Target_pnn2_Mar20.txt", sep="\t")


# Jan 0910
Target_pnnanalysis2_Jan0910 <- (cbind(wimtarget[1:length(candi_1)], base_vdsid[1:length(candi_1)], ss_vdsid[1:length(candi_1)], pnn_vdsid[1:length(candi_1)], pnn_vdsid2[1:length(candi_1)]))
colnames(Target_pnnanalysis2_Jan0910) <- c("wimid", "basevdsid", "ssvdsid", "pnnvdsid", "pnnvdsid2")

Target_pnnanalysis2_Jan0910_resultpnn2 <- table(Target_baseanalysis_Jan0910_obj  == pnn_vdsid2)
Target_pnnanalysis2_Jan0910_table2 <- cbind(Target_pnnanalysis2_Jan0910, Target_baseanalysis_Jan0910_obj)


utils::View(Target_pnnanalysis2_Jan0910_resultpnn2)

save(Target_pnnanalysis2_Jan0910_table2, file="./ProcessedData/Jan0910/Target_pnn2_Jan0910.RData")
write.table(Target_pnnanalysis2_Jan0910_table2, "./ProcessedData/Jan0910/Target_pnn2_Jan0910.txt", sep="\t")


# save
save.image("./ProcessedData/Oct02/FindTarget_Oct02.RData")
save.image("./ProcessedData/Mar20/FindTarget_Mar20.RData")
save.image("./ProcessedData/Jan0910/FindTarget_Jan0910.RData")
####################################################################################################### end
## find threshold
min_a_magdif<-vector()
for (i in 1: length(a_magdif)){
  min_a_magdif[i] <- min(a_magdif[[i]])
}
View(min_a_magdif)

max_pnn_prob <- vector()
for (i in 1: length(wimpnn)){
  if (length(wimpnn[[i]]) > 1) {
    max_pnn_prob[i] <- max(wimpnn[[i]][[2]][[1]])
  }
}

View(max_pnn_prob)
