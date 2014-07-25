utils:::menuInstallPkgs() 
rm(list=ls())
library(pnn)
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine") 

# load Oct 02
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/shiftandstretch_Oct02.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ Target_base_Oct02.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ wimobjout_Oct02.RData")
load("./ wimobjout1_Oct02.RData")
load("./ wimobjout10_Oct02.RData")
load("./ wimobjout5_Oct02.RData")
load("./ wimobjout2_Oct02.RData")

# load Mar 20
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/shiftandstretch_Mar20.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ Target_base_Mar20.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ wimobjout_Mar20.RData")
load("./ wimobjout1_Mar20.RData")
load("./ wimobjout10_Mar20.RData")
load("./ wimobjout5_Mar20.RData")
load("./ wimobjout2_Mar20.RData")

wim_obj_matrix <- diag(length(wimidx))
seqlevel50 <- 50
seqlevel10 <- 10
seqlevel5 <- 5
seqlevel2 <- 2
seqlevel1 <- 1
############################################################# do not run when loading RData
### target 1 & 2 :  base and after shift and stretch
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
  base_vdsid[i] <- vdssiglist[[j]][a]
  ss_vdsid[i] <- vdssiglist[[j]][b]
  j <- j+1
}


# Oct 02
wimtarget=wimtarget[1:length(candi_1)]

Target_baseanalysis_Oct02 <- (cbind(wimtarget, base_vdsid, ss_vdsid))
colnames(Target_baseanalysis_Oct02) <- c("wimid", "basevdsid", "ssvdsid")
Target_baseanalysis_Oct02_obj  <- (matching_Oct02$sigid  [ match ( wimtarget,matching_Oct02$wimsigid )])
Target_baseanalysis_Oct02_table <- cbind(Target_baseanalysis_Oct02, Target_baseanalysis_Oct02_obj)

Target_baseanalysis_Oct02_resultbase <- table(Target_baseanalysis_Oct02_obj == base_vdsid)
Target_baseanalysis_Oct02_resultss <- table(Target_baseanalysis_Oct02_obj == ss_vdsid)

utils::View(Target_baseanalysis_Oct02_resultbase)
utils::View(Target_baseanalysis_Oct02_resultss)

save(Target_baseanalysis_Oct02_table, file="./Results/Oct02/Target_base_Oct02.RData")
# write.table(Target_baseanalysis_Oct02_table, "./ProcessedData/Target_base_Oct02.txt", sep="\t")

# Mar 20
wimtarget=wimtarget[1:length(candi_1)]

Target_baseanalysis_Mar20 <- (cbind(wimtarget, base_vdsid, ss_vdsid))
colnames(Target_baseanalysis_Mar20) <- c("wimid", "basevdsid", "ssvdsid")
Target_baseanalysis_Mar20_obj  <- (matching_Mar20$sigid  [ match ( wimtarget,matching_Mar20$wimsigid )])
Target_baseanalysis_Mar20_table <- cbind(Target_baseanalysis_Mar20, Target_baseanalysis_Mar20_obj)

Target_baseanalysis_Mar20_resultbase <- table(Target_baseanalysis_Mar20_obj == base_vdsid)
Target_baseanalysis_Mar20_resultss <- table(Target_baseanalysis_Mar20_obj == ss_vdsid)

utils::View(Target_baseanalysis_Mar20_resultbase)
utils::View(Target_baseanalysis_Mar20_resultss)

save(Target_baseanalysis_Mar20_table, file=" Target_base_Mar20.RData")
write.table(Target_baseanalysis_Mar20_table, "./ProcessedData/Target_base_Mar20.txt", sep="\t")

### target 3 : PNN
# wim data ready for PNN
# wimobjout10 - seq level 10

# wimobj <- c()
# wimobjout1 <- c()
# wimobjout50 <- c()
# wimobjout10 <- c()
# wimobjout5 <- c()
# wimobjout2 <- c()
# 
# 
# for (w in 1: length(wimidx)){
# 
#   
#   wimindex <- wimidx[w]
#   inwimsig <- wimsig_IM[wimindex+1,]
#   wimindex <- wimindex+1
#   
#   while (wimsig_IM[wimindex+1,1] < 100){
#     inwimsig <- rbind(inwimsig,wimsig_IM[wimindex+1,])
#     wimindex <- wimindex+1
#   }
#   inwimsig <- f.normalization(inwimsig)
#   splinewim <- f.interpolation(inwimsig,num,no_round)
#     
#   wimobj <- c(splinewim[,2])
#   wimobj <- t(wimobj)
#   
#   wimobjout1 <-rbind(wimobjout1,  wimobj)
#   
#   wimobj50 <- wimobj[seq(1, length(wimobj),seqlevel50)]
#   wimobjout50 <-rbind(wimobjout50,  wimobj50)
# 
#   wimobj10 <- wimobj[seq(1, length(wimobj),seqlevel10)]
#   wimobjout10 <-rbind(wimobjout10,  wimobj10)
#   
#   wimobj5 <- wimobj[seq(1, length(wimobj),seqlevel5)]
#   wimobjout5 <-rbind(wimobjout5,  wimobj5)
#   
#   wimobj2 <- wimobj[seq(1, length(wimobj),seqlevel2)]
#   wimobjout2 <-rbind(wimobjout2,  wimobj2)
# }
# row.names(wimobjout1 ) <- NULL
# row.names(wimobjout50 ) <- NULL 
# row.names(wimobjout10 ) <- NULL 
# row.names(wimobjout5) <- NULL 
# row.names(wimobjout2) <- NULL 

#write.table( wimobjout10, "./DataIrvine/ProcessedData/wimobjout10_Oct02.txt", sep="\t")
# save(wimobjout1, file=" wimobjout1_Oct02.RData")
# save(wimobjout10, file=" wimobjout10_Oct02.RData")
# save(wimobjout5, file=" wimobjout5_Oct02.RData")
# save(wimobjout2, file=" wimobjout2_Oct02.RData")

save(wimobjout1, file=" wimobjout1_Mar20.RData")
save(wimobjout50, file=" wimobjout50_Mar20.RData")
save(wimobjout10, file=" wimobjout10_Mar20.RData")
save(wimobjout5, file=" wimobjout5_Mar20.RData")
save(wimobjout2, file=" wimobjout2_Mar20.RData")

############################################################################## start here when loading RData
### AFTER LOADING START HERE!!!
# run pnn
wim_obj_matrix <- diag(length(wimidx))
wim_obj <- c()
candi1 <- list()
candi2 <- list()
candi3 <- list()
candi4 <- list()
candi5 <- list()
candi6 <- list()
candi7 <- list()
candi8 <- list()


pnn_md <- data.frame()
candi1_unlist <- c()
candi2_unlist <- c()
candi3_unlist <- c()
candi4_unlist <- c()
candi5_unlist <- c()
candi6_unlist <- c()
candi7_unlist <- c()
candi8_unlist <- c()

find_pnnindex <- list()
find_pnn <- list()
find_pnn_order <- list()
len_find_pnn <- c()

# find index for pnn
for (w in 1: length(wimidx)){
  
  min_a_magdif = min(a_magdif[[w]])
  
  find_pnnindex[w] <- list( which( a_magdif[[w]] < min_a_magdif * 1.5 ))
  len_find_pnn[w] <- length(find_pnnindex[[w]])
  find_pnn_order[[w]] <-  rank(a_magdif[[w]])
  

}


seqlevel <- seqlevel2 # decide seqlevel
wimobjout <- wimobjout2
wimidxcls <- 20
sigma <- 1
Target_pnnanalysis_Oct02_resultpnn  <- table(c(0,0)) # Oct 02
Target_pnnanalysis_Mar20_resultpnn  <- table(c(0,0)) # Mar 20


 for (w in 1: length(wimidx)){ 

      
    if (len_find_pnn[w] > 1) {
    
      wim_obj <-(wim_obj_matrix[,w])
      
      if (w < (  (length(wimidx))-wimidxcls   )){
          #pnn_md <- data.frame(wim_obj, wimobjout[1:length(wimidx),])
          pnn_md <- data.frame(wim_obj[w:(w+wimidxcls)], wimobjout[w:(w+wimidxcls),])
      }
      
      else {
        pnn_md <- data.frame(wim_obj[(w-wimidxcls):w], wimobjout[(w-wimidxcls):w,])
      }
      
      
       
          nn <- learn( pnn_md, category.column=1)
          nn <- smooth(nn, sigma)
      
        
        if (len_find_pnn[w]==2) { 
          candi1_unlist <- unlist(candi_1[w])
          candi1_unlist <-  candi1_unlist[1001:2000]
          candi1_unlist <- candi1_unlist[seq(1, length(candi1_unlist),seqlevel)]         
          candi1[[w]] <- f.pnn(seqlevel, nn, candi1_unlist)
          
          candi2_unlist <- unlist(candi_2[w])
          candi2_unlist <-  candi2_unlist[1001:2000]
          candi2_unlist <- candi2_unlist[seq(1, length(candi2_unlist),seqlevel)]
          candi2[[w]] <- f.pnn(seqlevel, nn, candi2_unlist)
        }
        
        
        if (len_find_pnn[w]==3) { 
          candi1_unlist <- unlist(candi_1[w])
          candi1_unlist <-  candi1_unlist[1001:2000]
          candi1_unlist <- candi1_unlist[seq(1, length(candi1_unlist),seqlevel)]
          candi1[[w]] <- f.pnn(seqlevel, nn, candi1_unlist)
          
          candi2_unlist <- unlist(candi_2[w])
          candi2_unlist <-  candi2_unlist[1001:2000]
          candi2_unlist <- candi2_unlist[seq(1, length(candi2_unlist),seqlevel)]
          candi2[[w]] <- f.pnn(seqlevel, nn, candi2_unlist)
          
          candi3_unlist <- unlist(candi_3[w])
          candi3_unlist <-  candi3_unlist[1001:2000]
          candi3_unlist <- candi3_unlist[seq(1, length(candi3_unlist),seqlevel)]
          candi3[[w]] <- f.pnn(seqlevel, nn, candi3_unlist)
        }
    
      if (len_find_pnn[w]==4){ 
        candi1_unlist <- unlist(candi_1[w])
        candi1_unlist <-  candi1_unlist[1001:2000]
        candi1_unlist <- candi1_unlist[seq(1, length(candi1_unlist),seqlevel)]
        candi1[[w]] <- f.pnn(seqlevel, nn, candi1_unlist)
        
        candi2_unlist <- unlist(candi_2[w])
        candi2_unlist <-  candi2_unlist[1001:2000]
        candi2_unlist <- candi2_unlist[seq(1, length(candi2_unlist),seqlevel)]
        candi2[[w]] <- f.pnn(seqlevel, nn, candi2_unlist)
        
        candi3_unlist <- unlist(candi_3[w])
        candi3_unlist <-  candi3_unlist[1001:2000]
        candi3_unlist <- candi3_unlist[seq(1, length(candi3_unlist),seqlevel)]
        candi3[[w]] <- f.pnn(seqlevel, nn, candi3_unlist)
  
        candi4_unlist <- unlist(candi_4[w])
        candi4_unlist <-  candi4_unlist[1001:2000]
        candi4_unlist <- candi4_unlist[seq(1, length(candi4_unlist),seqlevel)]
        candi4[[w]] <- f.pnn(seqlevel, nn, candi4_unlist)
      }

    if (len_find_pnn[w]==5) { 
      candi1_unlist <- unlist(candi_1[w])
      candi1_unlist <-  candi1_unlist[1001:2000]
      candi1_unlist <- candi1_unlist[seq(1, length(candi1_unlist),seqlevel)]
      candi1[[w]] <- f.pnn(seqlevel, nn, candi1_unlist)
      
      candi2_unlist <- unlist(candi_2[w])
      candi2_unlist <-  candi2_unlist[1001:2000]
      candi2_unlist <- candi2_unlist[seq(1, length(candi2_unlist),seqlevel)]
      candi2[[w]] <- f.pnn(seqlevel, nn, candi2_unlist)
      
      candi3_unlist <- unlist(candi_3[w])
      candi3_unlist <-  candi3_unlist[1001:2000]
      candi3_unlist <- candi3_unlist[seq(1, length(candi3_unlist),seqlevel)]
      candi3[[w]] <- f.pnn(seqlevel, nn, candi3_unlist)
      
      candi4_unlist <- unlist(candi_4[w])
      candi4_unlist <-  candi4_unlist[1001:2000]
      candi4_unlist <- candi4_unlist[seq(1, length(candi4_unlist),seqlevel)]
      candi4[[w]] <- f.pnn(seqlevel, nn, candi4_unlist)
      
      candi5_unlist <- unlist(candi_5[w])
      candi5_unlist <-  candi5_unlist[1001:2000]
      candi5_unlist <- candi5_unlist[seq(1, length(candi5_unlist),seqlevel)]
      candi5[[w]] <- f.pnn(seqlevel, nn, candi5_unlist)
    }

    if (len_find_pnn[w]==6) { 
      candi1_unlist <- unlist(candi_1[w])
      candi1_unlist <-  candi1_unlist[1001:2000]
      candi1_unlist <- candi1_unlist[seq(1, length(candi1_unlist),seqlevel)]
      candi1[[w]] <- f.pnn(seqlevel, nn, candi1_unlist)
      
      candi2_unlist <- unlist(candi_2[w])
      candi2_unlist <-  candi2_unlist[1001:2000]
      candi2_unlist <- candi2_unlist[seq(1, length(candi2_unlist),seqlevel)]
      candi2[[w]] <- f.pnn(seqlevel, nn, candi2_unlist)
      
      candi3_unlist <- unlist(candi_3[w])
      candi3_unlist <-  candi3_unlist[1001:2000]
      candi3_unlist <- candi3_unlist[seq(1, length(candi3_unlist),seqlevel)]
      candi3[[w]] <- f.pnn(seqlevel, nn, candi3_unlist)
      
      candi4_unlist <- unlist(candi_4[w])
      candi4_unlist <-  candi4_unlist[1001:2000]
      candi4_unlist <- candi4_unlist[seq(1, length(candi4_unlist),seqlevel)]
      candi4[[w]] <- f.pnn(seqlevel, nn, candi4_unlist)
      
      candi5_unlist <- unlist(candi_5[w])
      candi5_unlist <-  candi5_unlist[1001:2000]
      candi5_unlist <- candi5_unlist[seq(1, length(candi5_unlist),seqlevel)]
      candi5[[w]] <- f.pnn(seqlevel, nn, candi5_unlist)
      
      candi6_unlist <- unlist(candi_6[w])
      candi6_unlist <-  candi6_unlist[1001:2000]
      candi6_unlist <- candi6_unlist[seq(1, length(candi6_unlist),seqlevel)]
      candi6[[w]] <- f.pnn(seqlevel, nn, candi6_unlist)
    }
    
    
    if (len_find_pnn[w]==7) { 
      candi1_unlist <- unlist(candi_1[w])
      candi1_unlist <-  candi1_unlist[1001:2000]
      candi1_unlist <- candi1_unlist[seq(1, length(candi1_unlist),seqlevel)]
      candi1[[w]] <- f.pnn(seqlevel, nn, candi1_unlist)
      
      candi2_unlist <- unlist(candi_2[w])
      candi2_unlist <-  candi2_unlist[1001:2000]
      candi2_unlist <- candi2_unlist[seq(1, length(candi2_unlist),seqlevel)]
      candi2[[w]] <- f.pnn(seqlevel, nn, candi2_unlist)
      
      candi3_unlist <- unlist(candi_3[w])
      candi3_unlist <-  candi3_unlist[1001:2000]
      candi3_unlist <- candi3_unlist[seq(1, length(candi3_unlist),seqlevel)]
      candi3[[w]] <- f.pnn(seqlevel, nn, candi3_unlist)
      
      candi4_unlist <- unlist(candi_4[w])
      candi4_unlist <-  candi4_unlist[1001:2000]
      candi4_unlist <- candi4_unlist[seq(1, length(candi4_unlist),seqlevel)]
      candi4[[w]] <- f.pnn(seqlevel, nn, candi4_unlist)
      
      candi5_unlist <- unlist(candi_5[w])
      candi5_unlist <-  candi5_unlist[1001:2000]
      candi5_unlist <- candi5_unlist[seq(1, length(candi5_unlist),seqlevel)]
      candi5[[w]] <- f.pnn(seqlevel, nn, candi5_unlist)
      
      candi6_unlist <- unlist(candi_6[w])
      candi6_unlist <-  candi6_unlist[1001:2000]
      candi6_unlist <- candi6_unlist[seq(1, length(candi6_unlist),seqlevel)]
      candi6[[w]] <- f.pnn(seqlevel, nn, candi6_unlist)
      
      candi7_unlist <- unlist(candi_7[w])
      candi7_unlist <-  candi7_unlist[1001:2000]
      candi7_unlist <- candi7_unlist[seq(1, length(candi7_unlist),seqlevel)]
      candi7[[w]] <- f.pnn(seqlevel, nn, candi7_unlist)
    }
    
    if (len_find_pnn[w]==8) { 
      candi1_unlist <- unlist(candi_1[w])
      candi1_unlist <-  candi1_unlist[1001:2000]
      candi1_unlist <- candi1_unlist[seq(1, length(candi1_unlist),seqlevel)]
      candi1[[w]] <- f.pnn(seqlevel, nn, candi1_unlist)
      
      candi2_unlist <- unlist(candi_2[w])
      candi2_unlist <-  candi2_unlist[1001:2000]
      candi2_unlist <- candi2_unlist[seq(1, length(candi2_unlist),seqlevel)]
      candi2[[w]] <- f.pnn(seqlevel, nn, candi2_unlist)
      
      candi3_unlist <- unlist(candi_3[w])
      candi3_unlist <-  candi3_unlist[1001:2000]
      candi3_unlist <- candi3_unlist[seq(1, length(candi3_unlist),seqlevel)]
      candi3[[w]] <- f.pnn(seqlevel, nn, candi3_unlist)
      
      candi4_unlist <- unlist(candi_4[w])
      candi4_unlist <-  candi4_unlist[1001:2000]
      candi4_unlist <- candi4_unlist[seq(1, length(candi4_unlist),seqlevel)]
      candi4[[w]] <- f.pnn(seqlevel, nn, candi4_unlist)
      
      candi5_unlist <- unlist(candi_5[w])
      candi5_unlist <-  candi5_unlist[1001:2000]
      candi5_unlist <- candi5_unlist[seq(1, length(candi5_unlist),seqlevel)]
      candi5[[w]] <- f.pnn(seqlevel, nn, candi5_unlist)
      
      candi6_unlist <- unlist(candi_6[w])
      candi6_unlist <-  candi6_unlist[1001:2000]
      candi6_unlist <- candi6_unlist[seq(1, length(candi6_unlist),seqlevel)]
      candi6[[w]] <- f.pnn(seqlevel, nn, candi6_unlist)
      
      candi7_unlist <- unlist(candi_7[w])
      candi7_unlist <-  candi7_unlist[1001:2000]
      candi7_unlist <- candi7_unlist[seq(1, length(candi7_unlist),seqlevel)]
      candi7[[w]] <- f.pnn(seqlevel, nn, candi7_unlist)
      
      candi8_unlist <- unlist(candi_8[w])
      candi8_unlist <-  candi8_unlist[1001:2000]
      candi8_unlist <- candi8_unlist[seq(1, length(candi8_unlist),seqlevel)]
      candi8[[w]] <- f.pnn(seqlevel, nn, candi8_unlist)
    }

}}

candi1_idx <- matrix(, length(wimidx), 3)
candi2_idx <- matrix(, length(wimidx), 3)
candi3_idx <- matrix(, length(wimidx), 3)
candi4_idx <- matrix(, length(wimidx), 3)
candi5_idx <- matrix(, length(wimidx), 3)
candi6_idx <- matrix(, length(wimidx), 3)
candi7_idx <- matrix(, length(wimidx), 3)
candi8_idx <- matrix(, length(wimidx), 3)

j <- 0
k <- 0
l <- 0
m <- 0
n <- 0
p <- 0
q <- 0
r <- 0

for (i in 1: length(candi1)){
  if (length(candi1[[i]]) == 2 ){
    j<- j+1
    candi1_idx[j,1] <- i
    prob <- unlist(candi1[[i]][2])
    candi1_idx[j,2] <- as.numeric(prob[[1]][1])
    candi1_idx[j,3] <- prob[[2]][1]
  }
}

for (i in 1: length(candi2)){
  if (length(candi2[[i]]) == 2 ){
    k<- k+1
    candi2_idx[k,1] <- i
    prob <- unlist(candi2[[i]][2])
    candi2_idx[k,2] <- as.numeric(prob[[1]][1])
    candi2_idx[k,3] <- prob[[2]][1]
  }
}

for (i in 1: length(candi3)){
  if (length(candi3[[i]]) == 2 ){
    l<- l+1
    candi3_idx[l,1] <- i
    prob <- unlist(candi3[[i]][2])
    candi3_idx[l,2] <- as.numeric(prob[[1]][1])
    candi3_idx[l,3] <- prob[[2]][1]
  }
}

for (i in 1: length(candi4)){
  if (length(candi4[[i]]) == 2 ){
    m<- m+1
    candi4_idx[m,1] <- i
    prob <- unlist(candi4[[i]][2])
    candi4_idx[m,2] <- as.numeric(prob[[1]][1])
    candi4_idx[m,3] <- prob[[2]][1]
  }
}

for (i in 1: length(candi5)){
  if (length(candi5[[i]]) == 2 ){
    n<- n+1
    candi5_idx[n,1] <- i
    prob <- unlist(candi5[[i]][2])
    candi5_idx[n,2] <- as.numeric(prob[[1]][1])
    candi5_idx[n,3] <- prob[[2]][1]
  }
}

for (i in 1: length(candi6)){
  if (length(candi6[[i]]) == 2 ){
    p<- p+1
    candi6_idx[p,1] <- i
    prob <- unlist(candi6[[i]][2])
    candi6_idx[p,2] <- as.numeric(prob[[1]][1])
    candi6_idx[p,3] <- prob[[2]][1]
  }
}

for (i in 1: length(candi7)){
  if (length(candi7[[i]]) == 2 ){
    q<- q+1
    candi7_idx[q,1] <- i
    prob <- unlist(candi7[[i]][2])
    candi7_idx[q,2] <- as.numeric(prob[[1]][1])
    candi7_idx[q,3] <- prob[[2]][1]
  }
}

for (i in 1: length(candi8)){
  if (length(candi8[[i]]) == 2 ){
    r<- r+1
    candi8_idx[r,1] <- i
    prob <- unlist(candi8[[i]][2])
    candi8_idx[r,2] <- as.numeric(prob[[1]][1])
    candi8_idx[r,3] <- prob[[2]][1]
  }
}


### candi_all_idx

candi_all_idx <- matrix (, sum(!is.na(candi1_idx[,1])), 10)
candi_all_idx[,1] <- candi1_idx[1:sum(!is.na(candi1_idx[,1])) ,1]
candi_all_idx[,2] <- candi1_idx[,3] [match (candi_all_idx[,1],  candi1_idx[1:sum(!is.na(candi1_idx[,1])),1]) ]
candi_all_idx[,3] <- candi2_idx[,3] [match (candi_all_idx[,1],  candi2_idx[1:sum(!is.na(candi1_idx[,1])),1]) ]
candi_all_idx[,4] <- candi3_idx[,3] [match (candi_all_idx[,1],  candi3_idx[1:sum(!is.na(candi1_idx[,1])),1]) ]
candi_all_idx[,5] <- candi4_idx[,3] [match (candi_all_idx[,1],  candi4_idx[1:sum(!is.na(candi1_idx[,1])),1]) ]
candi_all_idx[,6] <- candi5_idx[,3] [match (candi_all_idx[,1],  candi5_idx[1:sum(!is.na(candi1_idx[,1])),1]) ]
candi_all_idx[,7] <- candi6_idx[,3] [match (candi_all_idx[,1],  candi6_idx[1:sum(!is.na(candi1_idx[,1])),1]) ]
candi_all_idx[,8] <- candi7_idx[,3] [match (candi_all_idx[,1],  candi7_idx[1:sum(!is.na(candi1_idx[,1])),1]) ]
candi_all_idx[,9] <- candi8_idx[,3] [match (candi_all_idx[,1],  candi8_idx[1:sum(!is.na(candi1_idx[,1])),1]) ]

for (i in 1:length(candi_all_idx[,1])){
  candi_all_idx[i,10] <- c(0)
  
  if (!is.na (max (candi_all_idx[i,2:9]) )) {
    candi_all_idx[i,10] <- which.max (candi_all_idx[i,2:9])
  }
  else {
  
    candi_all_idx[i,10] <- c(1)
  }
}

pnn_vdsid <- ss_vdsid
for (i in 1:length(candi_all_idx[,1])){
  if (candi_all_idx[i,10] > 1) {
    a<- which( unlist( find_pnn_order[candi_all_idx[[i,1]] ] ) %in%  (candi_all_idx[i,10])   )
    pnn_vdsid[candi_all_idx[i,1]] <- vdssiglist[[candi_all_idx[i,1]]][a]  
  }
}

# Oct 02
Target_pnnanalysis_Oct02 <- (cbind(wimtarget, base_vdsid, ss_vdsid, pnn_vdsid))
colnames(Target_pnnanalysis_Oct02) <- c("wimid", "basevdsid", "ssvdsid", "pnnvdsid")

Target_pnnanalysis_Oct02_resultpnn <- table(Target_baseanalysis_Oct02_obj  == pnn_vdsid)
Target_pnnanalysis_Oct02_table <- cbind(Target_pnnanalysis_Oct02, Target_baseanalysis_Oct02_obj)

View(candi_all_idx)
utils::View(Target_pnnanalysis_Oct02_resultpnn)

save(Target_pnnanalysis_Oct02_table, file="./Results/Oct02/Target_pnn_Oct02.RData")
save.image("./Results/Oct02/FindTarget_Oct02.RData")
write.table(Target_pnnanalysis_Oct02_table, "./Results/Oct02/Target_pnn_Oct02.txt", sep="\t")

# Mar 20
Target_pnnanalysis_Mar20 <- (cbind(wimtarget, base_vdsid, ss_vdsid, pnn_vdsid))
colnames(Target_pnnanalysis_Mar20) <- c("wimid", "basevdsid", "ssvdsid", "pnnvdsid")

Target_pnnanalysis_Mar20_resultpnn <- table(Target_baseanalysis_Mar20_obj  == pnn_vdsid)
Target_pnnanalysis_Mar20_table <- cbind(Target_pnnanalysis_Mar20, Target_baseanalysis_Mar20_obj)

View(candi_all_idx)
utils::View(Target_pnnanalysis_Mar20_resultpnn)

save(Target_pnnanalysis_Mar20_table, file="./Results/Mar20/Target_pnn_Mar20.RData")
save.image("./Results/Mar20/FindTarget_Mar20.RData")
write.table(Target_pnnanalysis_Mar20_table, "./Results/Mar20/Target_pnn_Mar20.txt", sep="\t")

###end






### PCA
obj1<- princomp(wimout[1,])
obj2<- princomp(wimout[2,])

test2_1 <- approx(test2_1, n=999)
test2_2 <- approx(test2_2, n=999)
test2_3 <- approx(test2_3, n=999)

prcomp(X1)
princomp(X2)

### Plot

no=754
wimplot <- f.drawwimsignature (no)

a_basemagdif[38]
a_magdif[38]

n1=18
n2=3
vdsplot <- f.drawvdssignature (n1,n2)

n1=36
n2=3
vdsplot <- f.drawvdssignature (n1,n2)

temp <- unlist(candi_1[[77]])

plot( temp[1:1000], temp[1001:2000])

write.table(candi_2[[55]], "./DataIrvine/ProcessedData/test2.txt", sep="\t")
