#utils:::menuInstallPkgs() 
#library(gtools)
#library(plyr)
rm(list=ls())
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Oct02/06302014Oct02.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Mar20/07082014Mar20.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/tw1.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/07162014Jan0910.RData") 

setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine") 
setwd("C:/Users/Kyung Hyun/Dropbox/Kate/ReID/DataIrvine") 
#### loading functionbook2

# Oct 02
rm(Irvine.VDSOct02ML4sig,Irvine.VDSOct02ML5sig, Irvine.WIMOct02ML4sig, Irvine.WIMOct02ML5sig, 
   lb, ub, ud, ld, setduration, settime)
   
rm(Irvine.VDSOct02ML4Header)
rm(Irvine.VDSOct02ML5Header)
rm(Irvine.WIMOct02ML4Header)
rm(Irvine.WIMOct02ML5Header)

# Mar 20
rm(Irvine.VDSMar20ML4sig1,Irvine.VDSMar20ML4sig2, Irvine.VDSMar20ML5sig1, Irvine.VDSMar20ML5sig2,
   Irvine.WIMMar20ML4sig, Irvine.WIMMar20ML5sig, 
   lb, ub, ud, ld, setduration, settime)

rm(Irvine.VDSMar20ML4Header1, Irvine.VDSMar20ML4Header2)
rm(Irvine.VDSMar20ML5Header1, Irvine.VDSMar20ML5Header2)
rm(Irvine.WIMMar20ML4Header)
rm(Irvine.WIMMar20ML5Header)

# Jan 0910
rm(LC.Jan09ML3Header1, LC.Jan09ML3Header2, LC.Jan09ML4Header1, LC.Jan09ML4Header2, LC.Jan10ML3Header1,LC.Jan10ML3Header2)
matching <- matching_SOLC



### Input ready 
num=1000 #1000
no_round = 1000

wimobj <- c()
wimobjout <- c()

matching <-format(matching, scientific = FALSE) 
base_magdif <- c()
magdif <- c()
magdif2 <- c()
a_magdif <- list()
ss<- list()

vds_stret <-list()
vds_shift <-list()


rank1 <- list()
rank2 <- list()
rank3 <- list()
rank4 <- list()
rank5 <- list()
rank6 <- list()
rank7 <- list()
rank8 <- list()

candi_1 <- list()
candi_2 <- list()
candi_3 <- list()
candi_4 <- list()
candi_5 <- list()
candi_6 <- list()
candi_7 <- list()
candi_8 <- list()
candi_magdif <- list()
a_basemagdif <- list()

vds_swift_coeff = seq (-0.05, 0.05, by=0.0010)
vds_stret_coeff = seq ( 0.95, 1.05, by=0.010)

#wim

#for (w in 1:length(wimidx)){
#for (w in 1:3){  # for testing

w=9
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
    #write.table(inwimsig, "./ProcessedData/TestCode/inwimsig.txt", sep="\t")
    #write.table(splinewim, "./ProcessedData/TestCode/splinewim.txt", sep="\t")
    
    wimobj <- c(splinewim[,2])
    wimobj <- t(wimobj)
    wimobjout <-rbind(wimobjout,  wimobj)
    
    if (length(vdsidx[[w]]) < 1 ) { 
      
      
      a_magdif[w] <- list(c(99999))
      a_basemagdif[w] <- list(c(99999))
      
      
      candi_1[w] <- list(c(99999))
      candi_2[w] <- list(c(99999))
      candi_3[w] <- list(c(99999))
      candi_4[w] <- list(c(99999))
      candi_5[w] <- list(c(99999))
      candi_6[w] <- list(c(99999))
      candi_7[w] <- list(c(99999))
      candi_8[w] <- list(c(99999))
    }
     
    
    else {
  

              #vds 
              
              magdif2 <- c()
              base_magdif <- c()
              
              for (q in 1: length(vdsidx[[w]])){
              q=1
                
                    min_stretmagdif <- c()
                
                    if (is.na(vdsidx[[w]][q])) { 
                      magdif2[q] <- NA
                      ss[q] <- NA
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
                    write.table(splinevds, "./ProcessedData/TestCode/splinevds.txt", sep="\t")
                    
                    base_magdif[q] = sum (abs (splinewim - splinevds))
                  
                    
                    # first iteration
                    swift <- f.swift (splinevds, vds_swift_coeff, num , no_round )
                    stret <- f.stret (swift$matrix, vds_stret_coeff, num , no_round)
                    
                    # start iteration
                    
                    min_stretmagdif = stret$mv
                    min_swiftmagdif = swift$mv
                    
                    if ((min_stretmagdif - min_swiftmagdif ) < 0.5 ){
                      
                      vds_stret <- stret$matrix
                    }
                    
                    else {
                 
                          while (min_swiftmagdif - min_stretmagdif > 0.5) {
                            
                            swift <- f.swift (stret$matrix, vds_swift_coeff, num , no_round)
                            stret <- f.stret (swift$matrix, vds_stret_coeff, num , no_round)
                            
                            vds_swift <- swift$matrix
                            min_swiftmagdif <- swift$mv
                            vds_stret <- stret$matrix
                            min_stretmagdif <- stret$mv
                                                       
                          } 
                    }
                    
                    magdif2[q] <- (c(min_stretmagdif))  
                             
                    ss[q] <- list(vds_stret)
                    
                    rank_magdif2=rank(magdif2)
                    
                    rank1= ss[which(rank_magdif2 %in% c(1))]
                    rank2= ss[which(rank_magdif2 %in% c(2))]
                    rank3= ss[which(rank_magdif2 %in% c(3))]
                    rank4= ss[which(rank_magdif2 %in% c(4))]
                    rank5= ss[which(rank_magdif2 %in% c(5))]
                    rank6= ss[which(rank_magdif2 %in% c(6))]
                    rank7= ss[which(rank_magdif2 %in% c(7))]
                    rank8= ss[which(rank_magdif2 %in% c(8))]
               
                  }
              }
        a_magdif[w] <- list(magdif2)
        a_basemagdif[w] <- list(base_magdif)
    
       
       
        # candidate 
        candi_magdif <- rbind(candi_magdif, magdif)
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


write.table(candi_1[[1]], "./ProcessedData/TestCode/candi1.txt", sep="\t")
# min magdif
min_a_magdif<-vector()
for (i in 1: length(a_magdif)){
  min_a_magdif[i] <- min(a_magdif[[i]])
}

# save(wimobjout, file=" wimobjout_Mar20.RData")
# save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Mar20/shiftandstretch_Mar20.RData")

# save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/shiftandstretch_Oct02.RData")
# save(wimobjout, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/wimobjout_Mar20_ta1.RData")
# save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/SensitivityAnalysis/shiftandstretch_Mar20_ta1.RData")

save(wimobjout, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/wimobjout_Jan0910.RData")
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/ProcessedData/Jan0910/shiftandstretch_Jan0910.RData")

 ##############################################################end







#normalization
#inwimsig <- f.normalization(inwimsig)
#invdssig <- f.normalization(invdssig)
#utils::View(invdssig)

# interpolation
# num=1000; 
# 
# splinewim <- f.interpolation(inwimsig,num,no)
# splinevds <- f.interpolation(invdssig,num,no)
# colnames(splinevds) <- c("outvdstime", "outvdsmag")
# colnames(splinewim) <- c("outwimtime", "outwimmag")
# 
# utils::View(splinewim)
# utils::View(splinevds)

# swift
# 
# 1. before the transformation
# base_Magdif = sum (abs (splinewim - splinevds))


# 2. swift and stretch
# rm(vds_swift)
# rm(vds_stret)
# FUNCTION
# vds_swift_coeff = seq (-0.1, 0.1, by=0.0010)
# vds_stret_coeff = seq ( 0.8, 1.2, by=0.010)



# first iteration
# swift <- f.swift (splinevds, vds_swift_coeff, num )
# stret <- f.stret (swift$matrix, vds_stret_coeff, num )

# start iteration
# min_stretmagdif = stret$mv
# min_swiftmagdif = swift$mv
# 
# k = 1
# while (min_swiftmagdif - min_stretmagdif > 0.1) {
#   
#   swift <- f.swift (stret$matrix, vds_swift_coeff, num )
#   stret <- f.stret (swift$matrix, vds_stret_coeff, num )
#   
#   vds_swift <- swift$matrix
#   min_swiftmagdif <- swift$mv
#   vds_stret <- stret$matrix
#   min_stretmagdif <- stret$mv
#   k <- k+1
#   
# } 


#####################################################################################################end





# normalization
#inwimsig <- transform (inwimsig, newwimtime=inwimsig[,1] / inwimsig[nrow(inwimsig),1],
#                    newwimsig= inwimsig[,2] / (max(inwimsig[,2])))

#invdssig <- transform (invdssig, newvdstime=invdssig[,1] / invdssig[nrow(invdssig),1],
#                       newvdssig= invdssig[,2] / (max(invdssig[,2])))

