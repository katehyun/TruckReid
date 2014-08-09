#utils:::menuInstallPkgs() 
#library(gtools)
#library(plyr)
rm(list=ls())
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
#load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Oct02/06302014Oct02.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Mar20/07272014Mar20.RData")
load("./ProcessedData/Mar20/vdsobjoutMar20.RData")
load("./ProcessedData/Mar20/wimobjoutMar20.RData")
load("./ProcessedData/Mar20/vdsheaderIDMar20.RData")

#sensitivity
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/SensitivityAnalysis/Mar20tw30.RData")
load("./ProcessedData/Mar20/vdsobjoutMar20.RData")
load("./ProcessedData/SensitivityAnalysis/wimobjout_tw.RData")
load("./ProcessedData/Mar20/vdsheaderIDMar20.RData")
# load function book


#setwd("C:/Users/Kyung Hyun/Dropbox/Kate/ReID/TruckReid") 
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
   lb, ud, ld, lp, up, setduration, settime)

rm(Irvine.VDSMar20ML4Header1, Irvine.VDSMar20ML4Header2)
rm(Irvine.VDSMar20ML5Header1, Irvine.VDSMar20ML5Header2)
rm(Irvine.WIMMar20ML4Header)
rm(Irvine.WIMMar20ML5Header)





### Input ready 
num=1000
no_round = 1000

wimobj <- c()
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

vds_swift_coeff = seq (-0.20, 0.20, by=0.0010)
vds_stret_coeff = seq ( 0.80, 1.20, by=0.001)

#wim

 for (w in 1:length(wimidx)){
  


    splinewim <-wimobjout [w,]
        
    if (length(vdssiglist[[w]]) < 1 ) { 
      
      
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
              
              for (q in 1: length(vdssiglist[[w]])){
                
           
                
                    min_stretmagdif <- c()
                                        
                    splinevdsidx <-  match (vdssiglist[[w]][q] , vdsheaderID)
                   
                    splinevds <- vdsobjout[splinevdsidx,]
                   
                   
                    base_magdif[q] = sum (abs (splinewim - splinevds))
                  
                    
                    # first iteration
                    swift <- f.swift (splinevds, splinewim, vds_swift_coeff, num , no_round )
                    stret <- f.stret (swift$matrix, splinewim, vds_stret_coeff, num , no_round)
                    
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
    

 #save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Mar20/shiftandstretch_Mar20.RData")

#save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Oct02/shiftandstretch_Oct02.RData")

 save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/SensitivityAnalysis/shiftandstretch_Mar20_ta30.RData")



 ##############################################################end



