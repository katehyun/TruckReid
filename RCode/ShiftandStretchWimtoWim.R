#utils:::menuInstallPkgs() 
#library(gtools)
#library(plyr)
#rm(list=ls())

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/08082014Jan0910.RData") 

#setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
#setwd("C:/Users/Kyung Hyun/Dropbox/Kate/ReID/TruckReid") 
#### loading functionbook2
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData")


### Input ready 
num=1000 
no_round = 1000




base_magdif <- c()
magdif <- c()
magdif2 <- c()
a_magdif <- list()
ss<- list()

Up_stret <-list()
Up_shift <-list()


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



swift_coeff = seq (-0.20, 0.20, by=0.0010)
stret_coeff = seq ( 0.80, 1.20, by=0.001)


UpheaderID <- Upheader_new$sigid


#Down

for (w in 1:length(Downheader_ID)){

  splineDown <- Downobjout [w,]


    if (length(Upsiglist[[w]]) < 1 ) { 
      
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
  
        
        #Up 
        
        magdif2 <- c()
        base_magdif <- c()
        
         for (q in 1: length(Upsiglist[[w]])){
  
            
            min_stretmagdif <- c()
            splineUpidx <- match (Upsiglist[[w]][q] , UpheaderID)
            splineUp <- Upobjout[splineUpidx,]
            
            base_magdif[q] = sum (abs (splineDown - splineUp))
      
            
            # first iteration
            swift <- f.swift ( splineUp , splineDown, swift_coeff, num , no_round )
            stret <- f.stret (swift$matrix, splineDown , stret_coeff, num , no_round)
            
            # start iteration
            
            min_stretmagdif = stret$mv
            min_swiftmagdif = swift$mv
            
            if ((min_stretmagdif - min_swiftmagdif ) < 0.1 ){
              
              Up_stret <- stret$matrix
            }
            
            else {
              
              while (min_swiftmagdif - min_stretmagdif > 0.1) {
                
                swift <- f.swift (stret$matrix, swift_coeff, num , no_round)
                stret <- f.stret (swift$matrix, stret_coeff, num , no_round)
                
                Up_swift <- swift$matrix
                min_swiftmagdif <- swift$mv
                Up_stret <- stret$matrix
                min_stretmagdif <- stret$mv
                
              } 
            }
            
            magdif2[q] <- (c(min_stretmagdif))  
            
            ss[q] <- list(Up_stret)
            
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


#write.table(candi_1[[1]], "./ProcessedData/TestCode/candi1.txt", sep="\t")



save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/shiftandstretch_Jan0910.RData")

##############################################################end


