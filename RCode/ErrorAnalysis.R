rm(errormag, error_mag, errorsum)

errormag <- data.frame()
maxerror <- c()
minerror <- c()
meanerror <- c()

for (i in 1: length(Target_baseanalysis_Jan0910_table[,1])){
  
  Downsigid <- as.numeric(Target_baseanalysis_Jan0910_table[i,4])
  Upsigid <- as.numeric(Target_baseanalysis_Jan0910_table[i,8])
  FHWAclass<- as.numeric( Target_baseanalysis_Jan0910_table[i,1])

  error_mag <-  f.ErrorMag (Upsigid, Downsigid)
  
        if ( (Target_baseanalysis_Jan0910_table[i,6] ==  Target_baseanalysis_Jan0910_table[i,7]) & 
               (Target_baseanalysis_Jan0910_table[i,7] != 999)){
          error_mag[3] <- 1 }
        else {
          error_mag[3] <- 2
      }

  maxerror[i] <- max(error_mag[5: length(error_mag)])
  minerror[i] <- min(error_mag[5: length(error_mag)])
  meanerror[i] <- mean(error_mag[5: length(error_mag)])
  errormag <- rbind( errormag, error_mag)
  
}

FHWAclass<- as.numeric( Target_baseanalysis_Jan0910_table[,1])
errormag <- cbind(FHWAclass, errormag)

colnames(errormag)[1] <- c("FHWAclass")
colnames(errormag)[2] <- c("Downsigid")
colnames(errormag)[3] <- c("Upsigid")
colnames(errormag)[4] <- c("index")


rm(errorsum)
errorsum <- cbind( errormag$Downsigid, errormag$Upsigid, Target_baseanalysis_Jan0910_table[,1], errormag$index, rowSums(errormag[5:length(errormag[1,])]) , 
                   maxerror, minerror, meanerror )

colnames(errorsum)[1] <- c("Downsigid")
colnames(errorsum)[2] <- c("Upsigid")
colnames(errorsum)[3] <- c("FHWAClass")
colnames(errorsum)[4] <- c("index")
colnames(errorsum)[5] <- c("AUC")

wimsigbodytype=read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/wimsigbodytype.txt", header=T, fill=T,sep="\t")
truckbodytype <- wimsigbodytype[match ( errormag[,2], wimsigbodytype$wimsigid), 8]
trailerbodytype <- wimsigbodytype[match ( errormag[,2], wimsigbodytype$wimsigid), 9]

errormag <- ()
# aggregated bodytype (agtruck, agtrailer)

trailerbodylookup = read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/trailerbodyconfiguration.txt", 
                               header=T, sep="," ,fill=T)
  
trailerbodygroup <- trailerbodylookup [ match ( trailerbodytype,  trailerbodylookup[,2]), 4]

errorsum <- cbind( errorsum , truckbodytype, trailerbodytype,trailerbodygroup)
errormag <- cbind( trailerbodygroup, errormag)

setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result/")
write.table(errorsum, "errorsum.txt", sep="\t", row.names=FALSE)



# class 9
library(graphics)
rm(samples9,matching_errormag,matching_errormag9)

matching_errormag <- errormag[errormag[,5] == 1,]
matching_errormag9 <- matching_errormag[matching_errormag[,2] == 9,]
samples_matching9 <- matching_errormag9 [,6:length(matching_errormag9 [1,])]
samples_matching9 <- na.omit(samples_matching9)


    # fit <- princomp (samples, cor=TRUE)
fit <- kmeans(samples_matching9 , 3) 
matchingsamples9_km  <- data.frame( fit$cluster,  matching_errormag9[,1], samples_matching9 )

# matching & nonamtching
rm(all_errormag9, all_samples9)
all_errormag9 <- errormag[errormag[,2] == 9,]


all_samples9<- all_errormag9 [,6:length(all_errormag9 [1,])]
all_samples9 <- cbind(all_errormag9[,1],all_errormag9[,5], all_samples9)
all_samples9  <- na.omit(all_samples9)


fit_all <- kmeans(all_samples9[,3: length(all_samples9[1,])], 6)
all_samples9_km <- data.frame( fit_all$cluster, all_samples9)
colnames (all_samples9_km) [1] <- c("clusterid")
colnames (all_samples9_km) [2] <- c("bodygroup")
colnames (all_samples9_km) [3] <- c("matchindex")
