######################################################################
# After testing, should make function source files for each function #
######################################################################

### Import data
rm(list=ls()) # clear datalist
options(scipen=999) # disable scientic option

setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine") 
Irvine.GTM=read.table("./RawData/IrvineALLGTMasterlist.txt", header=T, fill=T)
Irvine.WIMRec=read.table("./RawData/IrvineALLWIM.txt", header=T, fill=T)

siglink=read.table("./RawData/signaturelink.txt", header=T, fill=T)
wimsiglink=read.table("./RawData/wimsignaturelink.txt", header=T, fill=T)
matching=merge(siglink, wimsiglink, by="vehid")
########################### Matching ###################################
#### import matching id - Oct 02

siglink_Oct02 <- subset (siglink, substr(siglink[,1],3,12) > 1349161200 & substr(siglink[,1],3,12) < 1349247600 )
wimsiglink_Oct02 <- subset (wimsiglink, substr(wimsiglink[,1],3,12) > 1349161200 & substr(wimsiglink[,1],3,12) < 1349247600 )
matching_Oct02 = merge (siglink_Oct02, wimsiglink_Oct02, by="vehid")

write.table(matching_Oct02, "./DataIrvine/ProcessedData/matching_Oct02.txt", sep="\t")

#### import matching id - Mar 20
siglink_Mar20 <- subset (siglink, substr(siglink[,1],3,12) > 1363762800 & substr(siglink[,1],3,12) < 1363849200 )
wimsiglink_Mar20 <- subset (wimsiglink, substr(wimsiglink[,1],3,12) > 1363762800 & substr(wimsiglink[,1],3,12) < 1363849200 )
matching_Mar20 = merge (siglink_Mar20, wimsiglink_Mar20, by="vehid")


write.table(matching_Mar20, "./ProcessedData/matching_Mar20.txt", sep="\t")

### import matching id - Jan 0910 (SO-LC)
matching_SOLC=read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine/RawData/LCJan/MatchingIDSOLC.txt")
colnames(matching_SOLC) <- c("SO", "LC")
matching_SOLC <- format(matching_SOLC, scientific=FALSE)

