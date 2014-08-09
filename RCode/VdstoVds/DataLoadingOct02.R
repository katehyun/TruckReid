################ Data Input- Oct 02 ####################
### Irvine Oct 02

Irvine.VDSOct02ML4Header=read.table("./RawData/VDSOct/ML41002/501_IST0001297_CA121002200404_fileIdx.txt",  fill=T)
Irvine.VDSOct02ML4sig=read.table("./RawData/VDSOct/ML41002/IST0001297_CA121002200404.txt", fill=T)
Irvine.VDSOct02ML5Header=read.table("./RawData/VDSOct/ML51002/501_IST0001210_CA121002200404_fileIdx.txt",  fill=T)
Irvine.VDSOct02ML5sig=read.table("./RawData/VDSOct/ML51002/IST0001210_CA121002200404.txt", fill=T)

Irvine.WIMOct02ML4Header=read.table("./RawData/WIMOct/ML41002/15fileIdx.txt", fill=T)
Irvine.WIMOct02ML4sig=read.table("./RawData/WIMOct/ML41002/IST0001030_CA121002203216.txt", fill=T)
Irvine.WIMOct02ML5Header=read.table("./RawData/WIMOct/ML51002/15fileIdx.txt",  fill=T)
Irvine.WIMOct02ML5sig=read.table("./RawData/WIMOct/ML51002/IST0001070_CA121002203217.txt", fill=T)
save.image("./RawData/IrvineRawOct02")


### col name for sig file
colnames(Irvine.VDSOct02ML4sig) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.VDSOct02ML5sig) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.WIMOct02ML4sig) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.WIMOct02ML5sig) <- c("id", "v2", "v3","v4","v5","v6")

Irvine.VDSOct02ML4sig <- subset(Irvine.VDSOct02ML4sig, select = c("id", "v2", "v3") )
Irvine.VDSOct02ML5sig <- subset(Irvine.VDSOct02ML5sig, select = c("id", "v2", "v3") )
Irvine.WIMOct02ML4sig <- subset(Irvine.WIMOct02ML4sig, select = c("id", "v2", "v3") )
Irvine.WIMOct02ML5sig <- subset(Irvine.WIMOct02ML5sig, select = c("id", "v2", "v3") )


### Add vehid in Header file 
# VDS Sep22 ML4
uctOct02 <- 1349161200
Irvine.VDSOct02ML4Header[,length(Irvine.VDSOct02ML4Header)+1]<- 1000*(uctOct02 + Irvine.VDSOct02ML4Header[,length(Irvine.VDSOct02ML4Header)])
#as.numeric(Irvine.VDSSep22ML4Header[2,12])

x<- rep(14,nrow(Irvine.VDSOct02ML4Header))
y<- Irvine.VDSOct02ML4Header[,ncol(Irvine.VDSOct02ML4Header)]
len <- length (Irvine.VDSOct02ML4Header)+1

for (i in 1:nrow(Irvine.VDSOct02ML4Header)){
  Irvine.VDSOct02ML4Header[i,len] <- paste (x[i],y[i], sep="")
}

colnames(Irvine.VDSOct02ML4Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

# VDS Sep22 ML5
Irvine.VDSOct02ML5Header[,length(Irvine.VDSOct02ML5Header)+1]<- 1000*(uctOct02 + Irvine.VDSOct02ML5Header[,length(Irvine.VDSOct02ML5Header)])

x<- rep(15,nrow(Irvine.VDSOct02ML5Header))
y<- Irvine.VDSOct02ML5Header[,ncol(Irvine.VDSOct02ML5Header)]
len <- length (Irvine.VDSOct02ML5Header)+1

for (i in 1:nrow(Irvine.VDSOct02ML5Header)){
  Irvine.VDSOct02ML5Header[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.VDSOct02ML5Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

# WIM Sep22 ML4
Irvine.WIMOct02ML4Header[,length(Irvine.WIMOct02ML4Header)+1]<- 
  1000*(uctOct02 + Irvine.WIMOct02ML4Header[,length(Irvine.WIMOct02ML4Header)])

x<- rep(24,nrow(Irvine.WIMOct02ML4Header))
y<- Irvine.WIMOct02ML4Header[,ncol(Irvine.WIMOct02ML4Header)]
len <- length (Irvine.WIMOct02ML4Header)+1

for (i in 1:nrow(Irvine.WIMOct02ML4Header)){
  Irvine.WIMOct02ML4Header[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.WIMOct02ML4Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","wimsigid")

# WIM Sep22 ML5
Irvine.WIMOct02ML5Header[,length(Irvine.WIMOct02ML5Header)+1]<- 
  1000*(uctOct02 + Irvine.WIMOct02ML5Header[,length(Irvine.WIMOct02ML5Header)])

x<- rep(25,nrow(Irvine.WIMOct02ML5Header))
y<- Irvine.WIMOct02ML5Header[,ncol(Irvine.WIMOct02ML5Header)]
len <- length (Irvine.WIMOct02ML5Header)+1

for (i in 1:nrow(Irvine.WIMOct02ML5Header)){
  Irvine.WIMOct02ML5Header[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.WIMOct02ML5Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","wimsigid")

### Add veh id in SIG file
#VDS
Irvine.VDSOct02ML4sig["vdssigid"] <- NA
Irvine.VDSOct02ML5sig["vdssigid"] <- NA

system.time({
  idx1=match(Irvine.VDSOct02ML4sig$id, Irvine.VDSOct02ML4Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.VDSOct02ML4sig$vdssigid[idx2] <- Irvine.VDSOct02ML4Header$vdssigid[idx1[idx2]]
})

system.time({
  idx1=match(Irvine.VDSOct02ML5sig$id, Irvine.VDSOct02ML5Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.VDSOct02ML5sig$vdssigid[idx2] <- Irvine.VDSOct02ML5Header$vdssigid[idx1[idx2]]
})

#WIM
Irvine.WIMOct02ML4sig["wimsigid"] <- NA
Irvine.WIMOct02ML5sig["wimsigid"] <- NA
system.time({
  idx1=match(Irvine.WIMOct02ML4sig$id, Irvine.WIMOct02ML4Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.WIMOct02ML4sig$wimsigid[idx2] <- Irvine.WIMOct02ML4Header$wimsigid[idx1[idx2]]
})

system.time({
  idx1=match(Irvine.WIMOct02ML5sig$id, Irvine.WIMOct02ML5Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.WIMOct02ML5sig$wimsigid[idx2] <- Irvine.WIMOct02ML5Header$wimsigid[idx1[idx2]]
})

### Merge into one file 
#Header file
Irvine.VDSOct02Header <- rbind(Irvine.VDSOct02ML4Header, Irvine.VDSOct02ML5Header)
Irvine.VDSOct02Header <- Irvine.VDSOct02Header[order(Irvine.VDSOct02Header[,12]),] 
colnames(Irvine.VDSOct02Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

Irvine.WIMOct02Header <- rbind(Irvine.WIMOct02ML4Header, Irvine.WIMOct02ML5Header)
Irvine.WIMOct02Header <- Irvine.WIMOct02Header[order(Irvine.WIMOct02Header[,12]),] 
colnames(Irvine.WIMOct02Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","wimsigid")

#SIG file
Irvine.VDSOct02sig <- rbind(Irvine.VDSOct02ML4sig, Irvine.VDSOct02ML5sig)
colnames(Irvine.VDSOct02sig) <- c("id", "mag", "v3","vdssigid")
Irvine.WIMOct02sig <- rbind(Irvine.WIMOct02ML4sig, Irvine.WIMOct02ML5sig)
colnames(Irvine.WIMOct02sig) <- c("id", "mag", "v3","wimsigid")

write.table(Irvine.VDSOct02Header, "./ProcessedData/Oct02/Irvine.VDSOct02Header.txt", sep="\t")
write.table(Irvine.WIMOct02Header, "./ProcessedData/Oct02/Irvine.WIMOct02Header.txt", sep="\t")
write.table(Irvine.VDSOct02sig, "./ProcessedData/Oct02/Irvine.VDSOct02sig.txt", sep="\t")
write.table(Irvine.WIMOct02sig, "./ProcessedData/Oct02/Irvine.WIMOct02sig.txt", sep="\t")
save.image("./ProcessedData/Oct02/IrvineLoadinOct02")
