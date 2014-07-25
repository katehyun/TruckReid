################ Data Input- Sep 22 ####################
### Irvine Sep 22
Irvine.VDSSep22ML4Header=read.table("./DataIrvine/RawData/VDSSep/ML40922/100fileIDx.txt",  fill=T)
Irvine.VDSSep22ML4sig=read.table("./DataIrvine/RawData/VDSSep/ML40922/IST0001297CA120922070000.txt", fill=T)
Irvine.VDSSep22ML5Header=read.table("./DataIrvine/RawData/VDSSep/ML50922/100fileIDx.txt",  fill=T)
Irvine.VDSSep22ML5sig=read.table("./DataIrvine/RawData/VDSSep/ML50922/IST0001210_CA120922070000.txt", fill=T)

Irvine.WIMSep22ML4Header=read.table("./DataIrvine/RawData/WIMSep/ML40922/100fileIDx.txt", fill=T)
Irvine.WIMSep22ML4sig=read.table("./DataIrvine/RawData/WIMSep/ML40922/IST0001070_CA120921172351.txt", fill=T)
Irvine.WIMSep22ML5Header=read.table("./DataIrvine/RawData/WIMSep/ML50922/100fileIDx.txt",  fill=T)
Irvine.WIMSep22ML5sig=read.table("./DataIrvine/RawData/WIMSep/ML50922/IST0001030_CA120921172351.txt", fill=T)
save.image("./DataIrvine/RawData/IrvineRawSep22")

### col name for sig file
colnames(Irvine.VDSSep22ML4sig) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.VDSSep22ML5sig) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.WIMSep22ML4sig) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.WIMSep22ML5sig) <- c("id", "v2", "v3","v4","v5","v6")


### Add vehid in Header file 
# VDS Sep22 ML4
uctSep22 <- 1348297200 
Irvine.VDSSep22ML4Header[,length(Irvine.VDSSep22ML4Header)+1]<- 1000*(uctSep22 + Irvine.VDSSep22ML4Header[,length(Irvine.VDSSep22ML4Header)])
#as.numeric(Irvine.VDSSep22ML4Header[2,12])

x<- rep(14,nrow(Irvine.VDSSep22ML4Header))
y<- Irvine.VDSSep22ML4Header[,ncol(Irvine.VDSSep22ML4Header)]
len <- length (Irvine.VDSSep22ML4Header)+1

for (i in 1:nrow(Irvine.VDSSep22ML4Header)){
  Irvine.VDSSep22ML4Header[i,len] <- paste (x[i],y[i], sep="")
}

colnames(Irvine.VDSSep22ML4Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

# VDS Sep22 ML5
Irvine.VDSSep22ML5Header[,length(Irvine.VDSSep22ML5Header)+1]<- 1000*(uctSep22 + Irvine.VDSSep22ML5Header[,length(Irvine.VDSSep22ML5Header)])

x<- rep(15,nrow(Irvine.VDSSep22ML5Header))
y<- Irvine.VDSSep22ML5Header[,ncol(Irvine.VDSSep22ML5Header)]
len <- length (Irvine.VDSSep22ML5Header)+1

for (i in 1:nrow(Irvine.VDSSep22ML5Header)){
  Irvine.VDSSep22ML5Header[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.VDSSep22ML5Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

# WIM Sep22 ML4
Irvine.WIMSep22ML4Header[,length(Irvine.WIMSep22ML4Header)+1]<- 
  1000*(uctSep22 + Irvine.WIMSep22ML4Header[,length(Irvine.WIMSep22ML4Header)])

x<- rep(24,nrow(Irvine.WIMSep22ML4Header))
y<- Irvine.WIMSep22ML4Header[,ncol(Irvine.WIMSep22ML4Header)]
len <- length (Irvine.WIMSep22ML4Header)+1

for (i in 1:nrow(Irvine.WIMSep22ML4Header)){
  Irvine.WIMSep22ML4Header[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.WIMSep22ML4Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","wimsigid")

# WIM Sep22 ML5
Irvine.WIMSep22ML5Header[,length(Irvine.WIMSep22ML5Header)+1]<- 1000*(uctSep22 + Irvine.WIMSep22ML5Header[,length(Irvine.WIMSep22ML5Header)])

x<- rep(25,nrow(Irvine.WIMSep22ML5Header))
y<- Irvine.WIMSep22ML5Header[,ncol(Irvine.WIMSep22ML5Header)]
len <- length (Irvine.WIMSep22ML5Header)+1

for (i in 1:nrow(Irvine.WIMSep22ML5Header)){
  Irvine.WIMSep22ML5Header[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.WIMSep22ML5Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","wimsigid")

### Add veh id in SIG file
#VDS
Irvine.VDSSep22ML4sig["vdssigid"] <- NA
Irvine.VDSSep22ML5sig["vdssigid"] <- NA

system.time({
  idx1=match(Irvine.VDSSep22ML4sig$id, Irvine.VDSSep22ML4Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.VDSSep22ML4sig$vdssigid[idx2] <- Irvine.VDSSep22ML4Header$vdssigid[idx1[idx2]]
})

system.time({
  idx1=match(Irvine.VDSSep22ML5sig$id, Irvine.VDSSep22ML5Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.VDSSep22ML5sig$vdssigid[idx2] <- Irvine.VDSSep22ML5Header$vdssigid[idx1[idx2]]
})

#WIM
Irvine.WIMSep22ML4sig["wimsigid"] <- NA
Irvine.WIMSep22ML5sig["wimsigid"] <- NA
system.time({
  idx1=match(Irvine.WIMSep22ML4sig$id, Irvine.WIMSep22ML4Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.WIMSep22ML4sig$wimsigid[idx2] <- Irvine.WIMSep22ML4Header$wimsigid[idx1[idx2]]
})

system.time({
  idx1=match(Irvine.WIMSep22ML5sig$id, Irvine.WIMSep22ML5Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.WIMSep22ML5sig$wimsigid[idx2] <- Irvine.WIMSep22ML5Header$wimsigid[idx1[idx2]]
})

### Merge into one file 
#Header file
Irvine.VDSSep22Header <- rbind(Irvine.VDSSep22ML4Header, Irvine.VDSSep22ML5Header)
Irvine.VDSSep22Header <- Irvine.VDSSep22Header[order(Irvine.VDSSep22Header[,12]),] 
colnames(Irvine.VDSSep22Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

Irvine.WIMSep22Header <- rbind(Irvine.WIMSep22ML4Header, Irvine.WIMSep22ML5Header)
Irvine.WIMSep22Header <- Irvine.WIMSep22Header[order(Irvine.WIMSep22Header[,12]),] 
colnames(Irvine.WIMSep22Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","wimsigid")

#SIG file
Irvine.VDSSep22sig <- rbind(Irvine.VDSSep22ML4sig, Irvine.VDSSep22ML5sig)
colnames(Irvine.VDSSep22sig) <- c("id", "mag", "v3","v4","v5","v6","vdssigid")
Irvine.WIMSep22sig <- rbind(Irvine.WIMSep22ML4sig, Irvine.WIMSep22ML5sig)
colnames(Irvine.WIMSep22sig) <- c("id", "mag", "v3","v4","v5","v6","wimsigid")


save.image("./DataIrvine/IrvineLoadinSep22")