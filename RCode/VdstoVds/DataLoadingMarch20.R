################ Data Input- Oct 02 ####################
### Irvine Mar 20


Irvine.VDSMar20ML4Header1=read.table("./RawData/VDSMar/ML40320/501_IST0001297_CA130320080000_fileIdx.txt",  fill=T)
Irvine.VDSMar20ML4Header2=read.table("./RawData/VDSMar/ML40320/501_IST0001297_CA130320171512_fileIdx.txt",  fill=T)
Irvine.VDSMar20ML4sig1=read.table("./RawData/VDSMar/ML40320/IST0001297_CA130320080000.txt", fill=T)
Irvine.VDSMar20ML4sig2=read.table("./RawData/VDSMar/ML40320/IST0001297_CA130320171512.txt", fill=T)
Irvine.VDSMar20ML5Header1=read.table("./RawData/VDSMar/ML50320/501_IST0001210_CA130320080000_fileIdx.txt",  fill=T)
Irvine.VDSMar20ML5Header2=read.table("./RawData/VDSMar/ML50320/501_IST0001210_CA130320171512_fileIdx.txt",  fill=T)
Irvine.VDSMar20ML5sig1=read.table("./RawData/VDSMar/ML50320/IST0001210_CA130320080000.txt", fill=T)
Irvine.VDSMar20ML5sig2=read.table("./RawData/VDSMar/ML50320/IST0001210_CA130320171512.txt", fill=T)

Irvine.WIMMar20ML4Header=read.table("./RawData/WIMMar/ML40320/15_IST0001090_CA130320070000_fileIdx.txt", fill=T)
Irvine.WIMMar20ML4sig=read.table("./RawData/WIMMar/ML40320/IST0001090_CA130320070000.txt", fill=T)
Irvine.WIMMar20ML5Header=read.table("./RawData/WIMMar/ML50320/15_IST0001035_CA130320070000_fileIdx.txt",  fill=T)
Irvine.WIMMar20ML5sig=read.table("./RawData/WIMMar/ML50320/IST0001035_CA130320070000.txt", fill=T)
save.image("./RawData/IrvineRawMar20")

rm(list=ls())
load("./RawData/IrvineRawMar20")

### col name for sig file
colnames(Irvine.VDSMar20ML4sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.VDSMar20ML4sig2) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.VDSMar20ML5sig1) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.VDSMar20ML5sig2) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.WIMMar20ML4sig) <- c("id", "v2", "v3","v4","v5","v6")
colnames(Irvine.WIMMar20ML5sig) <- c("id", "v2", "v3","v4","v5","v6")

Irvine.VDSMar20ML4sig1 <- subset(Irvine.VDSMar20ML4sig1, select = c("id", "v2", "v3") )
Irvine.VDSMar20ML4sig2 <- subset(Irvine.VDSMar20ML4sig2, select = c("id", "v2", "v3") )
Irvine.VDSMar20ML5sig1 <- subset(Irvine.VDSMar20ML5sig1, select = c("id", "v2", "v3") )
Irvine.VDSMar20ML5sig2 <- subset(Irvine.VDSMar20ML5sig2, select = c("id", "v2", "v3") )
Irvine.WIMMar20ML4sig <- subset(Irvine.WIMMar20ML4sig, select = c("id", "v2", "v3") )
Irvine.WIMMar20ML5sig <- subset(Irvine.WIMMar20ML5sig, select = c("id", "v2", "v3") )


### Add vehid in Header file 
# VDS Mar20 ML4 - header 1
uctMar20 <- 1363762800
Irvine.VDSMar20ML4Header1[,length(Irvine.VDSMar20ML4Header1)+1]<- 1000*(uctMar20 + Irvine.VDSMar20ML4Header1[,length(Irvine.VDSMar20ML4Header1)])


x<- rep(14,nrow(Irvine.VDSMar20ML4Header1))
y<- Irvine.VDSMar20ML4Header1[,ncol(Irvine.VDSMar20ML4Header1)]
len <- length (Irvine.VDSMar20ML4Header1)+1

for (i in 1:nrow(Irvine.VDSMar20ML4Header1)){
  Irvine.VDSMar20ML4Header1[i,len] <- paste (x[i],y[i], sep="")
}

colnames(Irvine.VDSMar20ML4Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

# VDS Mar20 ML4 - header 2
Irvine.VDSMar20ML4Header2[,length(Irvine.VDSMar20ML4Header2)+1]<- 1000*(uctMar20 + Irvine.VDSMar20ML4Header2[,length(Irvine.VDSMar20ML4Header2)])

x<- rep(14,nrow(Irvine.VDSMar20ML4Header2))
y<- Irvine.VDSMar20ML4Header2[,ncol(Irvine.VDSMar20ML4Header2)]
len <- length (Irvine.VDSMar20ML4Header2)+1

for (i in 1:nrow(Irvine.VDSMar20ML4Header2)){
  Irvine.VDSMar20ML4Header2[i,len] <- paste (x[i],y[i], sep="")
}

colnames(Irvine.VDSMar20ML4Header2) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

# VDS Mar20 ML5 - header 1
Irvine.VDSMar20ML5Header1[,length(Irvine.VDSMar20ML5Header1)+1]<- 1000*(uctMar20 + Irvine.VDSMar20ML5Header1[,length(Irvine.VDSMar20ML5Header1)])

x<- rep(15,nrow(Irvine.VDSMar20ML5Header1))
y<- Irvine.VDSMar20ML5Header1[,ncol(Irvine.VDSMar20ML5Header1)]
len <- length (Irvine.VDSMar20ML5Header1)+1

for (i in 1:nrow(Irvine.VDSMar20ML5Header1)){
  Irvine.VDSMar20ML5Header1[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.VDSMar20ML5Header1) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

# VDS Mar20 ML5 - header 2
Irvine.VDSMar20ML5Header2[,length(Irvine.VDSMar20ML5Header2)+1]<- 1000*(uctMar20 + Irvine.VDSMar20ML5Header2[,4])

x<- rep(15,nrow(Irvine.VDSMar20ML5Header2))
y<- Irvine.VDSMar20ML5Header2[,ncol(Irvine.VDSMar20ML5Header2)]
len <- length (Irvine.VDSMar20ML5Header2)+1

for (i in 1:nrow(Irvine.VDSMar20ML5Header2)){
  Irvine.VDSMar20ML5Header2[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.VDSMar20ML5Header2) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

# WIM Sep22 ML4
Irvine.WIMMar20ML4Header[,length(Irvine.WIMMar20ML4Header)+1]<- 
  1000*(uctMar20 + Irvine.WIMMar20ML4Header[,length(Irvine.WIMMar20ML4Header)])

x<- rep(24,nrow(Irvine.WIMMar20ML4Header))
y<- Irvine.WIMMar20ML4Header[,ncol(Irvine.WIMMar20ML4Header)]
len <- length (Irvine.WIMMar20ML4Header)+1

for (i in 1:nrow(Irvine.WIMMar20ML4Header)){
  Irvine.WIMMar20ML4Header[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.WIMMar20ML4Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","wimsigid")

# WIM Sep22 ML5
Irvine.WIMMar20ML5Header[,length(Irvine.WIMMar20ML5Header)+1]<- 
  1000*(uctMar20 + Irvine.WIMMar20ML5Header[,length(Irvine.WIMMar20ML5Header)])

x<- rep(25,nrow(Irvine.WIMMar20ML5Header))
y<- Irvine.WIMMar20ML5Header[,ncol(Irvine.WIMMar20ML5Header)]
len <- length (Irvine.WIMMar20ML5Header)+1

for (i in 1:nrow(Irvine.WIMMar20ML5Header)){
  Irvine.WIMMar20ML5Header[i,len] <- paste (x[i],y[i], sep="")
}
colnames(Irvine.WIMMar20ML5Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","wimsigid")

### Add veh id in SIG file
#VDS
Irvine.VDSMar20ML4sig1["vdssigid"] <- NA
Irvine.VDSMar20ML4sig2["vdssigid"] <- NA
Irvine.VDSMar20ML5sig1["vdssigid"] <- NA
Irvine.VDSMar20ML5sig2["vdssigid"] <- NA

system.time({
  idx1=match(Irvine.VDSMar20ML4sig1$id, Irvine.VDSMar20ML4Header1$id)
  idx2 = which(!is.na(idx1))
  Irvine.VDSMar20ML4sig1$vdssigid[idx2] <- Irvine.VDSMar20ML4Header1$vdssigid[idx1[idx2]]
})

system.time({
  idx1=match(Irvine.VDSMar20ML4sig2$id, Irvine.VDSMar20ML4Header2$id)
  idx2 = which(!is.na(idx1))
  Irvine.VDSMar20ML4sig2$vdssigid[idx2] <- Irvine.VDSMar20ML4Header2$vdssigid[idx1[idx2]]
})

system.time({
  idx1=match(Irvine.VDSMar20ML5sig1$id, Irvine.VDSMar20ML5Header1$id)
  idx2 = which(!is.na(idx1))
  Irvine.VDSMar20ML5sig1$vdssigid[idx2] <- Irvine.VDSMar20ML5Header1$vdssigid[idx1[idx2]]
})

system.time({
  idx1=match(Irvine.VDSMar20ML5sig2$id, Irvine.VDSMar20ML5Header2$id)
  idx2 = which(!is.na(idx1))
  Irvine.VDSMar20ML5sig2$vdssigid[idx2] <- Irvine.VDSMar20ML5Header2$vdssigid[idx1[idx2]]
})

#WIM
Irvine.WIMMar20ML4sig["wimsigid"] <- NA
Irvine.WIMMar20ML5sig["wimsigid"] <- NA
system.time({
  idx1=match(Irvine.WIMMar20ML4sig$id, Irvine.WIMMar20ML4Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.WIMMar20ML4sig$wimsigid[idx2] <- Irvine.WIMMar20ML4Header$wimsigid[idx1[idx2]]
})

system.time({
  idx1=match(Irvine.WIMMar20ML5sig$id, Irvine.WIMMar20ML5Header$id)
  idx2 = which(!is.na(idx1))
  Irvine.WIMMar20ML5sig$wimsigid[idx2] <- Irvine.WIMMar20ML5Header$wimsigid[idx1[idx2]]
})

### Merge into one file 
#Header file

Irvine.VDSMar20Header <- rbind(Irvine.VDSMar20ML4Header1, Irvine.VDSMar20ML4Header2, Irvine.VDSMar20ML5Header1, Irvine.VDSMar20ML5Header2)
                               
Irvine.VDSMar20Header <- Irvine.VDSMar20Header[order(Irvine.VDSMar20Header[,12]),] 
colnames(Irvine.VDSMar20Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","vdssigid")

Irvine.WIMMar20Header <- rbind(Irvine.WIMMar20ML4Header,  Irvine.WIMMar20ML5Header)
Irvine.WIMMar20Header <- Irvine.WIMMar20Header[order(Irvine.WIMMar20Header[,12]),] 
colnames(Irvine.WIMMar20Header) <- c("C", "id", "lane","v4","v5","v6","v7","v8","v9","v10","v11","utc","wimsigid")

#SIG file
Irvine.VDSMar20sig <- rbind(Irvine.VDSMar20ML4sig1, Irvine.VDSMar20ML4sig2,
                            Irvine.VDSMar20ML5sig1, Irvine.VDSMar20ML5sig2)
colnames(Irvine.VDSMar20sig) <- c("id", "mag", "v3","vdssigid")
Irvine.WIMMar20sig <- rbind(Irvine.WIMMar20ML4sig, Irvine.WIMMar20ML5sig)
colnames(Irvine.WIMMar20sig) <- c("id", "mag", "v3","wimsigid")

write.table(Irvine.VDSMar20Header, "./ProcessedData/Irvine.VDSMar20Header.txt", sep="\t")
write.table(Irvine.WIMMar20Header, "./ProcessedData/Irvine.WIMMar20Header.txt", sep="\t")
write.table(Irvine.VDSMar20sig, "./ProcessedData/Irvine.VDSMar20sig.txt", sep="\t")
write.table(Irvine.WIMMar20sig, "./ProcessedData/Irvine.WIMMar20sig.txt", sep="\t")
# load functionbook2
save.image("./IrvineLoadinMar20")
