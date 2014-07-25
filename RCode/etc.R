
# test (need more work)
rr <- Match(Y=Y,Tr=Tr,X=X,M=1);
vdsidx <- match (Irvine.VDSSep22Header [,12], %greaterthan% lb[1] & Irvine.VDSSep22Header [,12], ub[i]  )
for ( in 1: lenwim) {
  vdsidxtemp <- match ( Irvine.VDSSep22Header [,12] , lb[i] & Irvine.VDSSep22Header [,12] , ub[i])
  vdsidx <- cbind(vdsidx, vdsidxtemp)
}

# ex1
dt2 <- dt[,
          ToKeep := growth_rate > .SD[country == countrycompared,growth_rate],
          by = year][ToKeep == TRUE]
# ex2
which(m["a",]==maxvals) 
# ex3
x[!x == remove.value] 
x=x[which(x=="bobo")]


vdsidxtemp <- data.frame()
vdsidx<- match ( Irvine.VDSSep22_F_Sample[,1] , Irvine.VDSSep22sig_IMOnly[,3] )
for (i in 1:ncol(Irvine.VDSSep22_F_Sample)){
  vdsidxtemp <- match ( (Irvine.VDSSep22_F_Sample[,i]),Irvine.VDSSep22sig_IMOnly[,3] )
  vdsidx <- cbind(vdsidx, vdsidxtemp)
}





save(Irvine.VDSSep22_F_Sample,file="./DataIrvine/RCode/Irvine.VDSSep22_F_sample.Rda")


######################################################################

#utils::View(Irvine.WIMSep22Header)
