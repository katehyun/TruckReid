### function



# FUNCTION - normalization
f.normalization <- function (insig){
  outsig <- transform (insig, newtime=insig[,1] / insig[nrow(insig),1],
                       newsig= insig[,2] / (max(insig[,2]))) 
  outsig[1,4]=0
  outsig[1,5]=0
  return (outsig)
}

# FUNCTION - round
f.round <- function (insig, no_round){
  outsig <- insig * no_round
  outsig = round (outsig)
  outsig = outsig / no_round
}


# FUNCTION - spline interpolation
f.interpolation <- function (insig, num, no_round){ 
  outsig0 <- spline(insig[,4], insig[,5], num)
  outsig1 = data.frame(matrix(unlist(outsig0[1])))
  
  outsig2 <- f.round (outsig1, no_round)
  outtime = outsig2
  
  outmag = data.frame(matrix(unlist(outsig0[2])))
  outsig <- cbind(outtime, outmag)
  return (outsig)
}


# FUNCTION - swift
f.swift <- function (insig, vds_swift_coeff, num , no_round){
  
  vds_swift_mag <- rep(NA, num)
  vds_swift_time <- rep(NA, num)
  vds_swift_out <- data.frame (vds_swift_time, vds_swift_mag)
  swiftmag <- rep(NA, num)
  swifttime<- rep(NA, num)
  swift_magdif <- rep(NA,1)
  
  
  for (i in 1: length(vds_swift_coeff)){
    
      
    vds_swift_time <- insig[,1] + vds_swift_coeff[i]
    vds_swift_time <- f.round (vds_swift_time, no_round)
    vds_swift_out <- data.frame (vds_swift_time, vds_swift_mag)
    
    # LOOKUP!!!!!
    vds_swift_out$vds_swift_mag <- splinevds$outvdsmag [ match (vds_swift_time,splinevds$outvdstime )]  # no.. mag should be the same
    swiftmag <- cbind(vds_swift_out$vds_swift_mag, swiftmag)
    swifttime <- cbind(vds_swift_out$vds_swift_time, swifttime)
    
    swift_magdif2 =  (abs (splinewim$outwimmag - vds_swift_out $ vds_swift_mag))
    
    swift_magdif2[is.na(swift_magdif2)] <- 0 # replace NA to zero
    swift_magdif2= sum(swift_magdif2)
    swift_magdif <- cbind(swift_magdif2, swift_magdif) 
    
    min_swiftmagdif <- which.min (swift_magdif )
    minvalue <- swift_magdif[min_swiftmagdif]
    
    vds_swift <- cbind(swifttime[,min_swiftmagdif], swiftmag[,min_swiftmagdif])
    
  }
  return (list(matrix=vds_swift, mv=minvalue))
}


# FUNCTION - stret
f.stret <- function (insig, vds_stret_coeff, num, no_round ){
  
  vds_stret_mag <- rep(NA, num)
  vds_stret_time <- rep(NA, num)
  
  stretmag <- rep(NA, num)
  strettime<- rep(NA, num)
  stret_magdif <- rep(NA,1)
  
  for (i in 1: length(vds_stret_coeff)){
    
    vds_stret_time <- insig[,1]* vds_stret_coeff[i]
    vds_stret_time <- f.round (vds_stret_time, no_round)
    vds_stret_out <- data.frame (vds_stret_time, vds_stret_mag)
    
    # LOOKUP!!!!!
    vds_stret_out$vds_stret_mag <- splinevds$outvdsmag [ match (vds_stret_time,splinevds$outvdstime )]  
    stretmag <- cbind(vds_stret_out$vds_stret_mag, stretmag)
    strettime <- cbind(vds_stret_out$vds_stret_time, strettime)
    
    stret_magdif2 =  (abs (splinewim$outwimmag - vds_stret_out $ vds_stret_mag))
    
    stret_magdif2[is.na(stret_magdif2)] <- 0 # replace NA to zero
    stret_magdif2= sum(stret_magdif2)
    stret_magdif <- cbind(stret_magdif2, stret_magdif) 
    
    min_stretmagdif <- which.min (stret_magdif)
    minvalue <- stret_magdif[min_stretmagdif]
    
    vds_stret <- cbind(strettime[,min_stretmagdif], stretmag[,min_stretmagdif])
    
  }
  
  return (list(matrix=vds_stret, mv=minvalue) )
  
}




### draw signature
f.drawwimsignature <- function (no){
  
  sig_idx <- wimidx[no]
  insig <- wimsig_IM[sig_idx+1,]
  sig_idx <- sig_idx+1
  
  while (wimsig_IM[sig_idx+1,1] < 100){
    insig <- rbind(insig, wimsig_IM[sig_idx+1,])
    sig_idx <- sig_idx+1
  }
  
  insig <- f.normalization(insig)
  splinesig <- f.interpolation(insig,num,no_round)
  
  insig_time <- splinesig[,1]
  insig_mag <- splinesig[,2]
  
  sigplot <- plot(insig_time, insig_mag)
  
  return (sigplot)
}





f.drawvdssignature <- function (n1,n2){
  
  sig_idx <- vdsidx[[n1]][n2]
  insig <- vdssig_IM[sig_idx+1,]
  sig_idx <- sig_idx+1
  
  while (vdssig_IM[sig_idx+1,1] < 100){
    insig <- rbind(insig, vdssig_IM[sig_idx+1,])
    sig_idx <- sig_idx+1
  }
  
  insig <- f.normalization(insig)
  splinesig <- f.interpolation(insig,num,no_round)
  
  insig_time <- splinesig[,1]
  insig_mag <- splinesig[,2]
  
  sigplot <- plot(insig_time, insig_mag)
  
  return (sigplot)
}
