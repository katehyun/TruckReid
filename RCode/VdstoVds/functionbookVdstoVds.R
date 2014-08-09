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
    vds_swift_mag <- splinevds$outvdsmag
    
    #vds_swift_out <- data.frame (vds_swift_time, vds_swift_mag)
    
    # LOOKUP!!!!! 
    #swiftmag <- cbind(vds_swift_mag, swiftmag)
    swifttime <- cbind(vds_swift_time, swifttime)

    swift_tempmag <- vds_swift_mag [match (vds_swift_time, splinewim$outwimtime )]
    swift_tempmag[is.na(swift_tempmag)] <- vds_swift_mag
    
    swift_magdif2 = abs(splinewim$outwimmag - swift_tempmag )
    

    swift_magdif3= sum(swift_magdif2)
  
    swift_magdif <- cbind(swift_magdif3, swift_magdif) 
    
    min_swiftmagdif <- which.min (swift_magdif )
    minvalue <- swift_magdif[min_swiftmagdif]
    
    vds_swift <- cbind(swifttime[,min_swiftmagdif], vds_swift_mag)
    
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
    
    vds_stret_mag <- insig[,2]
    #vds_stret_out <- data.frame (vds_stret_time, vds_stret_mag)
    
    # LOOKUP!!!!! 
      
    stretmag <- cbind(vds_stret_mag, stretmag)
    strettime <- cbind(vds_stret_time, strettime)
    
    stret_tempmag <- vds_stret_mag [match (vds_stret_time, splinewim$outwimtime )]
    #stret_tempmag <- vds_stret_mag [match ( splinewim$outwimtime,vds_stret_time )]
    stret_tempmag[is.na(stret_tempmag)] <- vds_stret_mag
    
    stret_magdif2 = abs(splinewim$outwimmag - stret_tempmag )
    
    
    stret_magdif3= sum(stret_magdif2)
  
    stret_magdif <- cbind(stret_magdif3, stret_magdif) 
    
    min_stretmagdif <- which.min (stret_magdif)
    minvalue <- stret_magdif[min_stretmagdif]
    
    vds_stret <- cbind(strettime[,min_stretmagdif], vds_stret_mag)
    
  
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
  
  
  sigplot <- matplot(insig_time, insig_mag, main=paste("Target (Downstream)", no[1]))
  
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
  
  sigplot <- plot(insig_time, insig_mag, main=paste("Candidate (Upstream)", n1[1],"-", n2[1]))
  
  return (sigplot)
}


f.vdsdraw <- function (sigid){
  
  sig_idx <- which( vdssig_IM[,3] %in% sigid)
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
  
  sigplot <- plot(insig_time, insig_mag, main=paste("Candidate (Upstream)", sigid[1]))
  
  return (sigplot)
  
}
  

f.wimdraw <- function (sigid){
  
  sig_idx <- which( wimsig_IM[,3] %in% sigid)
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
  
  sigplot <- plot(insig_time, insig_mag, main=paste("Target (Downstream)", sigid[1]))
  
  return (sigplot)
  
}




f.pnn <- function ( nn, wimobjout1){
 
  cat <- list()
  prob <- list()
  category <- list()
  probs <- list()

    candi_guess <- list(guess(nn, wimobjout1)) 
    cat <- candi_guess[[1]][1]
    prob <- candi_guess[[1]][2]
    
  
  return (list(category <- cat , probs <- prob))
}


f.findpnn <- function (seqlevel, len_find_pnn, candi1, candi2, candi3, candi4, candi5, candi6, candi7, candi8 ){
 
  vdsobjout <- c()
  
  if (len_find_pnn == 2) {
    vdsobj <- unlist(candi1)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)
    
    vdsobj <- unlist(candi2)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)    
  }
  
  
  if (len_find_pnn == 3) {
    vdsobj <- unlist(candi1)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)
    
    vdsobj <- unlist(candi2)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi3)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
  }
  
  if (len_find_pnn == 4) {
    vdsobj <- unlist(candi1)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)
    
    vdsobj <- unlist(candi2)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi3)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi4)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
  }
  
  if (len_find_pnn == 5) {
    vdsobj <- unlist(candi1)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)
    
    vdsobj <- unlist(candi2)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi3)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi4)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi5)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
  }
  
  if (len_find_pnn == 6) {
    vdsobj <- unlist(candi1)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)
    
    vdsobj <- unlist(candi2)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi3)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi4)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi5)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi6)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
  }
  
  
  if (len_find_pnn == 7) {
    vdsobj <- unlist(candi1)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)
    
    vdsobj <- unlist(candi2)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi3)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi4)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi5)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi6)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi7)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
  }
  
  
  if (len_find_pnn >= 8) {
    vdsobj <- unlist(candi1)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)
    
    vdsobj <- unlist(candi2)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi3)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi4)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi5)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi6)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj)  
    
    vdsobj <- unlist(candi7)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj) 
    
    vdsobj <- unlist(candi8)
    vdsobj <-  vdsobj[1001:2000]
    vdsobj <- vdsobj[seq(1, length(vdsobj),seqlevel)]  
    vdsobjout <- rbind(vdsobjout, vdsobj) 
  }
  
  return (vdsobjout)
  
}