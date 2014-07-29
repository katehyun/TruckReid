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
f.swift <- function (insig, swift_coeff, num , no_round){
  
  Up_swift_mag <- rep(NA, num)
  Up_swift_time <- rep(NA, num)
  Up_swift_out <- data.frame (Up_swift_time, Up_swift_mag)
  swiftmag <- rep(NA, num)
  swifttime<- rep(NA, num)
  swift_magdif <- rep(NA,1)
  
  time <- seq(from= 0, to= 1, by = 1/num)
  
  for (i in 1: length(swift_coeff)){
    
    
    Up_swift_time <- time + swift_coeff[i]
    Up_swift_mag <- insig
    
   
    
    # LOOKUP!!!!! 
   
    swifttime <- cbind( Up_swift_time, swifttime)
    
    swift_tempmag <- Up_swift_mag [match (Up_swift_time, time )]
    swift_tempmag[is.na(swift_tempmag)] <- Up_swift_mag
    
    swift_magdif2 = abs(splineDown - swift_tempmag )
    swift_magdif3= sum(swift_magdif2)
    
    swift_magdif <- cbind(swift_magdif3, swift_magdif) 
    
    min_swiftmagdif <- which.min (swift_magdif )
    minvalue <- swift_magdif[min_swiftmagdif]
    
    Up_swift <- cbind(swifttime[,min_swiftmagdif], Up_swift_mag)
    
  }
  return (list(matrix=Up_swift, mv=minvalue))
}


# FUNCTION - stret
f.stret <- function (insig, stret_coeff, num, no_round ){
  
  time <- seq(from= 0, to= 1, by = 1/num)
  Up_stret_mag <- rep(NA, num)
  Up_stret_time <- rep(NA, num)
  
  stretmag <- rep(NA, num)
  strettime<- rep(NA, num)
  stret_magdif <- rep(NA,1)
  
  for (i in 1: length(stret_coeff)){
    
    
    Up_stret_time <- insig[,1]* stret_coeff[i]
     
    Up_stret_mag <- insig[,2]
    
    # LOOKUP!!!!! 
    
    stretmag <- cbind(Up_stret_mag, stretmag)
    strettime <- cbind(Up_stret_time, strettime)
    
    stret_tempmag <- Up_stret_mag [match (Up_stret_time, time )]
    stret_tempmag[is.na(stret_tempmag)] <- Up_stret_mag
    
    stret_magdif2 = abs(splineDown - stret_tempmag ) 
    stret_magdif3= sum(stret_magdif2)
    
    stret_magdif <- cbind(stret_magdif3, stret_magdif) 
    
    min_stretmagdif <- which.min (stret_magdif)
    minvalue <- stret_magdif[min_stretmagdif]
    
    Up_stret <- cbind(strettime[,min_stretmagdif], Up_stret_mag)
    
    
  }
  return (list(matrix=Up_stret, mv=minvalue) )
  
}




### draw signature
f.drawDownsignature <- function (no){
  
  sig_idx <- Downidx[no]
  insig <- Downsig_IM[sig_idx+1,]
  sig_idx <- sig_idx+1
  
  while (Downsig_IM[sig_idx+1,1] < 100){
    insig <- rbind(insig, Downsig_IM[sig_idx+1,])
    sig_idx <- sig_idx+1
  }
  
  insig <- f.normalization(insig)
  splinesig <- f.interpolation(insig,num,no_round)
  
  insig_time <- splinesig[,1]
  insig_mag <- splinesig[,2]
  
  
  sigplot <- matplot(insig_time, insig_mag, main=paste("Target (Downstream)", no[1]))
  
  return (sigplot)
}





f.drawUpsignature <- function (n1,n2){
  
  sig_idx <- Upidx[[n1]][n2]
  insig <- Upsig_IM[sig_idx+1,]
  sig_idx <- sig_idx+1
  
  while (Upsig_IM[sig_idx+1,1] < 100){
    insig <- rbind(insig, Upsig_IM[sig_idx+1,])
    sig_idx <- sig_idx+1
  }
  
  insig <- f.normalization(insig)
  splinesig <- f.interpolation(insig,num,no_round)
  
  insig_time <- splinesig[,1]
  insig_mag <- splinesig[,2]
  
  sigplot <- plot(insig_time, insig_mag, main=paste("Candidate (Upstream)", n1[1],"-", n2[1]))
  
  return (sigplot)
}


f.Updraw <- function (sigid){
  
  sig_idx <- which( Upsig_IM[,3] %in% sigid)
  insig <- Upsig_IM[sig_idx+1,]
  sig_idx <- sig_idx+1
  
  while (Upsig_IM[sig_idx+1,1] < 100){
    insig <- rbind(insig, Upsig_IM[sig_idx+1,])
    sig_idx <- sig_idx+1
  }
  
  insig <- f.normalization(insig)
  splinesig <- f.interpolation(insig,num,no_round)
  
  insig_time <- splinesig[,1]
  insig_mag <- splinesig[,2]
  
  sigplot <- plot(insig_time, insig_mag, main=paste("Candidate (Upstream)", sigid[1]))
  
  return (sigplot)
  
}


f.Downdraw <- function (sigid){
  
  sig_idx <- which( Downsig_IM[,3] %in% sigid)
  insig <- Downsig_IM[sig_idx+1,]
  sig_idx <- sig_idx+1
  
  while (Downsig_IM[sig_idx+1,1] < 100){
    insig <- rbind(insig, Downsig_IM[sig_idx+1,])
    sig_idx <- sig_idx+1
  }
  
  insig <- f.normalization(insig)
  splinesig <- f.interpolation(insig,num,no_round)
  
  insig_time <- splinesig[,1]
  insig_mag <- splinesig[,2]
  
  sigplot <- plot(insig_time, insig_mag, main=paste("Target (Downstream)", sigid[1]))
  
  return (sigplot)
  
}




f.pnn <- function ( nn, Downobjout1){
  
  cat <- list()
  prob <- list()
  category <- list()
  probs <- list()
  
  candi_guess <- list(guess(nn, Downobjout1)) 
  cat <- candi_guess[[1]][1]
  prob <- candi_guess[[1]][2]
  
  
  return (list(category <- cat , probs <- prob))
}


f.findpnn <- function (seqlevel, len_find_pnn, candi1, candi2, candi3, candi4, candi5, candi6, candi7, candi8 ){
  
  Upobjout <- c()
  
  if (len_find_pnn == 2) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)    
  }
  
  
  if (len_find_pnn == 3) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  if (len_find_pnn == 4) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  if (len_find_pnn == 5) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi5)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  if (len_find_pnn == 6) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi5)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi6)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  
  if (len_find_pnn == 7) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi5)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi6)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi7)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  
  if (len_find_pnn >= 8) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi5)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi6)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi7)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj) 
    
    Upobj <- unlist(candi8)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj) 
  }
  
  return (Upobjout)
  
}