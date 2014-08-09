

k<-208


magdif <- abs(candi_1[[k]][[1]][,2] - Downobjout[k,])
magdifplot <- plot(time,magdif ,main=paste("no", k))

magdif <- abs(candi_2[[k]][[1]][,2] - Downobjout[k,])
magdifplot <- plot(time,magdif ,main=paste("no", k))

magdif <- abs(candi_3[[k]][[1]][,2] - Downobjout[k,])
magdifplot <- plot(time,magdif ,main=paste("no", k))
