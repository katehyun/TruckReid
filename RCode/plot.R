### Plot
setwd( "C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Plot/Non_Matches") 
no <-27
Downplot <- f.drawDownsignature (no)


n1=27
n2=1
Upplot <- f.drawUpsignature (n1,n2)

n1=27
n2=2
Upplot <- f.drawUpsignature (n1,n2)



Downsigid <-531357854229659
Downplot <- f.Downdraw (Downsigid)

Upsigid <- 931357852168065
Upplot <- f.Updraw (Upsigid)
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ) 

Upsigid <-931357851573677
Upplot <- f.Updraw (Upsigid)
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ) 


jpeg( paste("Error", Downsigid[1] , Upsigid[1], ".jpeg"));
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ); 
dev.off()


n1=1
n2=27
UpplotAfterSS <- f.UpdrawAfterSS(n1,n2)


Upsigid <- 941357775815254
Downsigid <- 531357777730247
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ) 

library(pracma)
trapz(Errorplot,0)
require(pracma)


require(MESS)
auc(Errorplot, 0)
