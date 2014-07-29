setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/DataIrvine") 

rnThreshold <- 0.8
rn <- sort (sample(1:length(wimidx), length(wimidx) *rnThreshold, replace=F))
matchingVds <- Target_baseanalysis_Mar20_table[,4][!is.na(Target_baseanalysis_Mar20_table[,4])]
matchingRnVds <- matchingVds[-rn]

for (i in 1: length(vdssiglist)){
  OpenSystemVdsList[[i]] <- vdssiglist[[i]][vdssiglist[[i]] %in% matchingRnVds ]
}




