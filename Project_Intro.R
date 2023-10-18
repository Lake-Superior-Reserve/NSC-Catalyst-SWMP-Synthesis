library(here)
here()
WQ_all<-read.csv("SWMP_monthlyWQ.csv")


lks <- WQ_all[grep("lks", WQ_all$station), ]
grb <- WQ_all[grep("grb", WQ_all$station), ]
rkb <- WQ_all[grep("rkb", WQ_all$station), ]
sos <- WQ_all[grep("sos", WQ_all$station), ]
niw <- WQ_all[grep("niw", WQ_all$station), ]
gtm <- WQ_all[grep("gtm", WQ_all$station), ]
cbm <- WQ_all[grep("cbm", WQ_all$station), ]
pdb <- WQ_all[grep("pdb", WQ_all$station), ]
