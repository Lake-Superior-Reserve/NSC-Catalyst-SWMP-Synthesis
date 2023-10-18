library(here)
here()
WQ_all<-read.csv("SWMP_monthlyWQ.csv")
NUT_all<-read.csv("SWMP_monthlyNUT.csv")
MET_all<-read.csv("SWMP_monthlyMET.csv")


wq_lks <- WQ_all[grep("lks", WQ_all$station), ]
wq_grb <- WQ_all[grep("grb", WQ_all$station), ]
wq_rkb <- WQ_all[grep("rkb", WQ_all$station), ]
wq_sos <- WQ_all[grep("sos", WQ_all$station), ]
wq_niw <- WQ_all[grep("niw", WQ_all$station), ]
wq_gtm <- WQ_all[grep("gtm", WQ_all$station), ]
wq_cbm <- WQ_all[grep("cbm", WQ_all$station), ]
wq_pdb <- WQ_all[grep("pdb", WQ_all$station), ]

nut_lks <- NUT_all[grep("lks", NUT_all$station), ]
nut_grb <- NUT_all[grep("grb", NUT_all$station), ]
nut_rkb <- NUT_all[grep("rkb", NUT_all$station), ]
nut_sos <- NUT_all[grep("sos", NUT_all$station), ]
nut_niw <- NUT_all[grep("niw", NUT_all$station), ]
nut_gtm <- NUT_all[grep("gtm", NUT_all$station), ]
nut_cbm <- NUT_all[grep("cbm", NUT_all$station), ]
nut_pdb <- NUT_all[grep("pdb", NUT_all$station), ]

met_lks <- MET_all[grep("lks", MET_all$station), ]
met_grb <- MET_all[grep("grb", MET_all$station), ]
met_rkb <- MET_all[grep("rkb", MET_all$station), ]
met_sos <- MET_all[grep("sos", MET_all$station), ]
met_niw <- MET_all[grep("niw", MET_all$station), ]
met_gtm <- MET_all[grep("gtm", MET_all$station), ]
met_cbm <- MET_all[grep("cbm", MET_all$station), ]
met_pdb <- MET_all[grep("pdb", MET_all$station), ]