library(here)
library(dplyr)
library(reshape2) 
library (ggplot2)


here()
WQ_all<-read.csv("Data/SWMP_monthlyWQ.csv")
NUT_all<-read.csv("Data/SWMP_monthlyNUT.csv")
MET_all<-read.csv("Data/SWMP_monthlyMET.csv")


wq_lks <- WQ_all[grep("^lks", WQ_all$station), ]
wq_apa <- WQ_all[grep("^apa", WQ_all$station), ]
wq_grb <- WQ_all[grep("^grb", WQ_all$station), ]
wq_rkb <- WQ_all[grep("^rkb", WQ_all$station), ]
wq_sos <- WQ_all[grep("^sos", WQ_all$station), ]
wq_niw <- WQ_all[grep("^niw", WQ_all$station), ]
wq_gtm <- WQ_all[grep("^gtm", WQ_all$station), ]
wq_cbm <- WQ_all[grep("^cbm", WQ_all$station), ]
wq_pdb <- WQ_all[grep("^pdb", WQ_all$station), ]

nut_lks <- NUT_all[grep("^lks", NUT_all$station), ]
nut_apa <- NUT_all[grep("^apa", NUT_all$station), ]
nut_grb <- NUT_all[grep("^grb", NUT_all$station), ]
nut_rkb <- NUT_all[grep("^rkb", NUT_all$station), ]
nut_sos <- NUT_all[grep("^sos", NUT_all$station), ]
nut_niw <- NUT_all[grep("^niw", NUT_all$station), ]
nut_gtm <- NUT_all[grep("^gtm", NUT_all$station), ]
nut_cbm <- NUT_all[grep("^cbm", NUT_all$station), ]
nut_pdb <- NUT_all[grep("^pdb", NUT_all$station), ]

met_lks <- MET_all[grep("^lks", MET_all$station), ]
met_apa <- MET_all[grep("^apa", MET_all$station), ]
met_grb <- MET_all[grep("^grb", MET_all$station), ]
met_rkb <- MET_all[grep("^rkb", MET_all$station), ]
met_sos <- MET_all[grep("^sos", MET_all$station), ]
met_niw <- MET_all[grep("^niw", MET_all$station), ]
met_gtm <- MET_all[grep("^gtm", MET_all$station), ]
met_cbm <- MET_all[grep("^cbm", MET_all$station), ]
met_pdb <- MET_all[grep("^pdb", MET_all$station), ]

##############################

write.csv(wq_lks,"Data/wq_lks.csv")
write.csv(wq_apa,"Data/wq_apa.csv")
write.csv(wq_grb,"Data/wq_grb.csv")
write.csv(wq_rkb,"Data/wq_rkb.csv")
write.csv(wq_sos,"Data/wq_sos.csv") 
write.csv(wq_niw,"Data/wq_niw.csv") 
write.csv(wq_gtm,"Data/wq_gtm.csv") 
write.csv(wq_cbm,"Data/wq_cbm.csv") 
write.csv(wq_pdb,"Data/wq_pdb.csv") 

write.csv(nut_lks,"Data/nut_lks.csv")
write.csv(nut_apa,"Data/nut_apa.csv")
write.csv(nut_grb,"Data/nut_grb.csv")
write.csv(nut_rkb,"Data/nut_rkb.csv")
write.csv(nut_sos,"Data/nut_sos.csv") 
write.csv(nut_niw,"Data/nut_niw.csv") 
write.csv(nut_gtm,"Data/nut_gtm.csv") 
write.csv(nut_cbm,"Data/nut_cbm.csv") 
write.csv(nut_pdb,"Data/nut_pdb.csv") 

write.csv(met_lks,"Data/met_lks.csv")
write.csv(met_apa,"Data/met_apa.csv")
write.csv(met_grb,"Data/met_grb.csv")
write.csv(met_rkb,"Data/met_rkb.csv")
write.csv(met_sos,"Data/met_sos.csv") 
write.csv(met_niw,"Data/met_niw.csv") 
write.csv(met_gtm,"Data/met_gtm.csv") 
write.csv(met_cbm,"Data/met_cbm.csv") 
write.csv(met_pdb,"Data/met_pdb.csv") 

