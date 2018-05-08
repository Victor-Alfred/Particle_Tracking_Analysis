setwd("C:/Users/.../R Data Analysis/...")

all_trafficking_NoEGTA <- read.csv("all_trafficking_NoEGTA.csv")

library(ggplot2)

ggplot(all_trafficking_NoEGTA, aes(N9__NoSpotsPerCell, N3__MembNotch)) + 
  geom_point(aes(colour = QC1__NoOfAnalysedCells), size = 3.5) +
  theme_bw() +
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("N9_NoOfSpotsPerCell (z-score)") +
  ylab("N3_MembNotch (z-score)") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_colour_gradient(limits=c(-0.5, 2.5), high = "#CC0000", low = "#0000FF") + 
  geom_text(aes(label=GeneSymbol), size=3.5, vjust = 2) +
  annotate(geom="text", x=-0.5, y=3.4, label="NoEGTA", color="black", size=8, face="bold")

rm(all_trafficking_NoEGTA)



all_trafficking_EGTA <- read.csv("all_trafficking_EGTA.csv")

ggplot(all_trafficking_EGTA, aes(N9__NoSpotsPerCell, N3__MembNotch)) + 
  geom_point(aes(colour = QC1__NoOfAnalysedCells), size = 5) +
  theme_bw() +
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("N9_NoOfSpotsPerCell (z-score)") +
  ylab("N3_MembNotch (z-score)") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  labs (colour = "QC1 NoOfAnalysedCells") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_colour_gradient(limits=c(-0.5, 2.5), high = "#CC0000", low = "#0000FF") +
  theme(legend.text = element_text(size = 12, face = "bold", vjust = 2))+
  theme(legend.title = element_text(face = "bold", size = 15, vjust = 10)) +
  geom_text(aes(label=GeneSymbol), size=3.5, vjust = 2) +
  annotate(geom="text", x=-0.5, y=3.4, label="+EGTA", color="black", size=8, face="bold")

rm(all_trafficking_EGTA)


# plots of nuclear notch increased EGTA

NucNotch_incr_EGTA <- read.csv("NucNotch_incr_EGTA.csv")
str(NucNotch_incr_EGTA)

library(ggplot2)

ggplot(NucNotch_incr_EGTA, aes(N1__NucNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 5, colour="#B40404") +
  theme_bw() +
  xlab("N1__NucNotch") +
  ylab("QC1__NoOfAnalysedCells") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_text(aes(label=GeneSymbol), size=3.7, vjust = 2)

# Reduced Nuclear Notch EGTA

NucNotch_reduced_EGTA <- read.csv("NucNotch_reduced_EGTA.csv")

str(NucNotch_reduced_EGTA)

ggplot(NucNotch_reduced_EGTA, aes(N1__NucNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 5, colour="#B40404") +
  theme_bw() +
  xlab("N1__NucNotch") +
  ylab("QC1__NoOfAnalysedCells") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_text(aes(label=GeneSymbol), size=3.7, vjust = 2)


NucNotch_EGTA <- read.csv("NucNotch_EGTA.csv")

ggplot(NucNotch_EGTA, aes(N1__NucNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 5, colour="#B40404") +
  theme_bw() +
  xlab("N1__NucNotch") +
  ylab("QC1__NoOfAnalysedCells") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_text(aes(label=GeneSymbol), size=3.7, vjust = 2)

# plot for reduced membrane Notch NoEGTA


MemNotch_increased_NoEGTA <- read.csv("MemNotch_increased_NoEGTA.csv")

str(MemNotch_increased_NoEGTA)

# 1.15 subtracted from all other samples except negative control, NEG
MemNotch_increased_NoEGTA$New_N3_MembNotch <- MemNotch_increased_NoEGTA$N3__MembNotch - 1.15

write.csv(MemNotch_increased_NoEGTA, "MemNotch_increased_NoEGTA.csv")
 
MemNotch_increased_NoEGTA <- read.csv("MemNotch_increased_NoEGTA.csv")

ggplot(MemNotch_increased_NoEGTA, aes(New_N3_MembNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 5, colour="#B40404") +
  theme_bw() +
  xlab("N3_MembNotch (z-score)") +
  ylab("QC1_NoOfAnalysedCells (z-score)") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_text(aes(label=GeneSymbol), size=3.7, vjust = 2)

# PLOT for reduced membrane Notch NoEGTA
# 1.3 subtracted from negative control, NEG alone, MembNotch

MemNotch_reduced_NoEGTA <- read.csv("MemNotch_reduced_NoEGTA.csv")


ggplot(MemNotch_reduced_NoEGTA, aes(N3__MembNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 5, colour="#B40404") +
  theme_bw() +
  xlab("N3_MembNotch") +
  ylab("QC1_NoOfAnalysedCells") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_text(aes(label=GeneSymbol), size=3.7, vjust = 2)



# Reduced Nuclear Notch EGTA, MembNotch vs NucNotch, 1.4 subtracted 
# from nucNotch NEG

NucNotch_reduced_EGTA <- read.csv("NucNotch_reduced_EGTA.csv")

str(NucNotch_reduced_EGTA)

ggplot(NucNotch_reduced_EGTA, aes(N1__NucNotch, N3__MembNotch)) + 
  geom_point(size = 5, colour="#B40404") +
  theme_bw() +
  xlab("N1_NucNotch (z-score)") +
  ylab("N3_MembNotch (z-score)") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_text(aes(label=GeneSymbol), size=3.7, vjust = 2)


# Plot of Nuclear Notch Levels EGTA
NucNotch_EGTA <- read.csv("NucNotch_EGTA.csv")

str(NucNotch_EGTA)

ggplot(NucNotch_EGTA, aes(N1__NucNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 5, colour="#B40404") +
  theme_bw() +
  xlab("N1_NucNotch (z-score)") +
  ylab("QC1_NoOfAnalysedCells (z-score)") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold")) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_text(aes(label=GeneSymbol), size=3.7, vjust = 2)



