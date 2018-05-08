
# ANALYSIS PLOTS FOR FRANCIS DATA
# Viability plot No EGTA

viability <- read.csv("viability_for_plot_NoEGTA.csv")
str(viability)

library(ggplot2)
# grid extra helps to arrange several plots in  single image, demonstrated here
# with histograms of densities
library(gridExtra)

str(viability)

empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )


OTP <- subset(viability, GeneSymbol == "OTPctrl")

PSENEN_D <- subset(viability, GeneSymbol == "PSENEN_D")

PSENEN_S <- subset(viability, GeneSymbol == "PSENEN_S")

scatter <- ggplot(viability, aes(N4__CellNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 2) + geom_point(aes(colour = factor(Viability))) + 
  scale_colour_manual(values = c("red", "blue"))  +theme_bw()+
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("Cell Notch")+
  ylab("Number of Cells Analysed") + 
  geom_point(data = OTP, colour = "#9009B5", size = 3.5, shape =15) + 
  geom_text(data=OTP, label="NEG", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_S, colour = "#000000", size = 3.5, shape =15) + 
  geom_text(data=PSENEN_S, label="POS_S", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_D, colour = "#000000", size = 3.5, shape =15) +
  geom_text(data=PSENEN_D, label="POS_D", vjust = 1.5, fontface="bold")+ 
  ggtitle("Cell Viability No EGTA") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  

plot_top <- ggplot(viability, aes(N4__CellNotch, fill= Viability)) +
  geom_density(alpha=.5) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()+
  theme(legend.position = "none",axis.title.x = element_blank())

plot_right <- ggplot(viability, aes(QC1__NoOfAnalysedCells, fill=Viability)) +
  geom_density(alpha=.5) +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank())

grid.arrange(plot_top, empty, scatter, plot_right,
             ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

png("scatterplot",width=800)
grid.arrange(plot_top, empty, scatter, plot_right,
             ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
dev.off()

# Viability plot EGTA

ViableEGTA <- read.csv("ViableEGTA.csv")
ViableEGTA$Viability <- "Viable"

write.csv(ViableEGTA, "ViableEGTA.csv")

CytotoxicEGTA <- read.csv("CytotoxicEGTA.csv")
CytotoxicEGTA$Viability <- "Cytotoxic"

write.csv(CytotoxicEGTA, "CytotoxicEGTA.csv")

viability_EGTA <- read.csv("viability_for_plot_EGTA.csv")

OTP <- subset(viability_EGTA, GeneSymbol == "OTPctrl")

PSENEN_D <- subset(viability_EGTA, GeneSymbol == "PSENEN_D")

PSENEN_S <- subset(viability_EGTA, GeneSymbol == "PSENEN_S")

scatter <- ggplot(viability_EGTA, aes(N4__CellNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 2) + geom_point(aes(colour = factor(Viability))) + 
  scale_colour_manual(values = c("red", "blue"))  +theme_bw()+ 
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("Cell Notch")+
  ylab("Number of Cells Analysed") + 
  geom_point(data = OTP, colour = "#9009B5", size = 3.5, shape =15) + 
  geom_text(data=OTP, label="NEG", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_S, colour = "#000000", size = 3.5, shape =15) + 
  geom_text(data=PSENEN_S, label="POS_S", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_D, colour = "#000000", size = 3.5, shape =15) +
  geom_text(data=PSENEN_D, label="POS_D", vjust = 1.5, fontface="bold")+ 
  ggtitle("Cell Viability EGTA") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# Plot for Notch levels No EGTA

dir()

IncreasedNotch_NoEGTA <- read.csv("IncreasedNotch_NoEGTA.csv")

UnchangedNotch_NoEGTA <- read.csv("UnchangedNotch_NoEGTA.csv")

DecreasedNotch_NoEGTA <- read.csv("DecreasedNotch_NoEGTA.csv")

IncreasedNotch_NoEGTA$Notch_levels <- "Increased"
UnchangedNotch_NoEGTA$Notch_levels <- "Unchanged"
DecreasedNotch_NoEGTA$Notch_levels <- "Decreased"

write.csv(IncreasedNotch_NoEGTA, "IncreasedNotch_NoEGTA.csv")
write.csv(UnchangedNotch_NoEGTA, "UnchangedNotch_NoEGTA.csv")
write.csv(DecreasedNotch_NoEGTA, "DecreasedNotch_NoEGTA.csv")

dir()

notch_levels_NoEGTA <- read.csv("notch_levels_NoEGTA.csv")

summary(notch_levels_NoEGTA$Notch_levels)

OTP <- subset(notch_levels_NoEGTA, GeneSymbol == "OTPctrl")

PSENEN_D <- subset(notch_levels_NoEGTA, GeneSymbol == "PSENEN_D")

PSENEN_S <- subset(notch_levels_NoEGTA, GeneSymbol == "PSENEN_S")


scatter <- ggplot(notch_levels_NoEGTA, aes(N4__CellNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 2) + geom_point(aes(colour = factor(Notch_levels))) + 
  scale_colour_manual(values = c("red", "#120FF0", "#30874A"))  +theme_bw()+
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("Cell Notch")+
  ylab("Number of Cells Analysed") + 
  geom_point(data = OTP, colour = "#9009B5", size = 3.5, shape =15) + 
  geom_text(data=OTP, label="NEG", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_S, colour = "#000000", size = 3.5, shape =15) + 
  geom_text(data=PSENEN_S, label="POS_S", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_D, colour = "#000000", size = 3.5, shape =15) +
  geom_text(data=PSENEN_D, label="POS_D", vjust = 1.5, fontface="bold")+ 
  ggtitle("Notch Levels No EGTA") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))


plot_top <- ggplot(notch_levels_NoEGTA, aes(N4__CellNotch, fill= Notch_levels)) +
  geom_density(alpha=.5) +
  scale_fill_manual(values = c("red", "#120FF0", "#30874A")) +
  theme_bw()+
  theme(legend.position = "none",axis.title.x = element_blank())


plot_right <- ggplot(notch_levels_NoEGTA, aes(QC1__NoOfAnalysedCells, fill= Notch_levels)) +
  geom_density(alpha=.5) +
  coord_flip() +
  scale_fill_manual(values = c("red", "#120FF0", "#30874A")) +
  theme_bw()+
  theme(legend.position = "none",axis.title.y = element_blank())


grid.arrange(plot_top, empty, scatter, plot_right,
             ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

# Plot for Notch levels EGTA

UnchangedNotch_EGTA <- read.csv("UnchangedNotch_EGTA.csv")
IncreasedNotch_EGTA <- read.csv("IncreasedNotch_EGTA.csv")
DecreasedNotch_EGTA <- read.csv("DecreasedNotch_EGTA.csv")

UnchangedNotch_EGTA$Notch_levels <- "Unchanged"
IncreasedNotch_EGTA$Notch_levels <- "Increased"
DecreasedNotch_EGTA$Notch_levels <- "Decreased"

write.csv(UnchangedNotch_EGTA, "UnchangedNotch_EGTA.csv")
write.csv(IncreasedNotch_EGTA, "IncreasedNotch_EGTA.csv")
write.csv(DecreasedNotch_EGTA, "DecreasedNotch_EGTA.csv")

rm(UnchangedNotch_EGTA, IncreasedNotch_EGTA, DecreasedNotch_EGTA)

notch_levels_EGTA <- read.csv("notch_levels_EGTA.csv")

OTP <- subset(notch_levels_EGTA, GeneSymbol == "OTPctrl")

PSENEN_D <- subset(notch_levels_EGTA, GeneSymbol == "PSENEN_D")

PSENEN_S <- subset(notch_levels_EGTA, GeneSymbol == "PSENEN_S")

ggplot(notch_levels_EGTA, aes(N4__CellNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 2) + geom_point(aes(colour = factor(Notch_levels))) + 
  scale_colour_manual(values = c("red", "#120FF0", "#30874A"))  +theme_bw()+
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("Cell Notch")+
  ylab("Number of Cells Analysed") + 
  geom_point(data = OTP, colour = "#9009B5", size = 3.5, shape =15) + 
  geom_text(data=OTP, label="NEG", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_S, colour = "#000000", size = 3.5, shape =15) + 
  geom_text(data=PSENEN_S, label="POS_S", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_D, colour = "#000000", size = 3.5, shape =15) +
  geom_text(data=PSENEN_D, label="POS_D", vjust = 1.5, fontface="bold")+ 
  ggtitle("Notch Levels EGTA") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))


# Trafficking No EGTA

Trafficking_NoEGTA <- read.csv("Trafficking_NoEGTA.csv")

ggplot(Trafficking_NoEGTA, aes(N9__NoSpotsPerCell, N3__MembNotch)) + 
 geom_point(aes(colour = QC1__NoOfAnalysedCells), size = 2) +
 theme_bw() +
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("Number of spots per cells")+
  ylab("Membrane Notch") + 
  ggtitle("Number of Spots per cell") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_colour_gradient(limits=c(-0.5, 2.5), high = "#CC0000", low = "#0000FF") + 
  geom_text(aes(label=GeneSymbol), size=2.5, vjust = 2)

qplot(N9__NoSpotsPerCell, N3__MembNotch, data= Trafficking_NoEGTA, colour = QC1__NoOfAnalysedCells) + 
  geom_point(size = 2) +
  scale_colour_gradient(limits=c(-0.5, 2.5), high = "#CC0000", low = "#0000FF") +
  scale_size_continuous(limits = c(2,12)) + theme_bw() + 
  geom_text(aes(label=GeneSymbol), size=2.5, vjust = 2)

# Plots for membrane Notch levels

IncreasedMemNotch_NoEGTA <- read.csv("IncreasedMemNotch_NoEGTA.csv")
DecreasedMemNotch_NoEGTA <- read.csv("DecreasedMemNotch_NoEGTA.csv")
UnchangedMemNotch_NoEGTA <- read.csv("UnchangedMemNotch_NoEGTA.csv")

IncreasedMemNotch_NoEGTA$Memb_Notch_levels <- "Increased"
DecreasedMemNotch_NoEGTA$Memb_Notch_levels <- "Decreased"
UnchangedMemNotch_NoEGTA$Memb_Notch_levels <- "Unchanged"

write.csv(IncreasedMemNotch_NoEGTA, "IncreasedMemNotch_NoEGTA.csv")
write.csv(DecreasedMemNotch_NoEGTA, "DecreasedMemNotch_NoEGTA.csv")
write.csv(UnchangedMemNotch_NoEGTA, "UnchangedMemNotch_NoEGTA.csv")

str(IncreasedMemNotch_NoEGTA)
str(DecreasedMemNotch_NoEGTA)
str(UnchangedMemNotch_NoEGTA)

rm(DecreasedMemNotch_NoEGTA, IncreasedMemNotch_NoEGTA, UnchangedMemNotch_NoEGTA)

MemNotch_levels_NoEGTA <- read.csv("MemNotch_levels_NoEGTA.csv")

OTP <- subset(MemNotch_levels_NoEGTA, GeneSymbol == "OTPctrl")

PSENEN_D <- subset(MemNotch_levels_NoEGTA, GeneSymbol == "PSENEN_D")

PSENEN_S <- subset(MemNotch_levels_NoEGTA, GeneSymbol == "PSENEN_S")


ggplot(MemNotch_levels_NoEGTA, aes(N3__MembNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 2) + geom_point(aes(colour = factor(Memb_Notch_levels))) + 
  scale_colour_manual(values = c("red", "#120FF0", "#30874A"))  +theme_bw()+
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("Cell Notch")+
  ylab("Number of Cells Analysed") + 
  geom_point(data = OTP, colour = "#9009B5", size = 3.5, shape =15) + 
  geom_text(data=OTP, label="NEG", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_S, colour = "#000000", size = 3.5, shape =15) + 
  geom_text(data=PSENEN_S, label="POS_S", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_D, colour = "#000000", size = 3.5, shape =15) +
  geom_text(data=PSENEN_D, label="POS_D", vjust = 1.5, fontface="bold")+ 
  ggtitle("Membrane Notch Levels No EGTA") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

INCR <- subset(MemNotch_levels_NoEGTA, N3__MembNotch > 2.5)
DECR <- subset(MemNotch_levels_NoEGTA, N3__MembNotch < -0.5)

write.csv(INCR, "Increased_MemNotch_NoEGTA.csv")
write.csv(DECR, "Decreased_MemNotch_NoEGTA.csv")

# the 2 pos control, PSENEN_S nd PSENEN_D were removed from this list
# because they hd already been labelled, a new edited csv was imported
# to continue the analysis

rm(DECR)
DECR <- read.csv("Decreased_MemNotch_NoEGTA.csv")

ggplot(MemNotch_levels_NoEGTA, aes(N3__MembNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 2) + geom_point(aes(colour = factor(Memb_Notch_levels))) + 
  scale_colour_manual(values = c("red", "#120FF0", "#30874A"))  +theme_bw()+
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("Membrane Notch")+
  ylab("Number of Cells Analysed") + 
  # to add text label to specific points on the plot
  geom_point(data = OTP, colour = "#9009B5", size = 3.5, shape =15) + 
  geom_text(data=OTP, label="NEG", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_S, colour = "#000000", size = 3.5, shape =15) + 
  geom_text(data=PSENEN_S, label="POS_S", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_D, colour = "#000000", size = 3.5, shape =15) +
  geom_text(data=PSENEN_D, label="POS_D", vjust = 1.5, fontface="bold")+ 
  ggtitle("Membrane Notch Levels No EGTA") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + 
  # to add text label to specific class of points on the plot
  geom_text(data = INCR, aes(label=GeneSymbol), size=2.5, vjust = 2) +
  geom_text(data = DECR, aes(label=GeneSymbol), size=2.5, vjust = 2)

rm(DECR, OTP, PSENEN_D, PSENEN_S, MemNotch_levels_NoEGTA, INCR)

# Plots for nuclear notch EGTA

UnchangedNucNotchEGTA <- read.csv("UnchangedNucNotchEGTA.csv")
DecreasedNucNotchViableEGTA <- read.csv("DecreasedNucNotchViableEGTA.csv")
IncreasedNucNotchViableEGTA <- read.csv("IncreasedNucNotchViableEGTA.csv")

UnchangedNucNotchEGTA$Nuc_Notch_levels <- "Unchanged"
DecreasedNucNotchViableEGTA$Nuc_Notch_levels <- "Decreased"
IncreasedNucNotchViableEGTA$Nuc_Notch_levels <- "Increased"

write.csv(UnchangedNucNotchEGTA, "UnchangedNucNotchEGTA.csv")
write.csv(DecreasedNucNotchViableEGTA, "DecreasedNucNotchViableEGTA.csv")
write.csv(IncreasedNucNotchViableEGTA, "IncreasedNucNotchViableEGTA.csv")

rm(DecreasedNucNotchViableEGTA, IncreasedNucNotchViableEGTA, UnchangedNucNotchEGTA)

NucNotch_EGTA <- read.csv("NucNotch_EGTA.csv")

OTP <- subset(NucNotch_EGTA, GeneSymbol == "OTPctrl")

PSENEN_D <- subset(NucNotch_EGTA, GeneSymbol == "PSENEN_D")

PSENEN_S <- subset(NucNotch_EGTA, GeneSymbol == "PSENEN_S")


ggplot(NucNotch_EGTA, aes(N1__NucNotch, QC1__NoOfAnalysedCells)) + 
  geom_point(size = 2) + geom_point(aes(colour = factor(Nuc_Notch_levels))) + 
  scale_colour_manual(values = c("red", "#120FF0", "#30874A"))  +theme_bw()+
  theme(legend.position=c(1,1),legend.justification=c(1,1))+
  xlab("Nuclear Notch")+
  ylab("Number of Cells Analysed") + 
  # labs is the title for the colour legend
  labs (colour = "Nuclear Notch levels") +
  # to add text label to specific points on the plot
  geom_point(data = OTP, colour = "#9009B5", size = 3.5, shape =15) + 
  geom_text(data=OTP, label="NEG", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_S, colour = "#000000", size = 3.5, shape =15) + 
  geom_text(data=PSENEN_S, label="POS_S", vjust=1.5, fontface="bold") + 
  geom_point(data = PSENEN_D, colour = "#000000", size = 3.5, shape =15) +
  geom_text(data=PSENEN_D, label="POS_D", vjust = 1.5, fontface="bold")+ 
  ggtitle("Nuclear Notch EGTA") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 
