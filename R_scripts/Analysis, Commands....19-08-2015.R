19/08/2015

setwd("C:/Users/.../R Data Analysis/...")
dir()

PrimaryScreenData_control <- read.csv("PrimaryScreenData_control.csv")
PrimaryScreenData_EGTA <- read.csv("PrimaryScreenData_EGTA.csv")
PrimaryScreenData <- read.csv("PrimaryScreenData.csv")

CytotoxicEGTA <- subset(PrimaryScreenData_EGTA, PrimaryScreenData_EGTA$QC1__NoOfAnalysedCells < -0.5)
CytotoxicNoEGTA <- subset(PrimaryScreenData_control, PrimaryScreenData_control$QC1__NoOfAnalysedCells < -0.5)
write.csv(CytotoxicNoEGTA, "CytotoxicNoEGTA.csv")
write.csv(CytotoxicEGTA, "CytotoxicEGTA.csv")

#common genes between cytotoxic genes between NoEGTA and EGTA conditions
A <- intersect(CytotoxicNoEGTA$GeneSymbol, CytotoxicEGTA$GeneSymbol)
write.csv(A, "A.csv")

ViableNoEGTA <- subset(PrimaryScreenData_control, PrimaryScreenData_control$QC1__NoOfAnalysedCells > -0.5)
ViableEGTA <- subset(PrimaryScreenData_EGTA, PrimaryScreenData_EGTA$QC1__NoOfAnalysedCells > -0.5)
write.csv(ViableNoEGTA, "ViableNoEGTA.csv")
write.csv(ViableEGTA, "ViableEGTA.csv")

ViableEGTA <- as.data.frame(ViableEGTA)

TotalNotch_reduced <- subset(ViableNoEGTA, ViableNoEGTA$N4__CellNotch < -0.5)
TotalNotch_increased <- subset(ViableNoEGTA, ViableNoEGTA$N4__CellNotch > 3)
TotalNotch_unchanged <- subset(ViableNoEGTA, ViableNoEGTA$N4__CellNotch > -0.5 & ViableNoEGTA$N4__CellNotch < 3)
write.csv(TotalNotch_unchanged, "TotalNotch_unchanged.csv")
write.csv(TotalNotch_reduced, "TotalNotch_reduced.csv")
write.csv(TotalNotch_increased, "TotalNotch_increased.csv")

TotalNotchEGTA_reduced <- subset(ViableEGTA, ViableEGTA$N4__CellNotch < -0.5)
TotalNotchEGTA_increased <- subset(ViableEGTA, ViableEGTA$N4__CellNotch > 2.5)
TotalNotchEGTA_unchanged <- subset(ViableEGTA, ViableEGTA$N4__CellNotch > -0.5 & ViableEGTA$N4__CellNotch < 2.5)
write.csv(TotalNotchEGTA_reduced, "TotalNotchEGTA_reduced.csv")
write.csv(TotalNotchEGTA_unchanged, "TotalNotchEGTA_unchanged.csv")
write.csv(TotalNotchEGTA_increased, "TotalNotchEGTA_increased.csv")

CytoNotch_reduced <- subset(ViableNoEGTA, ViableNoEGTA$N2__CytoNotch < -0.5)
CytoNotch_increased <- subset(ViableNoEGTA, ViableNoEGTA$N2__CytoNotch > 2.5)
CytoNotch_unchanged <- subset(ViableNoEGTA, ViableNoEGTA$N2__CytoNotch > -0.5 & ViableNoEGTA$N2__CytoNotch < 2.5)
write.csv(CytoNotch_reduced, "CytoNotch_reduced.csv")
write.csv(CytoNotch_unchanged, "CytoNotch_unchanged.csv")
write.csv(CytoNotch_increased, "CytoNotch_increased.csv")

CytoNotchEGTA_reduced <- subset(ViableEGTA, ViableEGTA$N2__CytoNotch < -0.5)
CytoNotchEGTA_increased <- subset(ViableEGTA, ViableEGTA$N2__CytoNotch > 2.5)
CytoNotchEGTA_unchanged <- subset(ViableEGTA, ViableEGTA$N2__CytoNotch > -0.5 & ViableEGTA$N2__CytoNotch < 2.5)
write.csv(CytoNotchEGTA_reduced, "CytoNotchEGTA_reduced.csv")
write.csv(CytoNotchEGTA_unchanged, "CytoNotchEGTA_unchanged.csv")
write.csv(CytoNotchEGTA_increased, "CytoNotchEGTA_increased.csv")

SpotNotch_increased <- subset(ViableNoEGTA, ViableNoEGTA$N5__PercentOfCellsWithSpots > 1.7)
write.csv(SpotNotch_increased, "SpotNotch_increased.csv")

SpotNotch_increased <- subset(ViableNoEGTA, ViableNoEGTA$N5__PercentOfCellsWithSpots > 1.7)
write.csv(SpotNotch_increased, "SpotNotch_increased.csv")
SpotNotch_reduced <- subset(ViableNoEGTA, ViableNoEGTA$N5__PercentOfCellsWithSpots < -1 & ViableNoEGTA$N4__CellNotch > -0.5)
write.csv(SpotNotch_reduced, "SpotNotch_reduced.csv")
SpotNotch_unchanged <- subset(ViableNoEGTA, ViableNoEGTA$N5__PercentOfCellsWithSpots > -1 & ViableNoEGTA$N5__PercentOfCellsWithSpots < 1.7)
write.csv(SpotNotch_unchanged, "SpotNotch_unchanged.csv")

SpotNotchEGTA_increased <- subset(ViableEGTA, ViableEGTA$N5__PercentOfCellsWithSpots > 1.3)
write.csv(SpotNotchEGTA_increased, "SpotNotchEGTA_increased.csv")
SpotNotchEGTA_unchanged <- subset(ViableEGTA, ViableEGTA$N5__PercentOfCellsWithSpots < 1.3)
write.csv(SpotNotchEGTA_unchanged, "SpotNotchEGTA_unchanged.csv")


