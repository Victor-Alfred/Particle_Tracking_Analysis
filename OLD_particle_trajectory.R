setwd("Desktop/R_tracking/")

# read in datafiles using   data.frame(read.csv("XXXXXXXX>csv"))



# nanually determine the row coordinate of the starting position on each particle on X and Y-axis
a = which(newtracks[,3]==757)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[X, 8]      #where X is row positon of the original position

write.csv(newtracks, "newtracks.csv")


# calculate X-displacement for each particle
a = which(newtracks[,3]==381)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[2363, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==384)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[2488, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==428)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[2592, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==480)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[2696, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==519)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[2799, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==578)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[2913, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==586)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[3026, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==600)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[3129, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==660)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[3278, 8]

a = which(newtracks[,3]==713)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[3387, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==757)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[3513, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==777)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[3727, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==817)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[3877, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==850)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[3990, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==888)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[4094, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==889)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[4291, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==917)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[4413, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==936)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[4571, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==937)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[4692, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==951)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[4797, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==983)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[4901, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==996)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[5057, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==1002)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[5157, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==1056)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[5298, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==1081)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[5440, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==1124)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[5563, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==1132)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[1137, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==1137)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[5712, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==1180)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[5965, 8]

# calculate X-displacement for each particle
a = which(newtracks[,3]==1283)           #  Particle_ID
newtracks[a, 14] <- newtracks[a, 8] - newtracks[6072, 8]




### CALCULATE Y DISPLACEMENT ...COL 9


a = which(newtracks[,3]==5)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[1, 9]

a = which(newtracks[,3]==12)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[126, 9]

a = which(newtracks[,3]==22)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[265, 9]

a = which(newtracks[,3]==37)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[366, 9]

a = which(newtracks[,3]==51)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[491, 9]

a = which(newtracks[,3]==58)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[626, 9]

a = which(newtracks[,3]==122)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[756, 9]

a = which(newtracks[,3]==130)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[858, 9]

a = which(newtracks[,3]==174)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[970, 9]

a = which(newtracks[,3]==191)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[1082, 9]

a = which(newtracks[,3]==192)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[1202, 9]

a = which(newtracks[,3]==199)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[1342, 9]

a = which(newtracks[,3]==222)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[1446, 9]

a = which(newtracks[,3]==227)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[1550, 9]

a = which(newtracks[,3]==234)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[1725, 9]

a = which(newtracks[,3]==292)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[1830, 9]

a = which(newtracks[,3]==352)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[2027, 9]

# calculate X-displacement for each particle
a = which(newtracks[,3]==356)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[2135, 9]

a = which(newtracks[,3]==377)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[2247, 9]

a = which(newtracks[,3]==381)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[2363, 9]

a = which(newtracks[,3]==384)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[2488, 9]

a = which(newtracks[,3]==428)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[2592, 9]

a = which(newtracks[,3]==480)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[2696, 9]

a = which(newtracks[,3]==519)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[2799, 9]

a = which(newtracks[,3]==578)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[2913, 9]

a = which(newtracks[,3]==586)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[3026, 9]

a = which(newtracks[,3]==600)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[3129, 9]

a = which(newtracks[,3]==660)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[3278, 9]

a = which(newtracks[,3]==713)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[3387, 9]

a = which(newtracks[,3]==757)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[3513, 9]

a = which(newtracks[,3]==777)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[3727, 9]

a = which(newtracks[,3]==817)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[3877, 9]

a = which(newtracks[,3]==850)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[3990, 9]

a = which(newtracks[,3]==888)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[4094, 9]

a = which(newtracks[,3]==889)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[4291, 9]

a = which(newtracks[,3]==917)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[4413, 9]

a = which(newtracks[,3]==936)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[4571, 9]

a = which(newtracks[,3]==937)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[4692, 9]

a = which(newtracks[,3]==951)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[4797, 9]

a = which(newtracks[,3]==983)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[4901, 9]

a = which(newtracks[,3]==996)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[5057, 9]

a = which(newtracks[,3]==1002)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[5157, 9]

a = which(newtracks[,3]==1056)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[5298, 9]

a = which(newtracks[,3]==1081)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[5440, 9]

a = which(newtracks[,3]==1124)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[5563, 9]

a = which(newtracks[,3]==1132)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[1137, 9]

a = which(newtracks[,3]==1137)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[5712, 9]

a = which(newtracks[,3]==1180)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[5965, 9]

a = which(newtracks[,3]==1283)           #  Particle_ID
newtracks[a, 15] <- newtracks[a, 9] - newtracks[6072, 9]


# write all new changes to the dataset
write.csv(newtracks, "newtracks.csv")


# create new column of the dislacement angle using arctangent
newtracks[,, 18] <- (atan2(newtracks$DISP_Y, newtracks$DISP_X)*180)/pi

# create new column afor mean-square displacement and rename it 'MEAN_DISP'
newtracks[, 18] <- sqrt((DISP_X)^2 + (DISP_Y)^2)


# new function that adds 180 to every negative angle
pos_angles <- function(x) { 
  if(x < 0) x <- x + 180
  return(x)
}

# apply function to the ANGLES column of the dataframe, creating a
# new column called NEW_ANGLE
newtracks$NEW_ANGLE <- sapply(newtracks$ANGLE, pos_angles)

# write all changes to dataframe
write.csv(newtracks, "newtracks.csv")


# The histogram shape comparing between the +ve and -ve X displacments should not necesaarily be similar
# but should be similar for Y (if displacement is due to MTs MTs are oriented equally along the Y-length)

# create subsets of negative and positive long-axis(Y) displacements
DISP_Y_POS <- subset(newtracks, DISP_Y >= 0)
DISP_Y_NEG <- subset(newtracks, DISP_Y < 0)

# Also create subsets of negative and positive short-axis(X) displacements
DISP_X_POS <- subset(newtracks, DISP_X >= 0)
DISP_X_NEG <- subset(newtracks, DISP_X < 0)

# PLOT ANGLE VS DISPLACEMENT (or velocity?) as histogram
#separate plots for positive and negative Y-displacements
plot(newtracks$ANGLE, newtracks$MEAN_DISP, type = "h", lwd = 3) 

#Also create plots for -ve Y-disp, +ve Y disp, -ve X-disp, +ve X-disp

#How do I make the Y-axis displacement?
qplot(newtracks$NEW_ANGLE, geom="histogram", binwidth = 10, fill=I("gray"), col=I("black")) 


######################################################################

tracks_embr4_long <- subset(tracks_embr4, ave(TRACK_ID, TRACK_ID, FUN = length) > 80)
library(dplyr)
count(tracks_embr4_long, TRACKS_ID)
table(tracks_embr4_long$TRACK_ID)


library(ggplot2)

gg <- ggplot()
gg <- gg + geom_point(data=tracks_long_edit, aes(x=EDGE_TIME, y=Y_DISP, color=TRACK_ID, 
                                                 size=5, alpha=0.3))
gg <- gg + geom_line(data=depth.cv, aes(x=asDate, y=meandepth, size=cv))

###############################

newtracks <- data.frame(read.csv("newtracks.csv"))
max(table(newtracks$TRACK_ID))
# create dataframe subset of only those tracks that occur in at least half of the time points
newtracks_edited <- subset(newtracks, ave(TRACK_ID, TRACK_ID, FUN = length) > 30)
write.csv(newtracks_edited, "newtracks_edited.csv")
newtracks_edited <- data.frame(read.csv("newtracks_edited.csv"))  # save to csv


tracks_3 <- subset(newtracks_edited, TRACK_ID == 3)
tracks_6 <- subset(newtracks_edited, TRACK_ID == 6)
tracks_11 <- subset(newtracks_edited, TRACK_ID == 11)
tracks_16 <- subset(newtracks_edited, TRACK_ID == 16)
tracks_23 <- subset(newtracks_edited, TRACK_ID == 23)
tracks_27 <- subset(newtracks_edited, TRACK_ID == 27)
tracks_36 <- subset(newtracks_edited, TRACK_ID == 36)
tracks_49 <- subset(newtracks_edited, TRACK_ID == 49)
tracks_77 <- subset(newtracks_edited, TRACK_ID == 77)
tracks_89 <- subset(newtracks_edited, TRACK_ID == 89)
tracks_95 <- subset(newtracks_edited, TRACK_ID == 95)
tracks_105 <- subset(newtracks_edited, TRACK_ID == 105)
tracks_109 <- subset(newtracks_edited, TRACK_ID == 109)
tracks_125 <- subset(newtracks_edited, TRACK_ID == 125)
tracks_134 <- subset(newtracks_edited, TRACK_ID == 134)
tracks_137 <- subset(newtracks_edited, TRACK_ID == 137)
tracks_140 <- subset(newtracks_edited, TRACK_ID == 140)
tracks_141 <- subset(newtracks_edited, TRACK_ID == 141)
tracks_152 <- subset(newtracks_edited, TRACK_ID == 152)
tracks_153 <- subset(newtracks_edited, TRACK_ID == 153)
tracks_160 <- subset(newtracks_edited, TRACK_ID == 160)
tracks_196 <- subset(newtracks_edited, TRACK_ID == 196)
tracks_201 <- subset(newtracks_edited, TRACK_ID == 201)
tracks_204 <- subset(newtracks_edited, TRACK_ID == 204)
tracks_211 <- subset(newtracks_edited, TRACK_ID == 211)
tracks_218 <- subset(newtracks_edited, TRACK_ID == 218)
tracks_255 <- subset(newtracks_edited, TRACK_ID == 255)
tracks_271 <- subset(newtracks_edited, TRACK_ID == 271)
tracks_285 <- subset(newtracks_edited, TRACK_ID == 285)
tracks_286 <- subset(newtracks_edited, TRACK_ID == 286)
tracks_296 <- subset(newtracks_edited, TRACK_ID == 296)
tracks_335 <- subset(newtracks_edited, TRACK_ID == 335)
tracks_339 <- subset(newtracks_edited, TRACK_ID == 339)
tracks_343 <- subset(newtracks_edited, TRACK_ID == 343)
tracks_360 <- subset(newtracks_edited, TRACK_ID == 360)
tracks_362 <- subset(newtracks_edited, TRACK_ID == 362)
tracks_407 <- subset(newtracks_edited, TRACK_ID == 407)
tracks_419 <- subset(newtracks_edited, TRACK_ID == 419)



library(ggplot2)

#### long axis

ggplot(tracks_3,aes(TIME_MS,LONG_AXIS))+geom_line(aes(color="Particle_3"))+
  geom_line(data=tracks_6,aes(color="Particle_6"))+
  geom_point(data=tracks_6,aes(color="Particle_6"), size = 1)+
  geom_line(data=tracks_11,aes(color="Particle_11"))+
  geom_point(data=tracks_11,aes(color="Particle_11"), size = 1)+
  geom_line(data=tracks_16,aes(color="Particle_16"))+
  geom_point(data=tracks_16,aes(color="Particle_16"), size = 1)+
  geom_line(data=tracks_23,aes(color="Particle_23"))+
  geom_point(data=tracks_23,aes(color="Particle_23"), size = 1)+
  geom_line(data=tracks_27,aes(color="Particle_27"))+
  geom_point(data=tracks_27,aes(color="Particle_27"), size = 1)+
  geom_line(data=tracks_36,aes(color="Particle_36"))+
  geom_point(data=tracks_36,aes(color="Particle_36"), size = 1)+
  geom_line(data=tracks_49,aes(color="Particle_49"))+
  geom_point(data=tracks_49,aes(color="Particle_49"), size = 1)+
  geom_line(data=tracks_77,aes(color="Particle_77"))+
  geom_point(data=tracks_77,aes(color="Particle_77"), size = 1)+
  geom_line(data=tracks_89,aes(color="Particle_89"))+
  geom_point(data=tracks_89,aes(color="Particle_89"), size = 1)+
  geom_line(data=tracks_95,aes(color="Particle_95"))+
  geom_point(data=tracks_95,aes(color="Particle_95"), size = 1)+
  geom_line(data=tracks_105,aes(color="Particle_105"))+
  geom_point(data=tracks_105,aes(color="Particle_105"), size = 1)+
  geom_line(data=tracks_109,aes(color="Particle_109"))+
  geom_point(data=tracks_109,aes(color="Particle_109"), size = 1)+
  geom_line(data=tracks_125,aes(color="Particle_125"))+
  geom_point(data=tracks_125,aes(color="Particle_125"), size = 1)+
  geom_line(data=tracks_134,aes(color="Particle_134"))+
  geom_point(data=tracks_134,aes(color="Particle_134"), size = 1)+
  geom_line(data=tracks_137,aes(color="Particle_137"))+
  geom_point(data=tracks_137,aes(color="Particle_137"), size = 1)+
  geom_line(data=tracks_140,aes(color="Particle_140"))+
  geom_point(data=tracks_140,aes(color="Particle_140"), size = 1)+
  geom_line(data=tracks_141,aes(color="Particle_141"))+
  geom_point(data=tracks_141,aes(color="Particle_141"), size = 1)+
  geom_line(data=tracks_152,aes(color="Particle_152"))+
  geom_point(data=tracks_152,aes(color="Particle_152"), size = 1)+
  geom_line(data=tracks_153,aes(color="Particle_153"))+
  geom_point(data=tracks_153,aes(color="Particle_153"), size = 1)+
  geom_line(data=tracks_160,aes(color="Particle_160"))+
  geom_point(data=tracks_160,aes(color="Particle_160"), size = 1)+
  geom_line(data=tracks_196,aes(color="Particle_196"))+
  geom_point(data=tracks_196,aes(color="Particle_196"), size = 1)+
  geom_line(data=tracks_201,aes(color="Particle_201"))+
  geom_point(data=tracks_201,aes(color="Particle_201"), size = 1)+
  geom_line(data=tracks_204,aes(color="Particle_204"))+
  geom_point(data=tracks_204,aes(color="Particle_204"), size = 1)+
  geom_line(data=tracks_211,aes(color="Particle_211"))+
  geom_point(data=tracks_211,aes(color="Particle_211"), size = 1)+
  geom_line(data=tracks_218,aes(color="Particle_218"))+
  geom_point(data=tracks_218,aes(color="Particle_218"), size = 1)+
  geom_line(data=tracks_255,aes(color="Particle_255"))+
  geom_point(data=tracks_255,aes(color="Particle_255"), size = 1)+
  geom_line(data=tracks_271,aes(color="Particle_271"))+
  geom_point(data=tracks_271,aes(color="Particle_271"), size = 1)+
  geom_line(data=tracks_285,aes(color="Particle_285"))+
  geom_point(data=tracks_285,aes(color="Particle_285"), size = 1)+
  geom_line(data=tracks_296,aes(color="Particle_296"))+
  geom_point(data=tracks_296,aes(color="Particle_296"), size = 1)+
  geom_line(data=tracks_335,aes(color="Particle_335"))+
  geom_point(data=tracks_335,aes(color="Particle_335"), size = 1)+
  geom_line(data=tracks_339,aes(color="Particle_339"))+
  geom_point(data=tracks_339,aes(color="Particle_339"), size = 1)+
  geom_line(data=tracks_343,aes(color="Particle_343"))+
  geom_point(data=tracks_343,aes(color="Particle_343"), size = 1)+
  geom_line(data=tracks_360,aes(color="Particle_360"))+
  geom_point(data=tracks_360,aes(color="Particle_360"), size = 1)+
  geom_line(data=tracks_362,aes(color="Particle_362"))+
  geom_point(data=tracks_362,aes(color="Particle_362"), size = 1)+
  geom_line(data=tracks_407,aes(color="Particle_407"))+
  geom_point(data=tracks_407,aes(color="Particle_407"), size = 1)+
  geom_line(data=tracks_419,aes(color="Particle_419"))+
  geom_point(data=tracks_419,aes(color="Particle_419"), size = 1)+
  xlab("Time(s)")+
  ylab("Displacement along long axis")+
  scale_y_continuous(limits=c(-3.5, 3.5))


ggplot(tracks_7,aes(TIME_MS,LONG_AXIS))+geom_line(aes(color="Particle_7"))+
  geom_line(data=tracks_24,aes(color="Particle_24"))+
  geom_line(data=tracks_33,aes(color="Particle_33"))+
  geom_line(data=tracks_36,aes(color="Particle_36"))+
  geom_line(data=tracks_64,aes(color="Particle_64"))+
  geom_line(data=tracks_79,aes(color="Particle_79"))+
  geom_line(data=tracks_83,aes(color="Particle_83"))+
  geom_line(data=tracks_98,aes(color="Particle_98"))+
  geom_line(data=tracks_107,aes(color="Particle_107"))+
  geom_line(data=tracks_153,aes(color="Particle_153"))+
  geom_line(data=tracks_163,aes(color="Particle_163"))+
  geom_line(data=tracks_253,aes(color="Particle_253"))+
  theme_bw()+
  xlab("Time(s)")+
  ylab("Displacement along long axis")+
  scale_y_continuous(limits=c(-3.5, 3.5))


#### short axis

ggplot(tracks_3,aes(TIME_MS,SHORT_AXIS))+geom_line(aes(color="Particle_3"))+
  geom_line(data=tracks_6,aes(color="Particle_6"))+
  geom_point(data=tracks_6,aes(color="Particle_6"), size = 1)+
  geom_line(data=tracks_11,aes(color="Particle_11"))+
  geom_point(data=tracks_11,aes(color="Particle_11"), size = 1)+
  geom_line(data=tracks_16,aes(color="Particle_16"))+
  geom_point(data=tracks_16,aes(color="Particle_16"), size = 1)+
  geom_line(data=tracks_23,aes(color="Particle_23"))+
  geom_point(data=tracks_23,aes(color="Particle_23"), size = 1)+
  geom_line(data=tracks_27,aes(color="Particle_27"))+
  geom_point(data=tracks_27,aes(color="Particle_27"), size = 1)+
  geom_line(data=tracks_36,aes(color="Particle_36"))+
  geom_point(data=tracks_36,aes(color="Particle_36"), size = 1)+
  geom_line(data=tracks_49,aes(color="Particle_49"))+
  geom_point(data=tracks_49,aes(color="Particle_49"), size = 1)+
  geom_line(data=tracks_77,aes(color="Particle_77"))+
  geom_point(data=tracks_77,aes(color="Particle_77"), size = 1)+
  geom_line(data=tracks_89,aes(color="Particle_89"))+
  geom_point(data=tracks_89,aes(color="Particle_89"), size = 1)+
  geom_line(data=tracks_95,aes(color="Particle_95"))+
  geom_point(data=tracks_95,aes(color="Particle_95"), size = 1)+
  geom_line(data=tracks_105,aes(color="Particle_105"))+
  geom_point(data=tracks_105,aes(color="Particle_105"), size = 1)+
  geom_line(data=tracks_109,aes(color="Particle_109"))+
  geom_point(data=tracks_109,aes(color="Particle_109"), size = 1)+
  geom_line(data=tracks_125,aes(color="Particle_125"))+
  geom_point(data=tracks_125,aes(color="Particle_125"), size = 1)+
  geom_line(data=tracks_134,aes(color="Particle_134"))+
  geom_point(data=tracks_134,aes(color="Particle_134"), size = 1)+
  geom_line(data=tracks_137,aes(color="Particle_137"))+
  geom_point(data=tracks_137,aes(color="Particle_137"), size = 1)+
  geom_line(data=tracks_140,aes(color="Particle_140"))+
  geom_point(data=tracks_140,aes(color="Particle_140"), size = 1)+
  geom_line(data=tracks_141,aes(color="Particle_141"))+
  geom_point(data=tracks_141,aes(color="Particle_141"), size = 1)+
  geom_line(data=tracks_152,aes(color="Particle_152"))+
  geom_point(data=tracks_152,aes(color="Particle_152"), size = 1)+
  geom_line(data=tracks_153,aes(color="Particle_153"))+
  geom_point(data=tracks_153,aes(color="Particle_153"), size = 1)+
  geom_line(data=tracks_160,aes(color="Particle_160"))+
  geom_point(data=tracks_160,aes(color="Particle_160"), size = 1)+
  geom_line(data=tracks_196,aes(color="Particle_196"))+
  geom_point(data=tracks_196,aes(color="Particle_196"), size = 1)+
  geom_line(data=tracks_201,aes(color="Particle_201"))+
  geom_point(data=tracks_201,aes(color="Particle_201"), size = 1)+
  geom_line(data=tracks_204,aes(color="Particle_204"))+
  geom_point(data=tracks_204,aes(color="Particle_204"), size = 1)+
  geom_line(data=tracks_211,aes(color="Particle_211"))+
  geom_point(data=tracks_211,aes(color="Particle_211"), size = 1)+
  geom_line(data=tracks_218,aes(color="Particle_218"))+
  geom_point(data=tracks_218,aes(color="Particle_218"), size = 1)+
  geom_line(data=tracks_255,aes(color="Particle_255"))+
  geom_point(data=tracks_255,aes(color="Particle_255"), size = 1)+
  geom_line(data=tracks_271,aes(color="Particle_271"))+
  geom_point(data=tracks_271,aes(color="Particle_271"), size = 1)+
  geom_line(data=tracks_285,aes(color="Particle_285"))+
  geom_point(data=tracks_285,aes(color="Particle_285"), size = 1)+
  geom_line(data=tracks_296,aes(color="Particle_296"))+
  geom_point(data=tracks_296,aes(color="Particle_296"), size = 1)+
  geom_line(data=tracks_335,aes(color="Particle_335"))+
  geom_point(data=tracks_335,aes(color="Particle_335"), size = 1)+
  geom_line(data=tracks_339,aes(color="Particle_339"))+
  geom_point(data=tracks_339,aes(color="Particle_339"), size = 1)+
  geom_line(data=tracks_343,aes(color="Particle_343"))+
  geom_point(data=tracks_343,aes(color="Particle_343"), size = 1)+
  geom_line(data=tracks_360,aes(color="Particle_360"))+
  geom_point(data=tracks_360,aes(color="Particle_360"), size = 1)+
  geom_line(data=tracks_362,aes(color="Particle_362"))+
  geom_point(data=tracks_362,aes(color="Particle_362"), size = 1)+
  geom_line(data=tracks_407,aes(color="Particle_407"))+
  geom_point(data=tracks_407,aes(color="Particle_407"), size = 1)+
  geom_line(data=tracks_419,aes(color="Particle_419"))+
  geom_point(data=tracks_419,aes(color="Particle_419"), size = 1)+
  xlab("Time(s)")+
  ylab("Displacement along short axis")+
  scale_y_continuous(limits=c(-3.5, 3.5))


ggplot(tracks_7,aes(TIME_MS,SHORT_AXIS))+geom_line(aes(color="Particle_7"))+
  geom_line(data=tracks_24,aes(color="Particle_24"))+
  geom_line(data=tracks_33,aes(color="Particle_33"))+
  geom_line(data=tracks_36,aes(color="Particle_36"))+
  geom_line(data=tracks_64,aes(color="Particle_64"))+
  geom_line(data=tracks_79,aes(color="Particle_79"))+
  geom_line(data=tracks_83,aes(color="Particle_83"))+
  geom_line(data=tracks_98,aes(color="Particle_98"))+
  geom_line(data=tracks_107,aes(color="Particle_107"))+
  geom_line(data=tracks_153,aes(color="Particle_153"))+
  geom_line(data=tracks_163,aes(color="Particle_163"))+
  geom_line(data=tracks_253,aes(color="Particle_253"))+
  theme_bw()+
  xlab("Time(s)")+
  ylab("Displacement along short axis")+
  scale_y_continuous(limits=c(-3.5, 3.5))

###### SHORT VS LONG

ggplot(tracks_3,aes(SHORT_AXIS,LONG_AXIS))+geom_line(aes(color="Particle_3"))+
  geom_line(data=tracks_6,aes(color="Particle_6"))+
  geom_point(data=tracks_6,aes(color="Particle_6"), size = 1)+
  geom_line(data=tracks_11,aes(color="Particle_11"))+
  geom_point(data=tracks_11,aes(color="Particle_11"), size = 1)+
  geom_line(data=tracks_16,aes(color="Particle_16"))+
  geom_point(data=tracks_16,aes(color="Particle_16"), size = 1)+
  geom_line(data=tracks_23,aes(color="Particle_23"))+
  geom_point(data=tracks_23,aes(color="Particle_23"), size = 1)+
  geom_line(data=tracks_27,aes(color="Particle_27"))+
  geom_point(data=tracks_27,aes(color="Particle_27"), size = 1)+
  geom_line(data=tracks_36,aes(color="Particle_36"))+
  geom_point(data=tracks_36,aes(color="Particle_36"), size = 1)+
  geom_line(data=tracks_49,aes(color="Particle_49"))+
  geom_point(data=tracks_49,aes(color="Particle_49"), size = 1)+
  geom_line(data=tracks_77,aes(color="Particle_77"))+
  geom_point(data=tracks_77,aes(color="Particle_77"), size = 1)+
  geom_line(data=tracks_89,aes(color="Particle_89"))+
  geom_point(data=tracks_89,aes(color="Particle_89"), size = 1)+
  geom_line(data=tracks_95,aes(color="Particle_95"))+
  geom_point(data=tracks_95,aes(color="Particle_95"), size = 1)+
  geom_line(data=tracks_105,aes(color="Particle_105"))+
  geom_point(data=tracks_105,aes(color="Particle_105"), size = 1)+
  geom_line(data=tracks_109,aes(color="Particle_109"))+
  geom_point(data=tracks_109,aes(color="Particle_109"), size = 1)+
  geom_line(data=tracks_125,aes(color="Particle_125"))+
  geom_point(data=tracks_125,aes(color="Particle_125"), size = 1)+
  geom_line(data=tracks_134,aes(color="Particle_134"))+
  geom_point(data=tracks_134,aes(color="Particle_134"), size = 1)+
  geom_line(data=tracks_137,aes(color="Particle_137"))+
  geom_point(data=tracks_137,aes(color="Particle_137"), size = 1)+
  geom_line(data=tracks_140,aes(color="Particle_140"))+
  geom_point(data=tracks_140,aes(color="Particle_140"), size = 1)+
  geom_line(data=tracks_141,aes(color="Particle_141"))+
  geom_point(data=tracks_141,aes(color="Particle_141"), size = 1)+
  geom_line(data=tracks_152,aes(color="Particle_152"))+
  geom_point(data=tracks_152,aes(color="Particle_152"), size = 1)+
  geom_line(data=tracks_153,aes(color="Particle_153"))+
  geom_point(data=tracks_153,aes(color="Particle_153"), size = 1)+
  geom_line(data=tracks_160,aes(color="Particle_160"))+
  geom_point(data=tracks_160,aes(color="Particle_160"), size = 1)+
  geom_line(data=tracks_196,aes(color="Particle_196"))+
  geom_point(data=tracks_196,aes(color="Particle_196"), size = 1)+
  geom_line(data=tracks_201,aes(color="Particle_201"))+
  geom_point(data=tracks_201,aes(color="Particle_201"), size = 1)+
  geom_line(data=tracks_204,aes(color="Particle_204"))+
  geom_point(data=tracks_204,aes(color="Particle_204"), size = 1)+
  geom_line(data=tracks_211,aes(color="Particle_211"))+
  geom_point(data=tracks_211,aes(color="Particle_211"), size = 1)+
  geom_line(data=tracks_218,aes(color="Particle_218"))+
  geom_point(data=tracks_218,aes(color="Particle_218"), size = 1)+
  geom_line(data=tracks_255,aes(color="Particle_255"))+
  geom_point(data=tracks_255,aes(color="Particle_255"), size = 1)+
  geom_line(data=tracks_271,aes(color="Particle_271"))+
  geom_point(data=tracks_271,aes(color="Particle_271"), size = 1)+
  geom_line(data=tracks_285,aes(color="Particle_285"))+
  geom_point(data=tracks_285,aes(color="Particle_285"), size = 1)+
  geom_line(data=tracks_296,aes(color="Particle_296"))+
  geom_point(data=tracks_296,aes(color="Particle_296"), size = 1)+
  geom_line(data=tracks_335,aes(color="Particle_335"))+
  geom_point(data=tracks_335,aes(color="Particle_335"), size = 1)+
  geom_line(data=tracks_339,aes(color="Particle_339"))+
  geom_point(data=tracks_339,aes(color="Particle_339"), size = 1)+
  geom_line(data=tracks_343,aes(color="Particle_343"))+
  geom_point(data=tracks_343,aes(color="Particle_343"), size = 1)+
  geom_line(data=tracks_360,aes(color="Particle_360"))+
  geom_point(data=tracks_360,aes(color="Particle_360"), size = 1)+
  geom_line(data=tracks_362,aes(color="Particle_362"))+
  geom_point(data=tracks_362,aes(color="Particle_362"), size = 1)+
  geom_line(data=tracks_407,aes(color="Particle_407"))+
  geom_point(data=tracks_407,aes(color="Particle_407"), size = 1)+
  geom_line(data=tracks_419,aes(color="Particle_419"))+
  geom_point(data=tracks_419,aes(color="Particle_419"), size = 1)+
  xlab("Displacement along short axis")+
  ylab("Displacement along long axis")+
  scale_x_continuous(limits=c(-3.5, 3.5))+
  scale_y_continuous(limits=c(-3.5, 3.5))

## coord_cartesian(ylim=c(-4,4))+
## coord_cartesian(xlim=c(-4,4))+


ggplot(tracks_7,aes(SHORT_AXIS,LONG_AXIS))+geom_line(aes(color="Particle_7"))+
  geom_line(data=tracks_24,aes(color="Particle_24"))+
  geom_line(data=tracks_33,aes(color="Particle_33"))+
  geom_line(data=tracks_36,aes(color="Particle_36"))+
  geom_line(data=tracks_64,aes(color="Particle_64"))+
  geom_line(data=tracks_79,aes(color="Particle_79"))+
  geom_line(data=tracks_83,aes(color="Particle_83"))+
  geom_line(data=tracks_98,aes(color="Particle_98"))+
  geom_line(data=tracks_107,aes(color="Particle_107"))+
  geom_line(data=tracks_153,aes(color="Particle_153"))+
  geom_line(data=tracks_163,aes(color="Particle_163"))+
  geom_line(data=tracks_253,aes(color="Particle_253"))+
  theme_bw()+
  xlab("Displacement along short axis")+
  ylab("Displacement along long axis")



##########################################################################################



### Modify this for further analysis, grouping by TRACK_ID

grp1 <- subset(qpcr, Sample == "1a" | Sample == "3a" | Sample == "5a" | Sample == "7a")

grp2 <- subset(qpcr, Sample == "1b" | Sample == "3b" | Sample == "5b" | Sample == "7b")

grp3 <- subset(qpcr, Sample == "2a" | Sample == "4a" | Sample == "6a" | Sample == "8a")

grp4 <- subset(qpcr, Sample == "2b" | Sample == "4b" | Sample == "6b" | Sample == "8b")

# grp1 <- 1a, 3a, 5a, 7a

grp1 <- subset(qpcr, Sample == "1a" | Sample == "3a" | Sample == "5a" | Sample == "7a")

grp2 <- subset(qpcr, Sample == "1b" | Sample == "3b" | Sample == "5b" | Sample == "7b")

grp3 <- subset(qpcr, Sample == "2a" | Sample == "4a" | Sample == "6a" | Sample == "8a")

grp4 <- subset(qpcr, Sample == "2b" | Sample == "4b" | Sample == "6b" | Sample == "8b")

write.csv(grp1, "grp1.csv")

write.csv(grp2, "grp2.csv")

write.csv(grp3, "grp3.csv")

write.csv(grp4, "grp4.csv")

## Summarise data using syummarySE function applied to grpA
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

# This handles one numerical variable column
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

grp1_summary<- summarySE(grp1, measurevar="Ct",
                         groupvars=c("Sample", "gene"))

write.csv(grp1_summary, "grp1_summary.csv")

grp2_summary<- summarySE(grp2, measurevar="Ct",
                         groupvars=c("Sample", "gene"))

write.csv(grp2_summary, "grp2_summary.csv")

grp3_summary<- summarySE(grp3, measurevar="Ct",
                         groupvars=c("Sample", "gene"))

write.csv(grp3_summary, "grp3_summary.csv")

grp4_summary<- summarySE(grp4, measurevar="Ct",
                         groupvars=c("Sample", "gene"))

write.csv(grp4_summary, "grp4_summary.csv")

qpcr_summary<- summarySE(qpcr, measurevar="Ct",
                         groupvars=c("gene", "Sample"))

write.csv(qpcr_summary, "qpcr_summary.csv")
