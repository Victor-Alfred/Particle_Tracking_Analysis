# script to analyse results of particle tracking

setwd("C:/Users/spind/OneDrive - sheffield.ac.uk/1_Live_analysis/white/") # on Home PC

setwd("/Users/victoralfred/Desktop/1_Live_analysis/white")

rm(list=ls())

library(ggplot2)
library(dplyr)
library(lattice)
library(RColorBrewer)
# display.brewer.all()



# read in datafiles using   data.frame(read.csv("XXXXXXXX>csv"))
newtracks <- data.frame(read.csv("small_box_tracks.csv")) 

# create dataframe subset of only those tracks that occur in at least half of the time points
max(table(newtracks$TRACK_ID))
newtracks <- subset(newtracks, ave(TRACK_ID, TRACK_ID, FUN = length) >
                      (max(table(newtracks$TRACK_ID))/2))

# identify firstrows for each particle
newtracks_firstrows <- newtracks[ diff(c(0,newtracks$TRACK_ID)) != 0, ]

# new function that adds 180 to every negative angle
pos_angles <- function(x) { 
  if(x < 0) x <- x + 180
  return(x)
}

#########################

# function to calculate displacements and angles

# used with the "links in tracks" file from trackmate

tracks_calc <- function (a, b){
  c <- within(merge(a,b,by="TRACK_ID"), {
    EDGE_TIME <- EDGE_TIME.x
    TIME <- EDGE_TIME.x - EDGE_TIME.y
    X_LOCATION <- EDGE_X_LOCATION.x
    Y_LOCATION <- EDGE_Y_LOCATION.x
    DISP_X <- EDGE_X_LOCATION.x - EDGE_X_LOCATION.y # .y is the origin
    DISP_Y <- EDGE_Y_LOCATION.x - EDGE_Y_LOCATION.y
    DISP <- sqrt((DISP_X)^2 + (DISP_Y)^2)
    DISP_TYPE <- ifelse(DISP_Y >=0, "POS", "NEG")
    ANGLE <- sapply((atan2(DISP_Y, DISP_X)*180)/pi, pos_angles)
    ANGLE_RAD <- (ANGLE*pi)/180
    # ANGLE_CAT<-cut(ANGLE, seq(0,180,4), include.lowest = TRUE)
    VELOCITY_AUT <- VELOCITY.x  # automatically calculated velocity
    X_SINGLE <- ave(X_LOCATION, TRACK_ID,
                    FUN=function(x) c(0, diff(x))) # individual displacements along X
    Y_SINGLE <- ave(Y_LOCATION, TRACK_ID,   #individul displacements along Y
                    FUN=function(x) c(0, diff(x)))
    DISP_2 <- sqrt((X_SINGLE)^2 + (Y_SINGLE)^2)  # individual displacements
    ANGLE_SINGLE <- sapply((atan2(Y_SINGLE, X_SINGLE)*180)/pi, pos_angles)
    ANGLE_SINGLE_RAD <- (ANGLE_SINGLE*pi)/180
    ANGLE_CAT <-cut(ANGLE_SINGLE, seq(0,180,4), include.lowest = TRUE)
    TIME_SINGLE <- ave(TIME, TRACK_ID,
                       FUN=function(x) c(0, diff(x)))
    VELOCITY <- DISP_2/TIME_SINGLE
  })[,c("TRACK_ID","EDGE_TIME", "TIME","X_LOCATION", "Y_LOCATION",
        "DISP_X","DISP_Y", "DISP","DISP_TYPE", "ANGLE", "ANGLE_RAD", 
        "VELOCITY_AUT", "X_SINGLE", "Y_SINGLE", "DISP_2", "ANGLE_SINGLE", "ANGLE_SINGLE_RAD",
        "ANGLE_CAT", "TIME_SINGLE", "VELOCITY")]
  return(as.data.frame(c))
}

#####################################
 # used with the "spots in tracks" file from trackmate

tracks_calc <- function (a, b){
  c <- within(merge(a,b,by="TRACK_ID"), {
    EDGE_TIME <- POSITION_T.x
    TIME <- POSITION_T.x - POSITION_T.y
    X_LOCATION <- POSITION_X.x
    Y_LOCATION <- POSITION_Y.x
    DISP_X <- POSITION_X.x - POSITION_X.y # .y is the origin
    DISP_Y <- POSITION_Y.x - POSITION_Y.y
    DISP <- sqrt((DISP_X)^2 + (DISP_Y)^2)
    DISP_TYPE <- ifelse(DISP_Y >=0, "POS", "NEG")
    ANGLE <- sapply((atan2(DISP_Y, DISP_X)*180)/pi, pos_angles)
    ANGLE_RAD <- (ANGLE*pi)/180
    # ANGLE_CAT<-cut(ANGLE, seq(0,180,4), include.lowest = TRUE)
    X_SINGLE <- ave(X_LOCATION, TRACK_ID,
                    FUN=function(x) c(0, diff(x))) # individual displacements along X
    Y_SINGLE <- ave(Y_LOCATION, TRACK_ID,   #individul displacements along Y
                    FUN=function(x) c(0, diff(x)))
    DISP_2 <- sqrt((X_SINGLE)^2 + (Y_SINGLE)^2)  # individual displacements
    ANGLE_SINGLE <- sapply((atan2(Y_SINGLE, X_SINGLE)*180)/pi, pos_angles)
    ANGLE_SINGLE_RAD <- (ANGLE_SINGLE*pi)/180
    ANGLE_CAT <-cut(ANGLE_SINGLE, seq(0,180,4), include.lowest = TRUE)
    TIME_SINGLE <- ave(EDGE_TIME, TRACK_ID,
                       FUN=function(x) c(0, diff(x)))
    VELOCITY <- DISP_2/TIME_SINGLE
  })[,c("TRACK_ID","EDGE_TIME", "TIME","X_LOCATION", "Y_LOCATION",
        "DISP_X","DISP_Y", "DISP","DISP_TYPE", "ANGLE", "ANGLE_RAD", "X_SINGLE", "Y_SINGLE", "DISP_2", "ANGLE_SINGLE", "ANGLE_SINGLE_RAD",
        "ANGLE_CAT", "TIME_SINGLE", "VELOCITY")]
  return(as.data.frame(c))
}


##################

tracks_summary <- tracks_calc(newtracks, newtracks_firstrows)

# write.csv(tracks_summary, "W01.csv")  # save new dataframe
# tracks <- as.data.frame(read.csv("tracks_summary.csv"))

# create subsets of negative and positive long-axis(Y) displacements
Y_POS <- subset(tracks_summary,  Y_SINGLE > 0)
Y_NEG <- subset(tracks_summary,  Y_SINGLE < 0)
Y_ZERO <- subset(tracks_summary,  Y_SINGLE == 0)
X_Y_ZERO <- subset(tracks_summary,  X_SINGLE == 0 &  Y_SINGLE == 0) # stalled or paused events
X_POS <- subset(tracks_summary,  X_SINGLE > 0) # towards the left (MY LEFT)
X_NEG <- subset(tracks_summary,  X_SINGLE < 0) # towards the right
X_ZERO <- subset(tracks_summary,  X_SINGLE == 0)

# calculate mean velocities

mean(na.omit(tracks_summary$VELOCITY))
mean(na.omit(Y_POS$VELOCITY))
mean(na.omit(Y_NEG$VELOCITY))

################################################

#create normalised DISP for all displacements

tracks_norm <- as.data.frame(aggregate(tracks_summary$DISP_2 ~ tracks_summary$ANGLE_CAT,
                                       tracks_summary, sum))
names(tracks_norm) <- c("ANGLE_CAT", "DISP_SUM")
tracks_norm$DISP_NORM <- tracks_norm$DISP_SUM / sum(tracks_norm$DISP_SUM)
sum(tracks_norm$DISP_NORM)==1  # this should always be TRUE

# get the size of each bin, i.e. no of events within each angle
# n_events <- aggregate(tracks_summary$DISP_2 ~ tracks_summary$ANGLE_CAT, 
  #tracks_summary, length)

midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower+(upper-lower)/2, dp))
}

tracks_norm$ANGLE_CLASS <- midpoints(tracks_norm$ANGLE_CAT)
tracks_norm <- tracks_norm[, c(1, 4, 2, 3)]
tracks_norm <- tracks_norm[order(tracks_norm$ANGLE_CLASS),]

##########################################################

# create normalised DISP data for all positive and negative displacements

Y_POS_norm <- as.data.frame(aggregate(Y_POS$DISP_2 ~ Y_POS$ANGLE_CAT, Y_POS, sum))
names(Y_POS_norm) <- c("ANGLE_CAT", "DISP_SUM")
Y_POS_norm$DISP_NORM <- Y_POS_norm$DISP_SUM / sum(Y_POS_norm$DISP_SUM)
sum(Y_POS_norm$DISP_NORM) == 1

Y_POS_norm$ANGLE_CLASS <- midpoints(Y_POS_norm$ANGLE_CAT)
Y_POS_norm <- Y_POS_norm[, c(1, 4, 2, 3)]
Y_POS_norm <- Y_POS_norm[order(Y_POS_norm$ANGLE_CLASS),]


Y_NEG_norm <- as.data.frame(aggregate(Y_NEG$DISP_2 ~ Y_NEG$ANGLE_CAT, Y_NEG, sum))
names(Y_NEG_norm) <- c("ANGLE_CAT", "DISP_SUM")
Y_NEG_norm$DISP_NORM <- Y_NEG_norm$DISP_SUM / sum(Y_NEG_norm$DISP_SUM)
sum(Y_NEG_norm$DISP_NORM) == 1

Y_NEG_norm$ANGLE_CLASS <- midpoints(Y_NEG_norm$ANGLE_CAT)
Y_NEG_norm <- Y_NEG_norm[, c(1, 4, 2, 3)]
Y_NEG_norm <- Y_NEG_norm[order(Y_NEG_norm$ANGLE_CLASS),]

#######################################################

# MERGE ALL data into one dataframe

tracks_all_norm <- as.data.frame(Reduce
                                 (function(
                                   dtf1, dtf2) merge(dtf1, dtf2,
                                                     by = "ANGLE_CLASS", all = TRUE),
                                        list(tracks_norm, Y_POS_norm, Y_NEG_norm)))
names(tracks_all_norm)[c(4, 7, 10)] <- c("ALL_NORM","POS_NORM", "NEG_NORM")

write.csv(tracks_all_norm, "temp_norm.csv")

#############################################################################

# everything done with individual displacements

#################################################################


  
#############################################

# normalised displacement with angle for all single displacements

ggplot(na.omit(tracks_norm), aes(x=ANGLE_CAT,y=DISP_NORM)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", 
           col ="black", position="dodge") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_norm$DISP_NORM),
                                        by = 0.005),3)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Normalised displacement") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) 

################################
+
  annotate("text", 32, 0.07, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nn_events =", nrow(tracks_summary),
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         "\ntotal_time =", round(max(tracks_summary$TIME),2),
                         "sec",
                         collapse = " "), size = 5, hjust = 0)

###########################################################

annotate("text", 27, 1.8, 
         label = paste("n_particles =", nrow(newtracks_firstrows), 
                       "\nn_events =", nrow(tracks_summary), 
                       "\nt_interval =",
                       sort(tracks_summary$TIME,
                            partial=(length(tracks_summary$TIME))-1)[2]*1000,
                       "msec",
                       collapse = " "), size = 5, hjust = 0)


####################################################

 # normalised displacemnt with angle for Y_POS....downward?
  
ggplot(na.omit(Y_POS_norm), aes(x=ANGLE_CAT,y=DISP_NORM)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", 
           col ="black", position="dodge") + theme_bw() +
  scale_x_discrete(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_POS_norm$DISP_NORM),
                                        by = 0.005),3)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Normalised displacement") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15))

+
  annotate("text", 32, 0.07, 
           label = paste("\nn_events =", nrow(Y_POS),
                         collapse = " "), size = 5, hjust = 0)

##############################################

# normalised displacemnt with angle for Y_NEG....upward?

ggplot(na.omit(Y_NEG_norm), aes(x=ANGLE_CAT,y=DISP_NORM)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", 
           col ="black", position="dodge") + theme_bw() +
  scale_x_discrete(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_NEG_norm$DISP_NORM),
                                        by = 0.005),3)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Normalised displacement") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) 

+
  annotate("text", 32, 0.07, 
           label = paste("\nn_events =", nrow(Y_NEG),
                         collapse = " "), size = 5, hjust = 0)



############################################################
##########################################################

# Mean velocity per angle bin for individual displacements

ggplot(na.omit(tracks_summary), aes(x=ANGLE_CAT,y=VELOCITY)) +  
  geom_bar(stat = "summary", fun.y = "mean", width = 0.9, fill ="black", 
           col ="black", position="dodge") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$DISP_2),
                                        by = 0.05),2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of displacement (deg)") +
  ylab("Mean individual displacement (um)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15))

#################################################################

# Mean displacement per angle bin for individual displacements

ggplot(na.omit(tracks_summary), aes(x=ANGLE_CAT,y=DISP_2)) +  
  geom_bar(stat = "summary", fun.y = "mean", width = 0.9, fill ="black", 
           col ="black", position="dodge") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$DISP_2),
                                        by = 0.05),2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
              axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of displacement (deg)") +
  ylab("Mean individual displacement (um)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15))

+
  annotate("text", 32, 0.07, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\n_events =", nrow(tracks_summary),
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)

#############################


# UP and DOWN displacements together

ggplot(na.omit(tracks_summary),aes(x=ANGLE_CAT,y=DISP_2,fill=DISP_TYPE)) + 
  geom_bar(stat="summary", fun.y = "mean", position = "dodge", alpha=1) + 
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Mean displacement (um)") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8,"Dark2"))(2))

######################### Y_POS_Downward

ggplot(na.omit(Y_POS), aes(x=ANGLE_CAT,y=DISP_2)) +  
  geom_bar(stat = "summary", fun.y ="mean", width = 0.9, fill ="black", col ="black") + 
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_POS$DISP_2), 
                                        by = 0.5),25)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Mean displacement (um)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) 
+
  annotate("text", 25, 2, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nmean =", round(mean(Y_POS$DISP), 2), 
                         "±", round(sd(Y_POS$DISP), 2), "um",
                         "\nmax =", round(max(Y_POS$DISP), 2), "um",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)

################################# Y_NEG_Upward

ggplot(na.omit(Y_NEG), aes(x=ANGLE_CAT,y=DISP_2)) +  
  geom_bar(stat = "summary", fun.y = "mean", width = 0.9, fill ="black", col ="black") + 
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_NEG$DISP_2), 
                                        by = 0.25),2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Mean displacement (um)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) 
+
  annotate("text", 27, 1.7, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nmean =", round(mean(Y_NEG$DISP), 2), 
                         "±", round(sd(Y_NEG$DISP), 2), "um",
                         "\nmax =", round(max(Y_NEG$DISP), 2), "um",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)



###################################


# Displacement vs time

ggplot() + geom_line(data=tracks_summary, 
                     aes(x=TIME, y=DISP, 
                         group=TRACK_ID, colour = as.factor(TRACK_ID)), size =0.5) +
  geom_point(data=tracks_summary,
             aes(x=TIME, y=DISP, colour =as.factor(TRACK_ID)), size =0.3) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0),
                     breaks = round(seq(0, max(tracks_summary$TIME), 
                                        by = 0.5),2)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$DISP), 
                                        by = 0.5),2)) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none")


##############################################################

# Displacement vs time ....Y_POS downward

ggplot() + geom_line(data=Y_POS, 
                     aes(x=TIME, y=DISP, 
                         group=TRACK_ID, colour = as.factor(TRACK_ID)), size =0.5) +
  geom_point(data=Y_POS,
             aes(x=TIME, y=DISP, colour =as.factor(TRACK_ID)), size =0.3) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0),
                     breaks = round(seq(0, max(Y_POS$TIME), 
                                        by = 0.5),2)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_POS$DISP), 
                                        by = 0.5),2)) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none")


#############################################################

# Displacement vs time ....Y_NEG upward

ggplot() + geom_line(data=Y_NEG, 
                     aes(x=TIME, y=DISP, 
                         group=TRACK_ID, colour = as.factor(TRACK_ID)), size =0.5) +
  geom_point(data=Y_NEG,
             aes(x=TIME, y=DISP, colour =as.factor(TRACK_ID)), size =0.3) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0),
                     breaks = round(seq(0, max(Y_NEG$TIME), 
                                        by = 0.5),2)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_NEG$DISP), 
                                        by = 0.5),2)) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none")

#################################################################

####  Individual Displacement  ANGLE VS VELOCITY_CALC

ggplot(na.omit(tracks_summary), aes(x=ANGLE_CAT,y=VELOCITY)) +  
  geom_bar(stat = "summary", fun.y = "mean", width = 0.9, fill ="black", col ="black") +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0),
                     breaks = round(seq(0, max(tracks_summary$TIME), 
                                        by = 0.5),2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("mean velocity (um/sec)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) 
+
  annotate("text", 25, 7, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nmean =", round(mean(tracks_summary$VELOCITY), 2), 
                         "±", round(sd(tracks_summary$VELOCITY), 2), "um/sec",
                         "\nmax =", round(max(tracks_summary$VELOCITY), 2), "um/sec",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)

###########   Y_POS_Downward _ANGLE VS VELOCITY



########## Y_NEG_Upward_ANGLE VS VELOCITY







###############################################################
library(RColorBrewer)

display.brewer.all()
# Expand color set 'Dark 2' from 8 to number of particles
colourCount = length(unique(tracks_summary$TRACK_ID))
getPalette = colorRampPalette(brewer.pal(8, "Dark2"))
# Add "+ scale_colour_manual(values = getPalette(colourCount))" to ggplot to use palette

ggplot() + geom_line(data=tracks_summary, 
                     aes(x=DISP_X, y=DISP_Y, 
                         group=TRACK_ID, colour = as.factor(TRACK_ID)), size =0.5) +
  geom_point(data=tracks_summary,
             aes(x=DISP_X,y=DISP_Y, colour =as.factor(TRACK_ID)), size =0.3) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none") +
  coord_fixed()

#########

ggplot() + geom_line(data=tracks_summary, 
                     aes(x=X_LOCATION, y=Y_LOCATION, 
                         group=TRACK_ID, colour = as.factor(TRACK_ID)), size =0.5) +
  geom_point(data=tracks_summary,
             aes(x=X_LOCATION,y=Y_LOCATION, colour =as.factor(TRACK_ID)), size =0.3) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none") +
  coord_fixed()
##############

ggplot() + geom_line(data=tracks_summary, 
                     aes(x=TIME, y=DISP, 
                         group=TRACK_ID, colour = as.factor(TRACK_ID)), size =0.5) +
  geom_point(data=tracks_summary,
             aes(x=TIME, y=DISP, colour =as.factor(TRACK_ID)), size =0.3) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none")


################################################

library(dplyr)
newdata <- tracks_summary %>% select(TRACK_ID, X_SINGLE, Y_SINGLE) %>% filter(TRACK_ID == 19)
my_coordinates = cbind(newdata[1:10, ]$X_SINGLE,newdata[1:10, ]$Y_SINGLE)
my_coordinates1 <- my_coordinates[, c(2, 1)]
library(geoR)
my_cov= varcov.spatial(my_coordinates,cov.model="exp", cov.pars=c(0.2,25))


fit.ellipse <- function (x, y = NULL) {
  # from:
  # http://r.789695.n4.nabble.com/Fitting-a-half-ellipse-curve-tp2719037p2720560.html
  #
  # Least squares fitting of an ellipse to point data
  # using the algorithm described in: 
  #   Radim Halir & Jan Flusser. 1998. 
  #   Numerically stable direct least squares fitting of ellipses. 
  #   Proceedings of the 6th International Conference in Central Europe 
  #   on Computer Graphics and Visualization. WSCG '98, p. 125-132 
  #
  # Adapted from the original Matlab code by Michael Bedward (2010)
  # michael.bedward@gmail.com
  #
  # Subsequently improved by John Minter (2012)
  # 
  # Arguments: 
  # x, y - x and y coordinates of the data points.
  #        If a single arg is provided it is assumed to be a
  #        two column matrix.
  #
  # Returns a list with the following elements: 
  #
  # coef - coefficients of the ellipse as described by the general 
  #        quadratic:  ax^2 + bxy + cy^2 + dx + ey + f = 0 
  #
  # center - center x and y
  #
  # major - major semi-axis length
  #
  # minor - minor semi-axis length
  #
  EPS <- 1.0e-8 
  dat <- xy.coords(x, y) 
  
  D1 <- cbind(dat$x * dat$x, dat$x * dat$y, dat$y * dat$y) 
  D2 <- cbind(dat$x, dat$y, 1) 
  S1 <- t(D1) %*% D1 
  S2 <- t(D1) %*% D2 
  S3 <- t(D2) %*% D2 
  T <- -solve(S3) %*% t(S2) 
  M <- S1 + S2 %*% T 
  M <- rbind(M[3,] / 2, -M[2,], M[1,] / 2) 
  evec <- eigen(M)$vec 
  cond <- 4 * evec[1,] * evec[3,] - evec[2,]^2 
  a1 <- evec[, which(cond > 0)] 
  f <- c(a1, T %*% a1) 
  names(f) <- letters[1:6] 
  
  # calculate the center and lengths of the semi-axes 
  #
  # see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2288654/
  # J. R. Minter
  # for the center, linear algebra to the rescue
  # center is the solution to the pair of equations
  # 2ax +  by + d = 0
  # bx  + 2cy + e = 0
  # or
  # | 2a   b |   |x|   |-d|
  # |  b  2c | * |y| = |-e|
  # or
  # A x = b
  # or
  # x = Ainv b
  # or
  # x = solve(A) %*% b
  A <- matrix(c(2*f[1], f[2], f[2], 2*f[3]), nrow=2, ncol=2, byrow=T )
  b <- matrix(c(-f[4], -f[5]), nrow=2, ncol=1, byrow=T)
  soln <- solve(A) %*% b
  
  b2 <- f[2]^2 / 4
  
  center <- c(soln[1], soln[2]) 
  names(center) <- c("x", "y") 
  
  num  <- 2 * (f[1] * f[5]^2 / 4 + f[3] * f[4]^2 / 4 + f[6] * b2 - f[2]*f[4]*f[5]/4 - f[1]*f[3]*f[6]) 
  den1 <- (b2 - f[1]*f[3]) 
  den2 <- sqrt((f[1] - f[3])^2 + 4*b2) 
  den3 <- f[1] + f[3] 
  
  semi.axes <- sqrt(c( num / (den1 * (den2 - den3)),  num / (den1 * (-den2 - den3)) )) 
  
  # calculate the angle of rotation 
  term <- (f[1] - f[3]) / f[2] 
  angle <- atan(1 / term) / 2 
  
  list(coef=f, center = center, major = max(semi.axes), minor = min(semi.axes), angle = unname(angle)) 
}

get.ellipse <- function( fit, n=360 ) 
{
  # Calculate points on an ellipse described by 
  # the fit argument as returned by fit.ellipse 
  # 
  # n is the number of points to render 
  
  tt <- seq(0, 2*pi, length=n) 
  sa <- sin(fit$angle) 
  ca <- cos(fit$angle) 
  ct <- cos(tt) 
  st <- sin(tt) 
  
  x <- fit$center[1] + fit$maj * ct * ca - fit$min * st * sa 
  y <- fit$center[2] + fit$maj * ct * sa + fit$min * st * ca 
  
  cbind(x=x, y=y) 
}

X <- my_coordinates1
efit <- fit.ellipse(X)
e <- get.ellipse(efit)
plot(X) 
lines(e, col="red") 




library(dplyr)
newdata <- tracks_summary %>% select(TRACK_ID, TIME, X_LOCATION, 
                                     Y_LOCATION, DISP, X_SINGLE,
                                     Y_SINGLE, DISP_2) %>% filter(
                                       TRACK_ID == 15 | TRACK_ID == 48 |
                                         TRACK_ID == 57)

names(newdata) <- c("PARTICLE_ID",
                    "TIME", "X_POSITION", 
                    "Y_POSITION", "TOTAL_DISPLACEMENT", 
                    "X_DISPLACEMENT", "Y_DISPLACEMENT", 
                    "DISP_FROM_PREVIOUS_POSITION")

# my_coordinates = cbind(newdata[1:10, 2],newdata[1:10, 3]) # correct this obtain 
# single displacement coordinates for each 10 tracks per particle_ID

library(geoR)
my_cov= varcov.spatial(my_coordinates,cov.model="exp", cov.pars=c(0.2,25))

my_coordinates = cbind(newdata[1:10, ]$X_SINGLE,newdata[1:10, ]$Y_SINGLE)

