# script to analyse results of particle tracking

setwd("C:/Users/spind/OneDrive - sheffield.ac.uk/1_Live_analysis/white/") # on Home PC

setwd("/Users/victoralfred/Desktop/1_Live_analysis/white")

rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)
library(lattice)
library(RColorBrewer)
# display.brewer.all()



# read in datafiles using   data.frame(read.csv("XXXXXXXX>csv"))
newtracks <- data.frame(read.csv("1a_tracks.csv")) 

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

# function to calculate displacements and angles
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

tracks_summary <- tracks_calc(newtracks, newtracks_firstrows)

# write.csv(tracks_summary, "W01.csv")  # save new dataframe
# tracks <- as.data.frame(read.csv("tracks_summary.csv"))


#########################
library(ggplot2)

mean(tracks_summary$VELOCITY, na.rm=T); mean(tracks_summary$VELOCITY_AUT, na.rm=T)
sd(tracks_summary$VELOCITY, na.rm=T); sd(tracks_summary$VELOCITY_AUT, na.rm=T)

ggplot(data = tracks_summary) +
  geom_density(aes(x = VELOCITY_AUT, y=(..count..)/sum(..count..)), 
                 alpha=0.3, fill ="red",binwidth=10,position="dodge") +
  geom_vline(aes(xintercept=mean(VELOCITY_AUT, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=0.5) +
  geom_density(aes(x = VELOCITY, y=(..count..)/sum(..count..)), 
                 alpha=0.3,, fill ="green",binwidth=10,position="dodge") +
  geom_vline(aes(xintercept=mean(VELOCITY, na.rm=T)),   # Ignore NA values for mean
             color="green", linetype="dashed", size=0.5) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  xlab("Velocity") +
  ylab("Proportion") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        axis.title=element_text(size=15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

##

ggplot(data = tracks_summary) +
  geom_histogram(aes(x = VELOCITY_AUT, y=(..count..)/sum(..count..)), 
               alpha=0.3, fill ="red",binwidth=2,position="dodge") +
  geom_vline(aes(xintercept=mean(VELOCITY_AUT, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=0.5) +
  geom_histogram(aes(x = VELOCITY, y=(..count..)/sum(..count..)), 
               alpha=0.3,, fill ="green",binwidth=2,position="dodge") +
  geom_vline(aes(xintercept=mean(VELOCITY, na.rm=T)),   # Ignore NA values for mean
             color="green", linetype="dashed", size=0.5)



######################################

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