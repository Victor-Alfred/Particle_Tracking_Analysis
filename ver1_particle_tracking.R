setwd("Desktop/R_tracking/")

setwd("C:/Users/spind/OneDrive - sheffield.ac.uk/R_tracking/") # on Home PC

library(NbClust)
library(factoextra)
library(cluster)
library(ggplot2)
library(lattice)

Packages <- c("ggplot2", "RColorBrewer", "lattice")

lapply(Packages, library, character.only = TRUE)

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

display.brewer.all()

# read in datafiles using   data.frame(read.csv("XXXXXXXX>csv"))
newtracks <- data.frame(read.csv("newtracks.csv")) 

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
    ANGLE <- sapply((atan2(DISP_Y, DISP_X)*180)/pi, pos_angles)
    ANGLE_CAT<-cut(ANGLE, seq(0,180,5))
    VELOCITY <- VELOCITY.x
  })[,c("TRACK_ID","EDGE_TIME", "TIME","X_LOCATION", "Y_LOCATION",
        "DISP_X","DISP_Y", "DISP", "ANGLE", "ANGLE_CAT", "VELOCITY")]
  return(as.data.frame(c))
}

tracks_summary <- tracks_calc(newtracks, newtracks_firstrows)
write.csv(tracks_summary, "tracks_summary.csv")  # save new dataframe
# tracks <- as.data.frame(read.csv("tracks_summary.csv"))

# create subsets of negative and positive long-axis(Y) displacements
Y_POS <- subset(tracks_summary, DISP_Y >= 0)
Y_NEG <- subset(tracks_summary, DISP_Y < 0)

# Also create subsets of negative and positive short-axis(X) displacements
X_POS <- subset(tracks_summary, DISP_X >= 0)
X_NEG <- subset(tracks_summary, DISP_X < 0)

#################################################################

ggplot(na.omit(tracks_summary), aes(x=ANGLE_CAT,y=DISP/100)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", col ="black") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$DISP), 
                                        by = 0.5),1)) +
  theme(axis.text.x = element_text(size = 4, face="bold")) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) +
  annotate("text", 25, 2.5, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nmean =", round(mean(tracks_summary$DISP), 2), 
                         "±", round(sd(tracks_summary$DISP), 2), "um",
                         "\nmax =", round(max(tracks_summary$DISP), 2), "um",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)

######################### Y_POS_Downward

ggplot(na.omit(Y_POS), aes(x=ANGLE_CAT,y=DISP/100)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", col ="black") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$DISP), 
                                        by = 0.5),1)) +
  theme(axis.text.x = element_text(size = 4, face="bold")) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) +
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

ggplot(na.omit(Y_NEG), aes(x=ANGLE_CAT,y=DISP/100)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", col ="black") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_NEG$DISP), 
                                        by = 0.5),1)) +
  theme(axis.text.x = element_text(size = 4, face="bold")) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) +
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


#################################################################
####  Total Displacement_  ANGLE VS VELOCITY

ggplot(na.omit(tracks_summary), aes(x=ANGLE_CAT,y=VELOCITY/100)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", col ="black") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$VELOCITY), 
                                        by = 0.5),1)) +
  theme(axis.text.x = element_text(size = 4, face="bold")) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) +
  annotate("text", 25, 7, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nmean =", round(mean(tracks_summary$VELOCITY), 2), 
                         "±", round(sd(tracks_summary$VELOCITY), 2), "um/sec",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)

###########   Y_POS_Downward _ANGLE VS VELOCITY

ggplot(na.omit(Y_POS), aes(x=ANGLE_CAT,y=VELOCITY/100)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", col ="black") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_POS$VELOCITY), 
                                        by = 0.5),1)) +
  theme(axis.text.x = element_text(size = 4, face="bold")) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Velocity (um/sec)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) +
  annotate("text", 25, 3.5, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nmean =", round(mean(Y_POS$VELOCITY), 2), 
                         "±", round(sd(Y_POS$VELOCITY), 2), "um/sec",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)

########## Y_NEG_Upward_ANGLE VS VELOCITY

ggplot(na.omit(Y_NEG), aes(x=ANGLE_CAT,y=VELOCITY/100)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", col ="black") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_NEG$VELOCITY), 
                                        by = 0.5),1)) +
  theme(axis.text.x = element_text(size = 4, face="bold")) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Velocity (um/sec)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) +
  annotate("text", 27, 3.5, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nmean =", round(mean(Y_NEG$VELOCITY), 2), 
                         "±", round(sd(Y_NEG$VELOCITY), 2), "um/sec",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)

######################################


##########################################################
# PLOT ANGLE VS DISPLACEMENT (or velocity?) as histogram

plot(tracks_summary$ANGLE, tracks_summary$DISP, 
     type = "h", lwd = 15, col = "gray15",
     lend = "square",  # top of each bar, if left out, top of bars are more rounded
     yaxt='n', # remove y axis labels
     xaxt='n',
     yaxs="i",
     xaxs="i",
     ann=TRUE, # option to remove labels when FALSE
     # font.lab=2, # bold x and y axes labels, already used within 'title'
     xlab = "",  # use labels within 'title' to customise each axes individually
     ylab = "", 
     bty="n",
     main="wt1"); 
title(xlab="Angle of Displacement (degrees)", line=2.5, cex.lab=1.5, font.lab = 2);
title(ylab="Displacement (microns)", line=2.8, cex.lab=1.5, font.lab = 2);
par(#cex.lab=1.5, already used within 'title'
  cex.main = 2, cex.axis = 1.2); # size of axes labels, use this 'axis(2,cex.axis=1.2)' 
# to specifically change the y axis, 'cex.axis' changes size of tick labels
axis(1, las=1, at=seq(0, 180, by=30));  # option 'at=c(0:23)' sets the min and max axis value
axis(2, las=1, at=seq(0, 6, by=0.5)); # axis 2 (y axis), axis 1 (x axis), 
# las: the style of axis labels, 
# 0-always parallel to the axis [default], # 1: always horizontal, 
#2: always perpendicular to the axis, 3: always vertical.

# to specify position
n = length(tracks_summary$DISP)
text(120, sort(tracks_summary$DISP,partial=(length(tracks_summary$DISP))-1)[n]*0.75,
     paste("n =", nrow(newtracks_firstrows), 
           "\nInterval =",
           sort(tracks_summary$TIME,partial=(length(tracks_summary$TIME))-1)[2], "sec",
           collapse = " "), pos = 4, cex = 1.5)

temp <- locator(1)    # allows to click on intended position
text(temp, paste("n =", nrow(newtracks_firstrows), "\nInterval =",
                 sort(tracks_summary$TIME,partial=(length(tracks_summary$TIME))-1)[2], "sec",
                 collapse = " "), pos = 4, cex = 1.5)

n = length(tracks_summary$DISP)
sort(tracks_summary$DISP,partial=(length(tracks_summary$DISP))-1)[n] # finds largest value
max(tracks_summary$DISP)
sort(tracks_summary$DISP,partial=(length(tracks_summary$DISP))-1)[n-1] # finds second largest value
sort(tracks_summary$DISP,partial=(length(tracks_summary$DISP))-1)[1] # finds smallest value
max(tracks_summary$DISP)
sort(tracks_summary$DISP,partial=(length(tracks_summary$DISP))-1)[2] # finds second smallest value

###  Separate plots for positive and negative Y-displacements
# Plot for only positive displacements

plot(Y_POS$ANGLE, Y_POS$DISP, 
     type = "h", lwd = 15, col = "gray15",
     lend = "square",  # top of each bar, if left out, top of bars are more rounded
     yaxt='n', # remove y axis labels
     xaxt='n',
     yaxs="i",
     xaxs="i",
     ann=TRUE, # option to remove labels when FALSE
     # font.lab=2, # bold x and y axes labels, already used within 'title'
     xlab = "",  # use labels within 'title' to customise each axes individually
     ylab = "", 
     bty="n",
     main="wt1_downward"); 
title(xlab="Angle of Displacement (degrees)", line=2.5, cex.lab=1.5, font.lab = 2);
title(ylab="Displacement (microns)", line=2.8, cex.lab=1.5, font.lab = 2);
par(#cex.lab=1.5, already used within 'title'
  cex.main = 2, cex.axis = 1.2); # size of axes labels, use this 'axis(2,cex.axis=1.2)' 
# to specifically change the y axis, 'cex.axis' changes size of tick labels
axis(1, las=1, at=seq(0, 180, by=30));  # option 'at=c(0:23)' sets the min and max axis value
axis(2, las=1, at=seq(0, 6, by=0.5)); # axis 2 (y axis), axis 1 (x axis), 
# las: the style of axis labels, 
# 0-always parallel to the axis [default], # 1: always horizontal, 
#2: always perpendicular to the axis, 3: always vertical.

# to specify position
text(120, sort(tracks_summary$DISP,partial=(length(tracks_summary$DISP))-1)[n]*0.75,
     paste("n_particles =", nrow(newtracks_firstrows), "\nInterval =", "sec",
           sort(tracks_summary$TIME,partial=(length(tracks_summary$TIME))-1)[2],
           collapse = " "), pos = 4, cex = 1.5)

# Plot for only negative displacements
plot(Y_NEG$ANGLE, Y_NEG$DISP, 
     type = "h", lwd = 15, col = "gray15",
     lend = "square",  # top of each bar, if left out, top of bars are more rounded
     yaxt='n', # remove y axis labels
     xaxt='n',
     yaxs="i",
     xaxs="i",
     ann=TRUE, # option to remove labels when FALSE
     # font.lab=2, # bold x and y axes labels, already used within 'title'
     xlab = "",  # use labels within 'title' to customise each axes individually
     ylab = "", 
     bty="n",
     main="wt1_upward"); 
title(xlab="Angle of Displacement (degrees)", line=2.5, cex.lab=1.5, font.lab = 2);
title(ylab="Displacement (microns)", line=2.8, cex.lab=1.5, font.lab = 2);
par(#cex.lab=1.5, already used within 'title'
  cex.main = 2, cex.axis = 1.2); # size of axes labels, use this 'axis(2,cex.axis=1.2)' 
# to specifically change the y axis, 'cex.axis' changes size of tick labels
axis(1, las=1, at=seq(0, 180, by=30));  # option 'at=c(0:23)' sets the min and max axis value
axis(2, las=1, at=seq(0, 6, by=0.5)); # axis 2 (y axis), axis 1 (x axis), 
# las: the style of axis labels, 
# 0-always parallel to the axis [default], # 1: always horizontal, 
#2: always perpendicular to the axis, 3: always vertical.

# to specify position
text(120, sort(tracks_summary$DISP,partial=(length(tracks_summary$DISP))-1)[n]*0.75,
     paste("n =", nrow(newtracks_firstrows), "\nInterval in secs =", 
           sort(tracks_summary$TIME,partial=(length(tracks_summary$TIME))-1)[2],
           collapse = " "), pos = 4, cex = 1.5)

###############################################################

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




  



#Also create plots for -ve Y-disp, +ve Y disp, -ve X-disp, +ve X-disp

###############################################################################

# Histogram plot of velocity distribution

# use the ggplot code
hist(tracks_summary$VELOCITY, breaks = 50,
     col = "gray15", border = "black", yaxt='n', ylab = "",
     yaxs="i", xlab ="", cex.axis = 1.2,
     main="Velocity distribution");
title(xlab="Velocity", line=2.5, cex.lab=1.5, font.lab = 2);

temp <- locator(1)

text(temp,
     paste("n_particles =", nrow(newtracks_firstrows), 
           "\nMean (um/sec) =", round(mean(tracks_summary$VELOCITY), 2),
           "\nSD =", round(sd(tracks_summary$VELOCITY), 2),
           "\nMax =", round(max(tracks_summary$VELOCITY), 2),
           collapse = " "), pos = 4, cex = 1.3)

##########

ggplot(data=tracks_summary, aes(tracks_summary$VELOCITY)) + 
  geom_histogram(breaks=seq(0, 6, by = 0.1), 
                 col="black", 
                 fill="gray15", 
                 alpha = .8) + theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15, lineheight=.8, face="bold"),
        axis.text = element_text(size = 14), 
        axis.title=element_text(size=17)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(title="Velocity Distribution") +
  xlab("Velocity (micron/sec)") +
  annotate("text", x = 4, y=200, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nMean =", round(mean(tracks_summary$VELOCITY), 2), "um/sec",
                         "\nSD =", round(sd(tracks_summary$VELOCITY), 2),
                         "\nMax =", round(max(tracks_summary$VELOCITY), 2),
                         collapse = " "), size = 5.5, hjust = 0)


#####################################################

# select criteria/columns that should be used for clustering
df <- scale(tracks_summary[2:6])   #TIME, DISP_X, DISP_Y, DISP, ANGLE
#df <- scale(tracks_summary[c(2,5,6)])
df[is.na(df)] <- 0

# To determine optimum number of clusters
library(NbClust)
library(factoextra)
library(cluster)

set.seed(123)
nb <- NbClust(df, distance = "euclidean", min.nc = 2, 
              max.nc = 10, method = "complete", index ="all")

# nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

fviz_nbclust(nb) + theme_minimal()

barplot(table(nb$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Parameters",
        main="Optimisation of Cluster Number")

# nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

########################################################

# 0R the elbow-method to decide cluster number
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(df)

#######################################################################

# DO k-means clustering
set.seed(123)
km.res <- kmeans(df, 2, nstart = 25) # decide number of clusters after running NbClust
# k-means group number of each observation
km.res$cluster

# Visualize k-means clusters
fviz_cluster(km.res, data = df, geom = "point", stand = FALSE, 
             ellipse.type = "norm") + theme_bw() + theme(panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank())

data_clus_1 <- tracks_summary[km.res$cluster == 1,]
data_clus_2 <- tracks_summary[km.res$cluster == 2,]
data_clus_3 <- tracks_summary[km.res$cluster == 3,]

mean(data_clus_1$ANGLE)
mean(data_clus_2$ANGLE)
mean(data_clus_3$ANGLE)

#####################################################

#######  EDIT THESE BELOW

# to get more information about these clusters
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
# [1] 1468  596  945

table(control$GeneSymbol=="LP", fit.km$cluster)

fit.km$centers



par(lend="square")
bordercol <- "gray15"
fillcol <- "white"
linewidth <- 24

ggplot(data=X_plot) + 
  /'geom_histogram(aes(x=mid, y=total), fill="blue", stat="identity")
