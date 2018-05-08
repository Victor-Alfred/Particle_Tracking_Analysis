setwd("Desktop/R_tracking/")

setwd("C:/Users/spind/OneDrive - sheffield.ac.uk/R_tracking/") # on Home PC

library(ggplot2)
library(dplyr)
library(lattice)
library(RColorBrewer)
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
    DISP_TYPE <- ifelse(DISP_Y >=0, "POS", "NEG")
    ANGLE <- sapply((atan2(DISP_Y, DISP_X)*180)/pi, pos_angles)
    ANGLE_RAD <- (ANGLE*pi)/180
    ANGLE_CAT<-cut(ANGLE, seq(0,180,4), include.lowest = TRUE)
    VELOCITY <- VELOCITY.x
    X_SINGLE <- ave(X_LOCATION, TRACK_ID,
                    FUN=function(x) c(0, diff(x)))
    Y_SINGLE <- ave(Y_LOCATION, TRACK_ID,
                    FUN=function(x) c(0, diff(x)))
    DISP_2 <- sqrt((X_SINGLE)^2 + (Y_SINGLE)^2)
    ANGLE_SINGLE <- sapply((atan2(Y_SINGLE, X_SINGLE)*180)/pi, pos_angles)
    ANGLE_SINGLE_RAD <- (ANGLE_SINGLE*pi)/180
    ANGLE_SINGLE_CAT <-cut(ANGLE_SINGLE, seq(0,180,4), include.lowest = TRUE)
    TIME_SINGLE <- ave(TIME, TRACK_ID,
                       FUN=function(x) c(0, diff(x)))
    VELOCITY_CALC <- DISP_2/TIME_SINGLE
  })[,c("TRACK_ID","EDGE_TIME", "TIME","X_LOCATION", "Y_LOCATION",
        "DISP_X","DISP_Y", "DISP","DISP_TYPE", "ANGLE", "ANGLE_RAD", "ANGLE_CAT", 
        "VELOCITY", "X_SINGLE", "Y_SINGLE", "DISP_2", "ANGLE_SINGLE", "ANGLE_SINGLE_RAD",
        "ANGLE_SINGLE_CAT", "TIME_SINGLE", "VELOCITY_CALC")]
  return(as.data.frame(c))
}

# fit the data with a von mises distribution, determine mu, and then do θ-mu

# ANGLE <- sapply((((atan2(DISP_Y, DISP_X)*180)/pi) - 36.6912), pos_angles) 


tracks_summary <- tracks_calc(newtracks, newtracks_firstrows)

# Normalise the displacments for both total and single displacements

tracks_norm <- as.data.frame(aggregate(tracks_summary$DISP ~ tracks_summary$ANGLE_CAT, tracks_summary, mean))
names(tracks_norm) <- c("ANGLE_CAT", "DISP_MEAN")
tracks_norm$DISP_NORM <- tracks_norm$DISP_MEAN / sum(tracks_norm$DISP_MEAN)
sum(tracks_norm$DISP_NORM)

tracks_norm_SINGLE <- as.data.frame(aggregate(tracks_summary$DISP_2 ~ tracks_summary$ANGLE_SINGLE_CAT, tracks_summary, mean))
names(tracks_norm_SINGLE) <- c("ANGLE_CAT", "DISP_MEAN")
tracks_norm_SINGLE$DISP_NORM <- tracks_norm_SINGLE$DISP_MEAN / sum(tracks_norm_SINGLE$DISP_MEAN)
sum(tracks_norm_SINGLE$DISP_NORM)

tracks_calc_norm <- function (a, b){
  c <- within(merge(a,b,by="ANGLE_CAT"), {
    DISP_MEAN <- DISP_MEAN.x
    DISP_MEAN_SINGLE <- DISP_MEAN.y
    DISP_NORM <- DISP_NORM.x
    DISP_NORM_SINGLE <- DISP_NORM.y
  })[,c("ANGLE_CAT","DISP_MEAN", "DISP_MEAN_SINGLE","DISP_NORM", "DISP_NORM_SINGLE")]
  return(as.data.frame(c))
}

tracks_summary_norm <- tracks_calc_norm(tracks_norm, tracks_norm_SINGLE)

midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower+(upper-lower)/2, dp))
}



tracks_summary_norm$ANGLE_CLASS <- midpoints(tracks_summary_norm$ANGLE_CAT)
tracks_summary_norm <- tracks_summary_norm[, c(1, 6, 2, 3, 4, 5)]

write.csv(tracks_summary_norm, "tracks_summary_norm.csv")


library(NISTunits)
sapply(NISTradianTOdeg(atan2(tracks_summary$DISP_Y, tracks_summary$DISP_X)), pos_angles)
sapply(tracks_summary$ANGLE, NISTdegTOradian)


write.csv(tracks_summary, "tracks_summary.csv")  # save new dataframe
# tracks <- as.data.frame(read.csv("tracks_summary.csv"))

# create subsets of negative and positive long-axis(Y) displacements
Y_POS <- subset(tracks_summary, DISP_Y >= 0)
Y_NEG <- subset(tracks_summary, DISP_Y < 0)

# Also create subsets of negative and positive short-axis(X) displacements
X_POS <- subset(tracks_summary, DISP_X >= 0)
X_NEG <- subset(tracks_summary, DISP_X < 0)


library(circular)
library(CircStats)
library(NISTunits)    # cponverting beween degrees and rad
# Usage
NISTdegTOradian(180)
NISTradianTOdeg(pi)



circ.plot(tracks_summary$ANGLE, cex=0.5, pch=20, stack=T, bins=1000)  # Angle in DEG

theta = circular(tracks_summary$ANGLE)
theta_kde = density(theta, bw=100, type="K")
plot(theta_kde, main="", xlab="", ylab="", lwd=3)

circ.summary(tracks_summary$ANGLE_SINGLE)  # Angle in degrees
circ.summary(tracks_summary$ANGLE_SINGLE, rads = FALSE, fast = FALSE, tol = 1e-09, plot = TRUE)
circ.disp(tracks_summary$ANGLE_SINGLE)
vm.ml(theta,bias=T)   # ANGLE in RAD

circ.plot(Y_NEG$ANGLE, cex=0.5, pch=20, stack=T, bins=1000)

theta = circular(thetaRad)
theta = circular(tracks_summary$ANGLE_RAD_2)
theta_kde = density(theta, bw=100, type="K")
plot(theta_kde, main="", xlab="", ylab="", lwd=3)

circ.summary(tracks_summary$ANGLE)

# Obtain estimates of the parameters
est.kappa(tracks_summary$ANGLE_RAD_2, bias=T)
est.kappa(thetaRad, bias=T)
vm.ml(tracks_summary$ANGLE,bias=T)


watson(tracks_summary$ANGLE_RAD_2,dist="vm") #  Watson's Test for the von Mises Distribution
kuiper(tracks_summary$ANGLE_RAD_2)    # Kuiper's Test for the von Mises Distribution
rao.spacing(tracks_summary$ANGLE_RAD_2) # Rao’s spacing test compares the observed arc lengths 
r.test(tracks_summary$ANGLE) # Angle in DEG

rvonmises(n, mu, kappa, control.circular=list())
dvonmises(x, mu, kappa, log)

rdata=rvonmises(4239, 1.088878, 0.0459453, as.list(tracks_summary$ANGLE))
rose.diag(rdata, bins=50, prop=1.5, shrink=1)
points(rdata, pch=20, col="red")


#################################################################


  annotate("text", 27, 1.8, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nmean =", round(mean(tracks_summary$DISP), 2), 
                         "±", round(sd(tracks_summary$DISP), 2), "um",
                         "\nmax =", round(max(tracks_summary$DISP), 2), "um",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)

#############################################
ggplot(na.omit(tracks_summary), aes(x=ANGLE_CAT,y=DISP)) +  
  geom_bar(stat = "summary", fun.y = "mean", width = 0.9, fill ="black", 
           col ="black", position="dodge") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$DISP),
                                        by = 0.5),2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
              axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) 
+
  annotate("text", 27, 1.8, 
           label = paste("n_particles =", nrow(newtracks_firstrows), 
                         "\nmean =", round(mean(tracks_summary$DISP), 2), 
                         "±", round(sd(tracks_summary$DISP), 2), "um",
                         "\nmax =", round(max(tracks_summary$DISP), 2), "um",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)

#############################


# UP and DOWN displacements together

ggplot(na.omit(tracks_summary),aes(x=ANGLE_CAT,y=DISP,fill=DISP_TYPE)) + 
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
  ylab("Displacement (microns)") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8,"Dark2"))(2))

######################### Y_POS_Downward

ggplot(na.omit(Y_POS), aes(x=ANGLE_CAT,y=DISP)) +  
  geom_bar(stat = "summary", fun.y ="mean", width = 0.9, fill ="black", col ="black") + 
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$DISP), 
                                        by = 0.5),25)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
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

ggplot(na.omit(Y_NEG), aes(x=ANGLE_CAT,y=DISP)) +  
  geom_bar(stat = "summary", fun.y = "mean", width = 0.9, fill ="black", col ="black") + 
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_NEG$DISP), 
                                        by = 0.25),2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
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
# Single Displacement plot

ggplot(na.omit(tracks_summary), aes(x=ANGLE_SINGLE_CAT,y=DISP_2)) +  
  geom_bar(stat = "summary", fun.y = "mean", width = 0.9, fill ="black", 
           col ="black", position="dodge") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$DISP), 
                                        by = 0.05),2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15)) 


#################################################################

####  Total Displacement_  ANGLE VS VELOCITY

ggplot(na.omit(tracks_summary), aes(x=ANGLE_CAT,y=VELOCITY)) +  
  geom_bar(stat = "summary", fun.y = "mean", width = 0.9, fill ="black", col ="black") +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$VELOCITY), 
                                        by = 0.5),1)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Velocity (um/sec)") +
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

ggplot(na.omit(Y_POS), aes(x=ANGLE_CAT,y=VELOCITY/100)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", col ="black") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_POS$VELOCITY), 
                                        by = 0.5),1)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
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
                         "\nmax =", round(max(Y_POS$VELOCITY), 2), "um/sec",
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
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
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
                         "\nmax =", round(max(Y_NEG$VELOCITY), 2), "um/sec",
                         "\nt_interval =",
                         sort(tracks_summary$TIME,
                              partial=(length(tracks_summary$TIME))-1)[2]*1000,
                         "msec",
                         collapse = " "), size = 5, hjust = 0)



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





############################################################

# CLUSTERING ????


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

#######################################

ggplot(na.omit(Y_POS), aes(x=ANGLE_SINGLE_CAT,y=DISP_2/100)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", col ="black") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(Y_POS$DISP_2), 
                                        by = 0.1),2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15))

########################

ggplot(na.omit(tracks_summary), aes(x=ANGLE_SINGLE_CAT,y=DISP_2/100)) +  
  geom_bar(stat = "identity", width = 0.9, fill ="black", col ="black") + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = round(seq(0, max(tracks_summary$DISP_2), 
                                        by = 0.1),2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1)) +
  xlab("Angle of Displacement (degrees)") +
  ylab("Displacement (microns)") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15))

############################

ggplot(na.omit(tracks_summary),aes(x=ANGLE_CAT,y=DISP,fill=DISP_TYPE)) + 
  geom_bar(stat="identity",position = "dodge", alpha=1) + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(8,"Dark2"))(2))



ggplot(na.omit(tracks_summary),aes(x=ANGLE_CAT,y=DISP,fill=DISP_TYPE)) + 
  geom_bar(stat="identity",position = "position", alpha=0.2) + theme_bw() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust =1),
        axis.text.y = element_text(size = 10, vjust =1),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(8,"Dark2"))(2))

  

## Why using melt?  
to_plot <- data.frame(x=x,y1=y1,y2=y2)
melted<-melt(to_plot, id="x")
print(ggplot(melted,aes(x=x,y=value,fill=variable)) + 
        geom_bar(stat="identity",position = "identity", alpha=.3))


Packages <- c("ggplot2", "RColorBrewer", "lattice")

lapply(Packages, library, character.only = TRUE)

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}



display.brewer.all()


pts_on_unit_circle <- cbind(cos(tracks_summary$ANGLE * pi / 180), 
                            sin(tracks_summary$ANGLE * pi / 180))
d <- movMF(pts_on_unit_circle, number_of_mixed_vM_fxns)
mu <- atan2(d$theta[,2], d$theta[,1])
kappa <- sqrt(rowSums(d$theta^2))

