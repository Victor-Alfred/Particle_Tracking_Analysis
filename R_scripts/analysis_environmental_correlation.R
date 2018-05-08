setwd("C:/Users/Victor Alfred/R Data Analysis/Solomon/")
dir()

all_data <- read.csv("all_data.csv", na.strings=c("",".","NA"))
all_data <- as.data.frame(all_data)
str(all_data)

library(ggplot2)


# Degradation dynamics of FL and PH without any treatment or species .grpA

grpA <- subset(all_data, Species == "No_species" & Treatment  == "NO_BIO" )
write.csv(grpA, "grpA.csv")

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

# apply summarySE function to dataframe

all_data_summary_FL_<- summarySE(all_data, measurevar="FL_conc",
                            groupvars=c("Time", "POL_conc","Species", "Treatment"))

# This handles two numerical variable columns
kpal <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                 conf.interval=.95, .drop=TRUE) {
  library(plyr)
  Return.list <- list()
  # New version of length which can handle NA's: if na.rm==T, don't count them
  for (var in measurevar) {
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
                   var
    )
    
    # Rename the "mean" column   
    datac <- rename(datac, c("mean" = var))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    Return.list[[var]] <- datac
  }
  Return <- Return.list[[1]]
  for (i in 2:length(Return.list)) {
    Return <- merge(x=Return,y=Return.list[[i]],by=groupvars)
  }
  return(Return)
}

# apply kpal function to dataframe

all_data_summary <- kpal(all_data, measurevar=c("FL_conc", "PH_conc"),
                          groupvars=c("Time", "POL_conc","Species", "Treatment"))

write.csv(all_data_summary, "all_data_summary.csv")

# NAs formed from groups missing one data were manually edited and re-read again

all_data_summary <- read.csv("all_data_summary.csv")

# Create a subset that has not been treated at all by BIO or Species

all_data_summary_NO_BIO_SP_60 <- subset(all_data_summary, Species == "No_species" &
                                          Treatment == "NO_BIO" & POL_conc == 60)

write.csv(all_data_summary_NO_BIO_SP_60, "all_data_summary_NO_BIO_SP_60.csv")

dodge <- position_dodge(width=0.9)

pd <- position_dodge(0.5) # move them .05 to the left and right

ggplot(all_data_summary_NO_BIO_SP_60, aes(Time)) + theme_bw() +
  geom_point(aes(y = FL_conc, colour = "Fluoranthene"), size = 5, position = dodge) + 
  geom_point(aes(y = PH_conc, colour = "Phenanthrene"), size = 5, position = dodge) +
  geom_line(aes(y = FL_conc, colour = "Fluoranthene"), size = 2, position = dodge) +
  geom_line(aes(y = PH_conc, colour = "Phenanthrene"), size = 2, position = dodge)

FL_time <- read.csv("FL_time.csv")

cor.test(FL_time$FL_grp60, FL_time$FL_grp120)

ggplot(FL_time, aes(x=FL_grp60, y=FL_grp120)) + theme_bw() +
  geom_point(shape=1) +    # Use hollow circles
  geom_point(size = 2) +
  scale_colour_hue(l=50) + # Slightly darker palette than normal
  xlab("Fluoranthene")+
  ylab("Phenanthrene")+
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)    # Don't add shaded confidence region


# hostogram to check normal distribution
ggplot(data=all_data, aes(all_data$FL_conc)) + geom_histogram()

hist(all_data$FL_conc)

plot(density(all_data$FL_conc));plot(density(all_data$PH_conc))

shapiro.test(all_data$FL_conc)

qqnorm(all_data$FL_conc); qqline(all_data$FL_conc)

plot.ts(all_data$FL_conc)

FLtimeseries <- ts(all_data$FL_conc, frequency=2, start=c(0,1))

plot.ts(FLtimeseries)




ggplot(all_data_summary_NO_BIO_SP_60, aes(Time)) + theme_bw() +
  geom_point(aes(y = FL_conc, colour = "Fluoranthene"), size = 5, position = dodge) + 
  geom_point(aes(y = PH_conc, colour = "Phenanthrene"), size = 5, position = dodge) +
  geom_line(aes(y = FL_conc, colour = "Fluoranthene"), size = 2, position = dodge) +
  geom_line(aes(y = PH_conc, colour = "Phenanthrene"), size = 2, position = dodge) +
  geom_errorbar(position = dodge, aes(ymin=FL_conc - sd_FL, ymax=FL_conc + sd_FL), 
                size = 1, width=.5)







test_data <- read.csv("test_data.csv")
test_data<- melt(test_data, id="Time")

ggplot(data=test_data,
       aes(x=Time, y=value, colour=variable)) +
       geom_line()+
                                   
PSENEN_D <- subset(all_data, GeneSymbol == "PSENEN_D")






ggplot(all_data, aes(x=FL_conc, y=PH_conc)) + theme_bw() +
  geom_point(shape=1) +    # Use hollow circles
  geom_point(size = 2) +
  scale_colour_hue(l=50) + # Slightly darker palette than normal
  xlab("Fluoranthene")+
  ylab("Phenanthrene")+
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)    # Don't add shaded confidence region

cor.test(all_data$FL_conc, all_data$PH_conc)


grp60 <- subset(all_data, POL_conc == 60 )

write.csv(grp60, "grp60.csv")

grp120 <- subset(all_data, POL_conc == 120 )

write.csv(grp120, "grp120.csv")



ggplot(all_data, aes(x=FL_conc, y=PH_conc, color=Treatment))+ 
  theme_bw() +
  geom_point(shape=1)+    # Use hollow circles
  geom_point(size = 2)+
  scale_colour_hue(l=50) + # Slightly darker palette than normal
  xlab("Fluoranthene")+
  ylab("Phenanthrene")+
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) 


ggplot(all_data, aes(x=Time, y=FL_conc)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_point(size = 2, color="#B43104") +
  scale_colour_hue(l=50) + # Slightly darker palette than normal
  scale_x_discrete(breaks=c("0", "7", "14", "28"),
                   labels=c("0", "7", "14", "28")) +
  geom_smooth(method=lm,   # Add linear regression line
              se=TRUE)    # Don't add shaded confidence region