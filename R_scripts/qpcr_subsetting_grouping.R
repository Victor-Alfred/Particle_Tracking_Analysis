# script written to analyse data of qPCR analyses

dir()

setwd("C:/Users/Victor Alfred/R Data Analysis/qpcr/")
dir()

qpcr <- read.csv("qpcr_2.csv")
str(qpcr)

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

# Manually grouped the duplicates a and b together and obtained the means and SE

qpcr_duplicates <- read.csv("qpcr_duplicates_grouped_together.csv")

str(qpcr_duplicates)

require(dplyr)    # For the select function used to isolate specific columns


# Select the duplicate samples and specific columns for Replicate 1

qpcr_replicate_1 <- subset(qpcr_duplicates, NewSample == "1AB" | 
                             NewSample == "3AB" | NewSample == "5AB" | 
                             NewSample == "7AB") %>%
  select(NewSample, gene, MeanCT, MeanSE)

# OR

qpcr_replicate_1 <- qpcr_duplicates[qpcr_duplicates$NewSample == "1AB" | 
                                      qpcr_duplicates$NewSample == "3AB" |
                                      qpcr_duplicates$NewSample == "5AB" | 
                                      qpcr_duplicates$NewSample == "7AB", ] %>%
                select(NewSample, gene, MeanCT, MeanSE)

str(qpcr_replicate_1)

write.csv(qpcr_replicate_1, "qpcr_replicate_1.csv")

# Select the duplicate samples and specific columns for Replicate 2

qpcr_replicate_2 <- subset(qpcr_duplicates, NewSample == "2AB" | 
                             NewSample == "4AB" | NewSample == "6AB" | 
                             NewSample == "8AB") %>%
  select(NewSample, gene, MeanCT, MeanSE)

write.csv(qpcr_replicate_2, "qpcr_replicate_2.csv")
