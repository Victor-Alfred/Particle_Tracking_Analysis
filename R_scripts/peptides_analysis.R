# small script written to subset data from mass spec experiments

setwd("~/Desktop/Morelli/")
dir()

peptides <- as.data.frame(read.csv("peptides.csv"))

str(peptides)

require(dplyr)

# Extract all IP columns into a separate dataframe

peptides_ip <- select(peptides, Identified_Proteins, X140305_IP,X140331_02_ip,
                      X140331_IP, X141022_IP, X150220_IP, X150720_IP, X150803_IP,
                      X160303_gfpsnap29, X160509_IP, X160509_gfpsnap29) 

# Confirm that rows have correct number
nrow(peptides_ip)

write.csv(peptides_ip, "peptides_ip.csv")

# Extract control columns into separate dataframe

peptides_ctrl <- select(peptides, Identified_Proteins, X140305_ctrl, X140331_02_ctrl,
                        X140331_Ctrl, X141022_ctrl, X150220_Ctrl, X150720_Ctrl,
                        X150803_Ctrl, X160509_ctrl, X160509_gfp)

nrow(peptides_ctrl)

peptides_ctrl <- write.csv(peptides_ctrl, "peptides_ctrl.csv")

# Extract rows with values > 0 in at least one column

peptides_ip_extr<- peptides_ip[apply(peptides_ip[,-1], 1, function(x) !all(x==0)),]

write.csv(peptides_ip_extr, "peptides_ip_extr.csv")

#Extract peptides do not occur in any ip experiment
peptides_ip_extr_zero<- peptides_ip[apply(peptides_ip[,-1], 1, function(x) all(x==0)),]

write.csv(peptides_ip_extr_zero, "peptides_ip_extr_zero.csv")

# Extract peptides that occur in more than one control experiment
peptides_ctrl_extr<- peptides_ctrl[rowSums(peptides_ctrl==0) < 8, ]
write.csv(peptides_ctrl_extr, "peptides_ctrl_extr.csv")

#---------------------------------------------------------------------------------
# Alternatively, replace all 0's with NAs
# Replace all 0's in peptides_ctrl with NAs in new dataframe with new function, foo

foo <- function(x) {
        x[x == 0] <- NA
    x
}

peptides_ctrl_2 <- data.frame(lapply(peptides_ctrl, foo))
nrow(peptides_ctrl_2)
write.csv(peptides_ctrl_2, "peptides_ctrl_2.csv")
peptides_ctrl_extr<- peptides_ctrl_2[rowSums(is.na(peptides_ctrl_2)) < 8, ]
write.csv(peptides_ctrl_extr, "peptides_ctrl_extr.csv")
#-----------------------------------------------------------------------------------

# Add a new column with the number of times a peptide appears in both control and IP

peptide_count_ctrl <- function(x){
    x$count <- 9 - rowSums(x == 0)
    return(x)
}

peptides_ctrl_extr <- peptide_count(peptides_ctrl_extr)
write.csv(peptides_ctrl_extr, "peptides_ctrl_extr.csv")

peptide_count_ip <- function(x){
     x$count <- 10 - rowSums(x == 0)
     return(x)
}

peptides_ip_extr <- peptide_count_ip(peptides_ip_extr)
write.csv(peptides_ip_extr, "peptides_ip_extr.csv")

# Exclude the peptides in ip_extr that are also in ctrl_extr

peptides_specific <- peptides_ip_extr[!(peptides_ip_extr$Identified_Proteins %in% 
                               peptides_ctrl_extr$Identified_Proteins),]

write.csv(peptides_specific, "peptides_specific.csv")


#------------------------------------------------------------------------------
# Peptides in ctrl that occur at least once
peptides_ctrl_extr2<- peptides_ctrl[apply(peptides_ctrl[,-1], 1, function(x)
                                          !all(x==0)),]

write.csv(peptides_ctrl_extr2, "peptides_ctrl_extr2.csv")

# Use this new Ctrl list to filter out unspecific peptides from ip_extr

peptides_specific2 <- peptides_ip_extr[!(peptides_ip_extr$Identified_Proteins %in% 
                         peptides_ctrl_extr2$Identified_Proteins),]

write.csv(peptides_specific2, "peptides_specific2.csv")

# Retrieve all columns contained in the original dataframe for this subset

peptides_specific2_details <- peptides[peptides$Identified_Proteins %in%
                                peptides_specific2$Identified_Proteins, ]

# Peptides that do not occur at all in the ctrl.

peptides_ctrl_zero <- peptides_ctrl[apply(peptides_ctrl[,-1], 1, function(x)
                                          all(x==0)),]

write.csv(peptides_ctrl_zero, "peptides_ctrl_zero.csv")


