# a small script to analyse absorbance results of a BCA protein conc analysis from a 96-well plate

BCA <- read.csv("BCA.csv")

str(BCA)

BCA_df <- data.frame(BCA)

View(BCA_df)

BCA_stds <- subset(BCA_df, Well == c("A01", "A02", "A03", "A04", "A05", "A06"))

View(BCA_stds)

BCA_stds <- subset(BCA_df, Well == c("A01", "A02", "A03", "A04", "A05", "A06"), select=c(Well, Absorbance))
BCA_std

BCA_data1 <- subset(BCA_df, Well == c("D01", "D02", "D03"), select=c(Well, Absorbance))
View(BCA_data)
View(BCA_data1)
#save this data.
write.csv(BCA_data1, "BCA_data1.csv")
write.csv(BCA_stds, "BCA_stds.csv")
BCA_data2 <- subset(BCA_df, Well == c("F01", "F02", "F03", "F04", "F05", "F06"), select=c(Well, Absorbance))
BCA_data2
write.csv(BCA_data2, "BCA_data2.csv")
# Add a new column for the concentration of the BSA standards to the BCA_stds dataframe
BCA_stds[, "BSA"] <- c(0, 0.125, 0.25, 0.5, 1, 2)
# Subtract the absorbance of the blank (0.12) from the absorbance values of the standards, and include add this new column Cor_Abs to the BCA_stds dataframe
BCA_stds$Cor_Abs <- BCA_stds$Absorbance - 0.12
BCA_stds.df <- data.frame(BCA_stds)
#Create a new subset for the BSA standards containing the BSA conc. and the new corrected absorbance values
BCA_stds_cor <- subset(BCA_stds.df, Well == c("A01", "A02", "A03", "A04", "A05", "A06"), select=c(BSA, Cor_Abs))
View(BCA_stds_cor)
write.csv(BCA_stds_cor, "BCA_stds_cor.csv")
cor(BCA_stds_cor)
BCA_stds_matrix <- as.matrix(BCA_stds_cor)
BCA_stds_matrix <- as.matrix(nrow=6, ncol=6, BCA_stds_cor)

library(Hmisc)

# to calculate R^2, correlation coeffiecient between the two variables
cor(BCA_stds_cor$BSA, BCA_stds_cor$Cor_Abs)

#correct the absorbance values here by subtracting the blank, and include this new column in the data frame
BCA_data1$Cor_Abs <- BCA_data1$Absorbance - 0.12
#check the new dataframe
BCA_data1

#update the BCA_data1 dataframe to contain this new column
BCA_data1.df <- data.frame(BCA_data1)
BCA_data1_cor <- subset(BCA_data1.df, Well == c("D01", "D02", "D03"), select=c(Well, Cor_Abs))
write.csv(BCA_data1_cor, "BCA_data1_cor.csv")
BCA_data2$Cor_Abs <- BCA_data2$Absorbance - 0.12
BCA_data2
BCA_data2.df <- data.frame(BCA_data2)
BCA_data2.df
BCA_data2_cor <- subset(BCA_data2.df, Well == c("F01", "F02", "F03", "F04", "F05", "F06"), select=c(Well, Cor_Abs))
BCA_data2_cor
write.csv(BCA_data2_cor, "BCA_data2_cor.csv")
#Add sample labels to the data and update the file
BCA_data1_cor$Samples <- c("Mock", "5nM", "10nM")
write.csv(BCA_data1_cor, "BCA_data1_cor.csv")
BCA_data2_cor$Samples <- c("Mock-EGTA", "Mock+EGTA", "KDI-EGTA", "KDI+EGTA", "KDII-EGTA", "KDII+EGTA")
BCA_data2_cor
write.csv(BCA_data2_cor, "BCA_data2_cor.csv")
BCA_data1_cor

fit <- lm(BCA_stds_cor$Cor_Abs ~ BCA_stds_cor$BSA)
fit
plot(BCA_stds_cor$Cor_Abs ~ BCA_stds_cor$BSA)

#add regression ine to the plot above
abline(fit)
attributes(fit)

# the value 707.57 was obtained in excel, using whole number absorbance values, change the corrected absorbance values to the whole numbers by multiplying by 1000
data1 <- BCA_data1_cor$Cor_Abs * 1000
data1
conc_data1 <- data1/707.57
conc_data1
BCA_data1_cor$conc <- conc_data1
write.csv(BCA_data1_cor, "BCA_data1_cor.csv")
BCA_data2_cor$conc <- conc_data2
data2 <- BCA_data2_cor$Cor_Abs * 1000
conc_data2 <- data2/707.57
conc_data2
BCA_data2_cor$conc <- conc_data2
BCA_data2_cor
write.csv(BCA_data2_cor, "BCA_data2_cor.csv")

# merge all the analysed dataframes together into one, and save to csv
BCA_merged <- data.frame(BCA_stds_cor, BCA_data1_cor, BCA_data2_cor)
BCA_merged
write.csv(BCA_merged, "BCA_17.7.15.csv")