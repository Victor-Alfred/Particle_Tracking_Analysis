setwd("C:/Users/spind/OneDrive - sheffield.ac.uk/1_Live_analysis/white/") # on Home PC

setwd("/Users/victoralfred/Desktop/1_Live_analysis/white")

rm(list=ls())

library(ggplot2)
library(dplyr)
library(plyr)
library(ggpmisc)
library(lme4)
library(lattice)
library(RColorBrewer)
#display.brewer.all()

robustness_data <- as.data.frame(read.csv("wt_spas_robustness.csv"))
str(robustness_data)

###########################

# obtained from ....
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(size=2) +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


ggplotRegression(lm(von_mises_SD ~ eccentricity, data = robustness_data)) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15)) +
  stat_poly_eq(formula=y~x,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), 
               parse = TRUE)


####################

# performed on data with a single group

fit <- lm(von_mises_SD ~ eccentricity, robustness_data)
summary(fit)

# to obtain confidence intervals of the slope: calculated by slope +/- 1.96*SE
confint(fit, 'eccentricity', level=0.95)

#########################

display.brewer.all()
colourCount = length(unique(robustness_data$Genotype))
getPalette = colorRampPalette(brewer.pal(8, "Accent"))
 # Add "+ scale_colour_manual(values = getPalette(colourCount))" to ggplot

##################################

# Obtain regression equation for each group of genotype: obtained from ....

equation = function(file, y = file$von_mises_SD, x=file$eccentricity) {
  mod = lm(y ~ x,data=file)
  mod_sum = summary(mod)
  formula = sprintf("y= %.3f %+.3f*x", coef(mod)[1], coef(mod)[2])
  r = mod_sum$r.squared
  r2 = sprintf("r2= %.3f", r)
  x  = cor.test(~x + y,data=file)
  r0 = sprintf("r= %.3f", x[4])
  p1 = pf(mod_sum$fstatistic[1],mod_sum$fstatistic[2],mod_sum$fstatistic[3],lower.tail=F)
  p =sprintf("p = %.3f", p1)
  n0 = length(mod_sum$residual)
  n1 = sprintf("N = %.f", n0)
  data.frame(formula=formula, r=r0,r2=r2, p=p,n=n1, stringsAsFactors=FALSE)
}

equation_end = ddply(robustness_data, c("Genotype"), equation) 

############################
# To assign colours to groups

# group.colors <- c(A = "#333BFF", B = "#CC6600", C ="#9633FF", D = "#E2FF33", E = "#E3DB71")
# Add " + scale_fill_manual(values=group.colors)" to specify colors

########################

ggplot(robustness_data) + 
  aes(x=robustness_data$eccentricity, y=robustness_data$von_mises_SD, 
      col=as.factor(Genotype)) +
  geom_point(size = 1.5) +
  stat_smooth(method = "lm", se = FALSE) + 
  xlab("Eccentricity") +
  ylab("von-mises SD") +
  labs(fill='Genotype') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title=element_text(size=15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_colour_discrete(name  ="Genotype") +
  geom_text(data=equation_end, aes(x=c(0.98,0.98, 0.98, 0.98), y=c(57, 58, 59, 60), 
                                   label=formula), show_guide=F)


###################################

ggplot(aes(x = eccentricity, y = von_mises_SD), data = robustness_data) + 
  geom_point(aes(color = Genotype), size = 1.5) +
  geom_smooth(aes(color = Genotype), method= "lm", se = F) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.title=element_text(size=15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  geom_text(data=equation_end, aes(x=c(0.98,0.98, 0.98, 0.98), y=c(56, 58, 60, 62), 
                                   label=formula), show_guide=F)



######################

# To compare regression between Genotypes?? Which one of these is accurate?

anova(lm(von_mises_SD~eccentricity,data=robustness_data),
      lm(von_mises_SD~eccentricity*Genotype,data=robustness_data))

# OR

fm1 <- lm(von_mises_SD ~ Genotype + eccentricity, robustness_data)
fm3 <- lm(von_mises_SD ~ Genotype + eccentricity/Genotype, robustness_data)
anova(fm1, fm3)

# Comparing if a genotype differs from the rest?

is.WT <- robustness_data$Genotype == 'WT'; 
fmWT <- lm(von_mises_SD ~ Genotype + eccentricity/is.WT, robustness_data); 
anova(fm1, fmWT)


is.COS <- robustness_data$Genotype == 'COS'; 
fmCOS <- lm(von_mises_SD ~ Genotype + eccentricity/is.COS, robustness_data); 
anova(fm1, fmCOS)

########################################

#  from: https://www.r-bloggers.com/can-we-do-better-than-r-squared/
# estimated predictive R2

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}

pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  return(pred.r.squared)
}

# linear model

fit <- lm(von_mises_SD ~ eccentricity, robustness_data)

pred.r.squared <- pred_r_squared(fit)
pred.r.squared

###########################################

robustness_data$transport_deviation = abs(robustness_data$cell_direction - robustness_data$transport_direction)

ddply(robustness_data,~Genotype,summarise,mean=mean(transport_deviation),sd=sd(transport_deviation))

robustness_data$transport_alignment = robustness_data$eccentricity *
                                  robustness_data$von_mises_SD * 
                                  robustness_data$transport_deviation

ddply(robustness_data,~Genotype,summarise,mean=mean(transport_alignment),sd=sd(transport_alignment), 
      N = length(transport_alignment))

write.csv(robustness_data, "wt_spas_robustness.csv")


ggplot(robustness_data) + 
  aes(x=robustness_data$Genotype, y=robustness_data$transport_deviation) + 
  geom_boxplot() + geom_jitter(width = 0.2)

##########################



