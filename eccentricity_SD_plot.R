setwd("/Users/victoralfred/Desktop/Live_analysis/white")
robustness_data <- as.data.frame(read.csv("1_robustness.csv"))
str(robustness_data)


plot(robustness_data$von_mises_SD, robustness_data$cell_area)
plot(robustness_data$von_mises_SD, robustness_data$eccentricity)
library(ggplot2)


ggplot() + 
  geom_point(data=robustness_data,
             aes(x=von_mises_SD, y=eccentricity, colour =as.factor(direction)), size =2) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



ggplot(robustness_data) + 
  aes(x=robustness_data$von_mises_SD, y=robustness_data$eccentricity, 
      col=as.factor(robustness_data$direction)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", se = FALSE) + 
  xlab("SD") +
  ylab("Eccentricity") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
