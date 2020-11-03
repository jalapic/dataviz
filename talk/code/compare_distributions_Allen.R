## Modified from Allen et al 2019 Wellcome Open Research
  
#Load script
source('https://raw.githubusercontent.com/RainCloudPlots/RainCloudPlots/master/tutorial_R/summarySE.R')
source('https://raw.githubusercontent.com/RainCloudPlots/RainCloudPlots/master/tutorial_R/simulateData.R')
source('https://raw.githubusercontent.com/RainCloudPlots/RainCloudPlots/master/tutorial_R/R_rainclouds.R')


head(summary_simdat)


px <- ggplot(summary_simdat, aes(x = group, y = score_mean, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(data = summary_simdat, aes(ymin = score_mean - se, ymax = score_mean+se), width = .2)+
  guides(fill=FALSE)+
  ylim(0, 60) +
  ggtitle("Barplot ± SEM")+
  ylab("Score mean") +
  xlab("Group")+
  scale_fill_manual(values=c("#f5ca20","#4da9f0"))

px

#Histogram

py <- ggplot(simdat, aes(x = score, fill = group)) +
  geom_histogram(binwidth = 5,color="black", lwd=0.1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~ group) +
  guides(fill=FALSE)+
  xlab("Score")+
  ggtitle("Histogram")+
  scale_fill_manual(values=c("#f5ca20","#4da9f0"))+
  theme(strip.background = element_blank())
py

#boxplot

pz <- ggplot(simdat, aes(x = group, y = score, fill = group)) +
  geom_boxplot(outlier.size = .5)+
  guides(fill=FALSE)+
  ggtitle("Boxplot")+
  ylab("Score")+
  scale_fill_manual(values=c("#f5ca20","#4da9f0"))


library(gridExtra)  
grid.arrange(px,pz,py,nrow=1)  
  

