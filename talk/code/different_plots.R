## Code for Making Different types of Distribution Graphs

library(tidyverse)
theme_set(theme_classic())

set.seed(70)
Aa = rnorm(19, 10, 2)
Bb = rnorm(17, 15, 1.5)
Cc = rnorm(21, 18, 2.2)
Dd = rnorm(19, 23, 3)
Ee = rnorm(20, 26, 2)
Ff = rnorm(22, 29, 3.5)
Gg = rnorm(18, 32, 2)
Hh = rnorm(16, 33, 1)

df<-
data.frame(vals = c(Aa,Bb,Cc,Dd,Ee,Ff,Gg,Hh),
           group = c(rep("A", length(Aa)),
                     rep("B", length(Bb)),
                     rep("C", length(Cc)),
                     rep("D", length(Dd)),
                     rep("E", length(Ee)),
                     rep("F", length(Ff)),
                     rep("G", length(Gg)),
                     rep("H", length(Hh))
           ))

df

library(Hmisc)

df.sum <- df %>% group_by(group) %>% 
             summarise(mean=mean(vals),
                       median = median(vals),
                       sd=sd(vals),
                       n = n(),
                       lq = quantile(vals,.25),
                       uq = quantile(vals,.75)) %>%
  mutate(sem = sd/sqrt(n),
         sem2 = 2*(sd/sqrt(n)))

df.sum





## Dynamite
p1=ggplot() +
  geom_col(data = df.sum, aes(group,mean), fill = 'gray89', alpha=.4, color = "black") +
  geom_errorbar( aes(x=group,
                     ymin = mean-sem, ymax = mean+sem), 
                 data = df.sum, width = 0.2) +
  xlab("Group") +
  ylab("Mean +/- S.E.M") +
  ggtitle("Dynamite Plot")


## Strip Plot
p2=ggplot(data=df, aes(x=group, y=vals)) + 
  geom_jitter(width=.05, alpha=.3) + 
  theme_classic()+
  stat_summary(fun= median, fun.min=median, fun.max=median, geom="crossbar", width=0.5, color="red") +
  ggtitle("Strip Plot")+
  ylab("Score") +
  xlab("Group")


## Box Plot
p3=ggplot(data=df, aes(x=group, y=vals)) + 
  geom_boxplot(outlier.size=.9) + 
  theme_classic() +
  ggtitle("Boxplot")+
  ylab("Score")+
  xlab("Group")




## Violin Plot
p4=ggplot(data=df, aes(x=group, y=vals)) + 
  geom_violin(fill="gray44",alpha=.2,trim = FALSE) + 
  theme_classic() +
  ggtitle("Violin Plot") + 
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black"
  )+
  ylab("Score")+
  xlab("Group")



## Ridgeline Plot
library(ggridges)
library(ggplot2)

p5=ggplot(df, aes(x=vals, y=group)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, fill="grey80") +
  geom_text(data=df %>% group_by(group) %>% 
              summarise(vals=median(vals)),
            aes(label=sprintf("%1.1f", vals)), 
            position=position_nudge(y=-0.1), colour="red", size=2.5) +
  ggtitle("Ridgeline Plot") +
  xlab("Score")+
  ylab("Group")




## Raincloud plot

# ggplot(df, aes(x = vals, y = group)) +
#   geom_density_ridges(
#     jittered_points = TRUE, position = "raincloud",
#     alpha = 0.4, scale = 0.7
#   )
# 

# position = "points_jitter"
p6=ggplot(df, aes(x = vals, y = group)) +
  geom_density_ridges(
    jittered_points = TRUE, position = "points_jitter",
    alpha = 0.3, scale = 0.7, size=.5
  ) +
  xlab("Score")+
  ylab("Group")+
  ggtitle("Raincloud Plot")


# Rug plot

p7=      ggplot(df, aes(x = vals, y = group)) +
      geom_density_ridges(
        jittered_points = TRUE,
        position = position_points_jitter(width = 0.05, height = 0),
        point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
      ) +
        xlab("Score")+
        ylab("Group")+
        ggtitle("Rug Plot")
      


library(gridExtra)

grid.arrange(p1,p2,p3,p4,p5,p6,p7, nrow = 2)




#Boxplot overlaid data points
p8=ggplot(data=df, aes(x=group, y=vals)) + 
  geom_boxplot(outlier.shape=NA) + 
  theme_classic() +
  ggtitle("Boxplot with Points")+
  ylab("Score")+
  xlab("Group") +
  geom_jitter(width=.15, alpha=.4)
p8


#Notched Boxplots 
p9=ggplot(data=df, aes(x=group, y=vals)) + 
  geom_boxplot(outlier.shape=NA, notch = TRUE) + 
  theme_classic() +
  ggtitle("Notched Boxplot")+
  ylab("Score")+
  xlab("Group")
p9



grid.arrange(p2, p3,p8,p9, nrow = 1)



## Strip plots with median + IQR

df$group1 <- match(df$group,LETTERS[1:8])
df.sum$group1 <- match(df.sum$group,LETTERS[1:8])-.15

p10=ggplot() + 
  geom_jitter(data=df, aes(x=group1, y=vals),width=.05, alpha=.3) + 
  theme_classic()+
  ggtitle("Strip Plot: medians & IQR")+
  ylab("Score") +
  xlab("Group") + 
  geom_pointrange(data=df.sum, aes(x=group1, y=median, ymin = lq, ymax = uq),color='red')+
  scale_x_continuous(breaks=1:8,labels=LETTERS[1:8])

p10



## Strip plots with median + 95% confidence median

library(rcompanion)

groupwiseMedian(vals ~ group,
                data       = df,
                conf       = 0.95,
                R          = 5000,
                percentile = TRUE,
                bca        = FALSE,
                digits     = 3) -> medianconf



medianconf$group1 <- match(medianconf$group,LETTERS[1:8])-.15

p11=ggplot() + 
  geom_jitter(data=df, aes(x=group1, y=vals),width=.05, alpha=.3) + 
  theme_classic()+
  ggtitle("Strip Plot: 95% CI of median")+
  ylab("Score") +
  xlab("Group") + 
  geom_pointrange(data=medianconf, aes(x=group1, y=Median, ymin = Percentile.lower, ymax = Percentile.upper),color='dodgerblue')+
  scale_x_continuous(breaks=1:8,labels=LETTERS[1:8])

p11







grid.arrange(p2,p10,p11,nrow=1)
