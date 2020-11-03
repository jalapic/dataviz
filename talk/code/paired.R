### Paired Data

library(tidyverse)
theme_set(theme_classic())

x <- c(9.979896,  9.888075, 11.884131,  8.357905,  9.487193, 10.308105, 10.677518, 12.134670,
       11.710156, 9.753958,  9.431904, 11.937808, 10.395920, 10.374756, 10.361309)
  
  
y <- c(9.176737,  9.504186, 11.646672,  8.474969,  8.717509,  9.150576, 10.261958,
       11.149556, 12.198357,9.875420, 10.840575, 10.194270,  8.966235,  8.585355,
       10.518585)
  
mean(x)  #10.45
mean(y)  #9.95
sd(x)    # 1.07
sd(y)    # 1.15
sd(x)/sqrt(length(x)) #0.28
sd(y)/sqrt(length(y))  #0.30


df <- data.frame(vals=c(x,y), group = rep(c("Before","After"),each=15), id=1:15)
df$group<-factor(df$group,levels=c("Before","After"))
head(df)


df.sum <- df %>% group_by(group) %>% 
  summarise(mean = mean(vals), sd = sd(vals),n=n()) %>%
  mutate(sem = sd/sqrt(n))

## Dynamite
p1=ggplot() +
  geom_col(data = df.sum, aes(group,mean), fill = 'gray89', alpha=.4, color = "black") +
  geom_errorbar( aes(x=group,
                     ymin = mean-sem, ymax = mean+sem), 
                 data = df.sum, width = 0.2) +
  xlab("Group") +
  ylab("Mean +/- S.E.M") +
  ggtitle("Dynamite Plot")

p1


## Slope Graph

p2 = ggplot(df, aes(x=group, y=vals, group=id)) + 
  geom_point() +
  geom_line() +
  xlab("Group") +
  ylab("Score") +
  ggtitle("Slope Graph")

p2

## Difference Scores

df.wide <- df %>% pivot_wider(names_from = group, values_from = vals)
df.wide$dif <- df.wide$After - df.wide$Before

p3=ggplot(data=df.wide, aes(x=factor(1), y=dif)) + 
  geom_jitter(width=.05, alpha=.3, size=3) + 
  theme_classic()+
  geom_hline(yintercept=0, col="red", lwd=1, lty=2)+
  ggtitle("Difference Scores")+
  ylab("Score") +
  xlab("") +
  coord_flip() +
  scale_x_discrete(labels=NULL)

p3


### Paired Scatterplot
head(df.wide)
p4=ggplot(df.wide, aes(x=Before, y = After)) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color='red') +
  ggtitle("Paired Scatterplot")+
  ylab("After") +
  xlab("Before")
p4


library(gridExtra)

grid.arrange(p1,p2,p3,p4, nrow=1)

t.test(x,y,paired=T)





#### Code to find datasets....

nperms<-10000000

resx<-vector('list',nperms)
resy<-vector('list',nperms)
resse<-vector('list',nperms)
resmean<-vector('list',nperms)
respval<-vector('list',nperms)

for(i in 1:nperms){
x <-  rnorm(20, 10, 2)
y <-  rnorm(20, 10, 2)
resse[[i]]<-(sd(x)/sqrt(length(x)))- (sd(y)/sqrt(length(y)))
resmean[[i]]<-mean(x) - mean(y)
respval[[i]]<-t.test(x-y)[3][[1]]
resx[[i]]<-x
resy[[i]]<-y
}

a1 <- which(unlist(respval)<.05)
a2 <- which(abs(unlist(resse))<.5)
a3 <- which(abs(unlist(resmean))<.5)

tt <- table(c(a1,a2,a3))
tt[tt==3]


