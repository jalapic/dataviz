library(tidyverse)

df <- read_csv("talk/data/maternal.csv")  

head(df)

ggplot(df, aes(x=reorder(Behavior,-Percent), y = Percent, fill=Strain)) + 
  geom_col(alpha=.8,position='dodge') +
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(fill=NA,colour = "black"),
        text = element_text(size=14)) +
  xlab("") + 
  scale_fill_manual(values = c("#403891ff", "#a65c85ff", "#f68f46ff"))

