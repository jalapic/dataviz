### Penguins

#install.packages("palmerpenguins")

library(palmerpenguins)
head(penguins)

library(tidyverse)
library(ggtext) #remotes::install_github("wilkelab/ggtext")


ggplot(penguins, aes(x=bill_length_mm, y=bill_depth_mm,color=species))+
         geom_point(alpha=.5, size=2) +
         theme_minimal() +
#  scale_color_manual(values = c("#ed8907", "#be02e8", "#05755b"))
 scale_color_manual(
    name = NULL,
    values = c(Adelie = "#ed8907", Chinstrap = "#be02e8", Gentoo = "#05755b"),
   ) +
  labs(
    title = "**Palmer's Penguins**  
    <span style='font-size:11pt'>Bill length vs. bill width for 
    <span style='color:#ed8907;'>Adelie</span>, 
    <span style='color:#be02e8;'>Chinstrap</span>, and
    <span style='color:#05755b;'>Gentoo</span> penguins
    </span>",
    x = "Bill Length (mm)", y = "Bill Depth (mm)"
  ) +
  theme_minimal() +
  stat_smooth(method='lm',se=F)+
  theme(
    plot.title = element_markdown(lineheight = 1.1),
    legend.text = element_markdown(size = 11),
    legend.position = 'none'
  )
