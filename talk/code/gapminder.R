
## Gapminder

library(gapminder)
library(tidyverse)
head(gapminder)

df <- gapminder %>% filter(continent!="Oceania")
head(df)

gap.sum<-df %>%
  group_by(year,continent) %>%
  summarise(median = median(lifeExp),
            lq = quantile(lifeExp,.25),
            uq = quantile(lifeExp, .75),
  )

gap.sum

ggplot()+
  geom_line(data=df, aes(x=year, y=lifeExp, group=country),color="gray52",alpha=.2)+
  geom_ribbon(data=gap.sum, aes(x = year, ymin = lq, ymax = uq), fill = "#428bff", alpha=.3) +
  geom_line(data=gap.sum, aes(x= year, y = median),lwd=1, color="#093e94") +
  facet_wrap(~continent) +
  ylab("Life Expectancy (years)") +
  xlab("Year") +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(fill=NA,colour = "black"),
        text = element_text(size=14))
