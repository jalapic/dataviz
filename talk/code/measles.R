
### Measles Plots

# from:  https://rafalab.github.io/dsbook/data-visualization-principles.html

library(tidyverse)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)
names(us_contagious_diseases)


the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% 
  mutate(state = reorder(state, rate)) 


states <- c("Texas", "California", "New York", "Florida", "Massachusetts", "Oklahoma")

dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() + 
  ylab("Cases per 10,000")  + 
  geom_vline(xintercept=1963, col = "blue")

range(dat$year)

dat %>% filter(state %in% states & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() + 
  ylab("Cases per 10,000")  + 
  geom_vline(xintercept=1963, col = "blue") +
  facet_wrap(~state)+
  theme_minimal() +
  ggtitle("Measles Cases per 10,000 1928-2002")

dat %>% 
  ggplot(aes(year, rate, group=state)) +
  geom_line() + 
  ylab("Cases per 10,000")  + 
  geom_vline(xintercept=1963, col = "blue") +
  theme_minimal() +
  ggtitle("Measles Cases per 10,000 1928-2002")



dat %>% ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept=1963, col = "blue") +
  theme_minimal() +  
  theme(panel.grid = element_blank(), 
        legend.position="bottom", 
        text = element_text(size = 8)) +
  ggtitle(the_disease) + 
  ylab("") + xlab("")


avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE) / 
              sum(population, na.rm = TRUE) * 10000)

dat %>% 
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey30", 
            show.legend = FALSE, alpha = 0.1, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1.25) +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50), 
            mapping = aes(x, y, label="US average"), 
            color="black") + 
  geom_vline(xintercept=1963, col = "blue")


# with median and IQR 


