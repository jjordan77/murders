library(tidyverse)
library(ggrepel)
load("rda/murders.rda")

## Murder rate by state 

## Can note the large state to state variability by generating a barplot showing the murder rate by state: 

murders %>% mutate(abb = reorder(abb, rate)) %>%
  ggplot(aes(abb, rate)) +
  geom_bar(width = 0.5, stat = "identity", color = "black") +
  coord_flip()

ggsave("figs/barplot.png")

## For seeing total murders by state in a scatterplot (and colour-coding by region)

r <- murders %>% summarise(rate = sum(total) / sum(population) * 10^6) %>% .$rate

murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") + 
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()


