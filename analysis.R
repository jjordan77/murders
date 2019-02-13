library(tidyverse)
library(ggrepel)
load("rda/murders.rda")

## Murder rate by state (barplot)

## Can note the large state to state variability by generating a barplot showing the murder rate by state: 
## Murder rate calculated by total number of murders divided by 100,000

murders %>% mutate(abb = reorder(abb, rate)) %>%
  ggplot(aes(abb, rate)) +
  geom_bar(width = 0.5, stat = "identity", color = "black") +
  coord_flip() +
  xlab("State Abbreviation") +
  ylab("Murder Rate") + 
  ggtitle("Murder Rate by State") +
  theme_economist()

ggsave("figs/barplot.png")

## Murder rate by state (scatterplot)
## Based on total number of murders and colour-coded by region

## Plot also clearly shows that states with higher populations also have more murders.

national_murder_rate <- murders %>% summarise(rate = sum(total) / sum(population) * 10^6) %>% .$rate

murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(national_murder_rate), lty = 2, color = "darkgrey") + 
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

## Murder rate by region (boxplot)
## Examine the murder rate based on region. Individual points for each state.
murders %>% ggplot(aes(y=rate, x=region, fill = region)) + 
  geom_boxplot() + 
  geom_point() +
  xlab("Murder Rate") + 
  ylab("Region") +
  ggtitle("Murder Rate by Region") +
  theme_economist()


