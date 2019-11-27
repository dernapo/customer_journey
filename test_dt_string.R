#########################################################
## Evaluate frequency of customer journeys
#########################################################

## Load libraries ####
library(data.table)
library(ggplot2)
library(dplyr)
library(scales)

## Test data ####

dt <- data.table(options = LETTERS[sample(1:2, 200, replace=T)],
                 b=letters[1:5],
                 ID=sample(1:300, 200, replace=T))


## Evaluation ####

dt_summary <- dt[order(b)][, .(journey = paste0(options, collapse = ""), count = .N), by = ID]


dt_summary2 <- dt_summary[, .(count_n = .N), .(journey, count)][, quota := count_n/sum(count_n), .(count)]


## Display result ####

dt_summary2[count == 2, -c("count_n", "count")][order(-quota)]


## Visualization ####

dt_summary2[count == 2] %>% 
  ggplot(aes(x=journey, y = quota, fill = journey, label = paste0(100 *round(quota, 2), "%"))) +
  geom_col() +
  geom_text(hjust = 0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d() +
  theme(legend.position = "none") + 
  coord_flip()




