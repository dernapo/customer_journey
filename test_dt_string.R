#########################################################
## Evaluate frequency of customer journeys
#########################################################

## Load libraries ####
library(data.table)
library(ggplot2)
library(dplyr)
library(scales)

theme_set(theme_light())

## Test data ####

dt <- data.table(options = LETTERS[sample(1:2, 200, replace=T)],
                 b=letters[1:5],
                 ID=sample(1:100, 200, replace=T))


## Evaluation ####

dt_summary <- dt[order(b)][, .(journey = paste0(options, collapse = ""), count = .N), by = ID]


dt_summary2 <- dt_summary[, .(count_n = .N), .(journey, count)][, quota := count_n/sum(count_n), .(count)]


## Display result ####

n_loops <- 3

dt_summary2[count == n_loops, -c("count_n", "count")][order(-quota)]


## Visualization ####

dt_summary2[count == n_loops] %>% 
  ggplot(aes(x=reorder(journey, quota), 
             y = quota, 
             fill = journey, 
             label = paste0(100 *round(quota, 2), "%"))) +
  geom_col() +
  geom_text(hjust = 1, color = "black", size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "none") + 
  coord_flip() +
  labs(title = "Customer journey analysis", 
       subtitle = paste0("Most common journeys for ", n_loops, " loops and 2 options (A or B)"),
       x = "", 
       y = "")

ggsave(here::here("output", paste0(Sys.Date(), "_cust_journey.png")))


