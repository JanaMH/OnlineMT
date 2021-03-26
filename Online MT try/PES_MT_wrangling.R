setwd('~/GitHub/dsb_github/OnlineMT/Online MT try/')

library(magrittr)
library(tidyverse)
library(mousetrap)


fname <- 'mt_food.csv'
data_headers <- read_csv(fname, col_names = FALSE, n_max = 1) %>% unlist()
data_raw <- read_csv(fname, skip = 3, col_names = data_headers)

data_raw$xPos[[1]]

data_clean <- data_raw %>% 
  mutate(x_pos_list_str = str_split(xPos, pattern = 'a'),
         x_pos = map(x_pos_list_str, as.numeric),
         y_pos_list_str = str_split(yPos, pattern = 'a'),
         y_pos = map(y_pos_list_str, as.numeric),
         time_list_str = str_split(time, pattern = 'a'),
         time_list = map(time_list_str, as.numeric),
         length_x = map_dbl(x_pos, length),
         length_y = map_dbl(y_pos, length),
         length_t = map_dbl(time_list, length),
         load_t = str_split(onLoadTime, pattern = 'a') %>% map(as.numeric),
         ready_t = str_split(onReadyTime, pattern = 'a') %>% map(as.numeric),
         click_t = str_split(buttonClickTime, pattern = 'a') %>% map(as.numeric),
         latency_list = str_split(latency, pattern = 'a') %>% map(as.numeric),
         subject = row_number())

data_long <- data_clean %>% 
  select(subject, x_pos, y_pos, time_list, load_t, ready_t, click_t) %>% 
  unnest(c(x_pos, y_pos, time_list)) %>% 
  mutate(ready_time_lower = map2(ready_t, time_list, ~ .x < .y),
         click_time_higher = map2(click_t, time_list, ~ .x > .y),
         trial = map2_dbl(ready_time_lower, click_time_higher, ~ which(.x & .y)))


