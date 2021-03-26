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
         x_pos_list_numeric = map(x_pos_list_str, as.numeric),
         y_pos_list_str = str_split(yPos, pattern = 'a'),
         y_pos_list_numeric = map(y_pos_list_str, as.numeric),
         length_x = map_dbl(x_pos_list_numeric, length),
         length_y = map_dbl(y_pos_list_numeric, length),
         time_list_str = str_split(time, pattern = 'a'),
         length_t = map_dbl(time_list_str, length),
         submit_t
          )

data_clean$length_x == data_clean$length_y
# okay so x-y pairs always have the same length, that's good


data_clean$x_pos_list_str[[1]]

data_clean %>% 
  filter(index == 1) %>% 
  unnest(c(x_pos_list_numeric, y_pos_list_numeric)) %>% 
  ggplot(aes(x = x_pos_list_numeric, y = -y_pos_list_numeric)) +
  geom_point() 
