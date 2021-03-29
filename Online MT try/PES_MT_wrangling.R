setwd('~/GitHub/dsb_github/OnlineMT/Online MT try/')

library(magrittr)
library(tidyverse)
library(mousetrap)


fname <- 'mt_food.csv'
data_headers <- read_csv(fname, col_names = FALSE, n_max = 1) %>% unlist()
data_raw <- read_csv(fname, skip = 3, col_names = data_headers)

data_raw$xPos[[1]] # Is this just for inspection?

data_clean <- data_raw %>% 
  mutate(x_pos_list_str = str_split(xPos, pattern = 'a'), #splitting the string by separator "a"
         x_pos = map(x_pos_list_str, as.numeric), # turning x_pos_list_string in a numeric variable
         y_pos_list_str = str_split(yPos, pattern = 'a'),
         y_pos = map(y_pos_list_str, as.numeric),
         time_list_str = str_split(time, pattern = 'a'),
         time_list = map(time_list_str, as.numeric),
         length_x = map_dbl(x_pos, length),# creates a variable with how many objects x_pos contains
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
         trial = map2_dbl(ready_time_lower, click_time_higher, ~ which(.x & .y)), # here I'm not sure what is happening
         subject_trial = str_c(subject, trial, sep = '_'))

data_long %>% 
  filter(subject == 1) %>% 
  ggplot(aes(x = x_pos, y = -y_pos, color = as.factor(trial))) +
  geom_point() + 
  geom_path() +
  theme_bw() +
  theme(legend.position = 'none')
# Add condition variable to data_long
data_long$condition <- ifelse(data_long$trial <= 9,1,2)

# Do the x and y positions need to be rescaled such that the coordinate (0|0) is in the center of the screen?
# In this data set we have three distinctive browser resolutions: "1920x1080" "1920x1200" "1280x800"
# Can this be used to rescale the coordinates like so (example resolution 1920x1080): 
#scale_x <- function(x) {x - 1920/2} 
#scale_y <- function(y) {y - 1080/2}
#?
# Before or after mt_import_long?

# import to mouse-trap:

mouse_trap_data <- mt_import_long(data_long, 
                                  xpos_label = 'x_pos',
                                  ypos_label = 'y_pos',
                                  timestamps_label = 'time_list',
                                  mt_id_label = 'subject_trial')

mt_plot(data = mt_subset(mouse_trap_data, subject == 1), use = "trajectories")
## remapping the coordinates with mt_remap_symmetric
mouse_trap_data_remap <- mt_remap_symmetric(data = mouse_trap_data ,dimensions = c("xpos", "ypos"),remap_ypos= "down")
mt_plot(data = mt_subset(mouse_trap_data_remap, subject == 1), use = "trajectories")

# align trajectories into standard coordinate system --> this probably doesn't make sense when we haven't rescales the coordinates
#mouse_trap_data_aligned <- mt_align(data = mouse_trap_data_remap , align_start = F, align_end = T, align_side = "left", coordinates = c(0, 0, -1, 1.5))
#mt_plot(data = mt_subset(mouse_trap_data_aligned, subject == 1), use = "trajectories")

#time normalize the data
mouse_trap_data_normal <- mt_time_normalize(data = mouse_trap_data_remap)
mt_plot(data = mt_subset(mouse_trap_data_normal, subject == 1), use = "tn_trajectories")
#space normalize
mouse_trap_data_normal <- mt_spatialize(data = mouse_trap_data_normal)
mt_plot(data = mt_subset(mouse_trap_data_normal, subject == 1), use = "sp_trajectories")

#get important parameters
mouse_trap_data_normal<- mt_measures(data = mouse_trap_data_normal)

# Plotting trajectories
mt_plot(mouse_trap_data_normal, use="tn_trajectories",
        color="condition")
# We have two different start positions, is this due to the fact that we didn'T scale 
# according to browser resolution?

# Merge measures with trial data
mt_results <- dplyr::inner_join(
  mouse_trap_data_normal$data, mouse_trap_data_normal$measures,
  by="mt_id")

# Looking at the MAD (as simple example analysis)
describe(mt_results$MAD)
mad.t <- t.test(mt_results$MAD~mt_results$condition, paired = TRUE)
mad.t


