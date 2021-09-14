########################################
#                                      
#  Analysis of QB In-Stride Accuracy                                
#                                      
#                                      
#                                      
#  This script is the main data-prep.
#  
#  It must be run before any models 
#  are run. 
#
# current as of 9/13/2021
########################################



library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggtext)
library(ggrepel)
library(patchwork)
library(caret)
library(randomForest)
library(lme4)
library(gt)


weeks <- seq(1,17)

df_tracking <- data.frame()
for(w in weeks){
  
  #temperary dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("C:/Users/dtrei/Documents/papers/Random Data Stuff/R Projects/Big Data Bowl 2021/NFL-Tackle-Probability/week",w,".csv"),
                               col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}

# standardize x and y values based on play direction
df_tracking <- df_tracking %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))

rm(df_tracking_temp)

#standardize orientation
pbp <- df_tracking %>%
  mutate(o = ifelse(o <=180, o+180, o)) 

targets <- read.csv("C:/Users/dtrei/Documents/papers/Random Data Stuff/R Projects/Big Data Bowl 2021/NFL-Tackle-Probability/targetedReceiver.csv")
plays <- read.csv("C:/Users/dtrei/Documents/papers/Random Data Stuff/R Projects/Big Data Bowl 2021/NFL-Tackle-Probability/plays.csv")

QBs <- pbp %>%
  filter(position == "QB") %>%
  select(gameId, playId, nflId, displayName) %>%
  unique()

median_frames <- pbp %>%
  group_by(event) %>%
  summarise(frame = median(frameId))

games <- read.csv("C:/Users/dtrei/Documents/papers/Random Data Stuff/R Projects/Big Data Bowl 2021/NFL-Tackle-Probability/games.csv")


completed_passes <- pbp %>%
  mutate(complete = ifelse(event %in% c("pass_outcome_caught", "pass_outcome_touchdown"), 
                           1,
                           0)) %>%
  group_by(gameId, playId) %>%
  summarise(complete = max(complete))


# in-stride variables at pass-forward and pass-arrival
pass_forward_arrived <- pbp %>%
  left_join(targets, by = c("gameId", "playId")) %>%
  left_join(completed_passes, by = c("gameId", "playId")) %>%
  filter(nflId == targetNflId) %>%
  filter(event %in% c("pass_forward", "pass_arrived")) %>%
  group_by(gameId, playId) %>%
  mutate(n = n()) %>%
  filter(n == 2) %>%
  mutate(o_diff = ifelse(event == "pass_arrived", 
                         abs(lag(o)-o), NA),
         a_diff = ifelse(event == "pass_arrived", 
                         abs(lag(a)-a), NA),
         s_diff = ifelse(event == "pass_arrived", 
                         abs(lag(s)-s), NA)) 
# 
# pbp %>%
#   left_join(targets, by = c("gameId", "playId")) %>%
#   left_join(completed_passes, by = c("gameId", "playId")) %>%
#   filter(nflId == targetNflId) %>%
#   View()
#   

# receiver spatial relationship to QB variables 
QB_loc <- pbp %>%
  filter(position == "QB", event == "pass_forward") %>%
  select(gameId, playId, x, y) %>%
  rename(qb_x = x,
         qb_y = y) %>%
  unique() %>%
  inner_join(target_tracking) %>%
  mutate(qb_dist = sqrt(((qb_x - x_target_pass_forward)**2 +
                           (qb_y - y_target_pass_forward)**2) + 0.0001),
         qb_x_dist = abs(qb_x - x_target_pass_forward),
         y_dist = abs(qb_y - y_target_pass_forward),
         angle_to_qb =  REdaS::rad2deg(acos(y_dist/qb_dist))) %>%
  select(gameId, playId, angle_to_qb, qb_x_dist)


# tracking features for target receiver
target_tracking_pf <- pbp %>%
  left_join(targets, by = c("gameId", "playId")) %>%
  filter(nflId == targetNflId, event %in% c("pass_forward")) %>%
  select(gameId, playId, x:dir, event) %>%
  group_by(gameId, playId) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  padr::fill_by_function(o, median) %>%
  padr::fill_by_function(dir, median) %>%
  pivot_wider(id_cols = gameId:playId, names_from = event, values_from = x:dir, 
              names_prefix = "target_")

target_tracking_pa <- pbp %>%
  left_join(targets, by = c("gameId", "playId")) %>%
  filter(nflId == targetNflId, event %in% c("pass_arrived")) %>%
  select(gameId, playId, x:dir, event) %>%
  group_by(gameId, playId) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  padr::fill_by_function(o, median) %>%
  padr::fill_by_function(dir, median) %>%
  pivot_wider(id_cols = gameId:playId, names_from = event, values_from = x:dir, 
              names_prefix = "target_")

target_tracking <- inner_join(target_tracking_pf, target_tracking_pa,
                              by = c("gameId", "playId"))
  

# tracking features for defenders
non_target_tracking_pf <- pbp %>%
  filter(!(position %in% c("QB", "WR", "TE", "FB", "HB", "RB"))) %>%
  left_join(targets, by = c("gameId", "playId")) %>%
  filter(nflId != targetNflId, 
         event %in% c("pass_forward"),
         displayName != "Football") %>%
  select(gameId, playId, nflId, x:dir, event) %>%
  pivot_wider(id_cols = c(gameId, playId, nflId), names_from = event, 
              values_from = x:dir, values_fill = NA) %>%
  # filter(!is.na(o_pass_forward), 
  #        !is.na(dir_pass_forward)) %>%
  mutate(across(x_pass_forward:dir_pass_forward, as.character)) %>%
  mutate(across(x_pass_forward:dir_pass_forward, as.numeric)) 
  
non_target_tracking_pa <- pbp %>%
  filter(!(position %in% c("QB", "WR", "TE", "FB", "HB", "RB"))) %>%
  left_join(targets, by = c("gameId", "playId")) %>%
  filter(nflId != targetNflId, 
         event %in% c("pass_arrived"),
         displayName != "Football") %>%
  select(gameId, playId, nflId, x:dir, event) %>%
  pivot_wider(id_cols = c(gameId, playId, nflId), names_from = event, 
              values_from = x:dir, values_fill = NA) %>%
  # filter(!is.na(o_pass_forward), 
  #        !is.na(dir_pass_forward)) %>%
  mutate(across(x_pass_arrived:dir_pass_arrived, as.character)) %>%
  mutate(across(x_pass_arrived:dir_pass_arrived, as.numeric)) 


non_target_tracking <- inner_join(non_target_tracking_pf, non_target_tracking_pa,
                                  by = c("gameId", "playId", "nflId"))


# main nearest defender df
nearest_defenders <- left_join(non_target_tracking, target_tracking, 
                               by = c("gameId", "playId")) %>%
  mutate(x_dist_pf = x_pass_forward - x_target_pass_forward,
         x_dist_pa = x_pass_arrived - x_target_pass_arrived,
         y_dist_pf = y_pass_forward - y_target_pass_forward,
         y_dist_pa = y_pass_arrived - y_target_pass_arrived,
         s_diff_pf = s_pass_forward - s_target_pass_forward,
         s_diff_pa = s_pass_arrived - s_target_pass_arrived,
         a_diff_pf = a_pass_forward - a_target_pass_forward,
         a_diff_pa = a_pass_arrived - a_target_pass_arrived,
         dir_diff_pf = dir_pass_forward - dir_target_pass_forward,
         dir_diff_pa = dir_pass_arrived - dir_target_pass_arrived,
         euc_dist_pf = sqrt((x_pass_forward - x_target_pass_forward)**2 + 
                              (y_pass_forward - y_target_pass_forward)**2),
         euc_dist_pa = sqrt((x_pass_arrived - x_target_pass_arrived)**2 + 
                              (y_pass_arrived - y_target_pass_arrived)**2)) %>%
  group_by(gameId, playId) %>%
  mutate(nearest_def = case_when(
    euc_dist_pf == min(euc_dist_pf) ~ "nearest",
    euc_dist_pf ==  min(euc_dist_pf[euc_dist_pf != min(euc_dist_pf)]) ~ "second_nearest",
    T ~ "other")) %>%
  ungroup() %>%
  filter(!(is.na(x_pass_arrived)),
         nearest_def %in% c("nearest", "second_nearest")) %>%
  group_by(gameId, playId) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = gameId:playId, values_from = x_dist_pf:euc_dist_pa, 
              names_from = nearest_def)
  #tidytext::reorder_within(x = nflID, by = euc_dist_pf, within = c(gameId, playId))


pass_forward_arrived <- pass_forward_arrived %>%
  left_join(nearest_defenders, by = c("gameId", "playId"))


# EDA ---------------------------------------------------------------------


# this is just som eof my personal EDA. feel free to ignore
pbp %>%
  ggplot(., aes(x = frameId)) +
  geom_density(fill = "darkred", color = "darkred", alpha = .1) +
  theme_light() +
  geom_vline(xintercept = unlist(median_frames[median_frames$event == "pass_forward", 2]), 
             lty = 2) +
  annotate("text", x = unlist(median_frames[median_frames$event == "pass_forward", 2]) - 3,
           y = 0.0035, label = "Pass Forward", angle = 90)  +
  geom_vline(xintercept = unlist(median_frames[median_frames$event == "pass_arrived", 2]), 
             lty = 2) +
  annotate("text", x = unlist(median_frames[median_frames$event == "pass_arrived", 2]) - 3,
           y = 0.0035, label = "Pass Arrived", angle = 90)

pass_forward_arrived %>%
  na.omit() %>%
  ggplot(., aes(x = o_diff, y = reorder(route, o_diff), 
                fill = factor(complete),
                color = factor(complete))) +
  geom_density_ridges(scale = 1, 
                      size = 1.5,
                      #fill = "darkred", color = "darkred", 
                      alpha = .1) +
  theme_light() +
  labs(y = "Target Receiver Route",
       x = "Absolute Difference in Orientation\nFrom Pass Forward to Pass Arrival")


pass_forward_arrived %>%
  na.omit() %>%
  ggplot(., aes(x = o_diff, color = factor(complete))) +
  geom_density(alpha = .8, size = 1.5) +
  theme_light() +
  facet_wrap(~route, scale = "free") +
  labs(y = "Target Receiver Route",
       x = "Absolute Difference in Orientation\nFrom Pass Forward to Pass Arrival")


 

