########################################
#                                      
#  This script is the xYAC
#  (pass-forward) model
#  
#
#  current as of 9/13/2021
#  
########################################


# model results -----------------------------------------------------------

# -- in-stride models -- #

# naive predictions
naive_pred1_o <- test_mod_o %>%
  select(gameId, playId, route) %>%
  left_join(test_mod_o %>%
              group_by(route) %>%
              summarise(o_diff = mean(o_diff, na.rm = T)),
            by = "route")

naive_pred2_o <- test_mod_o %>%
  bind_cols(rep(mean(test_mod_o$o_diff), nrow(test_mod_o)))

naive_pred1_a <- test_mod_a %>%
  select(gameId, playId, route) %>%
  left_join(test_mod_a %>%
              group_by(route) %>%
              summarise(a_diff = mean(a_diff, na.rm = T)),
            by = "route")

naive_pred2_a <- test_mod_a %>%
  bind_cols(rep(mean(test_mod_a$a_diff), nrow(test_mod_a)))

naive_pred1_s <- test_mod_s %>%
  select(gameId, playId, route) %>%
  left_join(test_mod_s %>%
              group_by(route) %>%
              summarise(s_diff = mean(s_diff, na.rm = T)),
            by = "route")

naive_pred2_s <- test_mod_s %>%
  bind_cols(rep(mean(test_mod_s$s_diff), nrow(test_mod_s)))


# create DF with model results for table
mod_rmse <- c(RMSE(pred = pred_xgb_o, obs = test_mod_o$o_diff),
              RMSE(pred = pred_xgb_a, obs = test_mod_a$a_diff),
              RMSE(pred = pred_xgb_s, obs = test_mod_s$s_diff))


naive_rmse_strat <- c(RMSE(pred = naive_pred1_o$o_diff, obs = test_mod_o$o_diff),
                      RMSE(pred = naive_pred1_a$a_diff, obs = test_mod_a$a_diff),
                      RMSE(pred = naive_pred1_s$s_diff, obs = test_mod_s$s_diff)) #stratified by route


naive_rmse <- c(RMSE(pred = naive_pred2_o$...25, obs = test_mod_o$o_diff),
                RMSE(pred = naive_pred2_a$...25, obs = test_mod_a$a_diff),
                RMSE(pred = naive_pred2_s$...25, obs = test_mod_s$s_diff))

movement_type <- c("Orientation", "Acceleration", "Speed")

mod_results_table <- tibble(movement_type, mod_rmse, naive_rmse_strat, naive_rmse) %>%
  gt() %>%
  cols_label(
    movement_type = "Model",
    mod_rmse = "Model RMSE",
    naive_rmse_strat = "Stratified Naive RMSE",
    naive_rmse = "Naive RMSE"
  ) %>% 
  tab_header(
    title = "In-Stride XGB Model Results",
  ) %>%
  
  tab_footnote(
    footnote = "Stratified by target receiver route", 
    locations = cells_column_labels(
      columns = 3 # note
    )
  ) %>%
  tab_options(
    column_labels.font.weight = "bold"
  ) %>%
  fmt_number(
    columns = 2:4,
    decimals = 2
  )

gtsave(mod_results_table, "mod_results_table.png")


# -- pass-outcome models -- #

# naive predictions
naive_pred1_yac_pf <- test_mod_yac %>%
  left_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(gameId, playId, route) %>%
  left_join(test_mod %>%
              group_by(route) %>%
              summarise(yac = mean(yac, na.rm = T)),
            by = "route")

naive_pred2_yac_pf <- test_mod_yac %>%
  left_join(completed_passes) %>%
  filter(complete == 1) %>%
  bind_cols(rep(mean(test_mod_yac$yac), nrow(test_mod_yac)))

naive_pred1_yac_pa <- test_mod_yac_pa %>%
  left_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(gameId, playId, route) %>%
  left_join(test_mod %>%
              group_by(route) %>%
              summarise(yac = mean(yac, na.rm = T)),
            by = "route")

naive_pred2_yac_pa <- test_mod_yac_pa %>%
  left_join(completed_passes) %>%
  filter(complete == 1) %>%
  bind_cols(rep(mean(test_mod_yac_pa$yac), nrow(test_mod_yac_pa)))

naive_pred <- rep(sum(df_Mod_comp$passResult)/nrow(df_Mod_comp), nrow(test_mod_comp))

naive_pred1_comp <- test_mod_comp %>%
  select(gameId, playId, route) %>%
  left_join(test_mod_comp %>%
              group_by(route) %>%
              summarise(result = mean(passResult, na.rm = T)),
            by = "route")

# model results into DF for table
mod_outcome_results <- c(RMSE(pred = pred_xgb_yac, obs = test_mod_yac$yac),
                         RMSE(pred = pred_xgb_yac_pa, obs = test_mod_yac_pa$yac),
                         ModelMetrics::logLoss(test_mod_comp$passResult, pred_xgb_comp))


naive_outcome_strat <- c(RMSE(pred = naive_pred1_yac_pf$yac, obs = test_mod_yac$yac),
                         RMSE(pred = naive_pred1_yac_pa$yac, obs = test_mod_yac_pa$yac),
                         ModelMetrics::logLoss(test_mod_comp$passResult, naive_pred1_comp$result)) #stratified by route


naive_outcome <- c(RMSE(pred = naive_pred2_yac_pf$...33, obs = test_mod_yac$yac),
                RMSE(pred = naive_pred2_yac_pa$...31, obs = test_mod_yac_pa$yac),
                ModelMetrics::logLoss(test_mod_comp$passResult, naive_pred))

model_facet <- c("xYAC (pass-forward)", "xYAC (pass-arrival)", "CPOE")
eval_metric <- c("RMSE", "RMSE", "Log-Loss")


mod_outcomes_table <- tibble(model_facet, mod_outcome_results, 
                             naive_outcome_strat, naive_outcome, eval_metric) %>%
  gt(
    rowname_col = "model_facet",
    groupname_col = "eval_metric"
    ) %>%
  cols_label(
    eval_metric = "Metric",
    mod_outcome_results = "Metric Value",
    naive_outcome_strat = "Naive Metric\nValue (Strat.)",
    naive_outcome = "Naive Metric Value"
  ) %>% 
  tab_header(
    title = "Pass-Outcome XGB Model Results",
  ) %>%
  tab_footnote(
    footnote = "Stratified by target receiver route", 
    locations = cells_column_labels(
      columns = 2 # note
    )
  ) %>%
  tab_options(
    column_labels.font.weight = "bold"
  ) %>%
  fmt_number(
    columns = 1:4,
    decimals = 2
  ) %>%
  fmt_missing(
    columns = 3,
    missing_text = "-"
  )

gtsave(mod_outcomes_table, "mod_outcomes_table.png")


# data prep ---------------------------------------------------------------


completions <- left_join(df_Mod, plays %>%
                           filter(passResult != "S", passResult != "IN") %>%
                           select(gameId, playId, passResult)) %>%
  left_join(QBs, by = c("gameId", "playId")) %>%
  group_by(nflId) %>%
  summarise(completions = sum(passResult == "C", na.rm = T),
            n = n(),
            comp_pct = completions/n) 


yac <- pbp %>%
  left_join(targets) %>%
  filter(nflId == targetNflId) %>%
  select(gameId, playId, event, frameId, x) %>%
  group_by(gameId, playId) %>%
  mutate(lastFrame = max(frameId)) %>%
  left_join(completed_passes) %>%
  mutate(x = ifelse(event == "touchdown", 100, x)) %>%
  filter(complete == 1, event %in% c("pass_outcome_caught") | frameId == lastFrame) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 2) %>%
  mutate(event = case_when(
    event == "pass_outcome_caught" ~ "catch",
    event == "None" ~ "final_spot"
  )) %>%
  pivot_wider(id_cols = gameId:playId, names_from = event, values_from = x) %>%
  mutate(yac = round(final_spot - catch)) %>%
  select(gameId, playId, yac) %>%
  left_join(QBs) %>%
  group_by(nflId) %>%
  summarise(mean_yac = mean(yac, na.rm = T))


# main aggregation of all metrics by QB
by_QB <- df_Mod_a %>%
  left_join(df_Mod_o %>% select(gameId, playId, o_diff, pred_o_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_s %>% select(gameId, playId, s_diff, pred_s_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_yac %>% select(gameId, playId, yac, pred_yac, yac_oe), 
            by = c("gameId", "playId"))  %>%
  left_join(df_Mod_yac_pa %>% select(gameId, playId, pred_yac_pa, yac_diff, adj_yac_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_comp %>% select(gameId, playId, passResult, pred_comp, CPOE), 
            by = c("gameId", "playId")) %>%
  left_join(QBs, by = c("gameId", "playId")) %>%
  mutate(o_diff_oe = abs(o_diff - pred_o_diff),
         a_diff_oe =  a_diff - pred_a_diff,
         s_diff_oe = s_diff - pred_s_diff) %>%
  unique() %>%
  group_by(nflId, displayName) %>%
  summarise(n = n(),
            o_diff_oe = sum(o_diff_oe, na.rm = T)/n,
            o_diff = sum(o_diff, na.rm = T)/n,
            pred_o_diff = sum(pred_o_diff, na.rm = T)/n,
            s_diff_oe = sum(s_diff_oe, na.rm = T)/n,
            s_diff = sum(s_diff, na.rm = T)/n,
            pred_s_diff = sum(pred_s_diff, na.rm = T)/n,
            mean_yac = sum(yac, na.rm = T)/n,
            mean_pred_yac = sum(pred_yac, na.rm = T)/n,
            mean_yac_oe = sum(yac_oe, na.rm = T)/n,
            yac_diff = sum(yac_diff, na.rm = T)/n,
            adj_yac_diff = sum(adj_yac_diff, na.rm = T)/n,
            a_diff = sum(a_diff, na.rm = T)/n,
            pred_a_diff = sum(pred_a_diff, na.rm = T)/n,
            a_diff_oe = sum(a_diff_oe, na.rm = T)/n,
            CPOE = sum(CPOE, na.rm = T)/n,
            comp = sum(passResult == 1, na.rm = T),
            pred_comp = sum(pred_comp, na.rm = T)) %>%
  filter(n > 150) %>%
  #mutate(a = o_diff_oe/n, 
         # o_diff = o_diff/n ,
         # s_diff_oe = s_diff_oe/n,
         # s_diff = s_diff/n,
         # #n =#/n n(),
         # mean_yac = mean_yac/n  ,
         # mean_pred_yac = mean_pred_yac/n,
         # mean_yac_oe = mean_yac_oe/n, 
         # yac_diff = yac_diff/n,
         # adj_yac_diff = adj_yac_diff/n,
         # a_diff = a_diff/n         , 
         # pred_a_diff =  pred_a_diff/n,
         # a_diff_oe = a_diff_oe/n,
         # CPOE = CPOE/n
  #       ) %>%
  left_join(completions %>% select(-n), by = "nflId") %>%
  ungroup() 

# full PBP with all metrics
df_Final <- df_Mod_a %>%
  left_join(df_Mod_o %>% select(gameId, playId, o_diff, pred_o_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_s %>% select(gameId, playId, s_diff, pred_s_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_yac %>% select(gameId, playId, yac, pred_yac, yac_oe), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_comp %>% select(gameId, playId, pred_comp, CPOE), 
            by = c("gameId", "playId")) %>%
  left_join(QBs, by = c("gameId", "playId"))


# stability plots  ----------------------------------------------------------

# within-season stability

weeks_df <- games %>%
  select(gameId, week) %>%
  unique()


by_QB_first <- df_Mod_a %>%
  left_join(df_Mod_o %>% select(gameId, playId, o_diff, pred_o_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_s %>% select(gameId, playId, s_diff, pred_s_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_yac %>% select(gameId, playId, yac, pred_yac, yac_oe), 
            by = c("gameId", "playId"))  %>%
  left_join(df_Mod_yac_pa %>% select(gameId, playId, yac, yac_diff, adj_yac_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_comp %>% select(gameId, playId, passResult, pred_comp, CPOE), 
            by = c("gameId", "playId")) %>%
  left_join(QBs, by = c("gameId", "playId")) %>%
  left_join(weeks_df) %>%
  filter(week < 9) %>%
  mutate(o_diff_oe = abs(o_diff - pred_o_diff),
         a_diff_oe =  a_diff - pred_a_diff,
         s_diff_oe = s_diff - pred_s_diff) %>%
  group_by(nflId, displayName) %>%
  summarise(n = n(),
            o_diff_oe = sum(o_diff_oe, na.rm = T)/n,
            o_diff = sum(o_diff, na.rm = T)/n,
            pred_o_diff = sum(pred_o_diff, na.rm = T)/n,
            s_diff_oe = sum(s_diff_oe, na.rm = T)/n,
            s_diff = sum(s_diff, na.rm = T)/n,
            pred_s_diff = sum(pred_s_diff, na.rm = T)/n,
            mean_yac = sum(yac, na.rm = T)/n,
            mean_pred_yac = sum(pred_yac, na.rm = T)/n,
            mean_yac_oe = sum(yac_oe, na.rm = T)/n,
            yac_diff = sum(yac_diff, na.rm = T)/n,
            adj_yac_diff = sum(adj_yac_diff, na.rm = T)/n,
            a_diff = sum(a_diff, na.rm = T)/n,
            pred_a_diff = sum(pred_a_diff, na.rm = T)/n,
            a_diff_oe = sum(a_diff_oe, na.rm = T)/n,
            CPOE = sum(CPOE, na.rm = T)/n,
            comp = sum(passResult == 1, na.rm = T),
            pred_comp = sum(pred_comp, na.rm = T)) %>%
  left_join(completions %>% select(-n), by = "nflId") %>%
  ungroup() %>%
  mutate(half = "1") # first half

by_QB_second <- df_Mod_a %>%
  left_join(df_Mod_o %>% select(gameId, playId, o_diff, pred_o_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_s %>% select(gameId, playId, s_diff, pred_s_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_yac %>% select(gameId, playId, yac, pred_yac, yac_oe), 
            by = c("gameId", "playId"))  %>%
  left_join(df_Mod_yac_pa %>% select(gameId, playId, yac, yac_diff, adj_yac_diff), 
            by = c("gameId", "playId")) %>%
  left_join(df_Mod_comp %>% select(gameId, playId, passResult, pred_comp, CPOE), 
            by = c("gameId", "playId")) %>%
  left_join(QBs, by = c("gameId", "playId")) %>%
  left_join(weeks_df) %>%
  filter(week >= 9) %>%
  mutate(o_diff_oe = abs(o_diff - pred_o_diff),
         a_diff_oe =  a_diff - pred_a_diff,
         s_diff_oe = s_diff - pred_s_diff) %>%
  group_by(nflId, displayName) %>%
  summarise(n = n(),
            o_diff_oe = sum(o_diff_oe, na.rm = T)/n,
            o_diff = sum(o_diff, na.rm = T)/n,
            pred_o_diff = sum(pred_o_diff, na.rm = T)/n,
            s_diff_oe = sum(s_diff_oe, na.rm = T)/n,
            s_diff = sum(s_diff, na.rm = T)/n,
            pred_s_diff = sum(pred_s_diff, na.rm = T)/n,
            mean_yac = sum(yac, na.rm = T)/n,
            mean_pred_yac = sum(pred_yac, na.rm = T)/n,
            mean_yac_oe = sum(yac_oe, na.rm = T)/n,
            yac_diff = sum(yac_diff, na.rm = T)/n,
            adj_yac_diff = sum(adj_yac_diff, na.rm = T)/n,
            a_diff = sum(a_diff, na.rm = T)/n,
            pred_a_diff = sum(pred_a_diff, na.rm = T)/n,
            a_diff_oe = sum(a_diff_oe, na.rm = T)/n,
            CPOE = sum(CPOE, na.rm = T)/n,
            comp = sum(passResult == 1, na.rm = T),
            pred_comp = sum(pred_comp, na.rm = T)) %>%
  left_join(completions %>% select(-n), by = "nflId") %>%
  ungroup() %>%
  mutate(half = "2") # second half

# combined DF
half_splits <- bind_rows(by_QB_first, by_QB_second) %>%
  select(displayName, o_diff_oe, a_diff_oe, s_diff_oe, adj_yac_diff, n, half) %>%
  group_by(displayName) %>%
  mutate(n_ = n()) %>%
  filter(n_ == 2) %>%
  pivot_wider(id_cols = displayName, values_from = o_diff_oe:n, 
              names_from = half, names_prefix = "half_") %>%
  na.omit() %>%
  filter(n_half_1 > 100 & n_half_2 > 100)

o_diff_oe_stability_plot <- half_splits %>%
  ggplot(., aes(o_diff_oe_half_1, o_diff_oe_half_2)) +
  geom_smooth(method = "lm", color = "darkblue", fill = "lightgrey", alpha = .4) +
  geom_point(size = 3.5, color = "darkred", alpha = .8) +
  theme_light() +
  labs(x = "**Orientation** Difference Over Expected - Weeks 1-8",
       y = "**Orientation** Difference Over Expected - Weeks 9-17",
       title = "Within Season Correlation of<br>**Orientation** Difference Over Expected",
       subtitle = paste0("R^2 : ", 
                         round(cor(half_splits$o_diff_oe_half_1, 
                                   half_splits$o_diff_oe_half_2)**2, 3))) +
  theme(plot.subtitle = element_markdown(),
        plot.title = element_markdown(),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown())
  


a_diff_oe_stability_plot <- half_splits %>%
  ggplot(., aes(a_diff_oe_half_1, a_diff_oe_half_2)) +
  geom_smooth(method = "lm", color = "darkblue", fill = "lightgrey", alpha = .4) +
  geom_point(size = 3.5, color = "darkred", alpha = .8) +
  theme_light() +
  labs(x = "**Acceleration** Difference Over Expected - Weeks 1-8",
       y = "\n**Acceleration** Difference Over Expected - Weeks 9-17",
       title = "Within Season Correlation of<br>**Acceleration** Difference Over Expected",
       subtitle = paste0("R^2 : ", 
                         round(cor(half_splits$a_diff_oe_half_1, 
                                   half_splits$a_diff_oe_half_2)**2, 3))) +
  theme(plot.subtitle = element_markdown(),
        plot.title = element_markdown(),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown())

s_diff_oe_stability_plot <- half_splits %>%
  ggplot(., aes(s_diff_oe_half_1, s_diff_oe_half_2)) +
  geom_smooth(method = "lm", color = "darkblue", fill = "lightgrey", alpha = .4) +
  geom_point(size = 3.5, color = "darkred", alpha = .8) +
  theme_light() +
  labs(x = "**Speed** Difference Over Expected - Weeks 1-8",
       y = "\n**Speed** Difference Over Expected - Weeks 9-17",
       title = "Within Season Correlation of<br>**Speed** Difference Over Expected",
       subtitle = paste0("R^2 : ", 
                         round(cor(half_splits$s_diff_oe_half_1, 
                                   half_splits$s_diff_oe_half_2)**2, 3))) +
  theme(plot.subtitle = element_markdown(),
        plot.title = element_markdown(),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown())

yac_diff_oe_stability_plot <- half_splits %>%
  ggplot(., aes(adj_yac_diff_half_1, adj_yac_diff_half_2)) +
  geom_smooth(method = "lm", color = "darkblue", fill = "lightgrey", alpha = .4) +
  geom_point(size = 3.5, color = "darkred", alpha = .8) +
  geom_text_repel(aes(label = displayName)) +
  theme_light() +
  labs(x = "**YAC** Difference - Weeks 1-8",
       y = "\n**YAC** Difference - Weeks 9-17",
       title = "Within Season Correlation of<br>**YAC** Difference",
       subtitle = paste0("R^2 : ", 
                         round(cor(half_splits$adj_yac_diff_half_1, 
                                   half_splits$adj_yac_diff_half_2)**2, 3))) +
  theme(plot.subtitle = element_markdown(),
        plot.title = element_markdown(),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown())


stability_plots <- o_diff_oe_stability_plot + 
  a_diff_oe_stability_plot +
  s_diff_oe_stability_plot
ggsave("stability_plots.png", stability_plots, width = 15, height = 6)


# metric plots -------------------------------------------------------------------


# Metric vs. YAC Plots ####

# o_diff vs yac
rsq_o_yac <- round(cor(by_QB$o_diff_oe, by_QB$mean_yac_oe, use = "complete.obs")**2, 3)
o_diff_yacoe_plot <- by_QB %>%
  ggplot(., aes(o_diff_oe, mean_yac_oe)) +
  geom_hline(yintercept = mean(by_QB$mean_yac_oe), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(by_QB$o_diff_oe, na.rm = T), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = 0.3) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  #geom_text_repel(aes(label = displayName) , box.padding = 0.4, alpha = .8) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(2, 5), 
                        name = "Number\nof Passes") +
  theme_light() +
  labs(title = "Effect of **Orientation** Difference on YAC",
       subtitle = paste0("Min. 150 passes, R-Squared: ", rsq_o_yac),
       #caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Actual** - **Expected** Absolute Orientation Difference",
       y = "Average Yards After Catch Over Expected")+
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "none",
        plot.title = element_markdown()
        ) 

ggsave("o_diff_yacoe_plot.png", o_diff_yacoe_plot, width = 12, height = 8)


# a_diff vs yac
rsq_a_yac <- round(cor(by_QB$a_diff_oe, by_QB$mean_yac_oe, use = "complete.obs")**2, 3)

round(cor(by_QB$a_diff_oe[by_QB$displayName != "Nick Mullens" & 
                            by_QB$displayName != "Patrick Mahomes"],
          by_QB$mean_yac_oe[by_QB$displayName != "Nick Mullens" & 
                              by_QB$displayName != "Patrick Mahomes"],
          use = "complete.obs")**2, 3)
by_QB$mean_yac_oe[by_QB$displayName != "Nick Mullens"]

a_diff_yacoe_plot <- by_QB %>%
  ggplot(., aes(a_diff_oe, mean_yac_oe)) +
  geom_hline(yintercept = mean(by_QB$mean_yac_oe), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(by_QB$a_diff_oe, na.rm = T), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = 0.3) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  #geom_text_repel(aes(label = displayName) , box.padding = 0.4, alpha = .8) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(2, 5), 
                        name = "Number of Passes") +
  theme_light() +
  labs(title = "Effect of **Acceleration** Difference on YAC",
       subtitle = paste0("Min. 150 passes, R-Squared: ", rsq_a_yac),
       #caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Actual** - **Expected** Acceleration Difference",
       y = "Average Yards After Catch Over Expected")+
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        plot.title = element_markdown()
        ) 

ggsave("a_diff_yacoe_plot.png", a_diff_yacoe_plot, width = 12, height = 8)


# a_diff vs yac
rsq_s_yac <- round(cor(by_QB$s_diff_oe, by_QB$mean_yac_oe, use = "complete.obs")**2, 3)
s_diff_yacoe_plot <- by_QB %>%
  ggplot(., aes(s_diff_oe, mean_yac_oe)) +
  geom_hline(yintercept = mean(by_QB$mean_yac_oe), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(by_QB$s_diff_oe, na.rm = T), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = 0.3) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  #geom_text_repel(aes(label = displayName) , box.padding = 0.4, alpha = .8) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(2, 5), 
                        name = "Number\nof Passes") +
  theme_light() +
  labs(title = "Effect of **Speed** Difference on YAC",
       subtitle = paste0("Min. 150 passes, R-Squared: ", rsq_s_yac),
       #caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Actual** - **Expected** Speed Difference",
       y = "Average Yards After Catch Over Expected")+
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "none",
        plot.title = element_markdown()
        ) 

ggsave("s_diff_yacoe_plot.png", s_diff_yacoe_plot, width = 12, height = 8)



metric_YAC_plots_combo <- o_diff_yacoe_plot + a_diff_yacoe_plot + s_diff_yacoe_plot

ggsave("metric_YAC_plots_combo.png", metric_YAC_plots_combo, width = 15, height = 6)


# Metric vs. CPOE Plots ####

# odiff vs CPOE
rsq_o_CPOE <- round(cor(by_QB$o_diff_oe, by_QB$CPOE, use = "complete.obs")**2, 3)
o_diff_CPOE_plot <- by_QB %>%
  ggplot(., aes(o_diff_oe, CPOE)) +
  geom_hline(yintercept = mean(by_QB$CPOE), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(by_QB$o_diff_oe, na.rm = T), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = 0.3) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  #geom_text_repel(aes(label = displayName) , box.padding = 0.4, alpha = .8) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(2, 5), 
                        name = "Number\nof Passes") +
  theme_light() +
  labs(title = "Effect of **Orientation** Difference on CPOE",
       subtitle = paste0("Min. 150 passes, R-Squared: ", rsq_o_CPOE),
       #caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Actual** - **Expected** Absolute Orientation Difference",
       y = "Average CPOE")+
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "none",
        plot.title = element_markdown()) 

ggsave("o_diff_CPOE_plot.png", o_diff_CPOE_plot, width = 12, height = 8)



# a diff vs CPOE
rsq_a_cpoe <- round(cor(by_QB$a_diff_oe, by_QB$CPOE, use = "complete.obs")**2, 3)
a_diff_CPOE_plot <- by_QB  %>%
  ggplot(., aes(a_diff_oe, CPOE)) +
  geom_hline(yintercept = mean(by_QB$CPOE), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(by_QB$a_diff_oe, na.rm = T), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = .4) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  #geom_text_repel(aes(label = displayName), box.padding = 0.4) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(1, 5), 
                        name = "Number of Passes") +
  theme_light() +
  labs(title = "Effect of **Acceleration** Difference on CPOE",
       subtitle = paste0("Min. 150 passes, R-Squared: ", rsq_a_cpoe),
       # subtitle = "From Pass Forward to Pass Arrival",
       #caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Actual** - **Expected** Acceleration Difference",
       y = "Average  CPOE") +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_markdown())
ggsave("a_diff_CPOE_plot.png", a_diff_CPOE_plot, width = 12, height = 8)



# a diff vs CPOE
rsq_s_cpoe <- round(cor(by_QB$s_diff_oe, by_QB$CPOE, use = "complete.obs")**2, 3)
s_diff_CPOE_plot <- by_QB  %>%
  ggplot(., aes(s_diff_oe, CPOE)) +
  geom_hline(yintercept = mean(by_QB$CPOE), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(by_QB$s_diff_oe, na.rm = T), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = .4) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  #geom_text_repel(aes(label = displayName), box.padding = 0.4) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(1, 5), 
                        name = "Number\nof Passes") +
  theme_light() +
  labs(title = "Effect of **Speed** Difference on CPOE",
       subtitle = paste0("Min. 150 passes, R-Squared: ", rsq_s_cpoe),
       # subtitle = "From Pass Forward to Pass Arrival",
       #caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Actual** - **Expected** Speed Difference",
       y = "Average  CPOE") +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "none",
        plot.title = element_markdown())
ggsave("s_diff_CPOE_plot.png", s_diff_CPOE_plot, width = 12, height = 8)




metric_CPOE_plots_combo <- o_diff_CPOE_plot + a_diff_CPOE_plot + s_diff_CPOE_plot

ggsave("metric_CPOE_plots_combo.png", metric_CPOE_plots_combo, width = 15, height = 6)

# Metric vs. YAC diff Plots ####

# o_diff vs yac
rsq_o_yac_diff <- round(cor(by_QB$o_diff_oe, by_QB$adj_yac_diff, use = "complete.obs")**2, 3)
o_diff_yac_diff_plot <- by_QB %>%
  ggplot(., aes(o_diff_oe, adj_yac_diff)) +
  geom_hline(yintercept = mean(by_QB$adj_yac_diff), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(by_QB$o_diff_oe, na.rm = T), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = 0.3) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  #geom_text_repel(aes(label = displayName) , box.padding = 0.4, alpha = .8) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(2, 5), 
                        name = "Number\nof Passes") +
  theme_light() +
  labs(title = "Effect of **Orientation** Difference on YAC Difference",
       subtitle = paste0("Min. 150 passes, R-Squared: ", rsq_o_yac_diff),
       #caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Actual** - **Expected** Absolute Orientation Difference",
       y = "Average Yards After Catch Difference")+
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "none",
        plot.title = element_markdown()
  ) 

ggsave("o_diff_yac_diff_plot.png", o_diff_yac_diff_plot, width = 12, height = 8)


# a_diff vs yac
rsq_a_yac_diff <- round(cor(by_QB$a_diff_oe, by_QB$adj_yac_diff, use = "complete.obs")**2, 3)

round(cor(by_QB$a_diff_oe[by_QB$displayName != "Nick Mullens" & 
                            by_QB$displayName != "Patrick Mahomes"],
          by_QB$mean_yac_oe[by_QB$displayName != "Nick Mullens" & 
                              by_QB$displayName != "Patrick Mahomes"],
          use = "complete.obs")**2, 3)
by_QB$mean_yac_oe[by_QB$displayName != "Nick Mullens"]

a_diff_yac_diff_plot <- by_QB %>%
  ggplot(., aes(a_diff_oe, adj_yac_diff)) +
  geom_hline(yintercept = mean(by_QB$adj_yac_diff), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(by_QB$a_diff_oe, na.rm = T), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = 0.3) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  #geom_text_repel(aes(label = displayName) , box.padding = 0.4, alpha = .8) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(2, 5), 
                        name = "Number of Passes") +
  theme_light() +
  labs(title = "Effect of **Acceleration** Difference on YAC Difference",
       subtitle = paste0("Min. 150 passes, R-Squared: ", rsq_a_yac_diff),
       #caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Actual** - **Expected** Acceleration Difference",
       y = "Average Yards After Catch Difference")+
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        plot.title = element_markdown()
  ) 

ggsave("a_diff_yac_diff_plot.png", a_diff_yac_diff_plot, width = 12, height = 8)


# a_diff vs yac
rsq_s_yac_diff <- round(cor(by_QB$s_diff_oe, by_QB$adj_yac_diff, use = "complete.obs")**2, 3)

s_diff_yac_diff_plot <- by_QB %>%
  ggplot(., aes(s_diff_oe, adj_yac_diff)) +
  geom_hline(yintercept = mean(by_QB$adj_yac_diff), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(by_QB$s_diff_oe, na.rm = T), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = 0.3) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  #geom_text_repel(aes(label = displayName) , box.padding = 0.4, alpha = .8) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(2, 5), 
                        name = "Number\nof Passes") +
  theme_light() +
  labs(title = "Effect of **Speed** Difference on YAC Difference",
       subtitle = paste0("Min. 150 passes, R-Squared: ", rsq_s_yac_diff),
       #caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Actual** - **Expected** Speed Difference",
       y = "Average Yards After Catch Difference")+
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "none",
        plot.title = element_markdown()
  ) 

ggsave("s_diff_yac_diff_plot.png", s_diff_yac_diff_plot, width = 12, height = 8)



metric_YAC_diff_plots_combo <- o_diff_yac_diff_plot + a_diff_yac_diff_plot + s_diff_yac_diff_plot

ggsave("metric_YAC_diff_plots_combo.png", metric_YAC_diff_plots_combo, width = 15, height = 6)


# Metric OE Plots (not used in paper) ####

# actual - expected
o_diff_plot_QB <- by_QB  %>%
  ggplot(., aes(pred_o_diff, o_diff)) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  geom_smooth(method = "lm", color = "navy", size = 2) +
  #geom_text_repel(aes(label = displayName), box.padding = 0.4) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(1, 5), 
                        name = "Number of Passes") +
  theme_light() +
  labs(title = "Orientation Difference Between Pass Forward and Pass Arrival",
       subtitle = "Min. 150 Passes",
      # subtitle = "From Pass Forward to Pass Arrival",
       caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Expected** Orientation Difference (degrees)",
       y = "Average **Actual** Orientation Difference (degrees)") +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown())

ggsave("o_diff_plot_QB.png", o_diff_plot_QB, width = 12, height = 8)


# a diff vs CPOE
a_diff_oe_plot <- by_QB  %>%
  ggplot(., aes(a_diff_oe, a_diff)) +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = .4) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  geom_text_repel(aes(label = displayName), box.padding = 0.4) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(1, 5), 
                        name = "Number of Passes") +
  theme_light() +
  labs(title = "Acceleration Difference Over Expected",
       subtitle = "Min. 150 passes",
       # subtitle = "From Pass Forward to Pass Arrival",
       caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Expected** Acceleration Difference",
       y = "Average **Actual** Acceleration Difference") +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown()) +
  annotate("text", x = .125, y = 1.45, label = "Good", size = 4, fontface = "bold")+
  annotate("text", x = .025, y = 1.95, label = "Bad", size = 4, fontface = "bold")

ggsave("a_diff_oe_plot.png", a_diff_oe_plot, width = 12, height = 8)


# s diff 
s_diff_oe_plot <- by_QB  %>%
  ggplot(., aes(s_diff_oe, s_diff)) +
  geom_smooth(method = "lm", color = "navy", size = 1.3, fill = "lightgrey", alpha = .4) +
  geom_point(aes(size = n), color = "darkred", alpha = .8) +
  geom_text_repel(aes(label = displayName), box.padding = 0.4) +
  scale_size_continuous(breaks = seq(0, 600, by = 100), range = c(1, 5), 
                        name = "Number of Passes") +
  theme_light() +
  labs(title = "Speed Difference Over Expected",
       subtitle = "Min. 150 passes",
       # subtitle = "From Pass Forward to Pass Arrival",
       caption = "Plot: **@DaniTreisman** | Data: 2021 Big Data Bowl",
       x = "Average **Expected** Speed Difference",
       y = "Average **Actual** Speed Difference") +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.caption = element_markdown()) +
  annotate("text", x = .075, y = 0.85, label = "Bad", size = 4, fontface = "bold") +
  annotate("text", x = -.125, y = 1.15, label = "Good", size = 4, fontface = "bold")

ggsave("s_diff_oe_plot.png", s_diff_oe_plot, width = 12, height = 8)





#o_diff_combo_plot <- (o_diff_CPOE_plot + o_diff_yacoe_plot) / o_diff_plot_QB 
#a_diff_combo_plot <- (a_diff_CPOE_plot + a_diff_yacoe_plot) / a_diff_oe_plot 

#ggsave("o_diff_combo_plot.png", o_diff_combo_plot, width = 16, height = 12)
#ggsave("a_diff_combo_plot.png", a_diff_combo_plot, width = 16, height = 12)



# GT Table by QB ----------------------------------------------------------

all_metrics_table <- by_QB %>%
  select(displayName, s_diff_oe, a_diff_oe, o_diff_oe, mean_yac_oe, 
         adj_yac_diff, CPOE) %>%
  mutate(across(where(is.numeric), ~round(.x, 3))) %>%
  arrange(-s_diff_oe) %>%
  gt() %>%
  tab_options(heading.title.font.weight = "italic") %>%
  tab_header(title = md("**In-Stride and Play-Outcome Metrics for NFL QBs in 2020**"),
             subtitle = md("<span style = 'color:darkblue;'><b>Better</b></span> <span> <b>|</b> </span> <span style = 'color:darkred;'><b>Worse</b></span>")) %>%
  opt_row_striping() %>%
  cols_label(displayName = "QB",
             s_diff_oe = "SDOE",
             a_diff_oe = "ADOE",
             o_diff_oe = "ODOE",
             mean_yac_oe = "YACOE",
             adj_yac_diff = "YAC Diff.") %>% 
  data_color(
    columns = 2,
    colors = scales::col_numeric(
      palette = c("darkred", "lightgrey", "darkblue"),
      domain = NULL
    ),
    alpha = .8
  ) %>% 
  data_color(
    columns = 3,#vars(s_diff_oe),
    colors = scales::col_numeric(
      palette = c("darkblue", "lightgrey", "darkred"),
      domain = NULL
    ),
    alpha = .8
  ) %>% 
  data_color(
    columns = 4:7,#vars(s_diff_oe),
    colors = scales::col_numeric(
      palette = c("darkred", "lightgrey", "darkblue"),
      domain = NULL
    ),
    alpha = .8
  ) %>%
  tab_footnote(
    footnote = "Minimum 150 Passes", 
    locations = cells_column_labels(
      columns = 1 # note
    )
  ) %>%
  tab_footnote(
    footnote = "All metrics are averages per play",
    locations = cells_title(groups = "title") # note
  ) %>%  
  tab_source_note(source_note = md("**Data:** 2021 NFL Big Data Bowl")) %>% 
  fmt_number(
    columns = vars(o_diff_oe),
    decimals = 1
  ) %>% 
  fmt_number(
    columns = vars(mean_yac_oe),
    decimals = 2
  ) %>% 
  fmt_percent(
    columns = 7,
    scale_values = T
  ) %>% 
  tab_spanner(
    label = md("**Pass Outcome**"),
    columns = 5:7
  ) %>% 
  tab_spanner(
    label = md("**In-Stride**"),
    columns = 2:4
  )

gtsave(all_metrics_table, "all_metrics_Table.png", )


# variable importance plots -----------------------------------------------

in_stride_imp_plot <- (xgb_o_imp_plot + theme(plot.caption.position = "plot",
                                              plot.caption = element_markdown(hjust = 0))) + (xgb_a_imp_plot + labs(caption = "")) + (xgb_s_imp_plot  + labs(caption = "")) 
ggsave("In-Stride Importance.png", in_stride_imp_plot, width = 15, height = 6)



outcomes_imp_plot <- (xgb_yac_imp_plot + theme(plot.caption.position = "plot",
                                              plot.caption = element_markdown(hjust = 1))) + 
  (xgb_yac_pa_imp_plot + theme(plot.caption.position = "plot",
                             plot.caption = element_markdown(hjust = 1))) +
  (xgb_comp_imp_plot + theme(plot.caption.position = "plot",
                            plot.caption = element_markdown(hjust = 1))) 
  
ggsave("Pass Outcomes Importance.png", outcomes_imp_plot, width = 15, height = 6)




