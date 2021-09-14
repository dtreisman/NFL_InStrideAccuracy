########################################
#                                      
#  This script is the xYAC
#  (pass-forward) model
#  
#
#  current as of 9/13/2021
#  
########################################



# modeling ----------------------------------------------------------------

# variable for number of defenders downfield from the receiver 
defenders_downfield <- left_join(non_target_tracking, target_tracking, 
                                 by = c("gameId", "playId")) %>%
  mutate(downfield_from_receiver = ifelse(x_pass_forward > x_target_pass_forward, 1, 0)) %>%
  group_by(gameId, playId) %>%
  summarise(defenders_downfield = sum(downfield_from_receiver)) 

# situational variables
play_info <- plays %>%
  select(gameId, playId, quarter, down, yardsToGo, numberOfPassRushers, typeDropback, absoluteYardlineNumber)


# main DF for the model
df_Mod_yac <- pass_forward_arrived %>%
  ungroup() %>%
  select(gameId, playId, route, x, y, s, a, o, dir, event) %>%
  pivot_wider(id_cols = gameId:route, names_from = event, values_from = x:dir) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, unlist)) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, as.numeric)) %>%
  left_join(nearest_defenders, by = c("gameId", "playId")) %>%
  padr::fill_by_value(route, value = "Other") %>%
  na.omit() %>%
  inner_join(QB_loc) %>%
  select(!contains("arrived")) %>%
  select(!contains("pa_nearest")) %>%
  select(!contains("pa_second_nearest")) %>%
  left_join(play_info) 
#padr::fill_by_function(dir_pass_forward, mean) %>%
#padr::fill_by_function(dir_pass_arrived, mean) %>%
#padr::fill_by_function(o_diff, mean)

plot(density(df_Mod_yac$o_diff))
colSums(is.na(df_Mod_yac))

# add YAC
df_Mod_yac <- left_join(df_Mod_yac, pbp %>%
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
                      left_join(QBs) %>% select(gameId, playId, yac))


# filter out incomplete passes
df_Mod_yac <- df_Mod_yac %>%
  inner_join(completed_passes) %>%
  filter(complete == 1) %>%
  na.omit() %>%
  select(-complete) %>%
  left_join(defenders_downfield)



# train/test split and missing data imputation
set.seed(123)
idx_yac <- unlist(createDataPartition(df_Mod_yac$yac, p = .7))
train_mod_yac <- df_Mod_yac[idx_yac, ]
train_mod_yac <- train_mod_yac %>%
  padr::fill_by_function(angle_to_qb, mean) %>%
  padr::fill_by_function(numberOfPassRushers, median) %>%
  padr::fill_by_function(absoluteYardlineNumber, median)  %>%
  na.omit()
test_mod_yac <- df_Mod_yac[-idx_yac, ]
test_mod_yac <- test_mod_yac %>%
  padr::fill_by_function(angle_to_qb, mean) %>%
  padr::fill_by_function(numberOfPassRushers, median) %>%
  padr::fill_by_function(absoluteYardlineNumber, median)  %>%
  na.omit()


# random forest (not used) ------------------------------------------------


#tuneRF(x = train_mod_yac[, c(-1, -2, -30)], y = train_mod_yac$yac, trace = T, ntreeTry = 125)

fit_rf_yac <- randomForest(x = train_mod_yac[, c(-1, -2, -30)], y = train_mod_yac$yac,
                       do.trace = T, ntree = 250, mtry = 9)
plot(fit_rf_yac)
varImpPlot(fit_rf_yac)


# naive prediction (mean stratified by route)
naive_pred1 <- test_mod_yac %>%
  left_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(gameId, playId, route) %>%
  left_join(test_mod %>%
              group_by(route) %>%
              summarise(yac = mean(yac, na.rm = T)),
            by = "route")

# naive prediction (mean)
naive_pred2 <- test_mod_yac %>%
  left_join(completed_passes) %>%
  filter(complete == 1) %>%
  bind_cols(rep(mean(test_mod_yac$yac), nrow(test_mod_yac)))

pred_rf_yac <- predict(fit_rf_yac, test_mod_yac)


RMSE(pred = pred_rf_yac, obs = test_mod_yac$yac)
RMSE(pred = naive_pred1$yac, obs = test_mod_yac$yac) #stratified by route
RMSE(pred = naive_pred2$...33, obs = test_mod_yac$yac)


# xgboost -----------------------------------------------------------------

grid_tuning_yac <- expand.grid(
  nrounds = 500,
  max_depth = 8,
  eta = 0.0075,
  gamma = c(2, 3, 4, 5),
  colsample_bytree = .95,
  min_child_weight = 1,
  subsample = .6
)

train_control <- caret::trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE
)

train_mod_xgb_yac <- train_mod_yac %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  inner_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(-gameId, -playId, -yac, -complete) %>%
  as.matrix()
test_mod_xgb_yac <- test_mod_yac %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  inner_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(-gameId, -playId, -yac, -complete) %>%
  as.matrix()

xgb_caret_yac <- caret::train(
  x = train_mod_xgb_yac,
  y = train_mod_yac$yac,
  trControl = train_control,
  tuneGrid = grid_tuning_yac,
  method = "xgbTree",
  verbose = TRUE,
  maximize = F
)

str(train_mod_xg_yacb[, 2])
colSums(is.na(train_mod_yac))

# taken from somewhere on Google
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(xgb_caret_yac)

xgb_caret_yac$results %>%
  filter(RMSE == min(xgb_caret_yac$results$RMSE))

# final set of params
grid_tuned_yac <- expand.grid(
  nrounds = 500,
  max_depth = 8,
  eta = 0.0075,
  gamma = 5,
  colsample_bytree = .95,
  min_child_weight = 1,
  subsample = 0.6
)

# last fit
xgb_caret_final_yac <- caret::train(
  x = train_mod_xgb_yac,
  y = train_mod_yac$yac,
  trControl = train_control,
  tuneGrid = grid_tuned_yac,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


xgb_caret_final_yac$results

pred_xgb_yac <- predict(xgb_caret_final_yac, test_mod_xgb_yac)

RMSE(pred = pred_xgb_yac, obs = test_mod_yac$yac)
RMSE(pred = naive_pred1$yac, obs = test_mod_yac$yac) #stratified by route
RMSE(pred = naive_pred2$...33, obs = test_mod_yac$yac)


# full fit
df_Mod_xgb_yac <- df_Mod_yac %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  inner_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(-gameId, -playId, -yac, -complete) %>%
  as.matrix()


xgb_caret_full_yac <- caret::train(
  x = df_Mod_xgb_yac,
  y = df_Mod_yac$yac,
  trControl = train_control,
  tuneGrid = grid_tuned_yac,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


df_Mod_yac$pred_yac <- predict(xgb_caret_full_yac, df_Mod_xgb_yac) 

# df_Mod_yac <- df_Mod_yac %>%
#   left_join(df_Mod_ %>%
#   select(gameId, playId, pred_yac))

df_Mod_yac$yac_oe <- df_Mod_yac$yac - df_Mod_yac$pred_yac


# variable importance
ggplot(varImp(xgb_caret_full_yac)) +
  theme_minimal()

xgb_yac_imp <- varImp(xgb_caret_final_yac)$importance

# did this manually. Don't recommend...
cat(row.names(xgb_a_imp), sep = ", ")
xgb_yac_imp$vars <- c("Receiver Downfield Dist. from QB",
                      "X Distance - Nearest Def.",
                      "Speed Difference - 2nd Nearest Def.",
                      "Receiver X Location",
                      "Speed Difference - Nearest Def.",
                      "Receiver Speed",
                      "Target Route",
                      "X Distance - 2nd Nearest Def.",
                      "Y Distance - Nearest Def.", 
                      "Euclidean Distance Nearest Def.", 
                      "Receiver Direction",
                      "Receiver Y Location", 
                      "Play Start Yardline",
                      "Receiver Angle to QB",
                      "Y Distance - 2nd Nearest Def.",
                      "Euclidean Distance - 2nd Nearest Def.", 
                      "Acceleration Difference - Nearest Def.",
                      "Receiver Acceleration",  
                      "Direction Difference - 2nd Nearest Def.", 
                      "Receiver Orientation",
                      "Acceleration Difference - 2nd Nearest Def.",
                      "Direction Difference - Nearest Def.",
                      "Number of Pass Rushers",
                      "Number of Defenders Downfield",
                      "Yards to Go for 1st Down",
                      "Quarter",
                      "Down",
                      "Type of Dropback")
  
        

ggplot(varImp(xgb_caret_final_a)) +
  theme_minimal()

xgb_yac_imp_plot <- ggplot(xgb_yac_imp, aes(x = Overall, y = reorder(vars, Overall))) +
  geom_col(fill = "darkblue", width = .1, alpha = .5) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(#title = "eXtreme Gradient Boosted Model Variable Importance",
       subtitle = "**Yards After Catch** (pass forward) Model",
       x = "Importance", y = "",
       caption = "All tracking variables are calculated at *pass forward*") +
  theme(plot.caption = element_markdown(),
        plot.subtitle = element_markdown())

ggsave("YAC Variable Importance.png", xgb_yac_imp_plot, height = 5, width = 10)
