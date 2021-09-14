########################################
#                                      
#  This script is the xYAC
#  (pass-arrival) model
#  
#
#  current as of 9/13/2021
#  
########################################


# modeling ----------------------------------------------------------------

# situational variables and pass results
play_info_comp <- plays %>%
  select(gameId, playId, quarter, down, yardsToGo, numberOfPassRushers, 
         typeDropback, absoluteYardlineNumber, defendersInTheBox, passResult) %>%
  mutate(passResult = case_when(
    passResult == "C" ~ 1,
    passResult %in% c("I", "IN") ~ 0))


# main modeling DF
df_Mod_comp <- pass_forward_arrived %>%
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
  left_join(play_info_comp) %>%
  filter(!is.na(passResult))  
#padr::fill_by_function(dir_pass_forward, mean) %>%
#padr::fill_by_function(dir_pass_arrived, mean) %>%
#padr::fill_by_function(o_diff, mean)

plot(density(df_Mod_comp$o_diff))
colSums(is.na(df_Mod_comp))


# train/test split
idx_comp <- unlist(createDataPartition(df_Mod_comp$passResult, p = .7))
train_mod_comp <- df_Mod_comp[idx_comp, ]
train_mod_comp <- train_mod_comp %>%
  padr::fill_by_function(angle_to_qb, mean) %>%
  padr::fill_by_function(numberOfPassRushers, median) %>%
  padr::fill_by_function(absoluteYardlineNumber, median) %>%
  na.omit()
test_mod_comp <- df_Mod_comp[-idx_comp, ]
test_mod_comp <- test_mod_comp %>%
  padr::fill_by_function(angle_to_qb, mean) %>%
  padr::fill_by_function(numberOfPassRushers, median) %>%
  padr::fill_by_function(absoluteYardlineNumber, median) %>%
  na.omit()



# random forest (not used) ------------------------------------------------

table(train_mod_comp$passResult)

# tuneRF(x = train_mod_comp[, c(-1, -2, -31)], y = as.factor(train_mod_comp$passResult), trace = T, ntreeTry = 150)

fit_rf_comp <- randomForest(x = train_mod_comp[, c(-1, -2, -31)], 
                            y = as.factor(train_mod_comp$passResult),
                            do.trace = T, ntree = 250, mtry = 10, cutoff = c(.25, .75))
plot(fit_rf_comp)
varImpPlot(fit_rf_comp)


# naive prediction (mean)
naive_pred <- rep(sum(df_Mod_comp$passResult)/nrow(df_Mod_comp), nrow(test_mod_comp))

# naive prediction (mean stratified by route)
naive_pred1 <- test_mod_comp %>%
  select(gameId, playId, route) %>%
  left_join(test_mod_comp %>%
              group_by(route) %>%
              summarise(result = mean(passResult, na.rm = T)),
            by = "route")

pred_rf_comp <- predict(fit_rf_comp, test_mod_comp, type = "prob")[,2]


MLmetrics::LogLoss(pred_rf_comp, test_mod_comp$passResult)
MLmetrics::LogLoss(naive_pred, test_mod_comp$passResult)
MLmetrics::LogLoss(naive_pred1$result, test_mod_comp$passResult)


ModelMetrics::logLoss(test_mod_comp$passResult, pred_rf_comp)
ModelMetrics::logLoss(test_mod_comp$passResult, naive_pred)
ModelMetrics::logLoss(test_mod_comp$passResult, naive_pred1$result)


# xgboost -----------------------------------------------------------------

grid_tuning_comp <- expand.grid(
  nrounds = 1500,
  max_depth = 8,
  eta = 0.01,
  gamma = 5, 10,
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = .6
)

train_control <- caret::trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE
)

train_mod_xgb_comp <- train_mod_comp %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  select(-gameId, -playId, -passResult) %>%
  as.matrix()
test_mod_xgb_comp <- test_mod_comp %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  select(-gameId, -playId, -passResult) %>%
  as.matrix()

xgb_caret_comp <- caret::train(
  x = train_mod_xgb_comp,
  y = as.factor(train_mod_comp$passResult),
  trControl = train_control,
  tuneGrid = grid_tuning_comp,
  method = "xgbTree",
  verbose = TRUE,
  maximize = F
)

str(train_mod_xg_compb[, 2])
colSums(is.na(train_mod_comp))
xgb_caret_comp$results$Accuracy

# taken from somewhere on Google
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy))) +
    theme_bw()
}

tuneplot(xgb_caret_comp)

xgb_caret_comp$results %>%
  filter(RMSE == min(xgb_caret_comp$results$RMSE))

# final set of params
grid_tuned_comp <- expand.grid(
  nrounds = 1500,
  max_depth = 8,
  eta = 0.01,
  gamma = 5,
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = .6
)

# last fit
xgb_caret_final_comp <- caret::train(
  x = train_mod_xgb_comp,
  y = as.factor(train_mod_comp$passResult),
  trControl = train_control,
  tuneGrid = grid_tuned_comp,
  method = "xgbTree",
  verbose = TRUE,
  metric = "logloss",
  maximize = F
)


xgb_caret_final_comp$results

pred_xgb_comp <- predict(xgb_caret_final_comp, test_mod_xgb_comp, type = "prob")[, 2]

MLmetrics::LogLoss(pred_xgb_comp, test_mod_comp$passResult)
MLmetrics::LogLoss(naive_pred, test_mod_comp$passResult)

ModelMetrics::logLoss(test_mod_comp$passResult, pred_xgb_comp)
ModelMetrics::logLoss(test_mod_comp$passResult, naive_pred)



# full fit
df_Mod_xgb_comp <- df_Mod_comp %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  select(-gameId, -playId, -passResult) %>%
  as.matrix()



xgb_caret_full_comp <- caret::train(
  x = df_Mod_xgb_comp,
  y = as.factor(df_Mod_comp$passResult),
  trControl = train_control,
  tuneGrid = grid_tuned_comp,
  method = "xgbTree",
  verbose = TRUE,
  metric = "logloss",
  maximize = F
)

df_Mod_comp$pred_comp <- predict(xgb_caret_full_comp, df_Mod_xgb_comp, type = "prob")[,2]

# df_Mod_comp <- df_Mod_comp %>%
#   left_join(df_Mod_ %>%
#   select(gameId, playId, pred_comp))

df_Mod_comp$CPOE <- df_Mod_comp$passResult - df_Mod_comp$pred_comp


# variable importance
ggplot(varImp(xgb_caret_full_comp)) +
  theme_minimal()

xgb_comp_imp <- varImp(xgb_caret_final_comp)$importance

vars <- bind_cols(rownames(xgb_yac_imp), xgb_yac_imp$vars) %>%
  rename(var = ...1, v_name = ...2)

xgb_comp_imp$vars <- rownames(xgb_comp_imp)
xgb_comp_imp <- xgb_comp_imp %>%
  left_join(vars, by = c("vars" = "var")) %>%
  padr::fill_by_value(v_name, value = "Number of Defenders in the Box") %>%
  select(Overall, v_name) %>%
  rename(vars = v_name)



ggplot(varImp(xgb_caret_final_a)) +
  theme_minimal()

xgb_comp_imp_plot <- ggplot(xgb_comp_imp, aes(x = Overall, y = reorder(vars, Overall))) +
  geom_col(fill = "darkblue", width = .1, alpha = .5) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(#title = "eXtreme Gradient Boosted Model Variable Importance",
       subtitle = "**Completion Percentage** Model",
       x = "Importance", y = "",
       caption = "All tracking variables are calculated at *pass forward*") +
  theme(plot.caption = element_markdown(),
        plot.subtitle = element_markdown())

ggsave("Completion Percentage Variable Importance.png", xgb_comp_imp_plot, height = 5, width = 10)

