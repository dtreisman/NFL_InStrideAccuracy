########################################
#                                      
#  This script is the xSpeed
#  Difference model
#  
#
#  current as of 9/13/2021
#  
########################################

# modeling ----------------------------------------------------------------

# main DF for the model
df_Mod_s <- pass_forward_arrived %>%
  ungroup() %>%
  select(gameId, playId, route, x, y, s, a, o, dir, s_diff,  event) %>%
  pivot_wider(id_cols = gameId:route, names_from = event, values_from = x:dir) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, unlist)) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, as.numeric)) %>%
  left_join(pass_forward_arrived %>%
              select(gameId, playId, s_diff) %>%
              na.omit(), by = c("gameId", "playId")) %>%
  left_join(nearest_defenders, by = c("gameId", "playId")) %>%
  padr::fill_by_value(route, value = "Other") %>%
  na.omit() %>%
  inner_join(QB_loc) %>%
  select(!contains("arrived")) %>%
  select(!contains("pa_nearest")) %>%
  select(!contains("pa_second_nearest")) %>%
  na.omit()
#padr::fill_by_function(dir_pass_forward, mean) %>%
#padr::fill_by_function(dir_pass_arrived, mean) %>%
#padr::fill_by_function(a_diff, mean)

plot(density(df_Mod_s$s_diff))
colSums(is.na(df_Mod_s))



# train/test split
set.seed(123)
idx_s <- unlist(createDataPartition(df_Mod_s$s_diff, p = .7))
train_mod_s <- df_Mod_s[idx_s, ]
test_mod_s <- df_Mod_s[-idx_s, ]


# random forest (not used) ------------------------------------------------
# 

tuneRF(x = train_mod_s[, c(-1, -2, -10)], y = train_mod_s$s_diff, trace = T, ntreeTry = 125)

fit_rf_s <- randomForest(x = train_mod_s[, c(-1, -2, -10)], y = train_mod_s$s_diff,
                         do.trace = T, ntree = 250, mtry = 6)
plot(fit_rf_s)
varImpPlot(fit_rf_s)


# naive prediction (mean stratified by route)
naive_pred1 <- test_mod_s %>%
  select(gameId, playId, route) %>%
  left_join(test_mod_s %>%
              group_by(route) %>%
              summarise(s_diff = mean(s_diff, na.rm = T)),
            by = "route")

# naive prediction (mean)
naive_pred2 <- test_mod_s %>%
  bind_cols(rep(mean(test_mod_s$s_diff), nrow(test_mod_s)))

pred_rf_s <- predict(fit_rf_s, test_mod_s)


RMSE(pred = pred_rf_s, obs = test_mod_s$s_diff)
RMSE(pred = naive_pred1$s_diff, obs = test_mod_s$s_diff) #stratified by route
RMSE(pred = naive_pred2$...25, obs = test_mod_s$s_diff)


# xgboost -----------------------------------------------------------------


grid_tuning_s <- expand.grid(
  nrounds = 700,
  max_depth = 7,
  eta = 0.01,
  gamma = 5,
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

train_mod_xgb_s <- train_mod_s %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  select(-gameId, -playId, -s_diff) %>%
  as.matrix()
test_mod_xgb_s <- test_mod_s %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  select(-gameId, -playId, -s_diff) %>%
  as.matrix()


xgb_caret_s <- caret::train(
  x = train_mod_xgb_a,
  y = train_mod_a$a_diff,
  trControl = train_control,
  tuneGrid = grid_tuning_a,
  method = "xgbTree",
  verbose = TRUE,
  maximize = F
)


# taken from somewhere on Google
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(xgb_caret_s)

xgb_caret_s$results %>%
  filter(RMSE == min(xgb_caret_s$results$RMSE))

# final set of params
grid_tuned_s <- expand.grid(
  nrounds = 700,
  max_depth = 7,
  eta = 0.01,
  gamma = 5,
  colsample_bytree = .5,
  min_child_weight = 1,
  subsample = 0.6
)

# last fit
xgb_caret_final_s <- caret::train(
  x = train_mod_xgb_s,
  y = train_mod_s$s_diff,
  trControl = train_control,
  tuneGrid = grid_tuned_s,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


pred_xgb_s <- predict(xgb_caret_final_s, test_mod_xgb_s)

RMSE(pred = pred_xgb_s, obs = test_mod_s$s_diff)
RMSE(pred = naive_pred1$s_diff, obs = test_mod_s$s_diff) #stratified by route
RMSE(pred = naive_pred2$...25, obs = test_mod_s$s_diff)


# full fit
df_Mod_xgb_s <- df_Mod_s %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  select(-gameId, -playId, -s_diff) %>%
  as.matrix()

xgb_caret_full_s <- caret::train(
  x = df_Mod_xgb_s,
  y = df_Mod_s$s_diff,
  trControl = train_control,
  tuneGrid = grid_tuned_s,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


df_Mod_s$pred_s_diff <- predict(xgb_caret_full_s, df_Mod_xgb_s) 

# used as a look-up to rename variables to plain English
vars <- bind_cols(rownames(xgb_a_imp), xgb_a_imp$vars) %>%
  rename(var = ...1, v_name = ...2)

# variable importance
xgb_s_imp <- varImp(xgb_caret_final_s)$importance

xgb_s_imp$vars <- rownames(xgb_s_imp)
xgb_s_imp <- xgb_s_imp %>%
  left_join(vars, by = c("vars" = "var")) %>%
  select(Overall, v_name) %>%
  rename(vars = v_name)


ggplot(varImp(xgb_caret_final_s)) +
  theme_minimal()

xgb_s_imp_plot <- ggplot(xgb_s_imp, aes(x = Overall, y = reorder(vars, Overall))) +
  geom_col(fill = "darkblue", width = .1, alpha = .5) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(title = "eXtreme Gradient Boosted Model Variable Importance",
       subtitle = "**Speed** Difference Model",
       x = "Importance", y = "",
       caption = "All tracking variables are calculated at *pass forward*") +
  theme(plot.caption = element_markdown(),
        plot.title.position = "plot",
        plot.subtitle = element_markdown())

ggsave("Speed Variable Importance.png", xgb_s_imp_plot, height = 4, width = 10)
