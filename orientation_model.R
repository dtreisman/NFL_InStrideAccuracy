########################################
#                                      
#  This script is the xOrientation
#  Difference model
#  
#
#  current as of 9/13/2021
########################################




# modeling ----------------------------------------------------------------

# main DF for the model
df_Mod_o <- pass_forward_arrived %>%
  ungroup() %>%
  select(gameId, playId, route, x, y, s, a, o, dir, o_diff,  event) %>%
  pivot_wider(id_cols = gameId:route, names_from = event, values_from = x:dir) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, unlist)) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, as.numeric)) %>%
  left_join(pass_forward_arrived %>%
              select(gameId, playId, o_diff) %>%
              na.omit(), by = c("gameId", "playId")) %>%
  left_join(nearest_defenders, by = c("gameId", "playId")) %>%
  inner_join(QB_loc) %>%
  padr::fill_by_value(route, value = "Other") %>%
  na.omit() %>%
  select(!contains("arrived")) %>%
  select(!contains("pa_nearest")) %>%
  select(!contains("pa_second_nearest"))
#padr::fill_by_function(dir_pass_forward, mean) %>%
#padr::fill_by_function(dir_pass_arrived, mean) %>%
#padr::fill_by_function(o_diff, mean)

plot(density(df_Mod_o$o_diff))
colSums(is.na(df_Mod_o))



# train/test split
set.seed(123)
idx_o <- unlist(createDataPartition(df_Mod_o$o_diff, p = .7))
train_mod_o <- df_Mod_o[idx_o, ]
test_mod_o <- df_Mod_o[-idx_o, ]


#  random forest (not used) -----------------------------------------------


#tuneRF(x = train_mod_o[, c(-1, -2, -10)], y = train_mod_o$o_diff, trace = T, ntreeTry = 125)

fit_rf_o <- randomForest(x = train_mod_o[, c(-1, -2, -10)], y = train_mod_o$o_diff,
                       do.trace = T, ntree = 300, mtry = 4)

plot(fit_rf_o)
varImpPlot(fit_rf_o)

fit_rf_o$importance %>%
  View()

# naive prediction (mean O Diff stratified by route)
naive_pred1 <- test_mod_o %>%
  select(gameId, playId, route) %>%
  left_join(test_mod_o %>%
              group_by(route) %>%
              summarise(o_diff = mean(o_diff, na.rm = T)),
            by = "route")

# naive prediction (mean O Diff)
naive_pred2 <- test_mod_o %>%
  bind_cols(rep(mean(test_mod_o$o_diff), nrow(test_mod_o)))


pred_rf_o <- predict(fit_rf_o, test_mod_o)


RMSE(pred = pred_rf_o, obs = test_mod_o$o_diff)
RMSE(pred = naive_pred1$o_diff, obs = test_mod_o$o_diff) #stratified by route
RMSE(pred = naive_pred2$...25, obs = test_mod_o$o_diff)


# xgboost -----------------------------------------------------------------


grid_tuning_o <- expand.grid(
  nrounds = seq(700, 900, by = 50),
  max_depth = 4,
  eta = c(0.0075, 0.01, 0.02),
  gamma = c(.5, 1, 1.5),
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = c(.5, .75, .9)
)

train_control <- caret::trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE
)

train_mod_xgb_o <- train_mod_o %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  select(-gameId, -playId, -o_diff) %>%
  as.matrix()
test_mod_xgb_o <- test_mod_o %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  select(-gameId, -playId, -o_diff) %>%
  as.matrix()


xgb_caret_o <- caret::train(
  x = train_mod_xgb_o,
  y = train_mod_o$o_diff,
  trControl = train_control,
  tuneGrid = grid_tuning_o,
  method = "xgbTree",
  verbose = TRUE,
  maximize = F
)

str(train_mod_xgb_o[, 2])
colSums(is.na(train_mod_o))

# taken from somewhere on Google
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

xgb_caret_o$results
tuneplot(xgb_caret_o)


xgb_caret_o$results %>%
  filter(RMSE == min(xgb_caret_o$results$RMSE))

# final set of params
grid_tuned_o <- expand.grid(
  nrounds = 900,
  max_depth = 4,
  eta = 0.0075,
  gamma = 1,
  colsample_bytree = .9,
  min_child_weight = 1,
  subsample = 0.6
)

# last fit
xgb_caret_final_o <- caret::train(
  x = train_mod_xgb_o,
  y = train_mod_o$o_diff,
  trControl = train_control,
  tuneGrid = grid_tuned_o,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


xgb_caret_final_o$results

# variable importance
xgb_o_imp <- varImp(xgb_caret_final_o)$importance
cat(row.names(xgb_o_imp), sep = ", ")

# I did this renaming manually. Would not recommend...
xgb_o_imp$vars <- c("Receiver Downfield Dist. from QB", 
                    "Target Route", 
                    "Receiver Angle to QB",
                    "Acceleration Difference - 2nd Nearest Def.", 
                    "Acceleration Difference - Nearest Def.", 
                    "Speed Difference - Nearest Def.", 
                    "Speed Difference - 2nd Nearest Def.", 
                    "Receiver Orientation", "Euclidean Distance Nearest Def.", 
                    "Direction Diff. Nearest Def.", 
                    "Euclidean Distance - 2nd Nearest Def.", "X Distance - Nearest Def.", 
                    "Y Distance - 2nd Nearest Def.", "X Distance - 2nd Nearest Def.", 
                    "Receiver Speed", "Receiver Y Location", "Receiver X Location", 
                    "Receiver Acceleration", "Direction Difference - 2nd Nearest Def.", "Receiver Direction", 
                    "Y Distance - Nearest Def.")


xgb_o_imp_plot <- ggplot(xgb_o_imp, aes(x = Overall, y = reorder(vars, Overall))) +
  geom_col(fill = "darkblue", width = .1, alpha = .5) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(title = "eXtreme Gradient Boosted Model Variable Importance",
       subtitle = "**Orientation** Difference Model",
       x = "Importance", y = "",
       caption = "All tracking variables are calculated at *pass forward*") +
  theme(plot.caption = element_markdown(),
        plot.title.position = "plot",
        plot.subtitle = element_markdown())

ggsave("Orientation Variable Importance.png", xgb_o_imp_plot, height = 4, width = 10)



pred_xgb_o <- predict(xgb_caret_final_o, test_mod_xgb_o)

RMSE(pred = pred_xgb_o, obs = test_mod_o$o_diff)
RMSE(pred = naive_pred1$o_diff, obs = test_mod_o$o_diff) #stratified by route
RMSE(pred = naive_pred2$...25, obs = test_mod_o$o_diff)


# full fit
df_Mod_xgb_o <- df_Mod_o %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  select(-gameId, -playId, -o_diff) %>%
  as.matrix()

xgb_caret_full_o <- caret::train(
  x = df_Mod_xgb_o,
  y = df_Mod_o$o_diff,
  trControl = train_control,
  tuneGrid = grid_tuned_o,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


df_Mod_o$pred_o_diff <- predict(xgb_caret_full_o, df_Mod_xgb_o) 

