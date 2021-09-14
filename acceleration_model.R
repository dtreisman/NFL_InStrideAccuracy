########################################
#                                      
#  This script is the xAcceleration
#  Difference model
#  
#
#  current as of 9/13/2021
#  
########################################

# modeling ----------------------------------------------------------------

# main DF for the model
df_Mod_a <- pass_forward_arrived %>%
  ungroup() %>%
  select(gameId, playId, route, x, y, s, a, o, dir, a_diff,  event) %>%
  pivot_wider(id_cols = gameId:route, names_from = event, values_from = x:dir) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, unlist)) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, as.numeric)) %>%
  left_join(pass_forward_arrived %>%
              select(gameId, playId, a_diff) %>%
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

plot(density(df_Mod_a$a_diff))
colSums(is.na(df_Mod_a))


# train/test split
set.seed(123)
idx_a <- unlist(createDataPartition(df_Mod_a$a_diff, p = .7))
train_mod_a <- df_Mod_a[idx_a, ]
test_mod_a <- df_Mod_a[-idx_a, ]



# random forest (not used) ------------------------------------------------


#tuneRF(x = train_mod_a[, c(-1, -2, -10)], y = train_mod_a$a_diff, trace = T, ntreeTry = 125)

fit_rf_a <- randomForest(x = train_mod_a[, c(-1, -2, -10)], y = train_mod_a$a_diff,
                         do.trace = T, ntree = 250, mtry = 6)
plot(fit_rf_a)
varImpPlot(fit_rf_a)

# naive prediction (mean stratified by route)
naive_pred1 <- test_mod_a %>%
  select(gameId, playId, route) %>%
  left_join(test_mod_a %>%
              group_by(route) %>%
              summarise(a_diff = mean(a_diff, na.rm = T)),
            by = "route")

# naive prediction (mean)
naive_pred2 <- test_mod_a %>%
  bind_cols(rep(mean(test_mod_a$a_diff), nrow(test_mod_a)))

pred_rf_a <- predict(fit_rf_a, test_mod_a)


RMSE(pred = pred_rf_a, obs = test_mod_a$a_diff)
RMSE(pred = naive_pred1$a_diff, obs = test_mod_a$a_diff) #stratified by route
RMSE(pred = naive_pred2$...25, obs = test_mod_a$a_diff)


# xgboost -----------------------------------------------------------------


grid_tuning_a <- expand.grid(
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

train_mod_xgb_a <- train_mod_a %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  select(-gameId, -playId, -a_diff) %>%
  as.matrix()
test_mod_xgb_a <- test_mod_a %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  select(-gameId, -playId, -a_diff) %>%
  as.matrix()


xgb_caret_a <- caret::train(
  x = train_mod_xgb_a,
  y = train_mod_a$a_diff,
  trControl = train_control,
  tuneGrid = grid_tuning_a,
  method = "xgbTree",
  verbose = TRUE,
  maximize = F
)

str(train_mod_xgb_a[, 2])
colSums(is.na(train_mod_a))

# taken from somewhere on Google
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(xgb_caret_a)

xgb_caret_a$results %>%
  filter(RMSE == min(xgb_caret_a$results$RMSE))

# final set of params
grid_tuned_a <- expand.grid(
  nrounds = 700,
  max_depth = 7,
  eta = 0.01,
  gamma = 5,
  colsample_bytree = .5,
  min_child_weight = 1,
  subsample = 0.6
)

# last fit
xgb_caret_final_a <- caret::train(
  x = train_mod_xgb_a,
  y = train_mod_a$a_diff,
  trControl = train_control,
  tuneGrid = grid_tuned_a,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


pred_xgb_a <- predict(xgb_caret_final_a, test_mod_xgb_a)

RMSE(pred = pred_xgb_a, obs = test_mod_a$a_diff)
RMSE(pred = naive_pred1$a_diff, obs = test_mod_a$a_diff) #stratified by route
RMSE(pred = naive_pred2$...25, obs = test_mod_a$a_diff)

# full fit
df_Mod_xgb_a <- df_Mod_a %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  select(-gameId, -playId, -a_diff) %>%
  as.matrix()

xgb_caret_full_a <- caret::train(
  x = df_Mod_xgb_a,
  y = df_Mod_a$a_diff,
  trControl = train_control,
  tuneGrid = grid_tuned_a,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


df_Mod_a$pred_a_diff <- predict(xgb_caret_full_a, df_Mod_xgb_a) 

# variable importance
xgb_a_imp <- varImp(xgb_caret_final_a)$importance
cat(row.names(xgb_a_imp), sep = ", ")

# I did this renaming manually. Would not recommend...
xgb_a_imp$vars <- c("Acceleration Difference - 2nd Nearest Def.", 
                    "Acceleration Difference - Nearest Def.",
                    "Target Route",
                    "Receiver Downfield Dist. from QB",
                    "Speed Difference - 2nd Nearest Def.", 
                    "Euclidean Distance Nearest Def.",
                    "Receiver Angle to QB",
                    "Speed Difference - Nearest Def.",
                    "X Distance - Nearest Def.", 
                    "Direction Difference - Nearest Def.",
                    "X Distance - 2nd Nearest Def.", 
                    "Y Distance - Nearest Def.",
                    "Receiver Y Location", 
                    "Euclidean Distance - 2nd Nearest Def.", 
                    "Receiver Orientation", 
                    "Y Distance - 2nd Nearest Def.",
                    "Receiver Direction",
                    "Receiver X Location", 
                    "Receiver Acceleration", 
                    "Direction Difference - 2nd Nearest Def.", 
                    "Receiver Speed")


ggplot(varImp(xgb_caret_final_a)) +
  theme_minimal()

xgb_a_imp_plot <- ggplot(xgb_a_imp, aes(x = Overall, y = reorder(vars, Overall))) +
  geom_col(fill = "darkblue", width = .1, alpha = .5) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(title = "eXtreme Gradient Boosted Model Variable Importance",
       subtitle = "**Acceleration** Difference Model",
       x = "Importance", y = "",
       caption = "All tracking variables are calculated at *pass forward*") +
  theme(plot.caption = element_markdown(),
        plot.title.position = "plot",
        plot.subtitle = element_markdown())

ggsave("Acceleration Variable Importance.png", xgb_a_imp_plot, height = 4, width = 10)
