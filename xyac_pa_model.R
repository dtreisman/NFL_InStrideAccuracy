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
defenders_downfield_pa <- left_join(non_target_tracking, target_tracking, 
                                    by = c("gameId", "playId")) %>%
  mutate(downfield_from_receiver = ifelse(x_pass_arrived > x_target_pass_arrived, 1, 0)) %>%
  group_by(gameId, playId) %>%
  summarise(defenders_downfield = sum(downfield_from_receiver)) 

# situational variables
play_info <- plays %>%
  select(gameId, playId, quarter, down, yardsToGo, numberOfPassRushers, typeDropback, absoluteYardlineNumber)

# main DF for the model
df_Mod_yac_pa <- pass_forward_arrived %>%
  ungroup() %>%
  select(gameId, playId, route, x, y, s, a, o, dir, event) %>%
  pivot_wider(id_cols = gameId:route, names_from = event, values_from = x:dir) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, unlist)) %>%
  mutate(across(x_pass_forward:dir_pass_arrived, as.numeric)) %>%
  left_join(nearest_defenders, by = c("gameId", "playId")) %>%
  padr::fill_by_value(route, value = "Other") %>%
  select(!contains("forward")) %>%
  select(!contains("pf_nearest")) %>%
  select(!contains("pf_second_nearest")) %>%
  left_join(play_info) %>%
  na.omit() 
#padr::fill_by_function(dir_pass_forward, mean) %>%
#padr::fill_by_function(dir_pass_arrived, mean) %>%
#padr::fill_by_function(o_diff, mean)

plot(density(df_Mod_yac_pa$o_diff))
colSums(is.na(df_Mod_yac_pa))

# add yac
df_Mod_yac_pa <- left_join(df_Mod_yac_pa, pbp %>%
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
                          left_join(QBs) %>%select(gameId, playId, yac))


# filter out incomplete passes
df_Mod_yac_pa <- df_Mod_yac_pa %>%
  inner_join(completed_passes) %>%
  filter(complete == 1) %>%
  na.omit() %>%
  select(-complete) %>%
  left_join(defenders_downfield_pa)


# train/test split
set.seed(123)
idx_yac_pa <- unlist(createDataPartition(df_Mod_yac_pa$yac, p = .7))
train_mod_yac_pa <- df_Mod_yac_pa[idx_yac_pa, ]
train_mod_yac_pa <- train_mod_yac_pa %>%
  padr::fill_by_function(numberOfPassRushers, median) %>%
  padr::fill_by_function(absoluteYardlineNumber, median)  %>%
  na.omit()
test_mod_yac_pa <- df_Mod_yac_pa[-idx_yac_pa, ]
test_mod_yac_pa <- test_mod_yac_pa %>%
  padr::fill_by_function(numberOfPassRushers, median) %>%
  padr::fill_by_function(absoluteYardlineNumber, median)  %>%
  na.omit()



# random forest (not used) ------------------------------------------------



#tuneRF(x = train_mod_yac_pa[, c(-1, -2, -28)], y = train_mod_yac_pa$yac, trace = T, ntreeTry = 125)

fit_rf_yac_pa <- randomForest(x = train_mod_yac_pa[, c(-1, -2, -28)], y = train_mod_yac_pa$yac,
                           do.trace = T, ntree = 250, mtry = 8)
plot(fit_rf_yac_pa)
varImpPlot(fit_rf_yac_pa)


# naive prediction (mean stratified by route)
naive_pred1 <- test_mod_yac_pa %>%
  left_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(gameId, playId, route) %>%
  left_join(test_mod %>%
              group_by(route) %>%
              summarise(yac = mean(yac, na.rm = T)),
            by = "route")

# naive prediction (mean)
naive_pred2 <- test_mod_yac_pa %>%
  left_join(completed_passes) %>%
  filter(complete == 1) %>%
  bind_cols(rep(mean(test_mod_yac_pa$yac), nrow(test_mod_yac_pa)))

pred_rf_yac_pa <- predict(fit_rf_yac_pa, test_mod_yac_pa)


RMSE(pred = pred_rf_yac_pa, obs = test_mod_yac_pa$yac)
RMSE(pred = naive_pred1$yac, obs = test_mod_yac_pa$yac) #stratified by route
RMSE(pred = naive_pred2$...31, obs = test_mod_yac_pa$yac)


# xgboost -----------------------------------------------------------------

grid_tuning_yac_pa <- expand.grid(
  nrounds = seq(200, 400, by = 25),
  max_depth = 6,
  eta = 0.02,
  gamma = c(0, 1, 5, 10),
  colsample_bytree = .5,
  min_child_weight = 1,
  subsample = .6
)

train_control <- caret::trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE
)

train_mod_xgb_yac_pa <- train_mod_yac_pa %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  inner_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(-gameId, -playId, -yac, -complete) %>%
  as.matrix()
test_mod_xgb_yac_pa <- test_mod_yac_pa %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  inner_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(-gameId, -playId, -yac, -complete) %>%
  as.matrix()

xgb_caret_yac_pa <- caret::train(
  x = train_mod_xgb_yac_pa,
  y = train_mod_yac_pa$yac,
  trControl = train_control,
  tuneGrid = grid_tuning_yac_pa,
  method = "xgbTree",
  verbose = TRUE,
  maximize = F
)

str(train_mod_xg_yac_pab[, 2])
colSums(is.na(train_mod_yac_pa))

# taken from somewhere on Google
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(xgb_caret_yac_pa)

xgb_caret_yac_pa$results %>%
  View()
  filter(RMSE == min(xgb_caret_yac_pa$results$RMSE))

  
# final set of params
grid_tuned_yac_pa <- expand.grid(
  nrounds = 275,
  max_depth = 6,
  eta = 0.02,
  gamma = 1,
  colsample_bytree = .5,
  min_child_weight = 1,
  subsample = 0.6
)

# last fit
xgb_caret_final_yac_pa <- caret::train(
  x = train_mod_xgb_yac_pa,
  y = train_mod_yac_pa$yac,
  trControl = train_control,
  tuneGrid = grid_tuned_yac_pa,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


xgb_caret_final_yac_pa$results

pred_xgb_yac_pa <- predict(xgb_caret_final_yac_pa, test_mod_xgb_yac_pa)

RMSE(pred = pred_xgb_yac_pa, obs = test_mod_yac_pa$yac)
RMSE(pred = naive_pred1$yac, obs = test_mod_yac_pa$yac) #stratified by route
RMSE(pred = naive_pred2$...31, obs = test_mod_yac_pa$yac)


# full fit
df_Mod_xgb_yac_pa <- df_Mod_yac_pa %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  inner_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(-gameId, -playId, -yac, -complete) %>%
  as.matrix()

xgb_caret_full_yac_pa <- caret::train(
  x = df_Mod_xgb_yac_pa,
  y = df_Mod_yac_pa$yac,
  trControl = train_control,
  tuneGrid = grid_tuned_yac_pa,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)


df_Mod_yac_pa$pred_yac_pa <- predict(xgb_caret_full_yac_pa, df_Mod_xgb_yac_pa) 

# df_Mod_yac_pa <- df_Mod_yac_pa %>%
#   left_join(df_Mod_ %>%
#   select(gameId, playId, pred_yac_pa))

df_Mod_yac_pa <- left_join(df_Mod_yac_pa, df_Mod_yac %>%
                             select(gameId, playId, pred_yac)) 
df_Mod_yac_pa$yac_diff <- df_Mod_yac_pa$pred_yac - df_Mod_yac_pa$pred_yac_pa





# defensive adjustments using random intercepts
defenses <- left_join(
  plays %>%
    select(gameId, playId, possessionTeam),
  games %>%
    select(gameId, homeTeamAbbr, visitorTeamAbbr)
) %>%
  mutate(defTeam = ifelse(possessionTeam == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr)) %>%
  select(gameId, playId, defTeam)

df_Mod_yac_pa <- df_Mod_yac_pa %>%
  left_join(defenses)

# random effects model 
def_effects <- lmer(yac_diff ~ (1 | defTeam), data = df_Mod_yac_pa) %>%
  ranef()
def_effects <- def_effects$defTeam
def_effects$defTeam <- row.names(def_effects)

df_Mod_yac_pa <- df_Mod_yac_pa %>%
  left_join(def_effects) %>%
  rename(def_effects = `(Intercept)`) %>%
  mutate(adj_yac_diff = yac_diff - def_effects)


by_QB %>%
  ggplot(., aes(reorder(displayName, adj_yac_diff), adj_yac_diff)) +
  geom_point() +
  coord_flip()

plot(by_QB$o_diff_oe, by_QB$yac_diff)
plot(by_QB$yac_diff, by_QB$a_diff_oe)
plot(by_QB$yac_diff, by_QB$s_diff_oe)
plot(by_QB$yac_diff, by_QB$CPOE)



# variable importance
ggplot(varImp(xgb_caret_full_yac_pa)) +
  theme_minimal()

xgb_yac_pa_imp <- varImp(xgb_caret_final_yac_pa)$importance
cat(row.names(xgb_yac_pa_imp), sep = ", ")

# did this manually. Don't recommend...
xgb_yac_pa_imp$vars <- c( "Euclidean Distance Nearest Def.", 
                          "X Distance - Nearest Def.",
                          "Euclidean Distance - 2nd Nearest Def.", 
                          "Target Route",
                          "Y Distance - Nearest Def.", 
                          "X Distance - 2nd Nearest Def.",
                          "Speed Difference - Nearest Def.",
                          "Speed Difference - 2nd Nearest Def.",
                          "Receiver Orientation",
                          "Receiver Acceleration",
                          "Y Distance - 2nd Nearest Def.",
                          "Acceleration Difference - 2nd Nearest Def.",
                          "Receiver Y Location", 
                          "Number of Pass Rushers",
                          "Receiver X Location",
                          "Acceleration Difference - Nearest Def.",
                          "Direction Difference - 2nd Nearest Def.",
                          "Play Start Yardline",
                          "Receiver Speed",
                          "Direction Difference - Nearest Def.",
                          "Number of Defenders Downfield",
                          "Receiver Direction",
                          "Yards to Go for 1st Down",
                          "Quarter",
                          "Type of Dropback",
                          "Down")



ggplot(varImp(xgb_caret_final_yac_pa)) +
  theme_minimal()

xgb_yac_pa_imp_plot <- ggplot(xgb_yac_pa_imp, aes(x = Overall, y = reorder(vars, Overall))) +
  geom_col(fill = "darkblue", width = .1, alpha = .5) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(#title = "eXtreme Gradient Boosted Model Variable Importance",
       subtitle = "**Yards After Catch** (pass arrival) Model",
       x = "Importance", y = "",
       caption = "All tracking variables are calculated at *pass arrival*") +
  theme(plot.caption = element_markdown(),
        plot.subtitle = element_markdown())

ggsave("YAC Difference Variable Importance.png", xgb_yac_pa_imp_plot, height = 5, width = 10)
