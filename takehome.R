library(tidyverse)
library(isotree)
library(caret)

#read in data
reds <- read.csv('/Users/sam/Desktop/Intern Project/Reds/reds_takehome.csv')

#orient right and left handed pitchers for analysis
reds <- reds %>%
  mutate(HORIZONTAL_BREAK_ADJ = ifelse(THROW_SIDE_KEY == "R",  HORIZONTAL_BREAK, -HORIZONTAL_BREAK),
         RELEASE_SIDE_ADJ = ifelse(THROW_SIDE_KEY == "R", RELEASE_SIDE, -RELEASE_SIDE),
         HORIZONTAL_APPROACH_ANGLE_ADJ = ifelse(THROW_SIDE_KEY == "R", HORIZONTAL_APPROACH_ANGLE, -HORIZONTAL_APPROACH_ANGLE),
         full_pitch_name = case_when(
           PITCH_TYPE_TRACKED_KEY == 'FB' ~ 'fastball',
           PITCH_TYPE_TRACKED_KEY == 'SL' ~ 'slider',
           PITCH_TYPE_TRACKED_KEY == 'SF' ~ 'splitter',
           PITCH_TYPE_TRACKED_KEY == 'SI' ~ 'sinker',
           PITCH_TYPE_TRACKED_KEY == 'SW' ~ 'sweeper',
           PITCH_TYPE_TRACKED_KEY == 'CH' ~ 'changeup',
           PITCH_TYPE_TRACKED_KEY == 'CB' ~ 'curveball',
           PITCH_TYPE_TRACKED_KEY == 'CF' ~ 'cutter',
           PITCH_TYPE_TRACKED_KEY == 'KN' ~ 'knuckleball',
           TRUE ~ 'unknown'
         ))

#view unique pitch types
unique(reds$PITCH_TYPE_TRACKED_KEY)

#4 unknown pitches - can probably impute them
reds %>%
  filter(PITCH_TYPE_TRACKED_KEY == 'UN')

#829 is fastball (FB) - 16568 & 16569 & 16158 are the same pitcher probably splitter (SF) - 16158 is a slider (SL)
reds <- reds %>%
  mutate(PITCH_TYPE_TRACKED_KEY = case_when(
    PID == 829 ~ 'FB',
    PID == 16158 ~ 'SL',
    PID == 16568 | PID == 16569 ~ 'SF',
    TRUE ~ PITCH_TYPE_TRACKED_KEY
  ))

#get a count of each pitch type
pitch_cnt_sum <- reds %>%
  group_by(PITCHER_KEY) %>%
  summarise(pitch_cnt = n())

#identify outlier pitches that could be caused by high dew point
#slower fastballs, worse movement in breaking balls
pitch_summary_table <- reds %>%
  group_by(PITCH_TYPE_TRACKED_KEY) %>%
  summarise(cnt = n(),
            avg_velo = mean(RELEASE_SPEED),
            sd_velo = sd(RELEASE_SPEED),
            low_velo = avg_velo - sd_velo,
            avg_spin = mean(SPIN_RATE_ABSOLUTE),
            sd_spin = sd(SPIN_RATE_ABSOLUTE),
            low_spin = avg_spin - sd_spin,
            avg_vert = mean(INDUCED_VERTICAL_BREAK),
            sd_vert = sd(INDUCED_VERTICAL_BREAK),
            low_vert = avg_vert + sd_vert,
            avg_horz = mean(HORIZONTAL_BREAK_ADJ),
            sd_horz = sd(HORIZONTAL_BREAK_ADJ),
            low_horz = avg_horz + sd_horz)

#throw fast
fastballs <- c('CF','FB','SI')
#wants spin on the ball
breaking_balls <- c('SW','CB','SL')

#manually identify slower fastballs and low spin breaking balls
reds_po <- reds %>%
  left_join(pitch_summary_table, by = c('PITCH_TYPE_TRACKED_KEY')) %>%
  mutate(potential_outliers = ifelse(
    (PITCH_TYPE_TRACKED_KEY %in% fastballs & RELEASE_SPEED <= low_velo) |
      (PITCH_TYPE_TRACKED_KEY %in% breaking_balls & SPIN_RATE_ABSOLUTE <= low_spin) | 
      (PITCH_TYPE_TRACKED_KEY == 'CH' & HORIZONTAL_BREAK_ADJ >= low_horz), 1,0))

#manually identified outliers
nrow(reds_po[reds_po$potential_outliers==1,])

#manual outliers
ggplot(reds_po) +
  geom_point(aes(x = SPIN_RATE_ABSOLUTE, y= RELEASE_SPEED, color = as.factor(potential_outliers))) +
  facet_wrap(~PITCH_TYPE_TRACKED_KEY)

#manual outliers over plate location
ggplot(reds_po) +
  geom_point(aes(x = PLATE_X, y= PLATE_Z, color = as.factor(potential_outliers))) +
  facet_wrap(~PITCH_TYPE_TRACKED_KEY)

#isolation forest outlier detection method
u_pitch_types <- unique(reds_po$PITCH_TYPE_TRACKED_KEY)

preds <- list()

#loop through each pitch type and detect outliers using only plate location
for (pitch_type in u_pitch_types) {
  
  #divide up the data
  subset_pitch <- reds_po %>%
    filter(PITCH_TYPE_TRACKED_KEY == pitch_type)
  
  #build model
  iso_model <- isolation.forest(subset_pitch %>%
                                  select(PLATE_X, PLATE_Z), ntrees = 10, nthreads = 1)
  
  #get predictions
  scores <- predict(iso_model, subset_pitch %>%
                      select(PLATE_X, PLATE_Z), type = "avg_depth")
  
  #bind predictions back to subset data
  subset_iso_df <- cbind(subset_pitch, scores)
  
  #store subsetdf in a list
  preds[[pitch_type]] <- subset_iso_df
}

#bind all subsets back together
reds_iso <- do.call(rbind, preds)

#limit the outliers to only the bottom 5% of each pitch type
reds_iso_ <- reds_iso %>%
  group_by(PITCH_TYPE_TRACKED_KEY) %>%
  mutate(iso_outliers = ifelse(rank(scores) / n() <= 0.05, 1, 0)) %>%
  ungroup() 

#visual of outliers by plate location
ggplot(reds_iso_) +
  geom_point(aes(x = PLATE_X, y= PLATE_Z, color = as.factor(iso_outliers))) +
  labs(x= 'Plate X', y = 'Plate Z', title = 'Potential Outliers By Pitch Type') +
  scale_color_discrete(name = 'Outliers') +
  facet_wrap(~PITCH_TYPE_TRACKED_KEY)


#same thing with multivariate input instead of just plate location 
preds_mv <- list()

for (pitch_type in u_pitch_types) {
  
  #divide up the data
  subset_pitch <- reds_po %>%
    filter(PITCH_TYPE_TRACKED_KEY == pitch_type)
  
  #build model
  iso_model <- isolation.forest(subset_pitch %>%
                                  dplyr::select(PLATE_X,PLATE_Z, HORIZONTAL_APPROACH_ANGLE_ADJ, INDUCED_VERTICAL_BREAK, 
                                                           RELEASE_SPEED, SPIN_RATE_ABSOLUTE, RELEASE_SIDE_ADJ, HORIZONTAL_BREAK_ADJ, 
                                                           RELEASE_HEIGHT, RELEASE_EXTENSION), ntrees = 10, nthreads = 1)
  
  #get predictions
  scores <- predict(iso_model, subset_pitch %>%
                      dplyr::select(PLATE_X,PLATE_Z, HORIZONTAL_APPROACH_ANGLE_ADJ, INDUCED_VERTICAL_BREAK, 
                                    RELEASE_SPEED, SPIN_RATE_ABSOLUTE, RELEASE_SIDE_ADJ, HORIZONTAL_BREAK_ADJ, 
                                    RELEASE_HEIGHT, RELEASE_EXTENSION), type = "avg_depth")
  
  #bind predictions back to subset data
  subset_iso_df <- cbind(subset_pitch, scores)
  
  #store subsetdf in a list
  preds_mv[[pitch_type]] <- subset_iso_df
}

#bind all subsets back together
reds_iso_mv <- do.call(rbind, preds_mv)

#bottom 5% of pitches in terms of isolation forest will be our high dew point observations
reds_iso_mv_ <- reds_iso_mv %>%
  group_by(PITCH_TYPE_TRACKED_KEY) %>%
  mutate(iso_outliers = ifelse(rank(scores) / n() <= 0.05, 1, 0)) %>%
  ungroup() 

#pitches over plate location
ggplot(reds_iso_mv_) +
  geom_point(aes(x = PLATE_X, y= PLATE_Z, color = as.factor(iso_outliers))) +
  labs(x= 'Plate X', y = 'Plate Z', title = 'Potential Outliers By Pitch Type') +
  scale_color_discrete(name = 'Outliers') +
  facet_wrap(~PITCH_TYPE_TRACKED_KEY)

#will be using iso outliers as my target variable that indicates pitches with high dew point

#define formula 
dew_pt_formula <- formula(iso_outliers ~ PLATE_X + PLATE_Z + HORIZONTAL_APPROACH_ANGLE_ADJ + INDUCED_VERTICAL_BREAK +
RELEASE_SPEED + SPIN_RATE_ABSOLUTE + RELEASE_SIDE_ADJ + HORIZONTAL_BREAK_ADJ + 
RELEASE_HEIGHT + RELEASE_EXTENSION)

#make target as factor for classification modeling
reds_iso_mv_$iso_outliers <- as.factor(reds_iso_mv_$iso_outliers)

#name the different levels
levels(reds_iso_mv_$iso_outliers) <- c("Regular_Dew_Point", "High_Dew_Point")

#set seed for reproducibilty
set.seed(123)

#5 fold logistic regression 
ctrl <- trainControl(method = "cv", number = 5, savePredictions = "all", classProbs = TRUE)

#need to reclassify KN to model by pitch type since the sample size is too low
reds_iso_mv_ %>%
  group_by(PITCH_TYPE_TRACKED_KEY) %>%
  summarise(cnt = n())

reds_iso_mv_ <- reds_iso_mv_ %>%
  mutate(PITCH_TYPE_TRACKED_KEY = case_when(
    PITCH_TYPE_TRACKED_KEY == 'KN' ~ 'CB',
    TRUE ~ PITCH_TYPE_TRACKED_KEY
  ))

#redefine unique pitch types
u_pitch_types <- unique(reds_iso_mv_$PITCH_TYPE_TRACKED_KEY)

preds_glm <- c()

#linear model for each pitch type
for (pitch_type in u_pitch_types) {
  
  #divide up the data
  subset_pitch <- reds_iso_mv_ %>%
    filter(PITCH_TYPE_TRACKED_KEY == pitch_type)
  
  #glm model
  glm_model <- train(form = dew_pt_formula, data = subset_pitch, method = "glm", family = 'binomial', trControl = ctrl)
  
  #bind predictions back to subset data
  glm_df <- cbind(subset_pitch, as.data.frame(glm_model$pred) %>%
                    arrange(rowIndex))
  
  #store subsetdf in a list
  preds_glm[[pitch_type]] <- glm_df
}

#bind all subsets back together
reds_glm <- do.call(rbind, preds_glm)

#evaluate model performance using confusion matrix
confusionMatrix(reds_glm$pred, reds_glm$obs)


#do the same for random forest modeling
preds_rf <- c()

#random forest model for each pitch type
for (pitch_type in u_pitch_types) {
  
  #divide up the data
  subset_pitch <- reds_iso_mv_ %>%
    filter(PITCH_TYPE_TRACKED_KEY == pitch_type)
  
  #random forest model
  rf_model <- train(form = dew_pt_formula, data = subset_pitch, method = "rf", trControl = ctrl)
  
  #multiple different number of variables to try - grab the highest accuracy results
  best_iter <- rf_model$results$mtry[which.max(rf_model$results$Accuracy)]
  
  #bind predictions back to subset data
  rf_df <- cbind(subset_pitch, as.data.frame(rf_model$pred) %>%
                   filter(mtry == best_iter) %>%
                   arrange(rowIndex))
  
  #store subsetdf in a list
  preds_rf[[pitch_type]] <- rf_df
}

#bind all subsets back together
reds_rf <- do.call(rbind, preds_rf)

#evaluate model performance using confusion matrix
confusionMatrix(reds_rf$pred, reds_rf$obs)

#grab eval metrics from confusion matrix
confusionMatrix(reds_rf$pred, reds_rf$obs)$overall['Accuracy']
confusionMatrix(reds_rf$pred, reds_rf$obs)$byClass["Specificity"]

#print results for clarity
print(paste('GLM Accuracy:', round(confusionMatrix(reds_glm$pred, reds_glm$obs)$overall['Accuracy'],2), 'GLM Specificity:', round(confusionMatrix(reds_glm$pred, reds_glm$obs)$byClass["Specificity"],2)))
print(paste('RF Accuracy:', round(confusionMatrix(reds_rf$pred, reds_rf$obs)$overall['Accuracy'],2), 'RF Specificity:', round(confusionMatrix(reds_rf$pred, reds_rf$obs)$byClass["Specificity"],2)))

#1% higher overall accuracy between random forest and logistic regression 
# also a higher specificity which is the goal of detecting high dew point pitches (0.2188) glm to (0.3476) rf
final_results <- reds_rf %>%
  rename('DEWPOINT_AFFECTED' = 'High_Dew_Point') %>%
  dplyr::select(PID,DEWPOINT_AFFECTED)

#deliverable for project
# write.csv(final_results, '/Users/sam/Desktop/Intern Project/Reds/submission.csv', row.names = FALSE)





