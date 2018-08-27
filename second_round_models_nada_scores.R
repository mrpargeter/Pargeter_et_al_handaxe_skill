#####Random forest with perceptual motor execution score######
library(plyr)
library(dplyr)
library(ggplot2)
library(car)

#average scores by assessment
Perceptual_motor_execution_data_copy<-Perceptual_motor_execution_data %>%
  group_by(knapper,assessment) %>%
  dplyr::summarise_all (funs(mean)) %>% 
  filter(!is.na(score)) %>% 
  dplyr::select(-c(score_component, aggregated_score))

#merge perceptual motor coordination data to complete dataset

Perceptual_motor_execution_data_complete<-merge(handaxe_postpca_scaled_minus_outlier, Perceptual_motor_execution_data_copy[ , c("knapper","assessment","score")], by = c("knapper","assessment"), all.x=TRUE)

Perceptual_motor_execution_data_complete$Score<-NULL

scatterplotMatrix(~score+Dim.1+Dim.2+profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv+percent_unflaked_area+delta_weight+
                                     percent_bifacially_flaked+flake_scar_density,
                                   data=Perceptual_motor_execution_data_complete, main="Scaled data structure matrix")

####random forest
library(randomForest)

#create test_perceptual_motor and train_perceptual_motoring data

require(caTools)
set.seed(425) 
sample_perceptual_motor = sample.split(Perceptual_motor_execution_data_complete$knapper, SplitRatio = .7)
train_perceptual_motor = subset(Perceptual_motor_execution_data_complete, sample_perceptual_motor == TRUE)
test_perceptual_motor  = subset(Perceptual_motor_execution_data_complete, sample_perceptual_motor == FALSE)

hist(train_perceptual_motor$score)
hist(test_perceptual_motor$score)

qqplot(train_perceptual_motor$score,test_perceptual_motor$score)
abline(a = 0, b = 1, lty = 3)

wilcox.test(train_perceptual_motor$score,test_perceptual_motor$score)

####tune parameters

##Method to check for optimal value of predictors at each node (reduce out of bag errors)
#an internal error estimate of a random forest as it is being constructed.
#each tree is trained on about 2/3 of the total training data.

oob.err<-double(9)
test.err<-double(9)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:9) 
{set.seed(425) 
  rf=randomForest(score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                    +delta_weight+Dim.1+Dim.2
                    +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                  data = train_perceptual_motor,
                  mtry=mtry,
                  importance=TRUE,
                  ntree=1000) 
  
  oob.err[mtry] = rf$mse[1000] #Error of all Trees fitted
  
  pred<-predict(rf,test_perceptual_motor) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test_perceptual_motor, mean( (score - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ")
  
}

test.err
oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("center",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

#####then write model
###model 1_all variables

set.seed(425)
fit_perceptual_motor <- randomForest(score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                    +delta_weight+Dim.1+Dim.2
                    +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                    data=train_perceptual_motor, 
                    importance=TRUE,
                    mtry=3,          #mtry, # of params to test_perceptual_motor at each node, from test_perceptual_motor above we see error minimized at mtry 7
                    ntree=1000,
                    keep.inbag = TRUE)

print(fit_perceptual_motor)
plot(fit_perceptual_motor)

#variable importance plot, reorder in order of decreased mean squared error

names<-c("Profile Asymetry","Plan Asymetry","Delta Profile Thickness",
         "Delta Weight","Shape PC1","Shape PC2","Percent Unflaked Area","Percent Bifacially Flaked","Flake Scar Density")
perceptual_motor_var_importance_data<-data.frame(importance(fit_perceptual_motor,type = 1)) #permutation importance, variable is assigned values by random permutation by how much will the MSE increase.
perceptual_motor_var_importance_data$predictor<-names
perceptual_motor_var_importance_data$predictor=factor(perceptual_motor_var_importance_data$predictor, levels=perceptual_motor_var_importance_data$predictor[order(perceptual_motor_var_importance_data$X.IncMSE)], ordered=TRUE)
colnames(perceptual_motor_var_importance_data)[1] <- "perceptual_motor_IncMSE"            
                
#10 % cut off: shape PC2, Profile asymetry, Delta Profile CV                                                                                                                                                                  "delta_profile_thickness_cv","profile_asymetry_index")))
ggplot(perceptual_motor_var_importance_data,aes(perceptual_motor_IncMSE,predictor)) +
  geom_point(size=3,aes(color=predictor)) +
  xlab('Percentage decrease in mean squared error') + ylab('Predictor') +
  scale_colour_manual(values=c('black','black','black','black','black','black','black','black','black'),guide = FALSE)+
  theme(text = element_text(size=20))

#comparison of r2 values for train_perceptual_motor and test_perceptual_motor data

actual_train_perceptual_motor <- train_perceptual_motor$score
predicted_train_perceptual_motor <- unname(predict(fit_perceptual_motor))
R2_train_perceptual_motor <- 1 - (sum((actual_train_perceptual_motor-predicted_train_perceptual_motor)^2)/sum((actual_train_perceptual_motor-mean(actual_train_perceptual_motor))^2))

actual_test_perceptual_motor <- test_perceptual_motor$score
predicted_test_perceptual_motor <- unname(predict(fit_perceptual_motor, test_perceptual_motor))
R2_test_perceptual_motor<- 1 - (sum((actual_test_perceptual_motor-predicted_test_perceptual_motor)^2)/sum((actual_test_perceptual_motor-mean(actual_test_perceptual_motor))^2))

R2_train_perceptual_motor
R2_test_perceptual_motor

#test_perceptual_motor on predictions

Prediction_perceptual_motor_train <- predict(fit_perceptual_motor)
submit_perceptual_motor_train <- data.frame(Score_observed = train_perceptual_motor$score, Score_predicted = Prediction_perceptual_motor_train,knapper =  train_perceptual_motor$knapper, assessment = train_perceptual_motor$assessment)

Prediction_perceptual_motor_test <- predict(fit_perceptual_motor, test_perceptual_motor)
submit_perceptual_motor_test <- data.frame(Score_observed = test_perceptual_motor$score, Score_predicted = Prediction_perceptual_motor_test, knapper =  test_perceptual_motor$knapper, assessment = test_perceptual_motor$assessment)

submit_all_prediction_perceptual_motor <- rbind(submit_perceptual_motor_train,submit_perceptual_motor_test)

##all data mean error
submit_all_prediction_perceptual_motor_error <- submit_all_prediction_perceptual_motor$Score_observed - submit_all_prediction_perceptual_motor$Score_predicted

rmse(error_all)
mae(error_all) #0.313 on a scale of 0-5

#residuals

qqnorm((fit_perceptual_motor$predicted-train_perceptual_motor$score)/sd(fit_perceptual_motor$predicted-train_perceptual_motor$score))
qqline((fit_perceptual_motor$predicted-train_perceptual_motor$score)/sd(fit_perceptual_motor$predicted-train_perceptual_motor$score))

fit_perceptual_motor_residuals<-fit_perceptual_motor$predicted-train_perceptual_motor$score
hist(fit_perceptual_motor_residuals)

#####second model fit_perceptual_motor

set.seed(425)
second_round_fit_perceptual_motor_2 <- randomForest(score ~
                      plan_asymetry_index+
                      delta_weight+Dim.1+
                      percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                      data=train_perceptual_motor,
                      mtry=5,
                      #nodesize=7,
                      importance=TRUE, 
                      ntree=1000,
                      keep.inbag = TRUE)

print(second_round_fit_perceptual_motor_2)

actual_train_perceptual_motor <- train_perceptual_motor$score
predicted_train_perceptual_motor <- unname(predict(second_round_fit_perceptual_motor_2))
R2_train_perceptual_motor <- 1 - (sum((actual_train_perceptual_motor-predicted_train_perceptual_motor)^2)/sum((actual_train_perceptual_motor-mean(actual_train_perceptual_motor))^2))

actual_test_perceptual_motor <- test_perceptual_motor$score
predicted_test_perceptual_motor <- unname(predict(second_round_fit_perceptual_motor_2, test_perceptual_motor))
R2_test_perceptual_motor<- 1 - (sum((actual_test_perceptual_motor-predicted_test_perceptual_motor)^2)/sum((actual_test_perceptual_motor-mean(actual_test_perceptual_motor))^2))

R2_train_perceptual_motor
R2_test_perceptual_motor

#residuals

qqnorm((second_round_fit_perceptual_motor_2$predicted-train_perceptual_motor$score)/sd(second_round_fit_perceptual_motor_2$predicted-train_perceptual_motor$score))
qqline((second_round_fit_perceptual_motor_2$predicted-train_perceptual_motor$score)/sd(second_round_fit_perceptual_motor_2$predicted-train_perceptual_motor$score))

fit_perceptual_motor2_residuals<-second_round_fit_perceptual_motor_2$predicted-train_perceptual_motor$score
hist(fit_perceptual_motor2_residuals)

##all data mean error
Prediction_perceptual_motor_train <- predict(second_round_fit_perceptual_motor_2)
submit_perceptual_motor_train <- data.frame(Score_observed = train_perceptual_motor$score, Score_predicted = Prediction_perceptual_motor_train,knapper =  train_perceptual_motor$knapper, assessment = train_perceptual_motor$assessment)

Prediction_perceptual_motor_test <- predict(second_round_fit_perceptual_motor_2, test_perceptual_motor)
submit_perceptual_motor_test <- data.frame(Score_observed = test_perceptual_motor$score, Score_predicted = Prediction_perceptual_motor_test, knapper =  test_perceptual_motor$knapper, assessment = test_perceptual_motor$assessment)

submit_all_prediction_perceptual_motor <- rbind(submit_perceptual_motor_train,submit_perceptual_motor_test)

error_all <- submit_all_prediction_perceptual_motor$Score_observed - submit_all_prediction_perceptual_motor$Score_predicted

rmse(error_all)
mae(error_all) #0.54 on a scale of 0-5

####check overall skill to assessment plot
ggplot(data = submit_all_prediction_perceptual_motor)+
  aes(x = assessment,y = Score_predicted) +
  geom_point(alpha = 0.6) +
  scale_y_continuous(name="Predicted Score")+
  scale_x_discrete(name="Assessment")+
  geom_smooth(method = "loess",data = submit_all_prediction_perceptual_motor  %>%
                mutate(assessment = as.numeric(assessment)), se=T,na.rm = T,span = 0.5)+
  theme(text = element_text(size=20)) 

##confidence intervals-turning random forests into a statistical proceedure##
#run rfPredVar function from model building R code file

rf.preds_cart_perceptual_motor_execution <- rfPredVar(second_round_fit_perceptual_motor_2,rf.data=Perceptual_motor_execution_data_complete,CI=TRUE,tree.type='rf')

preds_perceptual_motor_execution <- rf.preds_cart_perceptual_motor_execution
preds_perceptual_motor_execution$measured <- Perceptual_motor_execution_data_complete[ ,'score']
preds_perceptual_motor_execution$tree <- 'Random Forest'
preds_perceptual_motor_execution$pred.ij.var <- NULL
preds_perceptual_motor_execution$deviance <- preds_perceptual_motor_execution$pred - preds_perceptual_motor_execution$measured
preds_perceptual_motor_execution$CI_length<-ifelse(preds_perceptual_motor_execution$u.ci > preds_perceptual_motor_execution$l.ci, preds_perceptual_motor_execution$u.ci - preds_perceptual_motor_execution$l.ci, preds_perceptual_motor_execution$l.ci - preds_perceptual_motor_execution$u.ci)

#add data strings to plot relationship with score
preds_perceptual_motor_execution$bifacial_extent <- rep(Perceptual_motor_execution_data_complete$percent_bifacially_flaked)
preds_perceptual_motor_execution$delta_weight <- rep(Perceptual_motor_execution_data_complete$delta_weight)
preds_perceptual_motor_execution$flake_scar <- rep(Perceptual_motor_execution_data_complete$flake_scar_density)
preds_perceptual_motor_execution$perc_unflaked <- rep(Perceptual_motor_execution_data_complete$percent_unflaked_area)
preds_perceptual_motor_execution$dim_1 <- rep(Perceptual_motor_execution_data_complete$Dim.1)
preds_perceptual_motor_execution$dim_2 <- rep(Perceptual_motor_execution_data_complete$Dim.2)
preds_perceptual_motor_execution$assessment <- rep(Perceptual_motor_execution_data_complete$assessment)
preds_perceptual_motor_execution$knapper <- rep(Perceptual_motor_execution_data_complete$knapper)

ggplot(preds_perceptual_motor_execution,aes(assessment,pred)) +
  geom_point(size=0.5) +
  geom_abline(intercept=0,slope=1,lty=2,color='#999999') +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  xlab('Assessments') + ylab('Predicted') +
  theme_bw() + 
  theme(legend.position = "none")

#####Random forest with outcomes score######

#average scores by assessment
outcomes_data_copy<-Outcomes_data %>%
  group_by(knapper,assessment) %>%
  dplyr::summarise_all (funs(mean)) %>% 
  filter(!is.na(score)) %>% 
  dplyr::select(-c(score_component, aggregated_score))

#merge outcomes data to complete dataset

outcomes_data_complete<-merge(handaxe_postpca_scaled_minus_outlier, outcomes_data_copy[ , c("knapper","assessment","score")], by = c("knapper","assessment"), all.x=TRUE)

outcomes_data_complete$Score<-NULL

####random forest
#create test_outcomes and train_outcomesing data

set.seed(425) 
sample_outcomes = sample.split(outcomes_data_complete$knapper, SplitRatio = .7)
train_outcomes = subset(outcomes_data_complete, sample_outcomes == TRUE)
test_outcomes  = subset(outcomes_data_complete, sample_outcomes == FALSE)

hist(train_outcomes$score)
hist(test_outcomes$score)

qqplot(train_outcomes$score,test_outcomes$score)
abline(a = 0, b = 1, lty = 3)

wilcox.test(train_outcomes$score,test_outcomes$score)

####tune parameters

##Method to check for optimal value of predictors at each node (reduce out of bag errors)
#an internal error estimate of a random forest as it is being constructed.
#each tree is trained on about 2/3 of the total training data.

oob.err<-double(9)
test.err<-double(9)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:9) 
{set.seed(425) 
  rf=randomForest(score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                    +delta_weight+Dim.1+Dim.2
                    +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                  data = train_outcomes,
                  mtry=mtry,
                  importance=TRUE,
                  ntree=10000) 
  
  oob.err[mtry] = rf$mse[10000] #Error of all Trees fitted
  
  pred<-predict(rf,test_outcomes) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test_outcomes, mean( (score - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ")
  
}

test.err
oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("center",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

####model 1_all variables

set.seed(425)
fit_outcomes <- randomForest(score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                    +delta_weight+Dim.1+Dim.2
                    +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                    data=train_outcomes, 
                    importance=TRUE,
                    mtry=2,          #mtry, # of params to test_outcomes at each node, from test_outcomes above we see error minimized at mtry 7
                    ntree=10000,
                    keep.inbag = TRUE)

print(fit_outcomes)
plot(fit_outcomes)

#variable importance plot
outcomes_var_importance_data<-data.frame(importance(fit_outcomes,type = 1)) #permutation importance, variable is assigned values by random permutation by how much will the MSE increase.
outcomes_var_importance_data$predictor<-names
outcomes_var_importance_data$predictor=factor(outcomes_var_importance_data$predictor, levels=outcomes_var_importance_data$predictor[order(outcomes_var_importance_data$X.IncMSE)], ordered=TRUE)
colnames(outcomes_var_importance_data)[1] <- "outcomes_IncMSE"            

#shows Dim 2, and % bifacially flaked, plan asymetry, delta weight the same and others gradual reduction with delta profile CV as lowest again                                                                                                                                                                "delta_profile_thickness_cv","profile_asymetry_index")))
ggplot(outcomes_var_importance_data,aes(outcomes_IncMSE,predictor)) +
  geom_point(size=3,aes(color=predictor)) +
  xlab('Percentage decrease in mean squared error') + ylab('Predictor') +
  scale_colour_discrete(guide = FALSE)+
  scale_colour_manual(values=c('black','black','black','black','black','black','black','black','black'),guide = FALSE)+
  theme(text = element_text(size=20))

#comparison of r2 values for train_outcomes and test_outcomes data

actual_train_outcomes <- train_outcomes$score
predicted_train_outcomes <- unname(predict(fit_outcomes))
R2_train_outcomes <- 1 - (sum((actual_train_outcomes-predicted_train_outcomes)^2)/sum((actual_train_outcomes-mean(actual_train_outcomes))^2))

actual_test_outcomes <- test_outcomes$score
predicted_test_outcomes <- unname(predict(fit_outcomes, test_outcomes))
R2_test_outcomes<- 1 - (sum((actual_test_outcomes-predicted_test_outcomes)^2)/sum((actual_test_outcomes-mean(actual_test_outcomes))^2))

R2_train_outcomes
R2_test_outcomes

#test_outcomes on predictions

Prediction_outcomes_train <- predict(fit_outcomes)
submit_outcomes_train <- data.frame(Score_observed = train_outcomes$score, Score_predicted = Prediction_outcomes_train,knapper =  train_outcomes$knapper, assessment = train_outcomes$assessment)

Prediction_outcomes_test <- predict(fit_outcomes, test_outcomes)
submit_outcomes_test <- data.frame(Score_observed = test_outcomes$score, Score_predicted = Prediction_outcomes_test, knapper =  test_outcomes$knapper, assessment = test_outcomes$assessment)

submit_all_outcomes <- rbind(submit_outcomes_train,submit_outcomes_test)

plot(submit_all_outcomes$Score_observed,submit_all_outcomes$Score_predicted)
summary(lm(Score_observed~Score_predicted, data=submit_all_outcomes))

error_all <- submit_all_outcomes$Score_observed - submit_all_outcomes$Score_predicted

##all data mean error

rmse(error_all)
mae(error_all) #0.28 on a scale of 0-5

#residuals

qqnorm((fit_outcomes$predicted-train_outcomes$score)/sd(fit_outcomes$predicted-train_outcomes$score))
qqline((fit_outcomes$predicted-train_outcomes$score)/sd(fit_outcomes$predicted-train_outcomes$score))

fit_residuals<-fit_outcomes$predicted-train_outcomes$score
hist(fit_residuals)

#####second model fit_outcomes
set.seed(425)
second_round_fit_2_outcomes <- randomForest(score ~
                                     profile_asymetry_index+plan_asymetry_index
                                    +delta_weight+Dim.1+Dim.2
                                     +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                                   data=train_outcomes,
                                   mtry=2,
                                   #nodesize=7,
                                   importance=TRUE, 
                                   ntree=1000,
                                   keep.inbag = TRUE)

print(second_round_fit_2_outcomes)

actual_train_outcomes <- train_outcomes$score
predicted_train_outcomes <- unname(predict(second_round_fit_2_outcomes))
R2_train_outcomes <- 1 - (sum((actual_train_outcomes-predicted_train_outcomes)^2)/sum((actual_train_outcomes-mean(actual_train_outcomes))^2))

actual_test_outcomes <- test_outcomes$score
predicted_test_outcomes <- unname(predict(second_round_fit_2_outcomes, test_outcomes))
R2_test_outcomes<- 1 - (sum((actual_test_outcomes-predicted_test_outcomes)^2)/sum((actual_test_outcomes-mean(actual_test_outcomes))^2))

R2_train_outcomes
R2_test_outcomes

#residuals

qqnorm((second_round_fit_2_outcomes$predicted-train_outcomes$score)/sd(second_round_fit_2_outcomes$predicted-train_outcomes$score))
qqline((second_round_fit_2_outcomes$predicted-train_outcomes$score)/sd(second_round_fit_2_outcomes$predicted-train_outcomes$score))

fit2_residuals<-second_round_fit_2_outcomes$predicted-train_outcomes$score
hist(fit2_residuals)

##all data mean error
Prediction_outcomes_train <- predict(second_round_fit_2_outcomes)
submit_outcomes_train <- data.frame(Score_observed = train_outcomes$score, Score_predicted = Prediction_outcomes_train,knapper =  train_outcomes$knapper, assessment = train_outcomes$assessment)

Prediction_outcomes_test <- predict(second_round_fit_2_outcomes, test_outcomes)
submit_outcomes_test <- data.frame(Score_observed = test_outcomes$score, Score_predicted = Prediction_outcomes_test, knapper =  test_outcomes$knapper, assessment = test_outcomes$assessment)

submit_all_outcomes <- rbind(submit_outcomes_train,submit_outcomes_test)

error_all <- submit_all_outcomes$Score_observed - submit_all_outcomes$Score_predicted

rmse(error_all)
mae(error_all)

####check overall skill to assessment plot
ggplot(data = submit_all_outcomes)+
  aes(x = assessment,y = Score_predicted) +
  geom_point(alpha = 0.6) +
  scale_y_continuous(name="Predicted Score")+
  scale_x_discrete(name="Assessment")+
  geom_smooth(method = "loess",data = submit_all_outcomes  %>%
                mutate(assessment = as.numeric(assessment)), se=T,na.rm = T,span = 0.5)+
  theme(text = element_text(size=20)) 

##confidence intervals-turning random forests into a statistical proceedure##
rf.preds_cart_outcomes <- rfPredVar(second_round_fit_2,rf.data=outcomes_data_complete,CI=TRUE,tree.type='rf')
rf.preds_cart_outcomes$measured <- outcomes_data_complete[ ,'score']
preds_outcomes <- rf.preds_cart_outcomes
preds_outcomes$tree <- 'Random Forest'
preds_outcomes$pred.ij.var <- NULL
preds_outcomes$deviance <- preds_outcomes$pred - preds_outcomes$measured
preds_outcomes$CI_length<-ifelse(preds_outcomes$u.ci > preds_outcomes$l.ci, preds_outcomes$u.ci - preds_outcomes$l.ci, preds_outcomes$l.ci - preds_outcomes$u.ci)

#add data strings to plot relationship with score
preds_outcomes$bifacial_extent <- rep(outcomes_data_complete$percent_bifacially_flaked)
preds_outcomes$flake_scar <- rep(outcomes_data_complete$flake_scar_density)
preds_outcomes$dim_1 <- rep(outcomes_data_complete$Dim.1)
preds_outcomes$assessment <- rep(outcomes_data_complete$assessment)
preds_outcomes$knapper <- rep(outcomes_data_complete$knapper)

ggplot(preds_outcomes,aes(measured,pred)) +
  geom_point(size=0.5) +
  geom_abline(intercept=0,slope=1,lty=2,color='#999999') +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  xlab('Actual') + ylab('Predicted') +
  theme_bw() + 
  theme(legend.position = "none")

#####Random forest with Strategic_understanding_data score######

#average scores by assessment
Strategic_understanding_data_copy<-Strategic_understanding_data %>%
  group_by(knapper,assessment) %>%
  dplyr::summarise_all (funs(mean)) %>% 
  filter(!is.na(score)) %>% 
  dplyr::select(-c(score_component, aggregated_score))

#merge perceptual motor coordination data to complete dataset

Strategic_understanding_data_complete<-merge(handaxe_postpca_scaled_minus_outlier, Strategic_understanding_data_copy[ , c("knapper","assessment","score")], by = c("knapper","assessment"), all.x=TRUE)

Strategic_understanding_data_complete$Score<-NULL

####random forest
#create test and training data
set.seed(425) 
sample_strategic = sample.split(Strategic_understanding_data_complete$knapper, SplitRatio = .7)
train_strategic = subset(Strategic_understanding_data_complete, sample_strategic == TRUE)
test_strategic  = subset(Strategic_understanding_data_complete, sample_strategic == FALSE)

hist(train_strategic$score)
hist(test_strategic$score)

qqplot(train_strategic$score,test_strategic$score)
abline(a = 0, b = 1, lty = 3)

wilcox.test(train_strategic$score,test_strategic$score)

####tune parameters
oob.err<-double(9)
test.err<-double(9)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:9) 
{set.seed(425) 
  rf=randomForest(score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                    +delta_weight+Dim.1+Dim.2
                    +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                  data = train_strategic,
                  mtry=mtry,
                  importance=TRUE,
                  ntree=1000) 
  
  oob.err[mtry] = rf$mse[1000] #Error of all Trees fitted
  
  pred<-predict(rf,test_strategic) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test_strategic, mean( (score - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ")
  
}

test.err
oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("center",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

####model 1_all variables

set.seed(425)
fit_strategic <- randomForest(score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                    +delta_weight+Dim.1+Dim.2
                    +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                    data=train_strategic, 
                    importance=TRUE,
                    #nodesize=7,    #node size set to c. 1 % of total data frame, conservative
                    mtry=6,          #mtry, # of params to test_strategic at each node, from test_strategic above we see error minimized at mtry 7
                    ntree=1000,
                    keep.inbag = TRUE)

print(fit_strategic)
plot(fit_strategic)

#variable importance plot
strategic_var_importance_data<-data.frame(importance(fit_strategic,type = 1)) #permutation importance, variable is assigned values by random permutation by how much will the MSE increase.
strategic_var_importance_data$predictor<-names
strategic_var_importance_data$predictor=factor(strategic_var_importance_data$predictor, levels=strategic_var_importance_data$predictor[order(strategic_var_importance_data$X.IncMSE)], ordered=TRUE)
colnames(strategic_var_importance_data)[1]<-"strategic_X.IncMSE"

#shows plan asymmetry and delta profile, delta weight much less important                                                                                                                                                                    "delta_profile_thickness_cv","profile_asymetry_index")))
ggplot(strategic_var_importance_data,aes(strategic_X.IncMSE,predictor)) +
  geom_point(size=3,aes(color=predictor)) +
  xlab('Percentage decrease in mean squared error') + ylab('Predictor') +
  scale_colour_discrete(guide = FALSE)+
  scale_colour_manual(values=c('black','black','black','black','black','black','black','black','black'),guide = FALSE)+
  theme(text = element_text(size=20))

#comparison of r2 values for train_strategic and test_strategic data

actual_train <- train_strategic$score
predicted_train <- unname(predict(fit_strategic))
R2_train <- 1 - (sum((actual_train-predicted_train)^2)/sum((actual_train-mean(actual_train))^2))

actual_test <- test_strategic$score
predicted_test <- unname(predict(fit_strategic, test_strategic))
R2_test<- 1 - (sum((actual_test-predicted_test)^2)/sum((actual_test-mean(actual_test))^2))

R2_train
R2_test

#residuals

qqnorm((fit_strategic$predicted-train_strategic$score)/sd(fit_strategic$predicted-train_strategic$score))
qqline((fit_strategic$predicted-train_strategic$score)/sd(fit_strategic$predicted-train_strategic$score))

fit_residuals<-fit_strategic$predicted-train_strategic$score
hist(fit_residuals)

#####second model fit_strategic
#write model
set.seed(425)
second_round_fit_2_strategic <- randomForest(score ~plan_asymetry_index+percent_bifacially_flaked,
                                   data=train_strategic,
                                   mtry=2,
                                   #nodesize=7,
                                   importance=TRUE, 
                                   ntree=1000,
                                   keep.inbag = TRUE)

print(second_round_fit_2_strategic)

actual_train <- train_strategic$score
predicted_train <- unname(predict(second_round_fit_2_strategic))
R2_train <- 1 - (sum((actual_train-predicted_train)^2)/sum((actual_train-mean(actual_train))^2))

actual_test <- test_strategic$score
predicted_test <- unname(predict(second_round_fit_2_strategic, test_strategic))
R2_test<- 1 - (sum((actual_test-predicted_test)^2)/sum((actual_test-mean(actual_test))^2))

R2_train
R2_test

#residuals

qqnorm((second_round_fit_2_strategic$predicted-train_strategic$score)/sd(second_round_fit_2_strategic$predicted-train_strategic$score))
qqline((second_round_fit_2_strategic$predicted-train_strategic$score)/sd(second_round_fit_2_strategic$predicted-train_strategic$score))

fit2_residuals<-second_round_fit_2_strategic$predicted-train_strategic$score
hist(fit2_residuals)

##all data mean error

Prediction_strategic_train <- predict(second_round_fit_2_strategic)
submit_strategic_train <- data.frame(Score_observed = train_strategic$score, Score_predicted = Prediction_strategic_train,knapper =  train_strategic$knapper, assessment = train_strategic$assessment)

Prediction_strategic_test <- predict(second_round_fit_2_strategic, test_strategic)
submit_strategic_test <- data.frame(Score_observed = test_strategic$score, Score_predicted = Prediction_strategic_test, knapper =  test_strategic$knapper, assessment = test_strategic$assessment)

submit_all_strategic <- rbind(submit_strategic_train,submit_strategic_test)

error_all <- submit_all_strategic$Score_observed - submit_all_strategic$Score_predicted

rmse(error_all)
mae(error_all) #0.3510674 on a scale of 0-5

####check overall skill to assessment plot
ggplot(data = submit_all_strategic)+
  aes(x = assessment,y = Score_predicted) +
  geom_point(alpha = 0.6) +
  scale_y_continuous(name="Predicted Score")+
  scale_x_discrete(name="Assessment")+
  geom_smooth(method = "loess",data = submit_all_strategic  %>%
                mutate(assessment = as.numeric(assessment)), se=T,na.rm = T,span = 0.5)+
  theme(text = element_text(size=20)) 

##confidence intervals-turning random forests into a statistical proceedure##
rf.preds_cart_strategic <- rfPredVar(second_round_fit_2_strategic,rf.data=Strategic_understanding_data_complete,CI=TRUE,tree.type='rf')
rf.preds_cart_strategic$measured <- Strategic_understanding_data_complete[ ,'score']
preds_strategic <- rf.preds_cart_strategic
preds_strategic$tree <- 'Random Forest'
preds_strategic$pred.ij.var <- NULL
preds_strategic$deviance <- rf.preds_cart_strategic$pred - rf.preds_cart_strategic$measured
preds_strategic$CI_length<-ifelse(rf.preds_cart_strategic$u.ci > rf.preds_cart_strategic$l.ci, rf.preds_cart_strategic$u.ci - rf.preds_cart_strategic$l.ci, rf.preds_cart_strategic$l.ci - rf.preds_cart_strategic$u.ci)

#add data strings to plot relationship with score
preds_strategic$bifacial_extent <- rep(Strategic_understanding_data_complete$percent_bifacially_flaked)
preds_strategic$flake_scar <- rep(Strategic_understanding_data_complete$flake_scar_density)
preds_strategic$dim_1 <- rep(Strategic_understanding_data_complete$Dim.1)
preds_strategic$assessment <- rep(Strategic_understanding_data_complete$assessment)
preds_strategic$knapper <- rep(Strategic_understanding_data_complete$knapper)

ggplot(preds_strategic,aes(measured,pred)) +
  geom_point(size=0.5) +
  geom_abline(intercept=0,slope=1,lty=2,color='#999999') +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  xlab('Actual') + ylab('Predicted') +
  theme_bw() + 
  theme(legend.position = "none")

#######combined random forest confidence interval plots####
#example if want to do it with rfpreds
preds_rf <-
  preds %>%
  mutate(score_component = rep("all", times=nrow(preds)))

complete_model_all <-
  submit_all_fit_2 %>%
  mutate(score_component = rep("all", times=nrow(submit_all_fit_2)))

strategic_model <-
  submit_all_strategic %>%
  mutate(score_component = rep("strategic", times=nrow(submit_all_strategic)))

outcomes_model <-
  submit_all_outcomes %>%
  mutate(score_component = rep("outcomes", times=nrow(submit_all_outcomes)))

perceptual_motor_execution_model <-
  submit_all_prediction_perceptual_motor  %>%
  mutate(score_component = rep("perceptual_motor", times=nrow(submit_all_prediction_perceptual_motor)))

all_models<-bind_rows(complete_model_all,strategic_model, outcomes_model,perceptual_motor_execution_model)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

#see loess lines approach below
models_assessment_score<-ggplot(all_models,aes(assessment,Score_predicted,color=score_component)) +
  geom_point(size=0.5) +
  #geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  xlab('Assessment') + ylab('Predicted Score') + 
  theme(legend.position = "none")+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  theme(text = element_text(size=20))+
  scale_color_manual(values=c("red","black", "blue","purple"))+
  geom_smooth(method = "loess", data = all_models  %>%
                mutate(assessment = as.numeric(assessment)), se=T,na.rm = T,span=0.2)

models_predicted_measured<-ggplot(all_models,aes(Score_observed,Score_predicted,color=score_component)) +
  geom_point(size=0.5) +
  #geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  xlab('Actual Score') + ylab('Predicted Score') +
  theme_bw() + 
  theme(legend.position = "none")+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  theme(text = element_text(size=20))+
  scale_color_manual(values=c("red","black", "blue","purple"))

##loess lines approach

boxplot(complete_model_all$Score_predicted~complete_model_all$assessment,col = "white",
        main = "",
        xlab = "Assessment",
        ylab = "Modelled Score",
        par(cex.lab=1.5),
        par(cex.axis=1.2),
        ylim = c(1, 4),
        cex.main=1.5) 

lines(lowess(complete_model_all$assessment, complete_model_all$Score_predicted, f=.2), col=2,lwd=3)

lines(lowess(perceptual_motor_execution_model$assessment, perceptual_motor_execution_model$Score_predicted, f=.2), col=2,lwd=4.5,lty=2)
lines(lowess(outcomes_model$assessment, outcomes_model$Score_predicted, f=.2), col=4,lwd=4.5,lty=3)
lines(lowess(strategic_model$assessment, strategic_model$Score_predicted, f=.2), col=3,lwd=4.5,lty=4)

boxplot(strategic_model$Score_predicted~strategic_model$assessment,col = "blue",
        main = "Strategic model",
        xlab = "Assessment",
        ylab = "Predicted score",
        par(cex.lab=1.5),
        par(cex.axis=1.2),
        ylim = c(0, 4.5),
        cex.main=1.5) 
lines(lowess(strategic_model$assessment, strategic_model$Score_predicted, f=.2), col=3,lwd=3)

boxplot(outcomes_model$Score_predicted~outcomes_model$assessment,col = "white",
        main = "Outcomes model",
        xlab = "Assessment",
        ylab = "Predicted score",
        par(cex.lab=1.5),
        par(cex.axis=1.2),
        ylim = c(0, 4.5),
        cex.main=1.5)
lines(lowess(outcomes_model$assessment, outcomes_model$Score_predicted, f=.2), col=4,lwd=3)

boxplot(perceptual_motor_execution_model$Score_predicted~perceptual_motor_execution_model$assessment,col = "orange",
        main = "Perceptual motor model",
        xlab = "Assessment",
        ylab = "Predicted score",
        par(cex.lab=1.5),
        par(cex.axis=1.2),
        ylim = c(0, 4.5),
        cex.main=1.5)
lines(lowess(perceptual_motor_execution_model$assessment, perceptual_motor_execution_model$Score_predicted, f=.2), col=5,lwd=3)
lines(lowess(outcomes_model$assessment, outcomes_model$Score_predicted, f=.2), col=4,lwd=3)
lines(lowess(strategic_model$assessment, strategic_model$Score_predicted, f=.2), col=3,lwd=3)
lines(lowess(complete_model_all$assessment, complete_model_all$Score_predicted, f=.2), col=7,lwd=3)

##compare predictor variables across models
all_predictor_vars<-merge(strategic_var_importance_data,outcomes_var_importance_data,by="predictor")
all_predictor_vars<-merge(all_predictor_vars,perceptual_motor_var_importance_data,by="predictor")
all_predictor_vars<-merge(all_predictor_vars,all_var_importance_data,by="predictor")

str(all_predictor_vars)

all_predictor_vars<-melt(all_predictor_vars, 
     variable.name = "model_type",
     value.names = c("strategic_X.IncMSE", "outcomes_IncMSE","perceptual_motor_IncMSE","All_X.IncMSE"))

ggplot(all_predictor_vars,aes(value,predictor)) +
  geom_point(size=3,aes(color=variable)) +
  xlab('Percentage decrease in mean squared error') + ylab('Predictor') +
  scale_colour_manual(values=c('black','red','blue','orange'),
                      labels = c("Strategic","Outcomes","Perceptual Motor","All data"))+
  theme(text = element_text(size=20))+ 
  guides(color=guide_legend("Model Component"))

##examine confidence interval lengths and deviance by model and assessment
detach(package:plyr)

all_models_ci <-
  all_models %>%
  group_by(score_component,assessment) %>%
  mutate(ave_ci_length = mean(CI_length))

all_models_ci$score_component<-car::recode(all_models_ci$score_component, "'all' = 'All data'; 'outcomes'='Outcomes'; 
       'perceptual_motor'='Perceptual Motor';'strategic'= 'Strategic'")

ggplot(all_models_ci, aes(score_component, ave_ci_length)) +
  geom_bar(aes(fill = score_component), position = "dodge", stat="identity")+
  facet_grid(~assessment)+ 
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(name="Average prediction confidence interval length")+
  scale_x_discrete(name="Score Component")+
  guides(fill=FALSE)+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(text = element_text(size=20))

ggplot(all_models_ci, aes(score_component, deviance)) +
  geom_bar(aes(fill = score_component), position = "dodge", stat="identity")+ 
  facet_grid(~assessment) +
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(name="Deviance of predicted values from observed values")+
  scale_x_discrete(name="Score Component")+
  guides(fill=FALSE)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(text = element_text(size=20)) +
  geom_point(aes(x = score_component), size=2,shape = 21, position = position_dodge(width = 1))+
  geom_hline(yintercept = 0.5,color="red")+
  geom_hline(yintercept = -0.5,color="red")

#########Combined psychometric data###########
psychometric<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Psychometric data/tol_em_data.csv")

#Assessment 1

psychometric<-
  psychometric %>%
  mutate(assessment=rep("1", times=nrow(psychometric)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

all_models<-
  all_models %>%
  mutate(knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_score_combined<-
  left_join(all_models,psychometric, by=c("assessment", "knapper"))  %>%
  mutate(ToL_1_EM_log=ToL_1_EM_start+1,
         score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_score_combined, score_component %in% "all"), aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

tol_1<-ggplot(data=psychometric_score_combined, aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Tower of London Error")+
  ylab(label="Predicted Score Assessment 1")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=15))

summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "all")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "outcomes")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "perceptual_motor")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "strategic")))

#Assessment 2

psychometric<-
  psychometric %>%
  mutate(assessment=rep("2", times=nrow(psychometric)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

all_models<-
  all_models %>%
  mutate(knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_score_combined<-
  left_join(all_models,psychometric, by=c("assessment", "knapper"))  %>%
  mutate(ToL_1_EM_log=ToL_1_EM_start+1,
         score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_score_combined, score_component %in% "all"), aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                       score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

tol_2<-ggplot(data=psychometric_score_combined, aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Tower of London Error")+
  ylab(label="Predicted Score Assessment 2")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=15))

summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "all")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "outcomes")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "perceptual_motor")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "strategic")))

##TOL to assessment 3

psychometric<-
  psychometric %>%
  mutate(assessment=rep("3", times=nrow(psychometric)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

all_models<-
  all_models %>%
  mutate(knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_score_combined<-
  left_join(all_models,psychometric, by=c("assessment", "knapper"))  %>%
  mutate(ToL_1_EM_log=ToL_1_EM_start+1,
         score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_score_combined, score_component %in% "all"), aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

tol_3<-ggplot(data=psychometric_score_combined, aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Tower of London Error")+
  ylab(label="Predicted Score Assessment 3")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=15))

summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "all")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "outcomes")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "perceptual_motor")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "strategic")))

##TOL to assessment 4

psychometric<-
  psychometric %>%
  mutate(assessment=rep("4", times=nrow(psychometric)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

all_models<-
  all_models %>%
  mutate(knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_score_combined<-
  left_join(all_models,psychometric, by=c("assessment", "knapper"))  %>%
  mutate(ToL_1_EM_log=ToL_1_EM_start+1,
         score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_score_combined, score_component %in% "all"), aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

tol_4<-ggplot(data=psychometric_score_combined, aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Tower of London Error")+
  ylab(label="Predicted Score Assessment 4")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=15))

summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "all")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "outcomes")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "perceptual_motor")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "strategic")))


##TOL to assessment 5

psychometric<-
  psychometric %>%
  mutate(assessment=rep("5", times=nrow(psychometric)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

all_models<-
  all_models %>%
  mutate(knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_score_combined<-
  left_join(all_models,psychometric, by=c("assessment", "knapper"))  %>%
  mutate(ToL_1_EM_log=ToL_1_EM_start+1,
         score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_score_combined, score_component %in% "all"), aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

tol_5<-ggplot(data=psychometric_score_combined, aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Tower of London Error")+
  ylab(label="Predicted Score Assessment 5")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=15))

summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "all")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "outcomes")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "perceptual_motor")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "strategic")))

##multiplot
multiplot(tol_1,tol_2,tol_3,tol_4,tol_5,cols=2)

##TOL to assessment 9

psychometric<-
  psychometric %>%
  mutate(assessment=rep("9", times=nrow(psychometric)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

all_models<-
  all_models %>%
  mutate(knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_score_combined<-
  left_join(all_models,psychometric, by=c("assessment", "knapper"))  %>%
  mutate(ToL_1_EM_log=ToL_1_EM_start+1,
         score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_score_combined, score_component %in% "all"), aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

ggplot(data=psychometric_score_combined, aes(x=ToL_1_EM_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Tower of London Error")+
  ylab(label="Predicted Score Assessment 9")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=15))

summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "all")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "outcomes")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "perceptual_motor")))
summary(lm(ToL_1_EM_start~Score_predicted, data=filter(psychometric_score_combined, score_component %in% "strategic")))

####Wisconsin card sort

psychometric_card_sort<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Psychometric data/card_pe_data.csv")

#Assessment 1

psychometric_card<-
  psychometric_card_sort %>%
  mutate(assessment=rep("1", times=nrow(psychometric_card_sort)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_card_score_combined<-
  left_join(all_models,psychometric_card, by=c("assessment", "knapper"))  %>%
  mutate(score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_card_score_combined, score_component %in% "all"), aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

ggplot(data=psychometric_card_score_combined, aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Card Sort Error")+
  ylab(label="Predicted Score Assessment 1")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=20))

summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "all")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "outcomes")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "perceptual_motor")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "strategic")))

#Assessment 2

psychometric_card<-
  psychometric_card_sort %>%
  mutate(assessment=rep("2", times=nrow(psychometric_card_sort)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_card_score_combined<-
  left_join(all_models,psychometric_card, by=c("assessment", "knapper"))  %>%
  mutate(score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_card_score_combined, score_component %in% "all"), aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

ggplot(data=psychometric_card_score_combined, aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Card Sort Error")+
  ylab(label="Predicted Score Assessment 2")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=20))

summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "all")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "outcomes")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "perceptual_motor")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "strategic")))

#Assessment 3

psychometric_card<-
  psychometric_card_sort %>%
  mutate(assessment=rep("3", times=nrow(psychometric_card_sort)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_card_score_combined<-
  left_join(all_models,psychometric_card, by=c("assessment", "knapper"))  %>%
  mutate(score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_card_score_combined, score_component %in% "all"), aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

ggplot(data=psychometric_card_score_combined, aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Card Sort Error")+
  ylab(label="Predicted Score Assessment 3")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=20))

summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "all")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "outcomes")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "perceptual_motor")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "strategic")))

#Assessment 4

psychometric_card<-
  psychometric_card_sort %>%
  mutate(assessment=rep("4", times=nrow(psychometric_card_sort)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_card_score_combined<-
  left_join(all_models,psychometric_card, by=c("assessment", "knapper"))  %>%
  mutate(score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_card_score_combined, score_component %in% "all"), aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

ggplot(data=psychometric_card_score_combined, aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Card Sort Error")+
  ylab(label="Predicted Score Assessment 4")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=20))

summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "all")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "outcomes")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "perceptual_motor")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "strategic")))

#Assessment 5

psychometric_card<-
  psychometric_card_sort %>%
  mutate(assessment=rep("5", times=nrow(psychometric_card_sort)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_card_score_combined<-
  left_join(all_models,psychometric_card, by=c("assessment", "knapper"))  %>%
  mutate(score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_card_score_combined, score_component %in% "all"), aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

ggplot(data=psychometric_card_score_combined, aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Card Sort Error")+
  ylab(label="Predicted Score Assessment 5")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=20))

summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "all")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "outcomes")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "perceptual_motor")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "strategic")))

#Assessment 9

psychometric_card<-
  psychometric_card_sort %>%
  mutate(assessment=rep("9", times=nrow(psychometric_card_sort)),
         knapper=as.factor(knapper),
         assessment=as.factor(assessment))

psychometric_card_score_combined<-
  left_join(all_models,psychometric_card, by=c("assessment", "knapper"))  %>%
  mutate(score_component = factor(score_component, levels=c("all","perceptual_motor","outcomes","strategic")))

ggplot(data=filter(psychometric_card_score_combined, score_component %in% "all"), aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)

model_names <- c(
  'all'="All Data",
  'outcomes'="Outcomes",
  'perceptual_motor'="Perceptual Motor",
  'strategic'="Strategic"
)

plot_facet_text <- data.frame(Score_predicted = 4,ToL_1_EM = 5,lab = "P-value = 0.0431",
                              score_component = factor("strategic",levels = c("all","outcomes","perceptual_motor","strategic")))

ggplot(data=psychometric_card_score_combined, aes(x=Card_sort_1_PE_start, y= Score_predicted))+
  geom_point()+
  #ylim(2.5,4.5)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  facet_grid(~score_component,labeller=labeller(score_component=model_names))+
  xlab(label="Card Sort Error")+
  ylab(label="Predicted Score Assessment 9")+ 
  #geom_text(data = ann_text,label = "P-value = 0.0431")+
  theme(text = element_text(size=20))

summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "all")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "outcomes")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "perceptual_motor")))
summary(lm(Card_sort_1_PE_start~Score_predicted, data=filter(psychometric_card_score_combined, score_component %in% "strategic")))

#############psychometrics against learning slope
#TOL
slope_tol_psychometrics<-merge(psychometric,individual_slopes[,c("knapper","slope")],by="knapper")

summary(lm(ToL_1_EM_start~slope, data=slope_psychometrics))
plot(ToL_1_EM_start~slope, data=slope_psychometrics)
summary(lm(ToL_1_EM_end~slope, data=slope_psychometrics))

#Wisc
slope_wisc_psychometrics<-merge(psychometric_card_sort,individual_slopes[,c("knapper","slope")],by="knapper")

summary(lm(Card_sort_1_PE_start~slope, data=slope_wisc_psychometrics))
summary(lm(Card_sort_1_PE_end~slope, data=slope_wisc_psychometrics))

###########Control group score extraction########

control_motor<-submit_all_prediction_perceptual_motor
colnames(control_motor)[colnames(control_motor)=="Score_predicted"] <- "perceptual_motor_score"
control_motor$Score_observed <- NULL

control_outcomes<-submit_all_outcomes
colnames(control_outcomes)[colnames(control_outcomes)=="Score_predicted"] <- "outcomes_score"
control_outcomes$Score_observed <- NULL

control_strategic<-submit_all_strategic
colnames(control_strategic)[colnames(control_strategic)=="Score_predicted"] <- "strategic_score"
control_strategic$Score_observed <- NULL

control_data<-merge(control_motor,control_outcomes,by=c("knapper","assessment"))
control_data<-merge(control_data,control_strategic,by=c("knapper","assessment"))
control_data<-subset(control_data,knapper %in% c("22","23","24","25","26","27","32","34"))        

write.csv(control_data,"control_sub_model_data.csv")

#####outlier handaxes-for Erin data set, not for manuscript
#perceptual motor
#merge perceptual motor coordination data to complete dataset
Perceptual_motor_execution_data_outlier<-merge(outlier_handaxes, Perceptual_motor_execution_data_copy[ , c("knapper","assessment","score")], by = c("knapper","assessment"), all.x=TRUE)
Prediction_outliers_outcomes <- predict(second_round_fit_2_outcomes, Perceptual_motor_execution_data_outlier)
submit_all_fit_2_outliers_outcomes <- data.frame(Score_observed = Perceptual_motor_execution_data_outlier$Score, Score_predicted = Prediction_outliers_outcomes, knapper =  Perceptual_motor_execution_data_outlier$knapper, assessment = Perceptual_motor_execution_data_outlier$assessment)

#Strategic
#merge perceptual motor coordination data to complete dataset
strategic_data_outlier<-merge(outlier_handaxes, Strategic_understanding_data_copy[ , c("knapper","assessment","score")], by = c("knapper","assessment"), all.x=TRUE)
Prediction_outliers_strategic <- predict(second_round_fit_2_strategic, strategic_data_outlier)
submit_all_fit_2_outliers_strategic<- data.frame(Score_observed = strategic_data_outlier$Score, Score_predicted = Prediction_outliers_strategic, knapper =  strategic_data_outlier$knapper, assessment = strategic_data_outlier$assessment)

#outcomes
#merge perceptual motor coordination data to complete dataset
outcomes_data_outlier<-merge(outlier_handaxes, outcomes_data_copy[ , c("knapper","assessment","score")], by = c("knapper","assessment"), all.x=TRUE)
Prediction_outliers_outcomes <- predict(second_round_fit_2_outcomes, outcomes_data_outlier)
submit_all_fit_2_outliers_outcomes<- data.frame(Score_observed = outcomes_data_outlier$Score, Score_predicted = Prediction_outliers_outcomes, knapper =  outcomes_data_outlier$knapper, assessment = outcomes_data_outlier$assessment)

strategic_model_outliers <-
  submit_all_fit_2_outliers_strategic %>%
  mutate(score_component = rep("strategic", times=nrow(submit_all_fit_2_outliers_strategic)))

outcomes_model_outliers <-
  submit_all_fit_2_outliers_outcomes %>%
  mutate(score_component = rep("outcomes", times=nrow(submit_all_fit_2_outliers_outcomes)))

perceptual_motor_execution_model_outliers <-
  submit_all_fit_2_outliers_outcomes  %>%
  mutate(score_component = rep("perceptual_motor", times=nrow(submit_all_fit_2_outliers_outcomes)))

all_models_outliers<-bind_rows(strategic_model_outliers, outcomes_model_outliers,perceptual_motor_execution_model_outliers)

all_models_outliers_components<-dcast(all_models_outliers,knapper+assessment ~ score_component,value.var = "Score_predicted")

#####control cases not scored by dietz -for Erin data set, not for manuscript
