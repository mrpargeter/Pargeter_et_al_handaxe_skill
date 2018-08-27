##model building with added control subjects

#scatterplot matrix
#Multicollinearity does not reduce the predictive power or reliability of the 
#model as a whole, at least within the sample data set; it only affects calculations 
#regarding individual predictors.

library(car)
scatterplotMatrix(~Score+Dim.1+Dim.2, data=all_experiment_data_controlpca_scaled,
                  main="Scaled data structure matrix")

scatterplotMatrix(~Score+profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv, data=all_experiment_data_controlpca_scaled,
                  main="Scaled data structure matrix")

scatterplotMatrix(~Score+percent_unflaked_area+delta_weight+
                    +percent_bifacially_flaked+flake_scar_density, data=all_experiment_data_controlpca_scaled,
                  main="Scaled data structure matrix")

complete_matrix<-scatterplotMatrix(~Score+Dim.1+Dim.2+profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv+percent_unflaked_area+delta_weight+
                                     percent_bifacially_flaked+flake_scar_density,
                                   data=all_experiment_data_controlpca_scaled, main="Scaled data structure matrix")

#########random forest
library(randomForest)

#Take a random sample of size N with replacement from the data (bootstrap sample).
#Take a random sample without replacement of the predictors.
#Construct a split by using predictors selected in Step 2.
#Repeat Steps 2 and 3 for each subsequent split until the tree is as large as desired. Do not prune. 
#Each tree is produced from a random sample of cases, and at each split a random sample of predictors.
#Drop the out-of-bag data down the tree. Store the class assigned to each observation along 
#with each observation's predictor values.

#random forests improves single trees by smoothing their decision thresholds

#Works:
#Variance reduction: the trees are more independent because of the combination of bootstrap samples and random draws of predictors.
#Bias reduction: local feature predictors will have the opportunity to define a split.
#also allows us to estimate relationships not possible with linear models

#create test and training data

library(dplyr)
set.seed(415) #important to set seed so samples are extracted the same way
train<-sample_frac(all_experiment_data_controlpca_scaled,.632, replace=F)
test<-sample_frac(all_experiment_data_controlpca_scaled,.368, replace=F)

hist(train$Score)
hist(test$Score)

qqplot(train$Score,test$Score)
abline(a = 0, b = 1, lty = 3)

wilcox.test(train$Score,test$Score)

###########Model 1
library(randomForest)
set.seed(415)
fit <- randomForest(Score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                    +delta_weight+Dim.1+Dim.2
                    +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                    data=train, 
                    importance=TRUE,
                    nodesize=7,    #node size set to c. 1 % of total data frame, conservative
                    mtry=6,          #mtry, # of params to test at each node, from test above we see error minimized at mtry 7
                    ntree=100,
                    keep.inbag = TRUE)

print(fit)
plot(fit)

#variable importance plot

names<-c("Profile Asymetry","Plan Asymetry","Delta Profile Thickness",
         "Delta Weight","Shape PC1","Shape PC2","Percent Unflaked Area","Percent Bifacially Flaked","Flake Scar Density")
var_importance_data<-data.frame(importance(fit,type = 1)) #permutation importance, variable is assigned values by random permutation by how much will the MSE increase.
var_importance_data$predictor<-names
var_importance_data$predictor<- factor(var_importance_data$predictor, levels=rev(c("Percent Bifacially Flaked","Percent Unflaked Area","Shape PC1","Shape PC2",
                                                                                   "Flake Scar Density","Delta Weight","Plan Asymetry",
                                                                                   "Delta Profile Thickness","Profile Asymetry")))
library(ggplot2)

variable_importance_plot<-
  ggplot(var_importance_data,aes(X.IncMSE,predictor)) +
  geom_point(size=4.5,aes(color=predictor)) +
  xlab('Percentage decrease in mean squared error') + ylab('Predictor') +
  scale_colour_manual(values=c('red','red','red','red','black','black','black','black','black'),guide = FALSE)+
  theme(text = element_text(size=20))

#comparison of r2 values for train and test data

actual_train <- train$Score
predicted_train <- unname(predict(fit, train))
R2_train <- 1 - (sum((actual_train-predicted_train)^2)/sum((actual_train-mean(actual_train))^2))

actual_test <- test$Score
predicted_test <- unname(predict(fit, test))
R2_test<- 1 - (sum((actual_test-predicted_test)^2)/sum((actual_test-mean(actual_test))^2))

R2_train
R2_test

#model OOB erro
matplot(1:100 , fit$mse, pch=19 , col="red",type="b",ylab="Mean Squared Error",xlab="Number of Trees")
legend("center",legend=c("Out of Bag Error"),pch=19, col="red")

#test on predictions
Prediction <- predict(fit, test)
submit <- data.frame(Score_observed = test$Score, Score_predicted = Prediction)
fit1_lm<-lm(Score_observed~Score_predicted, data=submit)
plot(submit$Score_observed,submit$Score_predicted)

#predicted R squared
pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  return(pred.r.squared)
}

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}

pred.r.squared <- pred_r_squared(fit1_lm)

#root mean square error, how well random forest was able to predict our test set outcomes.
RMSE <- sqrt(sum((Prediction - test$Score)^2)/length(Prediction))
print(RMSE/mean(test$Score)) 

##first post variable selection
set.seed(415)
fit_2 <- randomForest(Score ~
                        Dim.1+Dim.2+
                        percent_unflaked_area+percent_bifacially_flaked+plan_asymetry_index,
                      data=train,
                      mtry=3,
                      nodesize=7,
                      importance=TRUE, 
                      ntree=100,
                      keep.inbag = TRUE)

actual_train <- train$Score
predicted_train <- unname(predict(fit_2, train))
R2_train <- 1 - (sum((actual_train-predicted_train)^2)/sum((actual_train-mean(actual_train))^2))

actual_test <- test$Score
predicted_test <- unname(predict(fit_2, test))
R2_test<- 1 - (sum((actual_test-predicted_test)^2)/sum((actual_test-mean(actual_test))^2))

R2_train
R2_test

print(fit_2)
plot(fit_2)

#residuals

qqnorm((fit_2$predicted-train$Score)/sd(fit_2$predicted-train$Score))
qqline((fit_2$predicted-train$Score)/sd(fit_2$predicted-train$Score))

fit2_residuals<-fit_2$predicted-train$Score
hist(fit2_residuals)

#####test on predictions, train data, all data#######
Prediction_train <- predict(fit_2, train)
submit_train <- data.frame(Score_observed = train$Score, Score_predicted = Prediction_train, knapper =  train$knapper, assessment = train$assessment)

Prediction_test <- predict(fit_2, test)
submit_test <- data.frame(Score_observed = test$Score, Score_predicted = Prediction_test, knapper =  test$knapper, assessment = test$assessment)

Prediction_all <- predict(fit_2, all_experiment_data_controlpca_scaled)
submit_all <- data.frame(Score_observed = all_experiment_data_controlpca_scaled$Score, Score_predicted = Prediction_all, knapper =  all_experiment_data_controlpca_scaled$knapper, assessment = all_experiment_data_controlpca_scaled$assessment)

#round numbers to match nada
#submit$Score_predicted_rounded<-round_any(submit$Score_predicted,0.5)

prediction_train_lm<-lm(Score_observed~Score_predicted, data=submit_train)
summary(prediction_train_lm)
plot(submit_train$Score_observed,submit_train$Score_predicted)+
  title(main="Predicted vs. Observed Scores Training Data, r2=.86")

prediction_test_lm<-lm(Score_observed~Score_predicted, data=submit_test)
summary(prediction_test_lm)
plot(submit_test$Score_observed,submit_test$Score_predicted)+
  title(main="Predicted vs. Observed Scores Test Data, r2=.62")

prediction_all_lm<-lm(Score_observed~Score_predicted, data=submit_all)
summary(prediction_all_lm)

submit_all_reshaped <- melt(submit_all, id=c("knapper","assessment"))

#plot all data predictions against observations
ggplot(submit_all,size=1)+
  aes(x = Score_observed,y = Score_predicted) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(name="Observed Score")+
  scale_y_continuous(name="Predicted Score")+
  ggtitle("Predicted vs. Observed Scores")+
  geom_smooth(method = "lm", se=F,na.rm = T)+ 
  annotate("text", x = 4, y = 2, label = "R ^ 2 == 0.73",parse = TRUE,color="red",size=8)+ 
  annotate("text", x = 3.85, y = 1.5, label = "Predict_R ^ 2 == 0.62",parse = TRUE,color="red",size=8)+
  theme(text = element_text(size=20))

plot(submit_all$Score_observed,submit_all$Score_predicted)+
  title(main="Predicted vs. Observed Scores Test Data")

#root mean square error, how well random forest was able to predict our test set outcomes.
RMSE <- sqrt(sum((Prediction - test$Score)^2)/length(Prediction))
print(RMSE/mean(test$Score)) 
#We see that the RMSE is only about 17% as large as the mean of our outcome

#######confidence intervals-turning random forests into a statistical proceedure######
library(RFinfer)
rf.preds_cart_control <- rfPredVar(fit_2,rf.data=all_experiment_data_controlpca_scaled,CI=TRUE,tree.type='rf')
rf.preds_cart_control$measured <- all_experiment_data_controlpca_scaled[ ,'Score']
preds_control <- rf.preds_cart_control
preds_control$tree <- 'Random Forest'
preds_control$pred.ij.var <- NULL
preds_control$deviance <- rf.preds_cart_control$pred - rf.preds_cart_control$measured
preds_control$CI_length<-ifelse(rf.preds_cart_control$u.ci > rf.preds_cart_control$l.ci, rf.preds_cart_control$u.ci - rf.preds_cart_control$l.ci, rf.preds_cart_control$l.ci - rf.preds_cart_control$u.ci)

preds_control$assessment <- rep(all_experiment_data_controlpca_scaled$assessment)
preds_control$knapper <- rep(all_experiment_data_controlpca_scaled$knapper)

preds_control <-
  preds_control  %>%
  mutate(score_component = rep("control_model", times=nrow(preds_control)))

ggplot(preds_control,aes(measured,pred)) +
  geom_point(size=0.5) +
  geom_abline(intercept=0,slope=1,lty=2,color='#999999') +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  xlab('Actual') + ylab('Predicted') +
  theme_bw() + 
  theme(legend.position = "none")

###examine confidence interval length between two models
both_models<-bind_rows(preds_rf,preds_control)

both_models_ci <-
  both_models %>%
  group_by(score_component,assessment) %>%
  dplyr::summarize(ave_ci_length = mean(CI_length))

colourCount = length(unique(both_models_ci$assessment))
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(4, "Set1"))

ggplot(both_models_ci, aes(score_component, ave_ci_length)) +
  geom_bar(aes(fill = score_component), position = "dodge", stat="identity")+
  facet_grid(~assessment)+ 
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(name="Average prediction confidence interval length")+
  scale_x_discrete(name="Score Component")+
  guides(fill=FALSE)+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(text = element_text(size=20))

#########compare distance between predicted and actual####
both_models_distance <-
  both_models %>%
  group_by(score_component,assessment, knapper) %>%
  dplyr::summarize(distance = abs(pred-measured))

ggplot(both_models_distance, aes(score_component, distance)) +
  geom_bar(aes(fill = score_component), position = "dodge", stat="identity")+
  facet_grid(~assessment)+ 
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(name="Difference between actual and predicted score")+
  scale_x_discrete(name="Score Component")+
  guides(fill=FALSE)+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(text = element_text(size=20))


