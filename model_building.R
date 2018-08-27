library(ggplot2)
library(scales)
library(car)
library(randomForest)
library(caTools)
library(zoo)
library(reshape)
library(ztable)
library(tables)
library(tidyr)
library(dplyr)
library(knitr)
library(kableExtra)
library(caTools)
library(RColorBrewer)
library("PerformanceAnalytics")

###Basic plots-variables against assessment

#plot subjects by assessment number
handaxe_postpca_scaled$assessment<-as.factor(handaxe_postpca_scaled$assessment)
summary(handaxe_postpca_scaled$assessment)

assessment_plot<-ggplot(handaxe_postpca_scaled, aes(x=assessment))+
  geom_bar(aes(y = (..count..)/sum(..count..)), width=0.7, fill="steelblue")+
  theme_minimal()+
  scale_y_continuous(labels=percent)

#scatterplot matrix
#Multicollinearity does not reduce the predictive power or reliability of the 
#model as a whole, at least within the sample data set; it only affects calculations 
#regarding individual predictors.
scatterplotMatrix(~Score+Dim.1+Dim.2, data=handaxe_postpca_scaled,
                   main="Scaled data structure matrix")

scatterplotMatrix(~Score+profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv, data=handaxe_postpca_scaled,
                  main="Scaled data structure matrix")

scatterplotMatrix(~Score+percent_unflaked_area+delta_weight+
                    +percent_bifacially_flaked+flake_scar_density, data=handaxe_postpca_scaled,
                  main="Scaled data structure matrix")

complete_matrix<-scatterplotMatrix(~Score+Dim.1+Dim.2+profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv+percent_unflaked_area+delta_weight+
                    percent_bifacially_flaked+flake_scar_density,
                  data=handaxe_postpca_scaled, main="Scaled data structure matrix")

####outlier detection using cook's distance
score_model <- lm(Score ~ Dim.1+Dim.2+profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv+percent_unflaked_area+delta_weight+
                    percent_bifacially_flaked+flake_scar_density, data=handaxe_postpca_scaled)
cooksd <- cooks.distance(score_model)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>2*mean(cooksd, na.rm=T),names(cooksd),""), col="red")

####subset data to remove overly influential handaxes (five)
handaxe_postpca_scaled$cooks_distance<-abs(cooksd)

#NB: must follow through with this dataset
handaxe_postpca_scaled_minus_outlier<-subset(handaxe_postpca_scaled,cooksd < 0.04)
outlier_handaxes<-subset(handaxe_postpca_scaled,cooksd > 0.04)
write.csv(outlier_handaxes,"outlier_handaxes.csv")

scatterplotMatrix(~Score+Dim.1+Dim.2+profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv+percent_unflaked_area+delta_weight+
   percent_bifacially_flaked+flake_scar_density,
   data=handaxe_postpca_scaled_minus_outlier, main="Scaled data structure matrix")

####random forest

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
set.seed(425) 
sample = sample.split(handaxe_postpca_scaled_minus_outlier$knapper, SplitRatio = .70)
train = subset(handaxe_postpca_scaled_minus_outlier, sample == TRUE)
test  = subset(handaxe_postpca_scaled_minus_outlier, sample == FALSE)

hist(train$Score)
hist(test$Score)

qqplot(train$Score,test$Score,
       xlab="Training data score", ylab="Test data score",
       main="Comparison of test and training data score distributions")

wilcox.test(train$Score,test$Score)

####model 1_all variables

##Method to check for optimal value of predictors at each node (reduce out of bag errors)
#an internal error estimate of a random forest as it is being constructed.
#each tree is trained on about 2/3 of the total training data.

oob.err<-double(9)
test.err<-double(9)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:9) 
{set.seed(425) 
  rf=randomForest(Score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                  +delta_weight+Dim.1+Dim.2
                  +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                  data = train,
                  mtry=mtry,
                  importance=TRUE,
                  ntree=10000) 
  
  str(handaxe_postpca_scaled_minus_outlier)
  
  oob.err[mtry] = rf$mse[10000] #Error of all Trees fitted
  
  pred<-predict(rf,test) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test, mean( (Score - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ")
  
}

test.err
oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("center",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

#Deeper trees reduces the bias (nodesize); more trees reduces the variance (ntrees).
set.seed(425)
fit <- randomForest(Score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                      +delta_weight+Dim.1+Dim.2
                      +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                    data=train, 
                    importance=TRUE,  
                    mtry=2,          #mtry, # of params to test at each node, from test above we see error minimized at mtry 3
                    ntree=10000,
                    keep.inbag = TRUE)
plot(fit)
print(fit)
#model OOB erro
matplot(1:10000 , fit$mse, pch=19 , col="red",type="b",ylab="Mean Squared Error",xlab="Number of Trees")
legend("center",legend=c("Out of Bag Error"),pch=19, col="red")

#variable importance plot

names<-c("Profile Asymetry","Plan Asymetry","Delta Profile Thickness",
         "Delta Weight","Shape PC1","Shape PC2","Percent Unflaked Area","Percent Bifacially Flaked","Flake Scar Density")
all_var_importance_data<-data.frame(importance(fit,type = 1)) #permutation importance, variable is assigned values by random permutation by how much will the MSE increase.
all_var_importance_data$predictor<-names
all_var_importance_data$predictor<- factor(all_var_importance_data$predictor, levels=all_var_importance_data$predictor[order(all_var_importance_data$X.IncMSE)], ordered=TRUE)                                                                                             
colnames(all_var_importance_data)[1]<-"All_X.IncMSE"     
                                                                                                                                                         
variable_importance_plot<-
  ggplot(all_var_importance_data,aes(All_X.IncMSE,predictor)) +
  geom_point(size=4.5,aes(color=predictor)) +
  xlab('Percentage decrease in mean squared error') + ylab('Predictor') +
  scale_colour_manual(values=c('black','black','black','black','black','black','black','black','black'),guide = FALSE)+
  theme(text = element_text(size=20))
        
#comparison of r2 values for train and test data

actual_train <- train$Score
predicted_train <- unname(predict(fit))
R2_train <- 1 - (sum((actual_train-predicted_train)^2)/sum((actual_train-mean(actual_train))^2))

actual_test <- test$Score
predicted_test <- unname(predict(fit, test))
R2_test<- 1 - (sum((actual_test-predicted_test)^2)/sum((actual_test-mean(actual_test))^2))

R2_train
R2_test

#test on predictions
Prediction_train <- predict(fit)
submit_train <- data.frame(Score_observed = train$Score, Score_predicted = Prediction_train, knapper =  train$knapper, assessment = train$assessment)

fit1_lm<-lm(Score_observed~Score_predicted, data=submit_train)

Prediction_test <- predict(fit, test)
submit_test <- data.frame(Score_observed = test$Score, Score_predicted = Prediction_test, knapper =  test$knapper, assessment = test$assessment)

submit_all <- rbind(submit_train,submit_test)

error <- submit_all$Score_observed - submit_all$Score_predicted

#Mean Absolute Error, how well random forest was able to predict our test set outcomes.0.36
rmse <- function(error)
{
  sqrt(mean(error^2))
}

rmse(error)

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

mae(error) 

#residuals

qqnorm((fit$predicted-train$Score)/sd(fit$predicted-train$Score))
qqline((fit$predicted-train$Score)/sd(fit$predicted-train$Score))

fit_residuals<-fit$predicted-train$Score
hist(fit_residuals)

############first post variable selection

####tune parameters

##Method to check for optimal value of predictors at each node (reduce out of bag errors)
#an internal error estimate of a random forest as it is being constructed.
#each tree is trained on about 2/3 of the total training data.

oob.err<-double(6)
test.err<-double(6)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:6) 
{set.seed(425) 
  rf=randomForest(Score ~ Dim.2+
                        percent_unflaked_area+percent_bifacially_flaked+plan_asymetry_index+
                        delta_weight+flake_scar_density,
                  data = train,
                  mtry=mtry,
                  importance=TRUE,
                  ntree=10000) 
  
  str(handaxe_postpca_scaled_minus_outlier)
  
  oob.err[mtry] = rf$mse[10000] #Error of all Trees fitted
  
  pred<-predict(rf,test) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test, mean( (Score - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ")
  
}

test.err
oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("center",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

###then write the model
set.seed(425)
fit_2 <- randomForest(Score ~
                        Dim.2+
                        percent_unflaked_area+percent_bifacially_flaked+plan_asymetry_index+
                        delta_weight+flake_scar_density,
                      data=train,
                      mtry=2,
                      importance=TRUE, 
                      ntree=10000,
                      keep.inbag = TRUE)

print(fit_2)
plot(fit_2)

actual_train <- train$Score
predicted_train <- unname(predict(fit_2))
R2_train <- 1 - (sum((actual_train-predicted_train)^2)/sum((actual_train-mean(actual_train))^2))

actual_test <- test$Score
predicted_test <- unname(predict(fit_2, test))
R2_test<- 1 - (sum((actual_test-predicted_test)^2)/sum((actual_test-mean(actual_test))^2))

R2_train
R2_test

#residuals
qqnorm((fit_2$predicted-train$Score)/sd(fit_2$predicted-train$Score))
qqline((fit_2$predicted-train$Score)/sd(fit_2$predicted-train$Score))

fit2_residuals<-fit_2$predicted-train$Score
hist(fit2_residuals)

#Error rates
Prediction_train <- predict(fit_2)
submit_train <- data.frame(Score_observed = train$Score, Score_predicted = Prediction_train, knapper =  train$knapper, assessment = train$assessment)

Prediction_test <- predict(fit_2, test)
submit_test <- data.frame(Score_observed = test$Score, Score_predicted = Prediction_test, knapper =  test$knapper, assessment = test$assessment)

submit_all_fit_2 <- rbind(submit_train,submit_test)

error_fit_2 <- submit_all_fit_2$Score_observed - submit_all_fit_2$Score_predicted

rmse(error_fit_2)
mae(error_fit_2)

#plot all data predictions against observations
ggplot(submit_all_fit_2,size=1)+
  aes(x = Score_observed,y = Score_predicted) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(name="Observed Score")+
  scale_y_continuous(name="Predicted Score")+
  ggtitle("Predicted vs. Observed Scores")+
  geom_smooth(method = "lm", se=T,na.rm = T)+ 
  #annotate("text", x = 4, y = 2, label = "Model_R ^ 2 == 0.89",parse = TRUE,color="red",size=8)+ 
  #annotate("text", x = 3.85, y = 1.5, label = "Prediction_R ^ 2 == 0.69",parse = TRUE,color="red",size=8)+
  theme(text = element_text(size=20))

####check overall skill to assessment plot
ggplot(data = submit_all_fit_2)+
  aes(x = assessment,y = Score_predicted) +
  geom_point(alpha = 0.6) +
  scale_y_continuous(name="Predicted Score")+
  scale_x_discrete(name="Assessment")+
  geom_smooth(method = "loess",data = submit_all_fit_2  %>%
                mutate(assessment = as.numeric(assessment)), se=T,na.rm = T,span = 0.5)+
  theme(text = element_text(size=20)) 

#####classify outlier flakes-for Erin data set, not for manuscript
Prediction_outliers <- predict(fit_2, outlier_handaxes)
submit_all_fit_2_outliers <- data.frame(Score_observed = outlier_handaxes$Score, Score_predicted = Prediction_outliers, knapper =  outlier_handaxes$knapper, assessment = outlier_handaxes$assessment)

#####classify control handaxes not flakes-for Erin data set, not for manuscript
#control data created in control_handaxe_prep
Prediction_control_not_scored <- predict(fit_2, control_not_scored_handaxe_postpca_scaled)
submit_all_fit_2_control_not_scored <- data.frame(Score_observed = control_not_scored_handaxe_postpca_scaled$Score, Score_predicted = Prediction_control_not_scored, knapper =  control_not_scored_handaxe_postpca_scaled$knapper, assessment = control_not_scored_handaxe_postpca_scaled$assessment)

##########confidence intervals-turning random forests into a statistical proceedure###########

#Extract the prediction variances for the training data along with their 95% confidence intervals.
#use jackknife to omit one observation and recompute the estimate using the remaining observations
#after down-weighting each observation by an infinitesimal amount (infintisemal jacknife)
#estimating the variance of a statistic by using the variability between resamples rather 
#than using statistical distributions
#hus taking a step towards making random forests tools for statistical inference instead 
#of just black-box predictive algorithms.
#can then ask: Are there some points at which the random forest is more stable than others?
#This is essentially a Monte Carlo estimate of Equation with a bias correction subtracted off. 
#These estimates are asymptotically normal given a few key conditions, one of which is 
#that the underlying trees are honest 
#use two different tree types: conditional inference (honest tree type) or CART trees
#must have keepinbag=T  for RF model, so same resamples will be used when training 
#the forest using either type of trees, facilitating a true comparison

## function adapted from Stefan Wager
# https://github.com/swager/randomForestCI

rfPredVar <- function(random.forest,rf.data,pred.data=rf.data,CI=FALSE,tree.type='rf',prog.bar=FALSE) {
  
  if (is.null(random.forest$inbag)) {
    stop("Random forest must be trained with keep.inbag = TRUE")
  }
  if (length(unique(colSums(random.forest$inbag))) > 1) {
    stop("The keep.inbag field must store the number of times each observation was used
         \nMake sure the latest version of the randomForest package is installed from CRAN")
  }
  N.weights <- random.forest$inbag
  
  B <- ncol(N.weights)
  n <- nrow(N.weights)
  s <- sum(N.weights[ ,1])
  N <- Matrix::Matrix(N.weights,sparse=TRUE)
  N.avg <- Matrix::Matrix(Matrix::rowMeans(N),nrow(N),1)
  
  if (tree.type=='rf') pred <- predict(random.forest,newdata=pred.data,predict.all=TRUE)$individual
  if (tree.type=='ci') pred <- CB_cforest(rf=random.forest,pb=prog.bar,rf.d=rf.data,p.d=pred.data)$preds
  
  agg.preds <- rowMeans(pred)
  pred.centered <- pred - agg.preds
  pred.centered.sums <- Matrix::Matrix(rowSums(pred.centered), 1, nrow(pred.centered))
  
  C = N %*% t(pred.centered) - N.avg %*% pred.centered.sums
  raw.IJ <- Matrix::colSums(C^2) / B^2
  
  N.var <-  mean(Matrix::rowMeans(N^2) - Matrix::rowMeans(N)^2)
  boot.var <-  rowSums(pred.centered^2) / B
  bias.correction <-  n * N.var * boot.var / B
  pred.ij.var <- raw.IJ - bias.correction
  
  out <- data.frame('pred' = agg.preds,pred.ij.var)
  if (CI) {
    out <- data.frame(out,
                      'l.ci' = out$pred - (out$pred.ij.var * qnorm(0.975,lower.tail=T)),
                      'u.ci' = out$pred + (out$pred.ij.var * qnorm(0.975,lower.tail=T)))
  }
  return(out)
  }

rf.preds_cart <- rfPredVar(fit_2,rf.data=handaxe_postpca_scaled_minus_outlier,CI=TRUE,tree.type='rf')
preds <- rf.preds_cart
preds$tree <- 'Random Forest'
preds$pred.ij.var <- NULL
preds$measured <- handaxe_postpca_scaled_minus_outlier[ ,'Score']
preds$deviance <- preds$pred - preds$measured
preds$CI_length<-ifelse(preds$u.ci > preds$l.ci, preds$u.ci - preds$l.ci, preds$l.ci - preds$u.ci)

#add data strings to plot relationship with score
preds$bifacial_extent <- rep(handaxe_postpca_scaled_minus_outlier$percent_bifacially_flaked)
preds$flake_scar <- rep(handaxe_postpca_scaled_minus_outlier$flake_scar_density)
preds$perc_unflaked <- rep(handaxe_postpca_scaled_minus_outlier$percent_unflaked_area)
preds$delta_weight <- rep(handaxe_postpca_scaled_minus_outlier$delta_weight)
preds$dim_2 <- rep(handaxe_postpca_scaled_minus_outlier$Dim.2)
preds$plan_asymmetry <- rep(handaxe_postpca_scaled_minus_outlier$plan_asymetry_index)
preds$profile_asymetry_index <- rep(handaxe_postpca_scaled_minus_outlier$profile_asymetry_index)
preds$assessment <- rep(handaxe_postpca_scaled_minus_outlier$assessment)
preds$knapper <- rep(handaxe_postpca_scaled_minus_outlier$knapper)

ggplot(preds,aes(measured,pred)) +
  geom_point(size=0.5) +
  #geom_abline(intercept=0,slope=1,lty=2,color='#999999') +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  geom_smooth(method = "lm", se=T,na.rm = T)+
  xlab('Observed Score') + ylab('Predicted Score') +
  theme_bw() + 
  theme(legend.position = "none")

#root mean squared error: shows accurate predictions have lowest uncertainty: is what we want to see
tapply(preds$deviance,preds$tree,function(x) sqrt(sum(x^2)/length(x)))

ggplot(preds,aes(deviance,CI_length,color=tree)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Confidence interval length")+
  xlab("Deviance of predicted from observed values")

#plot variables in relation to predicted and observed scores

preds_rf<-subset(preds, tree %in% "Random Forest")

plot_predscores_vars_function<-function(data,x_var,x_axis_label,y_axis_label){
  plot<-ggplot(data=data,aes(x=x_var,y=pred)) +
    geom_point(size=0.5) +
    geom_smooth() +
    #geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
    xlab(x_axis_label) + ylab('Predicted Score')
  return(plot)
}

plot_predscores_vars_function_reverse<-function(data,x_var,x_axis_label,y_axis_label){
  plot<-ggplot(data=data,aes(x=x_var,y=pred)) +
    geom_point(size=0.5) +
    geom_smooth() +
    #geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
    xlab(x_axis_label) + ylab('Predicted Score')+
    scale_x_reverse()
  return(plot)
}

dim.2.plot<-plot_predscores_vars_function(preds_rf,preds_rf$dim_2,'Shape PCA dimension 2')
bifacial.plot<-plot_predscores_vars_function(preds_rf,preds_rf$bifacial_extent,'Bifacial extent')
scar.plot<-plot_predscores_vars_function(preds_rf,preds_rf$flake_scar,'Flake scar density')
unflaked.area.plot<-plot_predscores_vars_function_reverse(preds_rf,preds_rf$perc_unflaked,'Unflaked area')
plan.plot<-plot_predscores_vars_function_reverse(preds_rf,preds_rf$plan_asymmetry,'Plan asymmetry index')
weight.plot<-plot_predscores_vars_function_reverse(preds_rf,preds_rf$delta_weight,'Delta weight')

#multiplot
source("http://peterhaschke.com/Code/multiplot.R") #multiplot source code

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(dim.2.plot,bifacial.plot,scar.plot,unflaked.area.plot,weight.plot,plan.plot, cols = 2)

#####examine confidence interval lengths and deviance

detach(package:plyr)
preds_rf_ci <-
  preds_rf %>%
  group_by(assessment) %>%
  mutate(ave_ci_length = mean(CI_length))

#create color palette
colourCount = length(unique(preds_rf_ci$assessment))
getPalette = colorRampPalette(brewer.pal(4, "Set1"))

preds_rf_ci$assessment <- factor(preds_rf_ci$assessment, c("1", "2", "3", "4","5","6","7","8","9"))

ggplot(preds_rf_ci, aes(assessment, deviance, fill=assessment)) +
  geom_bar(position = "dodge", stat="identity")+ 
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(name="Deviance predicted from observed score")+
  scale_x_discrete(name="Assessment")+
  guides(fill=FALSE)+
  theme(text = element_text(size=20))+ 
  geom_bar(position = 'dodge', stat = 'summary', fun.y = 'mean') +
  geom_point(aes(x = assessment), size=2,shape = 21, position = position_dodge(width = 1))+ 
  geom_hline(color="red",yintercept = .50)+ 
  geom_hline(color="red",yintercept = -.50)

###################With expert handaxes included in original model######################
handaxe_postpca_scaled_reduced<-handaxe_postpca_scaled[ , -which(names(handaxe_postpca_scaled) %in% c("Core.Number", "delta_profile_thickness_cv"))]

novice_expert_combined<-merge(expert_complete_experiment_data_reduced_scaled, handaxe_postpca_scaled_reduced, 
                              by=c("knapper", "assessment", "experiment","condition",
                                   "Score","Training.Hours","delta_weight",
                                   "Dim.1", "Dim.2",
                                   "profile_asymetry_index","plan_asymetry_index",
                                   "percent_unflaked_area","percent_bifacially_flaked","flake_scar_density"), all = T)

####outlier detection using cook's distance
score_model_expert <- lm(Score ~ Dim.1+Dim.2+profile_asymetry_index+
                           plan_asymetry_index+
                           percent_unflaked_area+delta_weight+
                           percent_bifacially_flaked+flake_scar_density, 
                           data=novice_expert_combined)

cooksd_expert <- cooks.distance(score_model_expert)

plot(cooksd_expert, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd_expert)+1, y=cooksd_expert, labels=ifelse(cooksd_expert>2*mean(cooksd_expert, na.rm=T),names(cooksd_expert),""), col="red")

####subset data to remove overly influential handaxes
novice_expert_combined$cooks_distance<-abs(cooksd_expert)

#NB: must follow through with this dataset
novice_expert_handaxe_postpca_scaled_minus_outlier<-subset(novice_expert_combined,cooksd < 0.04)

#create test and training data
#important to set seed so samples are extracted the same way
require(caTools)
set.seed(150) 
sample_new = sample.split(novice_expert_handaxe_postpca_scaled_minus_outlier, SplitRatio = .7)
train_new = subset(novice_expert_handaxe_postpca_scaled_minus_outlier, sample_new == TRUE)
test_new  = subset(novice_expert_handaxe_postpca_scaled_minus_outlier, sample_new == FALSE)

hist(train_new$Score)
hist(test_new$Score)

qqplot(train_new$Score,test_new$Score,
       xlab="Training data score", ylab="Test data score",
       main="Comparison of test and training data score distributions")

wilcox.test(train_new$Score,test_new$Score)

#then write model (same structure as fit_2)

set.seed(150)
fit_4 <- randomForest(Score ~
                        plan_asymetry_index
                      +flake_scar_density+Dim.2   #minus Dim.1
                      +percent_unflaked_area+percent_bifacially_flaked,
                      data=train_new,
                      mtry=2,
                      importance=TRUE, 
                      ntree=10000,
                      keep.inbag = TRUE)

print(fit_4)
plot(fit_4)

varImpPlot(fit_4)

#residuals

qqnorm((fit_4$predicted-train_new$Score)/sd(fit_4$predicted-train_new$Score))
qqline((fit_4$predicted-train_new$Score)/sd(fit_4$predicted-train_new$Score))

fit_4_residuals<-fit_4$predicted-train_new$Score
hist(fit_4_residuals)

#plot all data predictions against observations

Prediction_all_expert <- predict(fit_4, novice_expert_combined)
submit_all_expert <- data.frame(Score_observed_expert = novice_expert_combined$Score, Score_predicted_expert = Prediction_all_expert, knapper =  novice_expert_combined$knapper, assessment = novice_expert_combined$assessment)

ggplot(submit_all_expert,size=1)+
  aes(x = Score_observed_expert,y = Score_predicted_expert) +
  geom_point(alpha = 0.6,aes(colour = cut(Score_observed_expert, c(1, 4.5, 4.6, 5)))) +
  scale_x_continuous(name="Observed Score")+
  scale_y_continuous(name="Predicted Score")+
  ggtitle("Predicted vs. Observed Scores")+
  geom_smooth(method = "lm", se=T,na.rm = T)+
  theme(text = element_text(size=20))+
  scale_color_manual(name = "",
                     values = c("(1,4.5]" = "black",
                                "(4.6,5]" = "red"))+ 
  theme(legend.position="none")

write.csv(submit_all_expert,"submit_all_expert.csv")

###################With expert handaxes predicted from previous model######################

test_expert<-expert_complete_experiment_data_reduced_scaled

Prediction <- predict(fit_2, test_expert)
submit <- data.frame(Score_observed = test_expert$Score, Score_predicted = Prediction, knapper =  test_expert$knapper, assessment = test_expert$assessment)

plot(submit$Score_observed,submit$Score_predicted)

#############extract individuals from the model################

###test on predictions NB: keeping seven outliers in for this part-effects individual vrb
handaxe_postpca_scaled_minus_control<-subset(handaxe_postpca_scaled, condition %in% "main")

Prediction_total <- predict(fit_2, handaxe_postpca_scaled_minus_control) 
submit_total <- data.frame(Score_observed = handaxe_postpca_scaled_minus_control$Score, Score_predicted = Prediction_total, knapper =  handaxe_postpca_scaled_minus_control$knapper, assessment = handaxe_postpca_scaled_minus_control$assessment, hours=handaxe_postpca_scaled_minus_control$Training.Hours)

####start filling missing data 
submit_total$hours <-car::recode(submit_total$hours, "86=93; 85=93; 74.5=71;45=47") #some later evals were based on extra days training

#create data frame representing all assessments, knappers, hours
assessment <- rep(c(1:9), times=17)
knapper <- rep(c(1,2,3,5,6,7,8,9,10,11,13,14,15,16,17,19,21), times=9)
hours <- rep(c(0,12,24,36,47,59,71,82,93), times=17)

df <- data.frame(assessment,knapper,hours)

#merge with original data set

complete_data <-merge(submit_total, df, by=c("assessment", "knapper","hours"), all = T)

##interpolate data MICE
library(mice)
tempData <- mice(complete_data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

tempData$imp$Score_predicted

stripplot(tempData, pch = 20, cex = 1.2)
completedData <- complete(tempData,5)

names(completedData)[names(completedData) == 'Score_predicted'] <- 'Score_predicted_imputed'
names(completedData)[names(completedData) == 'Score_observed'] <- 'Score_observed_imputed'

imputed_data <-merge(completedData, complete_data, by=c("assessment", "knapper","hours"), all = T)

imputed_data$predicted_imputed <- ifelse(imputed_data$Score_predicted %in% NA, imputed_data$Score_predicted_imputed,NA)
imputed_data$Score_observed_imputed<-NULL
imputed_data$Score_predicted_imputed<-NULL

##interpolate missing data-global interpolation
complete_data$predicted_score_interpolate_spline=na.spline(complete_data$Score_predicted)
complete_data$predicted_score_interpolate_linear=na.approx(complete_data$Score_predicted)

#create columns with only interpolated values
complete_data$spline <- ifelse(complete_data$Score_predicted %in% NA, complete_data$predicted_score_interpolate_spline,NA)
complete_data$linear <- ifelse(complete_data$Score_predicted %in% NA, complete_data$predicted_score_interpolate_linear,NA)

complete_data$predicted_score_interpolate_spline<-NULL
complete_data$predicted_score_interpolate_linear<-NULL

#reshape data to have predicted and observed scores in same column
mdata <- melt(complete_data, id=c("knapper","assessment","hours"))

mdata <- mdata %>%
  mutate(sqrt_hours = sqrt(hours),
         value = as.numeric(as.character(value))) 

#plot to check if linear relationship holds
mdata_subset<-filter(mdata, variable!="Score_observed", variable!="linear")

mdata_subset_geom_smooth<-
  mdata_subset %>%
  mutate(score=value,
         variable=rep("all",times=nrow(mdata_subset)),
         assessment = as.numeric(assessment))

ggplot(data = mdata_subset,size=1)+
  aes_string(x = "assessment",y = "value",colour = "variable") +
  geom_point(alpha = 0.6) +
  scale_color_manual(labels = c("", "Predicted Score","Interpolated Score"),values=c("Red", "Black","Blue")) +
  scale_y_continuous(name="Score")+
  scale_x_discrete(name="Assessment")+
  geom_smooth(method = "loess", data = mdata_subset_geom_smooth, se=T,na.rm = T,span=0.9)+ 
  labs(color='Score Component')+
  theme(text = element_text(size=20)) + 
  theme(legend.position="none")

#######calculate assessment score averages from interpolated and predicted values
##this task draws data from the section below and the individual slopes section

###check assessment score shapes (go with median)

one_test<-subset(mdata_subset,assessment=="1")
two_test<-subset(mdata_subset,assessment=="2")
three_test<-subset(mdata_subset,assessment=="3")
four_test<-subset(mdata_subset,assessment=="4")
five_test<-subset(mdata_subset,assessment=="5")
six_test<-subset(mdata_subset,assessment=="6")
seven_test<-subset(mdata_subset,assessment=="7")
eight_test<-subset(mdata_subset,assessment=="8")
nine_test<-subset(mdata_subset,assessment=="9")

individuals_relative_average<-
  na.omit(mdata_subset) %>%
  group_by(assessment) %>%
  mutate(score_median=median(value)) %>%
  ungroup() %>%
  mutate(score_relative_median=value-score_median)

write.csv(individuals_relative_average,"individuals_relative_average.csv")

#need to extract 1,5,6,7,9 for Erin's scoring (some individs have assessment 5,6,7 as one right before scan2)
individuals_relative_average_1<-subset(individuals_relative_average, assessment=="1")
individuals_relative_average_5<-subset(individuals_relative_average, assessment=="5")
individuals_relative_average_6<-subset(individuals_relative_average, assessment=="6")
individuals_relative_average_7<-subset(individuals_relative_average, assessment=="7")
individuals_relative_average_9<-subset(individuals_relative_average, assessment=="9")

individuals_relative_average_1_5_6_7_9<-rbind(individuals_relative_average_1,individuals_relative_average_5,individuals_relative_average_6,individuals_relative_average_7,individuals_relative_average_9)
individuals_relative_average_1_5_6_7_9$assessment_spread<-individuals_relative_average_1_5_6_7_9$assessment

#change value to score_relative_median (or value, depending on direct of change) and rename to score_relative_median_one for values relative to median
#also rerun code line above to remake 1_5_9 dataset
individuals_relative_average_1_5_6_7_9_spread<-
  individuals_relative_average_1_5_6_7_9 %>%
  select (c(knapper, assessment,assessment_spread,value)) %>%
  tidyr::spread(assessment_spread, value)%>% 
  dplyr::rename(score_relative_median_one = `1`,
                score_relative_median_five = `5`,
                score_relative_median_six = `6`,
                score_relative_median_seven = `7`,
                score_relative_median_nine = `9`)

#first from last scores
individuals_first_last<-
  subset(individuals_relative_average,assessment %in% c("1","9")) %>%
  select (c(knapper, assessment,value))

individuals_first_last<-tidyr::spread(individuals_first_last, assessment, value)
individuals_first_last$difference_first_last<-individuals_first_last$`9`-individuals_first_last$`1`

individuals_assessment_scores<-merge(individuals_relative_average_1_5_9,individuals_first_last,by="knapper")
individuals_assessment_scores$`1`<-NULL
individuals_assessment_scores$`9`<-NULL

###plot 

mdata_new_subset<-filter(mdata, variable %in% c("Score_predicted", "spline"))

createComparisonPlot_flakes <-
  function(subset_var_value) {
    comparison_plot <- ggplot(subset(mdata_new_subset, knapper %in% subset_var_value),
                        aes(as.numeric(assessment), value))+
      scale_colour_manual(values = c("red", "blue"))+
      geom_point(aes(col = variable), alpha = 0.6)  +
      scale_y_continuous(name="Score",limits = c(0, 5))+
      scale_x_continuous(name="Assessment",limits = c(1, 9),breaks=c(1,2,3,4,5,6,7,8,9))+
      ggtitle(subset_var_value)+
      geom_smooth(se=T)+ 
      theme(legend.position = "none")
    return(comparison_plot)
  }

knapper_1<-createComparisonPlot_flakes("1")
knapper_2<-createComparisonPlot_flakes(  "2")
knapper_3<-createComparisonPlot_flakes(  "3")
knapper_5<-createComparisonPlot_flakes(  "5")
knapper_6<-createComparisonPlot_flakes(  "6")
knapper_7<-createComparisonPlot_flakes(  "7")
knapper_8<-createComparisonPlot_flakes(  "8")
knapper_9<-createComparisonPlot_flakes(  "9")
knapper_10<-createComparisonPlot_flakes(  "10")
knapper_11<-createComparisonPlot_flakes(  "11")
knapper_13<-createComparisonPlot_flakes(  "13")
knapper_14<-createComparisonPlot_flakes(  "14")
knapper_15<-createComparisonPlot_flakes(  "15")
knapper_16<-createComparisonPlot_flakes(  "16")
knapper_17<-createComparisonPlot_flakes(  "17")
knapper_19<-createComparisonPlot_flakes(  "19")
knapper_21<-createComparisonPlot_flakes(  "21")

library(ggplot2)
library(gridExtra)
library(grid)

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

all_data_score_assessment<-multiplot(knapper_1,knapper_2,knapper_3,knapper_5,
                           knapper_6,knapper_7,knapper_8,knapper_9,
                           knapper_10,knapper_11,knapper_13,knapper_14,
                           knapper_15,knapper_16,knapper_17,knapper_19,
                           knapper_21,cols=4)

##interpolate missing data-individual interpolation
complete_data_indiv <-merge(submit_total, df, by=c("assessment", "knapper","hours"), all = T)
complete_data_indiv$hours<-as.numeric(complete_data_indiv$hours)
complete_data_indiv$sqrt_hours<-sqrt(complete_data_indiv$hours)

str(complete_data_indiv)

individual_interpolation = function(filter_var) {
  individual_interpolation_data = complete_data_indiv %>%
    dplyr::filter(knapper==filter_var) %>%
    mutate(predicted_score_interpolate_spline = na.spline(Score_predicted),
           predicted_score_interpolate_linear=na.approx(Score_predicted, rule=2),
           spline=ifelse(Score_predicted %in% NA, predicted_score_interpolate_spline,NA),
           linear=ifelse(Score_predicted %in% NA, predicted_score_interpolate_linear,NA)) %>% 
    dplyr::select(-c(predicted_score_interpolate_spline, predicted_score_interpolate_linear)) %>%
    melt(id=c("knapper","assessment","hours"))  %>%
    dplyr::rename(score=value)
  
  g = ggplot(subset(individual_interpolation_data, variable !="spline"),size=1)+
    aes(x = assessment,y = score) +
    geom_point(aes(colour = variable), alpha = 0.6) +
    scale_color_manual(labels = c("Observed","Predicted", "Interpolated"),values=c("red","orange", "blue")) +
    scale_x_discrete(name="Assessment")+
    scale_y_continuous(name="Score",limits = c(0, 5))+
    labs(color='')+
    ggtitle(filter_var)+
    geom_smooth(method = "loess", data = individual_interpolation_data %>% 
                  filter( variable !="spline") %>%
                  mutate(assessment = as.numeric(assessment)), se=T,na.rm = T)
  
  return(g)
  #(individual_interpolation_data)
} 

individual_interpolation_for_paper = function(filter_var) {
  individual_interpolation_data = complete_data_indiv %>%
    dplyr::filter(knapper==filter_var) %>%
    mutate(predicted_score_interpolate_spline = na.spline(Score_predicted),
           predicted_score_interpolate_linear=na.approx(Score_predicted, rule=2),
           spline=ifelse(Score_predicted %in% NA, predicted_score_interpolate_spline,NA),
           linear=ifelse(Score_predicted %in% NA, predicted_score_interpolate_linear,NA)) %>% 
    dplyr::select(-c(predicted_score_interpolate_spline, predicted_score_interpolate_linear)) %>%
    melt(id=c("knapper","assessment","hours"))  %>%
    dplyr::rename(score=value)
  
  g_2 = ggplot(subset(individual_interpolation_data, variable !="spline" & variable !="Score_observed" & variable !="sqrt_hours" & variable !="prediction_test"),size=1)+
    aes(x = assessment,y = score) +
    geom_point(aes(colour = variable), alpha = 0.6) +
    scale_color_manual(labels = c("Observed","Predicted", "Interpolated"),values=c("red","orange", "blue")) +
    scale_x_discrete(name="Assessment")+
    scale_y_continuous(name="Score",limits = c(0, 5))+
    labs(color='')+
    ggtitle(filter_var)+
    geom_smooth(method = "loess", data = individual_interpolation_data %>% 
                  filter(variable != "Score_observed" & variable !="spline") %>%
                  mutate(assessment = as.numeric(assessment)), se=F,na.rm = T, span=0.5)+
    theme(text = element_text(size=20),legend.position="none")
  
  return(g_2)
  #(individual_interpolation_data)
} 

individual_interp_1<-individual_interpolation("1")
individual_interp_2<-individual_interpolation("2")
individual_interp_3<-individual_interpolation("3")
individual_interp_5<-individual_interpolation("5")
individual_interp_6<-individual_interpolation("6")
individual_interp_7<-individual_interpolation("7")
individual_interp_8<-individual_interpolation("8")
individual_interp_9<-individual_interpolation("9")
individual_interp_10<-individual_interpolation("10")
individual_interp_11<-individual_interpolation("11")
individual_interp_13<-individual_interpolation("13")
individual_interp_14<-individual_interpolation("14")
individual_interp_15<-individual_interpolation("15")
individual_interp_16<-individual_interpolation("16")
individual_interp_17<-individual_interpolation("17")
individual_interp_19<-individual_interpolation("19")
individual_interp_21<-individual_interpolation("21")

individual_interpolation_loess<-grid_arrange_shared_legend(individual_interp_1,individual_interp_2,individual_interp_3,individual_interp_5,
                           individual_interp_6,individual_interp_7,individual_interp_8,
                           individual_interp_9,individual_interp_10,individual_interp_11,
                           individual_interp_14,individual_interp_15,
                           individual_interp_16,individual_interp_17,individual_interp_19,
                           individual_interp_21)

#individual interpolation linear model
exampleFnc <- function(knapper_number,score_to_use = "Predicted Score"){
  
  if (!score_to_use %in% c("Observed Score", "Predicted Score")) 
    stop("Please choose either 'Observed Score' or 'Predicted Score' for score_to_use")
  
  data_kn =  complete_data_indiv %>% subset(knapper %in% knapper_number) %>%
    mutate(score_to_use = score_to_use,
           score = ifelse(score_to_use == "Observed Score",
                          Score_observed,
                          Score_predicted),
           score_to_use = ifelse(is.na(score),
                                 "Interpolated",
                                 score_to_use))
  
  model_kn<-lm(score~sqrt_hours, data=data_kn)#
  data_kn$prediction = predict(model_kn, data_kn)
  
  data_kn$score = ifelse(is.na(data_kn$score),
                         data_kn$prediction,
                         data_kn$score)
  data_kn$score_to_use = factor(data_kn$score_to_use, levels = c(score_to_use, "Interpolated"))
  g = ggplot(data_kn,aes(x = sqrt_hours,y = score,col = score_to_use)) +
    geom_point(alpha = 0.6) +
    scale_color_manual(values=c("black","red")) +
    scale_x_continuous(name="Hours (square root)")+
    scale_y_continuous(name="Score",limits = c(0, 5))+
    labs(color='')+
    ggtitle(knapper_number)+ 
    theme(legend.position = "none")+
  geom_smooth(method = "lm", data = data_kn, se=T,na.rm = T)
  
  slope_df<-data.frame(coef(model_kn)[2])
  slope_df$knapper<-knapper_number
  
  return(g)
  #return(data_kn)
  #return(slope_df)
  
}

linearinterp_1<-exampleFnc("1",score_to_use = "Predicted Score")
linearinterp_1_obs<-exampleFnc("1",score_to_use = "Observed Score")
linearinterp_2<-exampleFnc("2",score_to_use = "Predicted Score")
linearinterp_3<-exampleFnc("3",score_to_use = "Predicted Score")
linearinterp_5<-exampleFnc("5",score_to_use = "Predicted Score")
linearinterp_5_obs<-exampleFnc("5",score_to_use = "Observed Score")
linearinterp_6<-exampleFnc("6",score_to_use = "Predicted Score")
linearinterp_6_obs<-exampleFnc("6",score_to_use = "Observed Score")
linearinterp_7<-exampleFnc("7",score_to_use = "Predicted Score")
linearinterp_8<-exampleFnc("8",score_to_use = "Predicted Score")
linearinterp_9<-exampleFnc("9",score_to_use = "Predicted Score")
linearinterp_10<-exampleFnc("10",score_to_use = "Predicted Score")
linearinterp_11<-exampleFnc("11",score_to_use = "Predicted Score")
linearinterp_13<-exampleFnc("13",score_to_use = "Predicted Score")
linearinterp_14<-exampleFnc("14",score_to_use = "Predicted Score")
linearinterp_15<-exampleFnc("15",score_to_use = "Predicted Score")
linearinterp_16<-exampleFnc("16",score_to_use = "Predicted Score")
linearinterp_16_obs<-exampleFnc("16",score_to_use = "Observed Score")
linearinterp_17<-exampleFnc("17",score_to_use = "Predicted Score")
linearinterp_19<-exampleFnc("19",score_to_use = "Predicted Score")
linearinterp_21<-exampleFnc("21",score_to_use = "Predicted Score")
linearinterp_21_obs<-exampleFnc("21",score_to_use = "Observed Score")

individual_interpolated_scores<-rbind(linearinterp_1,linearinterp_2,linearinterp_3,linearinterp_5,linearinterp_6,
                         linearinterp_7,linearinterp_8,linearinterp_9,linearinterp_10,linearinterp_11,
                         linearinterp_13,linearinterp_14,linearinterp_15,linearinterp_16,linearinterp_17,
                         linearinterp_19,linearinterp_21)

##do slopes differ significantly-add knapper as dummy variable
m<-lm(Score_predicted~sqrt_hours+as.numeric(knapper),data=individual_interpolated_scores)
summary(m)
summary(aov(m))

###return individual slopes
#must change function output to return slope_df before this
#overall slope
model_kn<-lm(Score_predicted~sqrt_hours, data=complete_data_indiv)
summary(model_kn)
linearinterp_all<-data.frame(coef(model_kn)[2])
linearinterp_all$knapper<-99

individual_slopes<-rbind(linearinterp_1,linearinterp_2,linearinterp_3,linearinterp_5,linearinterp_6,
      linearinterp_7,linearinterp_8,linearinterp_9,linearinterp_10,linearinterp_11,
      linearinterp_13,linearinterp_14,linearinterp_15,linearinterp_16,linearinterp_17,
      linearinterp_19,linearinterp_21)

  colnames(individual_slopes)[1] <- "slope"
  
  #create slope difference column
  individual_slopes$slope_difference<-individual_slopes$slope-linearinterp_all$coef.model_kn..2.
  
#use dataframe from above (median differences, difference start_end)
  
individuals_assessment_scores_final<-merge(individual_slopes,individuals_relative_average_1_5_9,by="knapper")

my_data <- individuals_assessment_scores_final[, c(3:6)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

#alternative=extract only assessment 1, 5, and 9 raw scores
individuals_assessment_raw_scores_final<-merge(individual_slopes,individuals_first_last,by="knapper")
  
#need to change the function output to return plot before running the next part

linear_interp_plot<-multiplot(linearinterp_1,linearinterp_2,linearinterp_3,linearinterp_5,
                              linearinterp_7,linearinterp_8,linearinterp_9,
                              linearinterp_10,linearinterp_13,linearinterp_14,
                              linearinterp_15,linearinterp_17,linearinterp_19,
                              linearinterp_21,cols = 4)

##########How many hours to reach a score of 5#######
#need to set the exampleFnc back to return dataset and rerun before this section

hours_to_five_function <- function(data_set){
  data_kn =  data_set 
  model_kn<-lm(score~sqrt_hours, data=data_kn)
  prediction_data_kn <- data.frame(score=numeric(40),
                                sqrt_hours=seq(11,50,1),
                                knapper=rep(data_set$knapper,40))
  prediction_data_kn$score = predict(model_kn, prediction_data_kn)
  hours_to_five_kn<-subset(prediction_data_kn,score >4.999062 & score <5.12)
  return(unique(hours_to_five_kn))
}

hours_to_five_better_function <- function(data_set){
  data_kn =  data_set 
  model_kn<-lm(score~sqrt_hours, data=data_kn)
  prediction_data_kn <- data.frame(score=numeric(40),
                                   sqrt_hours=seq(11,50,1),
                                   knapper=rep(data_set$knapper,40))
  prediction_data_kn$score = predict(model_kn, prediction_data_kn)
  hours_to_five_kn<-subset(prediction_data_kn,score >4.888689 & score <5.12)
  return(unique(hours_to_five_kn))
}

hours_to_five_best_function <- function(data_set){
  data_kn =  data_set 
  model_kn<-lm(score~sqrt_hours, data=data_kn)
  prediction_data_kn <- data.frame(score=numeric(40),
                                   sqrt_hours=seq(11,50,1),
                                   knapper=rep(data_set$knapper,40))
  prediction_data_kn$score = predict(model_kn, prediction_data_kn)
  hours_to_five_kn<-subset(prediction_data_kn,score >4.791679 & score <5.13)
  return(unique(hours_to_five_kn))
}

hours_to_five_short_function <- function(data_set){
  data_kn =  data_set 
  model_kn<-lm(score~sqrt_hours, data=data_kn)
  prediction_data_kn <- data.frame(score=numeric(40),
                                   sqrt_hours=seq(11,50,1),
                                   knapper=rep(data_set$knapper,40))
  prediction_data_kn$score = predict(model_kn, prediction_data_kn)
  hours_to_five_kn<-subset(prediction_data_kn,score >4.909062 & score <5.1)
  return(unique(hours_to_five_kn))
}

hours_to_five_medium_function <- function(data_set){
  data_kn =  data_set 
  model_kn<-lm(score~sqrt_hours, data=data_kn)
  prediction_data_kn <- data.frame(score=numeric(40),
                                   sqrt_hours=seq(11,50,1),
                                   knapper=rep(data_set$knapper,40))
  prediction_data_kn$score = predict(model_kn, prediction_data_kn)
  hours_to_five_kn<-subset(prediction_data_kn,score >4.999062 & score <5.15)
  return(unique(hours_to_five_kn))
}

hours_to_five_intermediate_function <- function(data_set){
  data_kn =  data_set 
  model_kn<-lm(score~sqrt_hours, data=data_kn)
  prediction_data_kn <- data.frame(score=numeric(40),
                                   sqrt_hours=seq(11,50,1),
                                   knapper=rep(data_set$knapper,40))
  prediction_data_kn$score = predict(model_kn, prediction_data_kn)
  hours_to_five_kn<-subset(prediction_data_kn,score >4.999062 & score <5.23)
  return(unique(hours_to_five_kn))
}

hours_to_five_long_function <- function(data_set){
  data_kn =  data_set 
  model_kn<-lm(score~sqrt_hours, data=data_kn)
  prediction_data_kn <- data.frame(score=numeric(1990),
                                   sqrt_hours=seq(11,2000,1),
                                   knapper=rep(data_set$knapper,1990))
  prediction_data_kn$score = predict(model_kn, prediction_data_kn)
  hours_to_five_kn<-subset(prediction_data_kn,score >5 & score <5.013)
  return(unique(hours_to_five_kn))
}

hours_to_five_function_1<-hours_to_five_intermediate_function(linearinterp_1)
hours_to_five_function_2<-hours_to_five_medium_function(linearinterp_2)
hours_to_five_function_3<-hours_to_five_short_function(linearinterp_3)
hours_to_five_function_5<-hours_to_five_function(linearinterp_5)
#hours_to_five_function_6<-hours_to_five_long_function(linearinterp_6)
hours_to_five_function_7<-hours_to_five_function(linearinterp_7)
hours_to_five_function_8<-hours_to_five_better_function(linearinterp_8)
hours_to_five_function_9<-hours_to_five_short_function(linearinterp_9)
hours_to_five_function_10<-hours_to_five_function(linearinterp_10)
hours_to_five_function_11<-hours_to_five_function(linearinterp_11)
hours_to_five_function_13<-hours_to_five_intermediate_function(linearinterp_13)
hours_to_five_function_14<-hours_to_five_function(linearinterp_14)
hours_to_five_function_15<-hours_to_five_best_function(linearinterp_15)
#hours_to_five_function_16<-hours_to_five_long_function(linearinterp_16)
hours_to_five_function_17<-hours_to_five_function(linearinterp_17)
hours_to_five_function_19<-hours_to_five_intermediate_function(linearinterp_19)
hours_to_five_function_21<-hours_to_five_function(linearinterp_21)

hours_to_five_data<-rbind(hours_to_five_function_1,hours_to_five_function_2,hours_to_five_function_3,
      hours_to_five_function_5,hours_to_five_function_7,
      hours_to_five_function_8,hours_to_five_function_9,hours_to_five_function_10,
      hours_to_five_function_11,hours_to_five_function_13,hours_to_five_function_14,
      hours_to_five_function_15,hours_to_five_function_17,
      hours_to_five_function_19,hours_to_five_function_21)

howmanyhours = function(filter_var) {
  individual_interpolation_data = complete_data_indiv %>%
    dplyr::filter(knapper==filter_var) %>%
    mutate(predicted_score_interpolate_spline = na.spline(Score_predicted),
           predicted_score_interpolate_linear=na.approx(Score_predicted, rule=2),
           spline=ifelse(Score_predicted %in% NA, predicted_score_interpolate_spline,NA),
           linear=ifelse(Score_predicted %in% NA, predicted_score_interpolate_linear,NA),
           hours_sqrt = sqrt(hours)) %>% 
    dplyr::select(-c(predicted_score_interpolate_spline, predicted_score_interpolate_linear, hours)) %>%
    melt(id=c("knapper","assessment","hours_sqrt"))  %>%
    dplyr::rename(score=value)
  
  g = ggplot(data=individual_interpolation_data %>% filter(variable %in% c("Score_predicted")),size=1)+
    aes(x = hours_sqrt,y = score) +
    geom_point(alpha = 0.6) +
    #scale_color_manual(labels = c("Observed","Predicted", "Interpolated"),values=c("red","black", "blue")) +
    scale_x_continuous(name="Hours (sqrt)", limits=c(0,20))+
    scale_y_continuous(name="Score",limits = c(0, 5))+
    coord_cartesian(xlim=c(0,20), ylim=c(0,5))+
    labs(color='')+
    ggtitle(filter_var)+
    geom_smooth(method = "lm", data = individual_interpolation_data %>% 
                 filter(variable != "Score_observed", variable !="spline") %>%
                  mutate(assessment = as.numeric(assessment)), se=T,na.rm = T,fullrange = TRUE)+ 
    #annotate("text", x = 2, y = 5, label = "p-value == 0.001",parse = TRUE,color="red",size=8)+ 
    #annotate("text", x = 2, y = 4.5, label = "slope == 0.23",parse = TRUE,color="red",size=8)+
    #annotate("text", x = 2, y = 4, label = "R ^ 2 == 0.81",parse = TRUE,color="red",size=8)+
    theme(text = element_text(size=20))
  return(g)
  return(individual_interpolation_data)
} 

howmanyhours_test = function(data_file,knapper_number) {
  g = ggplot(data_file,aes(x = sqrt_hours,y = score)) +
    geom_point(alpha = 0.6) +
    scale_x_continuous(name="Hours (square root)",limits = c(0, 20))+
    scale_y_continuous(name="Score",limits = c(0, 5))+
    ggtitle(knapper_number)+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  theme(text = element_text(size=20))
  return(g)
} 

howmany_1<-howmanyhours_test(linearinterp_1,"1")
howmany_2<-howmanyhours_test(linearinterp_2,"2")
howmany_3<-howmanyhours_test(linearinterp_3,"3")
howmany_5<-howmanyhours_test(linearinterp_5,"5")
howmany_6<-howmanyhours_test(linearinterp_6,"6")
howmany_7<-howmanyhours_test(linearinterp_7,"7")
howmany_8<-howmanyhours_test(linearinterp_8,"8")
howmany_9<-howmanyhours_test(linearinterp_9,"9")
howmany_10<-howmanyhours_test(linearinterp_10,"10")
howmany_11<-howmanyhours_test(linearinterp_11,"11")
howmany_13<-howmanyhours_test(linearinterp_13,"13")
howmany_14<-howmanyhours_test(linearinterp_14,"14")
howmany_15<-howmanyhours_test(linearinterp_15,"15")
howmany_16<-howmanyhours_test(linearinterp_16,"16")
howmany_17<-howmanyhours_test(linearinterp_17,"17")
howmany_19<-howmanyhours_test(linearinterp_19,"19")
howmany_21<-howmanyhours_test(linearinterp_21,"21")

howmany_hours<-multiplot(howmany_1, howmany_2, howmany_3, howmany_5,
                                             howmany_7, howmany_8,
                                             howmany_9, howmany_10, howmany_11,
                                             howmany_14, howmany_15,
                                             howmany_17, howmany_19,
                                             howmany_21,cols=4)

###############does initial score predict final score, nada vs the forest#########
str(complete_data_indiv)

correlation_data<-
  complete_data_indiv %>%
  group_by(knapper) %>%
  filter(assessment %in% c("1", "9")
         & Score_observed != "NA"
         &!knapper %in% c("13","15","17","3","6","8"))  %>% 
  ungroup() %>%
  select(-c(hours))

correlation_data<-as.data.frame(correlation_data) #issue is with dply vs reshape, need to turn into dataframe after dplyr
  
correlation_data<-
  melt(correlation_data, 
     variable.name = "score_category",
     value.names = c("Score_observed","Score_predicted"))

correlation_data<-tidyr::spread(correlation_data, assessment, value)
correlation_data <- plyr::rename(correlation_data,c("1" = "one", "9" = "nine"))

correlation_data_observed<-filter(correlation_data, variable=="Score_observed")
correlation_data_predicted<-filter(correlation_data, variable=="Score_predicted")

res_observed <- cor.test(correlation_data_observed$one, correlation_data_observed$nine, 
                method = "pearson")
res_observed

res_observed_plot<-ggplot(correlation_data_observed,size=1)+
  aes(x = one,y = nine) +
  geom_point(color="red",alpha = 0.6) +
  scale_y_continuous(name="Assessment 9 observed scores",limits = c(2.5, 4.6))+
  scale_x_continuous(name="Assessment 1 observed scores",limits = c(1, 2.25))+
  geom_smooth(method = "lm",se=F,na.rm = T)+
  ggtitle("Observed Scores")+
  theme(text = element_text(size=20))

res_predicted <- cor.test(correlation_data_predicted$one, correlation_data_predicted$nine, 
                          method = "pearson")
res_predicted

res_predicted_plot<-ggplot(correlation_data_predicted,size=1)+
  aes(x = one,y = nine) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(name="Assessment 1 predicted scores",limits = c(1, 3.25))+
  scale_y_continuous(name="Assessment 9 predicted scores",limits = c(2, 4.25))+
  geom_smooth(method = "lm",se=F,na.rm = T)+
  ggtitle("Predicted Scores")+
  theme(text = element_text(size=20))

multiplot(res_observed_plot,res_predicted_plot,cols=2)

#combined model with score set as a dummy variable
correlation_data_test<-subset(correlation_data,!variable %in% c("sqrt_hours","prediction_test"))
correlation_data_test$dummy<-ifelse(correlation_data_test$variable=="Score_observed",1,0)

summary(correlation_data_test)

summary(lm(one~nine+dummy, data=correlation_data_test))
summary(lm(one~nine, data=subset(correlation_data_test,variable=="Score_observed")))
summary(lm(one~nine, data=subset(correlation_data_test,variable=="Score_predicted")))

##correlations based on all model scores + interpolations: not great
individual_interpolated_scores_copy<-select(individual_interpolated_scores,c("assessment","knapper","score"))
individual_interpolated_scores_copy<-subset(individual_interpolated_scores_copy,assessment %in% c(1,9))

model_correlation_data<-tidyr::spread(individual_interpolated_scores_copy, assessment, score)
model_correlation_data <- plyr::rename(model_correlation_data,c("1" = "one", "9" = "nine"))

model_res_predicted <- cor.test(model_correlation_data$nine, model_correlation_data$one, 
                          method = "pearson")
model_res_predicted

ggplot(model_correlation_data,size=1)+
  aes(x = one,y = nine) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(name="Assessment 1 predicted scores",limits = c(1, 3.25))+
  scale_y_continuous(name="Assessment 9 predicted scores",limits = c(2, 4.25))+
  geom_smooth(method = "lm",se=F,na.rm = T)+
  ggtitle("Predicted Scores")+
  theme(text = element_text(size=20))


