#################Control handaxes not scored by Dietz, but good for the Erin
library(dplyr)
library(data.table)
#bring in control not scored handaxe data
control_not_scored_complete_experiment_data<-filter(all_experiment_data_controls_not_scored, experiment == "handaxe" & condition == "control")

control_not_scored_new<-dplyr::select(control_not_scored_complete_experiment_data, c(8:25,29))

# compute factor scores from the 'experts_new' data set with the 'handaxe_pca' PCA solution
control_not_scored_handaxe_scores<-data.frame(scale(control_not_scored_new, handaxe_pca$center, handaxe_pca$scale) %*% handaxe_pca$rotation)

#join individual scores back to complete handaxe data and delete width and thickness measurements

test_2<-control_not_scored_complete_experiment_data
test_2$fake_number<-1:nrow(test_2)
control_not_scored_handaxe_scores$fake_number<-1:nrow(control_not_scored_handaxe_scores)

control_not_scored_handaxe_postpca<-merge(control_not_scored_handaxe_scores[,c(1:2,20)],test_2,by="fake_number")

#rename cols
setnames(control_not_scored_handaxe_postpca, "PC1","Dim.1")
setnames(control_not_scored_handaxe_postpca, "PC2","Dim.2")

#select relevant cols
colnames(control_not_scored_handaxe_postpca)

#don't need profile thickness CV: only applying these data to the already build model
control_not_scored_handaxe_postpca_reduced<-dplyr::select(control_not_scored_handaxe_postpca, knapper, assessment, experiment,condition,Core.Number,
                                                          Score,Training.Hours,delta_weight,
                                                          Dim.1, Dim.2,
                                                          profile_asymetry_index,plan_asymetry_index,
                                                          percent_unflaked_area,percent_bifacially_flaked,percent_unflaked_area,
                                                          flake_scar_density)

#scale data-first need to add to larger dataset, then scale
#bind  together with handaxe postpca
handaxe_postpca_test<-
  handaxe_postpca%>%
  select(-delta_profile_thickness_cv) #control does not have this:only applying these data to the already build model

control_not_scored_all_handaxe_postpca_scaled<-rbind(control_not_scored_handaxe_postpca_reduced, handaxe_postpca_test)

#scale all data
control_not_scored_all_handaxe_postpca_scaled[,c(8:13,15)] <- scale(control_not_scored_all_handaxe_postpca_scaled[,c(8:13,15)])
#subset to not scored control
control_not_scored_handaxe_postpca_scaled<-subset(control_not_scored_all_handaxe_postpca_scaled,knapper %in% c(28,33,35,37))

