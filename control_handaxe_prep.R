library(dplyr)
library(data.table)

#IMPORTANT NOTE: HANDAXE_PCA HAS ALL CONTROL SUBJECTS IN IT
#DON'T NEED TO RUN THIS
#SCALING OCCURS IN THAT R CODE FILE

####calculate PCA scores for control handaxes based on old handaxe pca

#bring in control handaxe data
control_complete_experiment_data<-filter(all_experiment_data, experiment == "handaxe" & condition == "control")

control_new<-dplyr::select(control_complete_experiment_data, c(8:25,29))

# compute factor scores from the 'experts_new' data set with the 'handaxe_pca' PCA solution
control_handaxe_scores<-data.frame(scale(control_new, handaxe_pca$center, handaxe_pca$scale) %*% handaxe_pca$rotation)

#join individual scores back to complete handaxe data and delete width and thickness measurements

test_1<-control_complete_experiment_data
test_1$fake_number<-1:nrow(test_1)
control_handaxe_scores$fake_number<-1:nrow(control_handaxe_scores)

control_handaxe_postpca<-merge(control_handaxe_scores[,c(1:2,19)],test_1,by="fake_number")

#rename cols
library(data.table)
setnames(control_handaxe_postpca, "PC1","Dim.1")
setnames(control_handaxe_postpca, "PC2","Dim.2")

#select relevant cols
library(dplyr)
colnames(control_handaxe_postpca)

control_handaxe_postpca_reduced<-dplyr::select(control_handaxe_postpca, knapper, assessment, experiment,condition,Core.Number,
              Score,Training.Hours,delta_weight,
              Dim.1, Dim.2,
              profile_asymetry_index,plan_asymetry_index,
              percent_unflaked_area,percent_bifacially_flaked,percent_unflaked_area,
              flake_scar_density,
              delta_profile_thickness_cv)

#scale data
control_handaxe_postpca_scaled<-control_handaxe_postpca_reduced
control_handaxe_postpca_scaled[,c(8:16)] <- scale(control_handaxe_postpca_scaled[,c(8:16)])

#bind  together with expert handaxe postpca
all_experiment_data_controlpca_scaled<-rbind(control_handaxe_postpca_scaled, handaxe_postpca_scaled)
