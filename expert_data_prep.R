##############Core and handaxe weights####################
expert_merged_start_end_weight_data<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/expert_start_end_weight_data.csv", header = T, na.strings=c('',""))

######add shape data
expert_shape_asymetry<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/Handaxe shape data/merged_expert_plan_area_asymetry_data.csv",header = T, na.strings=c('',""))

#merge with delta weight
expert_shape_asymetry_weight_data<-merge(expert_shape_asymetry, expert_merged_start_end_weight_data, 
                                       by=c("knapper"), all = T)

####Attribute data
expert_handaxe_attribute_data<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Handaxe attributes/Attributes_data/Attribute data excel sheets/expert_handaxe_attribute_data_02_21_18.csv",header = T, na.strings=c('',""))

expert_complete_experiment_data<-merge(expert_shape_asymetry_weight_data, expert_handaxe_attribute_data, 
                                by=c("knapper","assessment"), all = T)

colnames(expert_complete_experiment_data)
expert_complete_experiment_data$X<-NULL
str(expert_complete_experiment_data)
expert_complete_experiment_data[,c(3:27,29:32)] <- sapply(expert_complete_experiment_data[,c(3:27,29:32)],as.numeric)
expert_complete_experiment_data$assessment <- as.factor(expert_complete_experiment_data$assessment)

expert_complete_experiment_data$percent_unflaked_area<-(expert_complete_experiment_data$unflaked.area..mm2./expert_complete_experiment_data$area)*100
expert_complete_experiment_data$percent_bifacially_flaked<-sample(seq(from = 98, to = 100, by = 0.1), size = 10, replace = TRUE)
expert_complete_experiment_data$flake_scar_density<-(expert_complete_experiment_data$scar.count/expert_complete_experiment_data$area)

expert_complete_experiment_data$scar.count<-NULL
expert_complete_experiment_data$unflaked.area_mm2<-NULL
expert_complete_experiment_data$bifacial.extent_mm<-NULL

expert_complete_experiment_data$area<-NULL
expert_complete_experiment_data$perimeter<-NULL

####calculate PCA scores for expert handaxes based on old handaxe pca

#bring in expert handaxe data
library(dplyr)
experts_new<- expert_complete_experiment_data[ , which(names(expert_complete_experiment_data) %in% 
       c("width_0.1","width_0.2","width_0.3","width_0.4",
      "width_0.5","width_0.6","width_0.7","width_0.8","width_0.9","thickness_0.1",
      "thickness_0.2","thickness_0.3","thickness_0.4","thickness_0.5","thickness_0.6",
     "thickness_0.7","thickness_0.8","thickness_0.9","max_length"))]

# compute factor scores from the 'experts_new' data set with the 'handaxe_pca' PCA solution
expert_handaxe_scores<-data.frame(scale(experts_new, handaxe_pca$center, handaxe_pca$scale) %*% handaxe_pca$rotation)

#join individual scores back to complete handaxe data and delete width and thickness measurements

test_1<-expert_complete_experiment_data
test_1$fake_number<-1:nrow(test_1)
expert_handaxe_scores$fake_number<-1:nrow(expert_handaxe_scores)

expert_handaxe_postpca<-merge(expert_handaxe_scores[,c("PC1","PC2","fake_number")],test_1,by="fake_number")

#rename cols
library(data.table)
setnames(expert_handaxe_postpca, "PC1","Dim.1")
setnames(expert_handaxe_postpca, "PC2","Dim.2")

#add column for experiment and condition
expert_handaxe_postpca$condition<-rep("main",length(expert_handaxe_postpca$fake_number))
expert_handaxe_postpca$experiment<-rep("handaxe",length(expert_handaxe_postpca$fake_number))
expert_handaxe_postpca$assessment<-rep("10",length(expert_handaxe_postpca$fake_number))
expert_handaxe_postpca$Training.Hours<-rep(100,length(expert_handaxe_postpca$fake_number))
expert_handaxe_postpca$Score<-rep(5,length(expert_handaxe_postpca$fake_number))

#select relevant cols
library(dplyr)
colnames(expert_handaxe_postpca)

expert_handaxe_postpca_reduced<-expert_handaxe_postpca[,c("knapper", "experiment","Score",
                                                          "Training.Hours","Dim.1", "Dim.2")]

#merge together with attributes data
expert_complete_experiment_data_merged<-merge(expert_handaxe_postpca_reduced, expert_complete_experiment_data, 
                                       by=c("knapper"), all = T)

colnames(expert_complete_experiment_data_merged)

expert_complete_experiment_data_reduced<-expert_complete_experiment_data_merged[,c("knapper", "assessment", "experiment","condition",
                                                          "Score","Training.Hours","delta_weight",
                                                          "Dim.1", "Dim.2",
                                                          "profile_asymetry_index","plan_asymetry_index",
                                                          "percent_unflaked_area","percent_bifacially_flaked","flake_scar_density")]

##NO DELTA PROFILE THICKNESS FOR EXPERTS
#scale data
#bind  together with handaxe postpca first to scale to the same population
handaxe_postpca_test<-
  handaxe_postpca%>%
  select(-c(Core.Number,delta_profile_thickness_cv)) #control does not have this:only applying these data to the already build model

expert_complete_experiment_data_reduced_scaled<-rbind(expert_complete_experiment_data_reduced, handaxe_postpca_test)
#can use data as is for the model building section from here (uses combined data anway)
