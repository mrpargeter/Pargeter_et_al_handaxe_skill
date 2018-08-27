
#subtract data for PCA
library(dplyr)
library(psych)
library(factoextra)

handaxe_shapes<- complete_experiment_data_handaxe_main[ , which(names(complete_experiment_data_handaxe_main) %in% 
              c("knapper","assessment","width_0.1","width_0.2","width_0.3","width_0.4",
              "width_0.5","width_0.6","width_0.7","width_0.8","width_0.9","thickness_0.1",
              "thickness_0.2","thickness_0.3","thickness_0.4","thickness_0.5","thickness_0.6",
              "thickness_0.7","thickness_0.8","thickness_0.9","max_length"))]

write.csv(handaxe_shapes,"handaxe_shape_measurements.csv")

knapper <- complete_experiment_data_handaxe_main[, 1]
assessment <- complete_experiment_data_handaxe_main[, 2]

#alternative method

handaxe_pca<-prcomp(handaxe_shapes[,-c(1:2)], scale. = T)

#compute standard deviation of each principal component
std_dev <- handaxe_pca$sdev

#compute variance
pr_var <- std_dev^2

pr_var[1:10]

prop_varex <- pr_var/sum(pr_var)

prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

####ggplot visualizations
fviz_eig(handaxe_pca) #screeplot

#graph of individuals
fviz_pca_ind(handaxe_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#graph of variables
fviz_pca_var(handaxe_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_contrib(handaxe_pca, choice = "var", axes = 1, top = 10)
fviz_contrib(handaxe_pca, choice = "var", axes = 2, top = 10)

###Access to the PCA results
# Eigenvalues
eig.val <- get_eigenvalue(handaxe_pca)
eig.val

# Results for Variables
res.var <- get_pca_var(handaxe_pca)
variable_loadings<-data.frame(res.var$coord)         # Coordinates

# Results for individuals
res.ind <- get_pca_ind(handaxe_pca)
Indiv_handaxe_scores<-data.frame(res.ind$coord)        # Coordinates

#join individual scores back to complete handaxe data and delete width and thickness measurements

test_1<-complete_experiment_data_handaxe_main
test_1$fake_number<-1:nrow(test_1)
Indiv_handaxe_scores$fake_number<-1:nrow(Indiv_handaxe_scores)

names(test_1)

handaxe_postpca<-merge(Indiv_handaxe_scores[,c("Dim.1","Dim.2","fake_number")],test_1,by="fake_number")

handaxe_postpca<- handaxe_postpca[ , -which(names(handaxe_postpca) %in% 
          c("fake_number","width_0.1","width_0.2","width_0.3","width_0.4","width_0.5","width_0.6","width_0.7","width_0.8","width_0.9","thickness_0.1","thickness_0.2",            
          "thickness_0.3","thickness_0.4","thickness_0.5","thickness_0.6","thickness_0.7","thickness_0.8","thickness_0.9"))]

library(dplyr)

colnames(handaxe_postpca)

handaxe_postpca<-dplyr::select(handaxe_postpca, knapper, assessment, experiment,condition,Core.Number,
                                 Score,Training.Hours,delta_weight,
                                 Dim.1, Dim.2,
                                  profile_asymetry_index,plan_asymetry_index,
                                  percent_unflaked_area,percent_bifacially_flaked,
                                  flake_scar_density,
                                  delta_profile_thickness_cv)

#scale data
handaxe_postpca_scaled<-handaxe_postpca

vars=c("Dim.1","Dim.2","delta_weight","profile_asymetry_index","plan_asymetry_index",
       "percent_unflaked_area","percent_bifacially_flaked",
       "flake_scar_density","delta_profile_thickness_cv")

handaxe_postpca_scaled[,vars] <- scale(handaxe_postpca[,vars])
