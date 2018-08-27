###Checking data structure

library(ggplot2)
library(sjstats)
library(ztable)
library(dplyr)

####show outliers
outlier_handaxe_names <- outlier_handaxes$Core.Number
control_handaxes<-subset(handaxe_postpca,condition %in% "control")
control_handaxe_names<-control_handaxes$Core.Number

#subset to work with new dataset
handaxe_postpca_for_overview<-handaxe_postpca
handaxe_postpca_for_overview_for_overview<-handaxe_postpca_for_overview

handaxe_postpca_for_overview$highlight <- ifelse(handaxe_postpca_for_overview$Core.Number %in% outlier_handaxe_names, "highlight", 
                                    ifelse(handaxe_postpca_for_overview$Core.Number %in% control_handaxe_names, "control","normal"))
textdf <- handaxe_postpca_for_overview[handaxe_postpca_for_overview$Core.Number %in% outlier_handaxe_names, ]
mycolours <- c("highlight" = "red", "control"="orange","normal" = "black")

###Plot predictors by assessment

createComparisonPlot<-function(df, y_var, y_label){
  comparison_plot<-ggplot(df, 
                          aes_string(x="assessment", y=y_var, fill = "assessment")) + 
    geom_boxplot(outlier.size = 0, outlier.colour = 'white')+
    geom_jitter(size=2,aes(colour = highlight))+
    scale_fill_grey(start = 0, end = .9) +
    scale_color_manual(values = c("red","orange","blue"))+
    theme(legend.position="none")+
    theme(text = element_text(size=15))
  
  comparison_plot<-comparison_plot + 
    #geom_point(aes_string(y=y_var, group='assessment'), 
    #           position = position_dodge(width=0.75),na.rm=TRUE)+
    labs(x = "Assessment", y = y_label)
  
  return(comparison_plot)
}
createAnova<-function(df, y_var){
  frm<-as.formula(sprintf("%s~%s", y_var, "assessment"))
  compare_aov<-aov(frm, data=df)
  return(compare_aov)
} #need to find a way to log var here
fullTests <- function(df, y_var, y_label){
  compare_aov = createAnova(df, y_var)
  print(summary(compare_aov))
  print(omega_sq(compare_aov))
  return(createComparisonPlot(df, y_var, y_label))
}

mass_remaining<-fullTests(handaxe_postpca_for_overview, y_var = 'delta_weight', y_label = "% mass remaining")
bifacially_flaked<-fullTests(handaxe_postpca_for_overview, y_var = 'percent_bifacially_flaked', y_label = "% bifacially flaked")
Profile_asymmetry_index<-fullTests(handaxe_postpca_for_overview, y_var = 'profile_asymetry_index', y_label = "Profile asymmetry")
plan_asymmetry_index<-fullTests(handaxe_postpca_for_overview, y_var = 'plan_asymetry_index', y_label = "Plan asymmetry")
unflaked_area<-fullTests(handaxe_postpca_for_overview, y_var = 'percent_unflaked_area', y_label = "% unflaked area")
scar_density<-fullTests(handaxe_postpca_for_overview, y_var = 'flake_scar_density', y_label = "Flake scar density")
profile_thickness_CV<-fullTests(handaxe_postpca_for_overview, y_var = 'delta_profile_thickness_cv', y_label = "Delta profile thickness CV")
dim_1_plot<-fullTests(handaxe_postpca_for_overview, y_var = 'Dim.1', y_label = "PCA shape 1")
dim_2_plot<-fullTests(handaxe_postpca_for_overview, y_var = 'Dim.2', y_label = "PCA shape 2")

multiplot(mass_remaining,bifacially_flaked,Profile_asymmetry_index,plan_asymmetry_index,unflaked_area,scar_density,profile_thickness_CV,dim_1_plot,dim_2_plot,cols=3)

###Plot predictors by inidividual

createComparisonPlot_knapper<-function(df, y_var, y_label){
  comparison_plot_knapper<-ggplot(df, 
                          aes_string(x="knapper", y=y_var, fill = "knapper")) + 
    geom_boxplot(outlier.size = 0, outlier.colour = 'white')+
    geom_jitter(size=2,aes(colour = highlight)) +
    scale_color_manual(values = c("red","orange","blue"))+
    scale_fill_grey(start = 0, end = .9) + 
    theme(legend.position="none")+
    theme(text = element_text(size=15))
  
  comparison_plot_knapper<-comparison_plot_knapper + 
    #geom_point(aes_string(y=y_var, group='knapper'), 
     #          position = position_dodge(width=0.75),na.rm=TRUE)+
    labs(x = "knapper", y = y_label)
  
  return(comparison_plot_knapper)
}
createAnova_knapper<-function(df, y_var){
  frm<-as.formula(sprintf("%s~%s", y_var, "knapper"))
  compare_aov<-aov(frm, data=df)
  return(compare_aov)
} #need to find a way to log var here
fullTests_knapper <- function(df, y_var, y_label){
  compare_aov = createAnova_knapper(df, y_var)
  print(summary(compare_aov))
  print(omega_sq(compare_aov))
  return(createComparisonPlot_knapper(df, y_var, y_label))
}

handaxe_postpca_for_overview$knapper<-
  factor(handaxe_postpca_for_overview$knapper, levels=c('1','2','3','5','6','7','8','9','10','11','13','14','15','16','17','19','21',"22","23","24","25","26","27","31","32","34"), ordered=TRUE)

delta_weight_plot <- fullTests_knapper(handaxe_postpca_for_overview, y_var = 'delta_weight', y_label = "% mass remaining")
percent_bifacial_plot <- fullTests_knapper(handaxe_postpca_for_overview, y_var = 'percent_bifacially_flaked', y_label = "% bifacially flaked")
profile_asymetry_index_plot <- fullTests_knapper(handaxe_postpca_for_overview, y_var = 'profile_asymetry_index', y_label = "Profile asymmetry")
plan_asymetry_index_plot <- fullTests_knapper(handaxe_postpca_for_overview, y_var = 'plan_asymetry_index', y_label = "Plan asymmetry")
percent_unflaked_area_plot <- fullTests_knapper(handaxe_postpca_for_overview, y_var = 'percent_unflaked_area', y_label = "% unflaked area")
flake_scar_density_plot <- fullTests_knapper(handaxe_postpca_for_overview, y_var = 'flake_scar_density', y_label = "Flake scar density")
delta_profile_thickness_cv_plot <- fullTests_knapper(handaxe_postpca_for_overview, y_var = 'delta_profile_thickness_cv', y_label = "Delta profile thickness CV")
dim_1_plot<-fullTests_knapper(handaxe_postpca_for_overview, y_var = 'Dim.1', y_label = "PCA shape 1")
dim_2_plot<-fullTests_knapper(handaxe_postpca_for_overview, y_var = 'Dim.2', y_label = "PCA shape 2")

multiplot(delta_weight_plot,percent_bifacial_plot,profile_asymetry_index_plot,plan_asymetry_index_plot,percent_unflaked_area_plot,flake_scar_density_plot,delta_profile_thickness_cv_plot,
          dim_1_plot,dim_2_plot,cols=3)

########variable by score
create_score_ComparisonPlot<-function(df, x_var, x_label, graph_title){
  score_comparison_plot<-ggplot(df, 
                          aes_string(y="Score", x=x_var)) + 
    geom_point(size=2.5)+
    scale_fill_grey(start = 0, end = .9) + 
    theme(legend.position="none")+
    scale_y_continuous(name = "Skill Score")+
    scale_x_continuous(name = x_label)+ 
    ggtitle(graph_title)+
    theme(text = element_text(size=20))
  
  return(score_comparison_plot)
}

flake_scar_density_plot <- create_score_ComparisonPlot(handaxe_postpca_for_overview, x_var = 'flake_scar_density', x_label = "Flake scar density", 
                                                                graph_title = "Flake scar density and Score")

percent_unflaked_area_score_plot <- create_score_ComparisonPlot(handaxe_postpca_for_overview, x_var = 'percent_unflaked_area', x_label = "Percentage unflaked area", 
                                                                 graph_title = "% Unflaked area and Score")

profile_asymetry_index_score_plot <- create_score_ComparisonPlot(handaxe_postpca_for_overview, x_var = 'profile_asymetry_index', x_label = "Profile asymetry index", 
                                                                    graph_title = "Profile asymmetry and Score")

plan_asymetry_index_score_plot <- create_score_ComparisonPlot(handaxe_postpca_for_overview, x_var = 'plan_asymetry_index', x_label = "Plan asymetry index", 
                                                                 graph_title = "Plan asymmetry and Score")

percent_bifacially_flaked_score_plot <- create_score_ComparisonPlot(handaxe_postpca_for_overview, x_var = 'percent_bifacially_flaked', x_label = "Percent bifacially flaked", 
                                                       graph_title = "percent_bifacially_flaked and Score")

delta_weight_score_plot <- create_score_ComparisonPlot(handaxe_postpca_for_overview, x_var = 'delta_weight', x_label = "Delta weight", 
                                                                  graph_title = "Delta weight and Score")

delta_profile_thickness_score_plot <- create_score_ComparisonPlot(handaxe_postpca_for_overview, x_var = 'delta_profile_thickness_cv', x_label = "Delta profile thickness CV", 
                                                     graph_title = "Delta profile thickness CV and Score")

######comparisons knapper and assessment

#The models are saturated with an interaction effecr and they needs at least 1 degree of freedom and more data to test the interaction effect 
interaction.plot(handaxe_postpca_for_overview$assessment, handaxe_postpca_for_overview$knapper, handaxe_postpca_for_overview$delta_weight)

delta_weight_aov<-aov(log(delta_weight)~knapper+assessment, data=handaxe_postpca_for_overview)
res<-delta_weight_aov$residuals
hist(res,main="Histogram of
residuals",xlab="Residuals")
summary(delta_weight_aov)
omega_sq(delta_weight_aov)
z<-ztable(delta_weight_aov)

interaction.plot(handaxe_postpca_for_overview$assessment, handaxe_postpca_for_overview$knapper, handaxe_postpca_for_overview$percent_bifacially_flaked)

percent_bifacial_aov<-aov(log(percent_bifacially_flaked+1)~knapper+assessment, data=handaxe_postpca_for_overview)
res<-percent_bifacial_aov$residuals
hist(res,main="Histogram of
     residuals",xlab="Residuals")
summary(percent_bifacial_aov)
ztable(percent_bifacial_aov,type="viewer")
omega_sq(percent_bifacial_aov)

profile_asymetry_index_aov<-aov(log(profile_asymetry_index)~knapper+assessment, data=handaxe_postpca_for_overview)
res<-profile_asymetry_index_aov$residuals
hist(res,main="Histogram of
     residuals",xlab="Residuals")
summary(profile_asymetry_index_aov)
ztable(profile_asymetry_index_aov,type="viewer")
omega_sq(profile_asymetry_index_aov)

plan_asymetry_index_aov<-aov(log(plan_asymetry_index)~knapper+assessment, data=handaxe_postpca_for_overview)
res<-plan_asymetry_index_aov$residuals
hist(res,main="Histogram of
     residuals",xlab="Residuals")
summary(plan_asymetry_index_aov)
ztable(plan_asymetry_index_aov,type="viewer")
omega_sq(plan_asymetry_index_aov)

percent_unflaked_area_aov<-aov(log(percent_unflaked_area+1)~knapper+assessment, data=handaxe_postpca_for_overview)
res<-percent_unflaked_area_aov$residuals
hist(res,main="Histogram of
     residuals",xlab="Residuals")
summary(percent_unflaked_area_aov)
omega_sq(percent_unflaked_area_aov)

flake_scar_density_aov<-aov(log(flake_scar_density+1)~knapper+assessment, data=handaxe_postpca_for_overview)
res<-flake_scar_density_aov$residuals
hist(res,main="Histogram of
     residuals",xlab="Residuals")
summary(flake_scar_density_aov)
omega_sq(flake_scar_density_aov)

delta_profile_thickness_cv_aov<-aov(delta_profile_thickness_cv~knapper+assessment, data=handaxe_postpca_for_overview)
res<-delta_profile_thickness_cv_aov$residuals
hist(res,main="Histogram of
     residuals",xlab="Residuals")
summary(delta_profile_thickness_cv_aov)
omega_sq(delta_profile_thickness_cv_aov)

pc1_aov<-aov(Dim.1~knapper+assessment, data=handaxe_postpca_for_overview)
res<-pc1_aov$residuals
hist(res,main="Histogram of
     residuals",xlab="Residuals")
summary(pc1_aov)
omega_sq(pc1_aov)

pc2_aov<-aov(Dim.2~knapper+assessment, data=handaxe_postpca_for_overview)
res<-pc2_aov$residuals
hist(res,main="Histogram of
     residuals",xlab="Residuals")
summary(pc2_aov)
omega_sq(pc2_aov)

#####predictors against each other
plot(log(handaxe_postpca_for_overview$flake_scar_density),log(handaxe_postpca_for_overview$percent_unflaked_area))
cor.test(log(handaxe_postpca_for_overview$flake_scar_density+1),log(handaxe_postpca_for_overview$delta_weight+1))
plot(handaxe_postpca_for_overview$flake_scar_density,handaxe_postpca_for_overview$Dim.2)
plot(handaxe_postpca_for_overview$flake_scar_density,handaxe_postpca_for_overview$Dim.1)

###covariance matrix
summary(handaxe_postpca_for_overview)

handaxe_covariance_data<-select(handaxe_postpca_for_overview,knapper,delta_weight,Dim.1,Dim.2,profile_asymetry_index,
                                plan_asymetry_index,percent_unflaked_area,percent_bifacially_flaked,flake_scar_density,
                                delta_profile_thickness_cv)
handaxe_covariance_data<-
  handaxe_covariance_data %>%
  mutate_at(c(2:10), funs(c(scale(.))))

matrix<-format(data.frame(cor(handaxe_covariance_data[,c(2:10)])),scientific=FALSE)

write.csv(matrix,"handaxe_correlation_matrix.csv")
