#####Add handaxe nada score data######CHANGE INPUT FILE IF WANT TO DO CONTROL OR NON-CONTROL VERSION
nada_scores<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Skill assessment scores/nada_scores_detail_with_controls.csv",header = T, na.strings=c('',""))

nada_scores<-
  nada_scores %>%
  mutate_if(is.integer,as.factor)%>%
  filter(!(assessment == 3 & knapper == 14))

summary(nada_scores$score_component)

#plot: score_component by assessment
abandonment_data<-subset(nada_scores, score_component %in% "abandoment")
bifacial_plane_data<-subset(nada_scores, score_component %in% "bifacial_plane")
hammerstone_choice_data<-subset(nada_scores, score_component %in% "hammerstone choice")
platform_angle_data<-subset(nada_scores, score_component %in% "platform angle")
platform_preparation_data<-subset(nada_scores, score_component %in% "platform preparation")
shaping_data<-subset(nada_scores, score_component %in% "shaping")
stacks_data<-subset(nada_scores, score_component %in% "stacks")
strategy_data<-subset(nada_scores, score_component %in% "strategy")
striking_angle_force_data<-subset(nada_scores, score_component %in% "striking angle_force")
thinning_data<-subset(nada_scores, score_component %in% "thinning")

plot_score_function<-function(data,plot_title){
  plot<-ggplot(data=data,aes(x=assessment,y=score, color=assessment)) +
    scale_color_brewer(palette="Paired") +
    theme_bw() +
    geom_violin()+
    geom_point(shape=16, alpha = 0.3,position = position_jitter(w = 0.2, h = 0))+
    ggtitle(plot_title)+ 
    theme(legend.position="none") +
    ylim(0,5)
  return(plot)
}

abandonment_plot<-plot_score_function(abandonment_data,"Abandonment")
bifacial_plane_data_plot<-plot_score_function(bifacial_plane_data,"Bifacial Plane")
hammerstone_choice_data_plot<-plot_score_function(hammerstone_choice_data,"Hammerstone")
platform_angle_data_plot<-plot_score_function(platform_angle_data,"Platform Angle")
platform_preparation_data_plot<-plot_score_function(platform_preparation_data,"Platform Preparation")
shaping_data_plot<-plot_score_function(shaping_data,"Shaping")
stacks_data_plot<-plot_score_function(stacks_data,"Stacks")
strategy_data_plot<-plot_score_function(strategy_data,"Strategy")
striking_angle_force_data_plot<-plot_score_function(striking_angle_force_data,"Striking Angle")
thinning_data_plot<-plot_score_function(thinning_data,"Thinning")

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
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

multiplot(abandonment_plot,bifacial_plane_data_plot,hammerstone_choice_data_plot,platform_angle_data_plot,
          platform_preparation_data_plot,shaping_data_plot,stacks_data_plot,strategy_data_plot,
          striking_angle_force_data_plot,thinning_data_plot, cols=3)

#hammerstone,platform prep, shaping, stacks, thinning?

plot_score_individual_function<-function(data,plot_title){
  plot<-ggplot(data=data,aes(x=knapper,y=score, color=knapper)) +
    scale_fill_distiller(palette="Paired") +
    theme_bw() +
    geom_violin()+
    geom_point(shape=16, alpha = 0.3,position = position_jitter(w = 0.2, h = 0))+
    ggtitle(plot_title)+ 
    theme(legend.position="none")
  return(plot)
}

abandonment_data_indiv_plot<-plot_score_individual_function(abandonment_data,"Abandonment")
bifacial_data_indiv_plot<-plot_score_individual_function(bifacial_plane_data,"Bifacial Plane")
hammer_data_indiv_plot<-plot_score_individual_function(hammerstone_choice_data,"Hammerstone")
platform_angle_data_indiv_plot<-plot_score_individual_function(platform_angle_data,"Platform Angle")
platform_prep_data_indiv_plot<-plot_score_individual_function(platform_preparation_data,"Platform Preparation")
shaping_data_indiv_plot<-plot_score_individual_function(shaping_data,"Shaping")
stacks_data_indiv_plot<-plot_score_individual_function(stacks_data,"Stacks")
strategyy_data_indiv_plot<-plot_score_individual_function(strategy_data,"Strategy")
striking_data_indiv_plot<-plot_score_individual_function(striking_angle_force_data,"Striking Angle")
thinning_data_indiv_plot<-plot_score_individual_function(thinning_data,"Thinning")

multiplot(abandonment_data_indiv_plot,bifacial_data_indiv_plot,hammer_data_indiv_plot,platform_angle_data_indiv_plot,
          platform_prep_data_indiv_plot,shaping_data_indiv_plot,stacks_data_indiv_plot,strategyy_data_indiv_plot,
          striking_data_indiv_plot,thinning_data_indiv_plot, cols=3)

####assessment onto score component
assessment1_data<-subset(nada_scores, assessment %in% "1")
assessment2_data<-subset(nada_scores, assessment %in% "2")
assessment3_data<-subset(nada_scores, assessment %in% "3")
assessment4_data<-subset(nada_scores, assessment %in% "4")
assessment5_data<-subset(nada_scores, assessment %in% "5")
assessment6_data<-subset(nada_scores, assessment %in% "6")
assessment7_data<-subset(nada_scores, assessment %in% "7")
assessment8_data<-subset(nada_scores, assessment %in% "8")
assessment9_data<-subset(nada_scores, assessment %in% "9")

my.labels <- c("abandon","biface","hammer","angle","plat.prep","shape",
               "stack","strategy","strike/angle","thin")

plot_score_assessment_function<-function(data,plot_title){
  plot<-ggplot(data=data,aes(x=score_component,y=score, color=score_component)) +
    scale_fill_distiller(palette="Paired") +
    theme_bw() +
    geom_violin()+
    geom_point(shape=16, alpha = 0.3,position = position_jitter(w = 0.2, h = 0))+
    ggtitle(plot_title)+ 
    theme(legend.position="none",axis.text.x = element_text(angle=90, hjust=1))+
    scale_x_discrete(labels= my.labels)+
    ylim(0,5)
  return(plot)
}

assessment_one_plot<-plot_score_assessment_function(assessment1_data,"assessment1_data")
assessment_two_plot<-plot_score_assessment_function(assessment2_data,"assessment2_data")
assessment_three_plot<-plot_score_assessment_function(assessment3_data,"assessment3_data")
assessment_four_plot<-plot_score_assessment_function(assessment4_data,"assessment4_data")
assessment_five_plot<-plot_score_assessment_function(assessment5_data,"assessment5_data")
assessment_six_plot<-plot_score_assessment_function(assessment6_data,"assessment6_data")
assessment_seven_plot<-plot_score_assessment_function(assessment7_data,"assessment7_data")
assessment_eight_plot<-plot_score_assessment_function(assessment8_data,"assessment8_data")
assessment_nine_plot<-plot_score_assessment_function(assessment9_data,"assessment9_data")

multiplot(assessment_one_plot,assessment_two_plot,assessment_three_plot,assessment_four_plot,
          assessment_five_plot,assessment_six_plot,assessment_seven_plot,assessment_eight_plot,
          assessment_nine_plot, cols=3)

####divide into strategy and action
nada_scores$aggregated_score <- 
  ifelse(nada_scores$score_component %in% c("hammerstone choice","platform angle","platform preparation","striking angle_force"), "Perceptual motor execution",
      ifelse(nada_scores$score_component %in% c("thinning","shaping","stacks"), "Outcomes", "Strategic understanding"))

#plot by score aggregated component# check that control or non control input file used:see start of the code
Perceptual_motor_execution_data<-subset(nada_scores, aggregated_score %in% "Perceptual motor execution")
Outcomes_data<-subset(nada_scores, aggregated_score %in% "Outcomes")
Strategic_understanding_data<-subset(nada_scores, aggregated_score %in% "Strategic understanding")

#agg component _ assessment

Perceptual_motor_execution_data_plot<-plot_score_function(Perceptual_motor_execution_data,"Perceptual_motor_execution_data")
outcomes_data_plot<-plot_score_function(Outcomes_data,"Outcomes_data")
Strategic_understanding_data_plot<-plot_score_function(Strategic_understanding_data,"Strategic_understanding_data")

multiplot(Perceptual_motor_execution_data_plot,outcomes_data_plot,Strategic_understanding_data_plot, cols=1)

#individual
Perceptual_motor_execution_indivi_data_plot<-plot_score_individual_function(Perceptual_motor_execution_data,"Perceptual_motor_execution_data")
outcomes_data_indiv_plot<-plot_score_individual_function(Outcomes_data,"Outcomes")
Strategic_understanding_indiv_data_plot<-plot_score_individual_function(Strategic_understanding_data,"Strategic_understanding")

multiplot(Perceptual_motor_execution_indivi_data_plot,outcomes_data_indiv_plot,Strategic_understanding_indiv_data_plot, cols=1)

#assessment_agg component
my.labels_2 <- c("perception","outcomes","strategy")

plot_agg_score_assessment_function<-function(data,plot_title){
  plot<-ggplot(data=data,aes(x=aggregated_score,y=score, color=aggregated_score)) +
    scale_fill_distiller(palette="Paired") +
    theme_bw() +
    geom_violin()+
    geom_point(shape=16, alpha = 0.3,position = position_jitter(w = 0.2, h = 0))+
    ggtitle(plot_title)+ 
    theme(legend.position="none",axis.text.x = element_text(angle=90, hjust=1))+
    scale_x_discrete(labels= my.labels_2)+
    ylim(0,5)
  return(plot)
}

assessment_one_agg_plot<-plot_agg_score_assessment_function(assessment1_data,"assessment1_data")
assessment_two_agg_plot<-plot_agg_score_assessment_function(assessment2_data,"assessment2_data")
assessment_three_agg_plot<-plot_agg_score_assessment_function(assessment3_data,"assessment3_data")
assessment_four_agg_plot<-plot_agg_score_assessment_function(assessment4_data,"assessment4_data")
assessment_five_agg_plot<-plot_agg_score_assessment_function(assessment5_data,"assessment5_data")
assessment_six_agg_plot<-plot_agg_score_assessment_function(assessment6_data,"assessment6_data")
assessment_seven_agg_plot<-plot_agg_score_assessment_function(assessment7_data,"assessment7_data")
assessment_eight_agg_plot<-plot_agg_score_assessment_function(assessment8_data,"assessment8_data")
assessment_nine_agg_plot<-plot_agg_score_assessment_function(assessment9_data,"assessment9_data")

multiplot(assessment_one_agg_plot,assessment_two_agg_plot,assessment_three_agg_plot,
          assessment_four_agg_plot,
          assessment_five_agg_plot,assessment_six_agg_plot,assessment_seven_agg_plot,
          assessment_eight_agg_plot,assessment_nine_agg_plot,cols=3)

##############PCA approach#################
#subtract data for PCA
library(dplyr)

str(nada_scores)

library(reshape)

nada_scores_for_pca <- na.omit(dcast(nada_scores, knapper + assessment ~ score_component,value.var="score"))

nada_pca<-prcomp(nada_scores_for_pca[,-c(1:2)], scale. = F)

#compute standard deviation of each principal component
std_dev <- nada_pca$sdev

#compute variance
pr_var <- std_dev^2

pr_var[1:10]

prop_varex <- pr_var/sum(pr_var)

prop_varex[1:10]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

####ggplot visualizations
library(factoextra)

fviz_eig(nada_pca) #screeplot

#graph of individuals
fviz_pca_ind(nada_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#graph of variables
fviz_pca_var(nada_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_contrib(nada_pca, choice = "var", axes = 1, top = 10)
fviz_contrib(nada_pca, choice = "var", axes = 2, top = 10)

###Access to the PCA results
# Eigenvalues
eig.val <- get_eigenvalue(nada_pca)
eig.val

# Results for Variables
res.var <- get_pca_var(nada_pca)
variable_loadings<-data.frame(res.var$coord)         # Coordinates

# Results for individuals
res.ind <- get_pca_ind(nada_pca)
Indiv_handaxe_scores<-data.frame(res.ind$coord)        # Coordinates

