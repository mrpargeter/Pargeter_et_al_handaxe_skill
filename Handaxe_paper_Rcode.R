#############################################################
#  Rcode Compendium for Paregter et al.
# 'Handaxe making skill acquisition: Experimental insights into 
# 'learning trajectories and individual differences'
# Under review with the Journal of Human Evolution
#############################################################

#######################
# R session information
#######################

#platform       x86_64-apple-darwin15.6.0   
#arch           x86_64                      
#os             darwin15.6.0                
#system         x86_64, darwin15.6.0        
#status                                     
#major          3                           
#minor          5.1                         
#year           2018                        
#month          07                          
#day            02                          
#svn rev        74947                       
#language       R                           
#version.string R version 3.5.1 (2018-07-02)
#nickname       Feather Spray   

################
# Notes on package versions
# We use the scale_color_virdis addition to ggplot
# Please make sure you are running the latest version of ggplot (>2.2.1)
# Also, the recode function in the 'car' package overwrites 'dplyr's'
# recode function (the one we use). To stop this from happening
# make sure that dplyr loads after car
################

################
# Load libraries
################

list.of.packages <-
  c(
    "raster",
    "car",
    "reshape2",
    "data.table",
    "psych",
    "factoextra",
    "ggplot2",
    "scales",
    "randomForest",
    "caTools",
    "zoo",
    "kableExtra",
    "caTools",
    "RColorBrewer",
    "PerformanceAnalytics",
    "gridExtra",
    "grid",
    "lmPerm",
    "lubridate",
    "survival",
    "fANCOVA",
    "zeallot",
    "dplyr",
    "tidyr"
    )

# check to see if all the required pkgs are installed, if not, install them

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

# load required libraries

lapply(list.of.packages, require, character.only = TRUE)

############################## 
# Set working directory and source files
##############################

# Set to source directory with all .csv datafiles provided
# through the Open Science repository

setwd("../Pargeter_et_al_handaxe_data_files")

## Add datasets

# Size/shape data

profile_filenames<-list.files("Regular_handaxes/Profile", pattern="*.txt")
plan_filenames<-list.files("Regular_handaxes/Plan", pattern="*.txt")
area_filenames<-list.files("Regular_handaxes/Area", pattern="*.csv")
control_profile_filenames<-list.files("Control_handaxes/Profile", pattern="*.txt")
control_plan_filenames<-list.files("Control_handaxes/Profile", pattern="*.txt")
control_area_data<-read.csv("Control_handaxes/Area/Results.csv", header = T)
asymmetry<-read.csv("flip_test.csv")
control_asymetry<-read.csv("flip_test_control.csv")
expert_profile_filenames<-list.files("Expert_handaxes/Profile", pattern="*.txt")
expert_plan_filenames<-list.files("Expert_handaxes/Plan", pattern="*.txt")
expert_area_filenames<-list.files("Expert_handaxes/Area", pattern="*.csv")
asymmetry_expert<-read.csv("flip_test_expert.csv")
control_experiment_data<-read.csv("control_experiment_data_09_27_17.csv", header = T, na.strings=c('',""))

# Cores

core_profile_filenames<-list.files("Cores/Profile", pattern="*.txt")
control_core_profile_filenames<-list.files("Cores/Control_profile", pattern="*.txt")

# Cores and scores

handaxe_core_numbers_scores<-read.csv("handaxe_experiment_scores_core_number_09_26_17.csv", header = T, na.strings=c('',""))
prediction_core_numbers_scores<-read.csv("prediction_experiment_scores_core_number.csv", header = T, na.strings=c('',""))
handaxe_start_weight<-read.csv("handaxe_start_weights.csv", header = T, na.strings=c('',""))
prediction_start_weight<-read.csv("prediction_start_weights.csv", header = T, na.strings=c('',""))
handaxe_end_weight<-read.csv("handaxe_end_weights.csv", header = T, na.strings=c('',""))
prediction_end_weight<-read.csv("prediction_end_weights.csv", header = T, na.strings=c('',""))
expert_merged_start_end_weight_data<-read.csv("expert_start_end_weight_data.csv", header = T, na.strings=c('',""))

# Attribute data

main_attribute_data<-read.csv("main_attribute_data_01_09_18.csv",header = T, na.strings=c('',""))
control_attribute_data<-read.csv("control_handaxe_attribute_data_04_18_18.csv",header = T, na.strings=c('',""))
expert_handaxe_attribute_data<-read.csv("expert_handaxe_attribute_data_02_21_18.csv",header = T, na.strings=c('',""))

# Psychometric data

tower_of_london<-read.csv("tol_start_compiled.csv")
wisc_card_sort<-read.csv("wisc_start_compiled_data.csv")

# Training data

subject_dates_hours<-read.csv("dates_hours_overall_only_enrolled.csv")

## Start series of functions that defines the paper's main datasets and statistical tests

##############################
# Open Read function
# Calls txt and csv files into R
##############################

  # open txt files function
  open_read<-function(filename, foldername){
  open_file<-read.delim(paste(foldername,"/",filename,sep=""),header=F,sep='_')
  colnames(open_file)<-c("variable","measurement_point","measurement")
  open_file$handaxe_number<-gsub(".txt","",filename)
  return(open_file)
}

##############################
# Merge handaxe shape data function
# Merges size data for shape analysis
# Returns size data file
##############################

  merge_shape_data_function<-function(plan_names,profile_names,plan_pathway,profile_pathway) {
   
  # combine txt files for each recording set
  handaxe_plan_measurements<-do.call(rbind,lapply(plan_names,open_read,plan_pathway))
  handaxe_profile_measurements<-do.call(rbind,lapply(profile_names,open_read,profile_pathway))
  
  # clean and reshape plan data 
  handaxe_plan_measurements_reshape<-handaxe_plan_measurements %>%
    mutate(variable=recode(handaxe_plan_measurements$variable, Width = "width"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_width_mm=measurement*25.4) %>%
    select(-c(variable,measurement)) %>%
    separate(col=handaxe_number, into=c("knapper", "assessment"), sep="_") %>%
    dcast(knapper + assessment ~ measurement_point,value.var="shape_width_mm")
  
  # clean and reshape profile data 
  handaxe_profile_measurements_reshape<-handaxe_profile_measurements %>%
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*10) %>%
    select(-c(variable,measurement)) %>%
    separate(handaxe_number, c("knapper", "assessment"), sep="_") %>%
    dcast(knapper + assessment ~ measurement_point,value.var="shape_thickness_mm")
  
  handaxe_profile_measurements_reshape<-handaxe_profile_measurements_reshape %>%
    mutate(handaxe_profile_thickness_cv=apply(handaxe_profile_measurements_reshape[,c(3:11)], 1, cv))

  # create merged shape data
  merged_shape_data<-handaxe_plan_measurements_reshape %>% 
    full_join(handaxe_profile_measurements_reshape,by=c("assessment","knapper")) %>%
    mutate_if(is.character,as.factor)
 
  return(merged_shape_data)
}

  test<-merge_shape_data_function(plan_filenames,profile_filenames,"Regular_handaxes/Plan","Regular_handaxes/Profile")

##############################
# Open Read Area function
# Calls area and size csv files into R
##############################

  open_read_area<-function(filename, foldername){
  open_file_area<-read.csv(paste(foldername,"/",filename,sep=""),header=T)
  colnames(open_file_area)<-c("assessment","area","perimeter","x","y","max_width","max_length")
  open_file_area$knapper<-gsub(".csv","",filename)
  open_file_area$knapper<-sapply(strsplit(open_file_area$knapper, split='_', fixed=TRUE), function(x) (x[2]))
  return(open_file_area)
}

##############################
# Size_area function
# Merges and returns size and area data
##############################

  size_area_function<-function(area_names,area_pathway) {
  
  # combine csv files,delete extraneous columns
  handaxe_area_measurements<-do.call(rbind,lapply(area_names,open_read_area,area_pathway))
  col_names_list=c("x","y","max_width")
  handaxe_area_measurements[col_names_list]<-NULL
  
  # restructure dataframe
  handaxe_area_measurements<- handaxe_area_measurements %>%
    select(knapper, assessment,area, perimeter,max_length) %>%
    mutate(area_picture=area*645.16*2, # multiply by 2 to get area for both faces
           perimeter=perimeter*25.4,   # convert inches squared to mm squared
           max_length=max_length*25.4,
           assessment=as.factor(assessment),
           knapper=as.factor(knapper))
  
  # merge shape and area datasets
  test_plan_area_data<-merge_shape_data_function(plan_filenames,
                                                 profile_filenames,
                                                 "Regular_handaxes/Plan",
                                                 "Regular_handaxes/Profile") %>% 
    full_join(handaxe_area_measurements,by=c("assessment","knapper")) %>%
    mutate_if(is.character,as.factor) %>%
    select(assessment,knapper,width_0.1,width_0.2,width_0.3,width_0.4,
           width_0.5,width_0.6,width_0.7,width_0.8,width_0.9,
           thickness_0.1,thickness_0.2,thickness_0.3,thickness_0.4,
           thickness_0.5,thickness_0.6,thickness_0.7,thickness_0.8,
           thickness_0.9,max_length,handaxe_profile_thickness_cv,area_picture,
           perimeter)
  
  merged_plan_area_data<-test_plan_area_data %>%
    mutate(max_thickness=apply(test_plan_area_data[, 12:20], 1, max),
           max_width=apply(test_plan_area_data[, 3:11], 1, max),
           geomean=apply(test_plan_area_data[c(3:21)],1,geometric.mean))
  
  merged_plan_area_data[c(3:21)]=merged_plan_area_data[c(3:21)]/merged_plan_area_data$geomean
  merged_plan_area_data$geomean<-NULL
  
  return(merged_plan_area_data)
}

  test<-size_area_function(area_filenames,"Regular_handaxes/Area")

##############################
# Add asymmetry function
# Adds asymmetry data to size/shape data
# Returns size and asymmetry data
##############################

  add_asymmetry_function<-function(asymmetry_data) {
  
  test<-asymmetry_data %>%
    spread(view, asymetry_index) %>%
    rename(plan_asymetry_index = plan,
           profile_asymetry_index = profile) %>%
    mutate_if(is.integer,as.factor) %>%
    mutate_if(is.character,as.factor) %>%
    subset(knapper %in% c("1","2","3","5","6",
                          "7","8","9","10","11",
                          "13","14","15","16",
                          "17","19","21"))
  
  test_2<-size_area_function(area_filenames,"Regular_handaxes/Area") %>%
  mutate_if(is.character,as.factor)
  
  # create merged asymmetry shape data
  merged_plan_area_asymmetry_data<-test %>% 
    full_join(test_2,by=c("assessment","knapper")) %>%
    mutate_if(is.character,as.factor) %>%
    mutate(experiment=rep("handaxe", length(knapper)),
           condition=rep("main", length(knapper)))
  
  return(merged_plan_area_asymmetry_data)
}

  test<-add_asymmetry_function(asymmetry)

##############################
# Control Handaxe Shapes function
# Functions below repeate steps above for control handaxes
# Returns control size data for shape analysis
##############################

  merge_control_shape_data_function<-function(control_plan_names,control_profile_names,control_plan_pathway,control_profile_pathway) {
  
  # combine txt files for each recording set
  control_handaxe_plan_measurements<-do.call(rbind,lapply(control_plan_names,open_read,control_plan_pathway))
  control_handaxe_profile_measurements<-do.call(rbind,lapply(control_profile_names,open_read,control_profile_pathway))
  
  # clean and reshape plan data 
  control_handaxe_plan_measurements_reshape<-control_handaxe_plan_measurements %>%
    mutate(variable=dplyr::recode(control_handaxe_plan_measurements$variable, Width = "width"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_width_mm=measurement*10) %>%
    select(-c(variable,measurement)) %>%
    separate(handaxe_number, c("knapper", "assessment"), sep="_") %>%
    dcast(knapper + assessment ~ measurement_point,value.var='shape_width_mm')
  
  # clean and reshape profile data 
  control_handaxe_profile_measurements_reshape<-control_handaxe_profile_measurements %>%
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*10) %>%
    select(-c(variable,measurement)) %>%
    separate(handaxe_number, c("knapper", "assessment"), sep="_") %>%
    dcast(knapper + assessment ~ measurement_point,value.var='shape_thickness_mm')
  
  control_handaxe_profile_measurements_reshape<-control_handaxe_profile_measurements_reshape %>%
    mutate(handaxe_profile_thickness_cv=apply(control_handaxe_profile_measurements_reshape[,c(3:11)], 1, cv))
  
  # create merged shape data
  merged_control_shape_data<-control_handaxe_plan_measurements_reshape %>% 
    full_join(control_handaxe_profile_measurements_reshape,by=c("assessment","knapper")) %>%
    mutate_if(is.character,as.factor)
  
  return(merged_control_shape_data)
}

  test<-merge_control_shape_data_function(control_plan_filenames,
                                        control_profile_filenames,
                                        "Control_handaxes/Plan",
                                        "Control_handaxes/Profile")
  
##############################
# Control size area function
# Merges size and area data for control handaxes
# Returns control size area data file
##############################

  control_size_area_function<-function(area_data) {
  
  # restructure dataframe
  control_handaxe_area_measurements<- area_data %>%
    rename(max_length = Height,
           area=Area,
           perimeter=Perimeter) %>%
    select(knapper, assessment,area, perimeter,max_length) %>%
    mutate(area_picture=area*100*2, # multiply by 2 to get area for both faces
           perimeter=perimeter*10,   # convert cm squared to mm squared
           max_length=max_length*10,
           assessment=as.factor(assessment),
           knapper=as.factor(knapper))
  
  # merge shape and area datasets
  control_test_plan_area_data<-merge_control_shape_data_function(control_plan_filenames,
                                                                 control_profile_filenames,
                                                                 "Control_handaxes/Plan",
                                                                 "Control_handaxes/Profile") %>% 
    full_join(control_handaxe_area_measurements,by=c("assessment","knapper")) %>%
    mutate_if(is.character,as.factor) %>%
    select(assessment,knapper,width_0.1,width_0.2,width_0.3,width_0.4,
           width_0.5,width_0.6,width_0.7,width_0.8,width_0.9,
           thickness_0.1,thickness_0.2,thickness_0.3,thickness_0.4,
           thickness_0.5,thickness_0.6,thickness_0.7,thickness_0.8,
           thickness_0.9,max_length,handaxe_profile_thickness_cv,area_picture,
           perimeter)
  
  merged_control_plan_area_data<-control_test_plan_area_data %>%
    mutate(max_thickness=apply(control_test_plan_area_data[, 12:20], 1, max),
           max_width=apply(control_test_plan_area_data[, 3:11], 1, max),
           geomean=apply(control_test_plan_area_data[c(3:21)],1,geometric.mean))
  
  merged_control_plan_area_data[c(3:21)]=merged_control_plan_area_data[c(3:21)]/merged_control_plan_area_data$geomean
  merged_control_plan_area_data$geomean<-NULL
  
  return(merged_control_plan_area_data)
}

  test<-control_size_area_function(control_area_data)

##############################
# Control add asymmetry function
# Adds asymmetry data to size/shape data
# Returns control size and area data file
##############################

  control_add_asymmetry_function<-function(control_asymmetry_data) {
  
  test<-control_asymmetry_data %>%
    spread(view, asymetry_index) %>%
    rename(plan_asymetry_index = plan,
           profile_asymetry_index = profile) %>%
    mutate_if(is.integer,as.factor)
  
  # create merged asymmetry shape data
  control_merged_plan_area_asymmetry_data<-test %>% 
    full_join(control_size_area_function(control_area_data),by=c("assessment","knapper")) %>%
    mutate_if(is.character,as.factor) %>%
    mutate(experiment=rep("handaxe", length(knapper)),
           condition=rep("control", length(knapper))) 
  
  return(control_merged_plan_area_asymmetry_data)
}

  test<-control_add_asymmetry_function(control_asymetry)

##############################
# Merge control and main handaxe data
# Returns merged control and main handaxe datafile
##############################

  merge_control_main_handaxe_data_function<-function(asymmetry_data,control_asymmetry_data) {

merged_plan_area_asymetry_data<-add_asymmetry_function(asymmetry)
merged_control_plan_area_asymetry_data<-control_add_asymmetry_function(control_asymmetry_data)

complete_handaxe_experiment_shape_data<-rbind(merged_plan_area_asymetry_data,merged_control_plan_area_asymetry_data) %>%
  select(experiment,condition,assessment,knapper,
         width_0.1,width_0.2,width_0.3,width_0.4,width_0.5,width_0.6,width_0.7,width_0.8,width_0.9,
         thickness_0.1,thickness_0.2,thickness_0.3,thickness_0.4,thickness_0.5,thickness_0.6,           
         thickness_0.7,thickness_0.8,thickness_0.9,
         max_length,max_width,max_thickness,
         handaxe_profile_thickness_cv,
         area_picture, perimeter, 
         plan_asymetry_index,profile_asymetry_index)

return(complete_handaxe_experiment_shape_data)

}

  test<-merge_control_main_handaxe_data_function(asymmetry,control_asymetry)

##############################
# Expert Merge shape data function
# Merges size data for expert shape analysis
# Returns expert size data
##############################

  expert_merge_shape_data_function<-function(plan_names,profile_names,plan_pathway,profile_pathway) {
  
  # combine txt files for each recording set
  handaxe_plan_measurements<-do.call(rbind,lapply(plan_names,open_read,plan_pathway))
  handaxe_profile_measurements<-do.call(rbind,lapply(profile_names,open_read,profile_pathway))
  
  # clean and reshape plan data 
  handaxe_plan_measurements_reshape<-handaxe_plan_measurements %>%
    mutate(variable=dplyr::recode(handaxe_plan_measurements$variable, Width = "width"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_width_mm=measurement*10) %>%
    rename(knapper=handaxe_number) %>%
    select(-c(variable,measurement)) %>%
    dcast(knapper ~ measurement_point,value.var="shape_width_mm")
  
  # clean and reshape profile data 
  handaxe_profile_measurements_reshape<-handaxe_profile_measurements %>%
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*10) %>%
    select(-c(variable,measurement)) %>%
    rename(knapper=handaxe_number) %>%
    dcast(knapper ~ measurement_point,value.var="shape_thickness_mm")
  
  handaxe_profile_measurements_reshape<-handaxe_profile_measurements_reshape %>%
    mutate(handaxe_profile_thickness_cv=apply(handaxe_profile_measurements_reshape[,c(2:10)], 1, cv))
  
  # create merged shape data
  merged_expert_shape_data<-handaxe_plan_measurements_reshape %>% 
    full_join(handaxe_profile_measurements_reshape,by=c("knapper")) %>%
    mutate_if(is.character,as.factor)
  
  return(merged_expert_shape_data)
}

  test<-expert_merge_shape_data_function(expert_plan_filenames,expert_profile_filenames,"Expert_handaxes/Plan","Expert_handaxes/Profile")

##############################
# Expert Open Read Area function
# Calls expert area and size csv files into R
##############################

  expert_open_read_area<-function(filename, foldername){
  open_file_area<-read.csv(paste(foldername,"/",filename,sep=""),header=T)
  colnames(open_file_area)<-c("knapper","area","perimeter","x","y","max_width","max_length")
  return(open_file_area)
}

##############################
# Expert Size_area function
# Merges expert size and area data
# Returns expert size and area data
##############################

  expert_size_area_function<-function(area_names,area_pathway) {
  
  # combine csv files,delete extraneous columns
  handaxe_area_measurements<-do.call(rbind,lapply(area_names,expert_open_read_area,area_pathway))
  col_names_list=c("x","y","max_width")
  handaxe_area_measurements[col_names_list]<-NULL
  
  # restructure dataframe
  handaxe_area_measurements<- handaxe_area_measurements %>%
    rename(area_picture='area') %>%
    select(knapper, area_picture, perimeter,max_length) %>%
    mutate(area=area_picture*100*2, # multiply by 2 to get area for both faces
           perimeter=perimeter*10,   # convert cm to mm squared
           max_length=max_length*10,
           knapper=as.factor(knapper))
  
  # merge shape and area datasets
  test_plan_area_data<-expert_merge_shape_data_function(expert_plan_filenames,
                                                        expert_profile_filenames,
                                                        "Expert_handaxes/Plan",
                                                        "Expert_handaxes/Profile") %>% 
    mutate_if(is.character,as.factor) %>%
    full_join(handaxe_area_measurements,by=c("knapper"))  %>%
    select(knapper,width_0.1,width_0.2,width_0.3,width_0.4,
           width_0.5,width_0.6,width_0.7,width_0.8,width_0.9,
           thickness_0.1,thickness_0.2,thickness_0.3,thickness_0.4,
           thickness_0.5,thickness_0.6,thickness_0.7,thickness_0.8,
           thickness_0.9,max_length,area,
           perimeter)
  
  expert_merged_plan_area_data<-test_plan_area_data %>%
    mutate(max_thickness=apply(test_plan_area_data[, 12:20], 1, max),
           max_width=apply(test_plan_area_data[, 3:11], 1, max),
           geomean=apply(test_plan_area_data[c(3:21)],1,geometric.mean))
  
  expert_merged_plan_area_data[c(3:20)]=expert_merged_plan_area_data[c(3:20)]/expert_merged_plan_area_data$geomean
  expert_merged_plan_area_data$geomean<-NULL
  
  return(expert_merged_plan_area_data)
}

  test<-expert_size_area_function(expert_area_filenames,"Expert_handaxes/Area")

##############################
# Add expert asymmetry function
# Adds expert asymmetry data to size/shape data
# Returns expert asymmetry and size data
##############################

  expert_add_asymmetry_function<-function(asymmetry_data) {
  
  test<-asymmetry_data %>%
    spread(view, asymetry_index) %>%
    rename(plan_asymetry_index = plan,
           profile_asymetry_index = profile) %>%
    mutate_if(is.integer,as.factor) %>%
    mutate_if(is.character,as.factor)
  
  test_2<-expert_size_area_function(expert_area_filenames,"Expert_handaxes/Area") %>%
    mutate_if(is.character,as.factor)
  
  # create merged asymmetry shape data
  merged_expert_plan_area_asymetry_data<-test %>% 
    full_join(test_2,by=c("knapper")) %>%
    mutate_if(is.character,as.factor) %>%
    mutate(experiment=rep("handaxe", length(knapper)),
           condition=rep("main", length(knapper)))
  
  return(merged_expert_plan_area_asymetry_data)
}

  test<-expert_add_asymmetry_function(asymmetry_expert)

##########################################################################################
# Cores data
##########################################################################################

##############################
# Cores Open Read function
# Calls core txt files into R
##############################

  open_read_cores<-function(filename, foldername){
  open_file<-read.delim(paste(foldername,"/",filename,sep=""),header=F,sep='_')
  colnames(open_file)<-c("variable","measurement_point","measurement")
  open_file$Core.Number<-gsub(".txt","",filename)
  return(open_file)
}

##############################
# Cores Merge size data function
# Merges regular and control core size data
# Returns regular and control core data
##############################

  core_merge_size_data_function<-function(main_profile_names,
                                        main_profile_pathway,
                                        control_profile_names,
                                        control_profile_pathway) {
  
  # main combine txt files for each recording set
  core_profile_measurements<-do.call(rbind,lapply(main_profile_names,open_read_cores,main_profile_pathway))
  
  # main clean and reshape core profile data 
  core_profile_measurements_reshape<-core_profile_measurements %>%
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*10,
           Core.Number=as.numeric(Core.Number),
           Core.Number = paste('h', core_profile_measurements$Core.Number, sep='_')) %>%
    select(-c(variable,measurement)) %>%
    dcast(... ~ measurement_point,value.var="shape_thickness_mm")
  
  core_profile_measurements_reshape<-core_profile_measurements_reshape %>%
    mutate(core_profile_thickness_cv=apply(core_profile_measurements_reshape[,c(2:10)], 1, cv))
  
  # control combine txt files for each recording set
  control_core_profile_measurements<-do.call(rbind,lapply(control_profile_names,open_read_cores,control_profile_pathway))
  
  # control clean and reshape core profile data 
  control_core_profile_measurements_reshape<-control_core_profile_measurements %>%
    mutate(variable=dplyr::recode(variable, Width = "thickness"),
           measurement_point = paste(variable, measurement_point, sep="_"),
           shape_thickness_mm=measurement*10,
           Core.Number=as.numeric(Core.Number),
           Core.Number = paste('h', control_core_profile_measurements$Core.Number, sep='_')) %>%
    select(-c(variable,measurement)) %>%
    dcast(... ~ measurement_point,value.var="shape_thickness_mm")
  
  control_core_profile_measurements_reshape<-control_core_profile_measurements_reshape %>%
    mutate(core_profile_thickness_cv=apply(control_core_profile_measurements_reshape[,c(2:10)], 1, cv))
  
  # merge datasets
  core_profile_measurements_reshape<-merge(core_profile_measurements_reshape,control_core_profile_measurements_reshape, all=T )
  
}

  test<-core_merge_size_data_function(core_profile_filenames,
                                    "Cores/Profile",
                                    control_core_profile_filenames,
                                    "Cores/Control_profile")

##########################################################################################
# Join handaxe/cores shape data to attributes and prediction core data
# And trainer handaxe skill scores data
##########################################################################################

##############################
# Merge handaxe and prediction core numbers, scores, and attribute data
# Deletes controls not scored
# Removes cores not used in the experiment
# Returns complete experiment_experiment_data
##############################

  merge_handaxe_prediction_controls_core_score_function<-function(
  handaxe_core_numbers,
  prediction_core_numbers,
  handaxe_start,
  prediction_start,
  handaxe_end,
  prediction_end,
  control_data,
  main_attributes,
  control_attributes) {
  
  # Add designators
  handaxe_core_numbers$Core.Number = paste('h', handaxe_core_numbers$Core.Number, sep='_')
  prediction_core_numbers$Core.Number = paste('p', prediction_core_numbers$Core.Number, sep='_')
  
  # Merge core numbers_scores files
  merged_experiment_data<-bind_rows(handaxe_core_numbers, prediction_core_numbers) %>%
    mutate(Core.Number=as.factor(Core.Number),
           Knapper=as.factor(Knapper),
           Assessment=as.factor(Assessment),
           condition=rep("main",length(Core.Number))) %>%
    rename(experiment='Experiment')
  
  # Core weights: Add designators
  
  handaxe_start$Core.Number = paste('h', handaxe_start$Core.Number, sep='_')
  handaxe_start$experiment<-rep("handaxe",length(handaxe_start$Core.Number))
  
  prediction_start$Core.Number = paste('p', prediction_start$Core.Number, sep='_')
  prediction_start$experiment<-rep("prediction",length(prediction_start$Core.Number))
  
  merged_start_weight_data<-
    bind_rows(handaxe_start, prediction_start)
  
  # Remove cores not used in the experiment
  
  cores_not_used<-c('h_77','h_84','h_97','h_101','h_105','h_106','h_113','h_114',
                    'h_115','h_118','h_124','h_125','h_126','h_129','h_132','h_141',
                    'h_143','h_144','h_146','h_147','h_149','h_150','p_34','p_85',
                    'p_92','p_103','p_104','p_114','p_116','p_117','p_119','p_122',
                    'p_123','p_124','p_125','p_126','p_131','p_133','p_138','p_140',
                    'p_141','p_142','p_145','p_146','p_147','p_148','p_149')
  
  merged_start_weight_data<-merged_start_weight_data[!merged_start_weight_data$Core.Number 
                                                     %in% cores_not_used,]
  
  # End weights: Add designators
  handaxe_end$Core.Number = paste('h', handaxe_end$Core.Number, sep='_')
  prediction_end$Core.Number = paste('p', prediction_end$Core.Number, sep='_')
  
  merged_end_weight_data<-
    bind_rows(handaxe_end, prediction_end) %>%
    rename(experiment='Experiment')
  
  # Merge handaxe and prediction core start and end weights
  merged_start_end_weight_data<-
    full_join(merged_start_weight_data,merged_end_weight_data, 
              by=c("Core.Number","experiment")) %>%
    rename(Core.start.weight='Weight') %>%
    mutate(condition=rep("main",length(Core.Number)),
           Knapper=as.factor(Knapper),
           Assessment=as.factor(Assessment))
  
  # Merge with pre-experiment handaxe data above
  
  merged_experiment_weight_data<-
    full_join(merged_start_end_weight_data,
              merged_experiment_data,
              by=c("Knapper","Assessment","experiment","condition")) %>%
    select(-c(Core.Number.y,Core.starting.weight)) %>%
    select(Knapper:Assessment,experiment,condition,
           Core.Number.x,Score,Training.Hours,
           Core.start.weight,
           Core.end.weight) %>%
    rename(Core.Number="Core.Number.x") 
  
  # Add controls
  
  control_data<-control_data %>%
    mutate(Knapper=as.factor(Knapper),
           Assessment=as.factor(Assessment),
           Core.Number=as.factor(Core.Number),
           Core.Number=ifelse(Experiment == "handaxe",
                              paste("h",control_experiment_data$Core.Number,sep = "_"),
                              paste("p",control_experiment_data$Core.Number,sep = "_"))) %>%
    rename(experiment="Experiment",
           condition="Condition") 
  
  #Merge with data from above 
  
  complete_handaxe_experiment_data<-
    full_join(merged_experiment_weight_data, control_data, 
              by=c("Knapper","Assessment",
                   "Score","Core.Number","condition",
                   "experiment","Core.start.weight",
                   "Core.end.weight")) %>%
    rename(knapper="Knapper",
           assessment="Assessment") %>%
    mutate(Core.Number=as.factor(Core.Number),
           condition = dplyr::recode(condition, Control = "control"))
  
  #Merge shape and experiment data
  
  complete_experiment_data<-
    full_join(merge_control_main_handaxe_data_function(asymmetry,control_asymetry), 
              complete_handaxe_experiment_data, 
              by=c("knapper","assessment","condition","experiment")) %>%
    mutate(delta_weight=Core.end.weight/Core.start.weight*100,
           delta_weight=ifelse(delta_weight > 100,100,delta_weight)) %>%
    select(knapper, assessment, experiment,condition,
           Training.Hours,Score,Core.Number,delta_weight,
           width_0.1,width_0.2,width_0.3,width_0.4,
           width_0.5,width_0.6,width_0.7,width_0.8,width_0.9,
           thickness_0.1,thickness_0.2,thickness_0.3,
           thickness_0.4,thickness_0.5,thickness_0.6,           
           thickness_0.7,thickness_0.8,thickness_0.9,
           handaxe_profile_thickness_cv,profile_asymetry_index,plan_asymetry_index,
           area_picture,perimeter,max_length, max_width,max_thickness)
  
  #delete rows for subjects who withdrew
  
  to_remove<- which(with(complete_experiment_data, 
                         (knapper==12 & assessment==1) | 
                           (knapper==14 & assessment==3) |
                           (knapper==20 & assessment==1) |
                           (knapper==20 & assessment==2) |
                           (knapper==4 & assessment==1)))
  
  complete_experiment_data<-complete_experiment_data[-to_remove,]
  
  # Add attribute data
  
  attribute_data<-full_join(main_attributes,control_attributes,
                            by = c("knapper", "assessment", 
                                   "scar.count", 
                                   "unflaked.area_mm2", 
                                   "bifacial.extent_mm")) %>%
    mutate(knapper=as.factor(knapper),
           assessment=as.factor(assessment))
  
  # Join to full experimental data and calculate vars
  
  all_experiment_data<-full_join(complete_experiment_data, attribute_data, 
                                 by=c("knapper","assessment")) %>%
    mutate(Training.Hours=ifelse(Training.Hours.x %in% NA,
                                 Training.Hours.y,
                                 Training.Hours.x),
           area_box=(max_length*max_width*max_thickness)*2+
             (max_length*max_thickness)*2+
             (max_thickness*max_width)*2,
           percent_unflaked_area=(unflaked.area_mm2/area_picture)*100,
           percent_bifacially_flaked=(bifacial.extent_mm/perimeter)*100,
           flake_scar_density=scar.count/area_box) %>%
    select(-c(scar.count,unflaked.area_mm2,bifacial.extent_mm,
              area_picture,area_box,perimeter,
              Training.Hours.x,Training.Hours.y)) 
  
  # Add core profile measurements and merge with all experiment data from above
  
  core_thickness_cv<-core_merge_size_data_function(core_profile_filenames,
                                                   "Cores/Profile",
                                                   control_core_profile_filenames,
                                                   "Cores/Control_profile") %>%
    select(Core.Number,core_profile_thickness_cv)
  
  all_experiment_data_plus_corecv<-full_join(all_experiment_data,core_thickness_cv,
                                                     by=c('Core.Number')) %>%
    subset(!Core.Number %in% c('h_104','h_144','h_147','h_150') & !knapper == "33") %>%
    subset(experiment=="handaxe") %>%
    mutate(core_profile_thickness_cv=as.numeric(as.character(core_profile_thickness_cv)),
           handaxe_profile_thickness_cv=as.numeric(as.character(handaxe_profile_thickness_cv)),
           delta_profile_thickness_cv=core_profile_thickness_cv-handaxe_profile_thickness_cv) %>%
    select(-c(handaxe_profile_thickness_cv,core_profile_thickness_cv))
  
  return(all_experiment_data_plus_corecv)
}

  test<-merge_handaxe_prediction_controls_core_score_function(handaxe_core_numbers_scores,
                                                            prediction_core_numbers_scores,
                                                            handaxe_start_weight,
                                                            prediction_start_weight,
                                                            handaxe_end_weight,
                                                            prediction_end_weight,
                                                            control_experiment_data,
                                                            main_attribute_data,
                                                            control_attribute_data)

##############################
# Do core numbers match handaxe numbers function
# Checks if core numbers match in experiment and cores datasets
##############################

  do_numbers_match_function<-function(function_1,function_2) {
  
  
  s<-function_1(handaxe_core_numbers_scores, prediction_core_numbers_scores,
                                                        handaxe_start_weight,
                                                        prediction_start_weight,
                                                        handaxe_end_weight,
                                                        prediction_end_weight,
                                                        control_experiment_data,
                                                        main_attribute_data,
                                                        control_attribute_data) %>%
    subset(experiment=="handaxe") %>%
    select(Core.Number) %>%
    mutate(Core.Number=as.factor(Core.Number))
  

  s_core<-function_2(core_profile_filenames,
                                        "Cores/Profile",
                                        control_core_profile_filenames,
                                        "Cores/Control_profile") %>%
    select(Core.Number) %>%
    mutate(Core.Number=as.factor(Core.Number)) %>%
    distinct(Core.Number)

different_core_numbers<-s_core$Core.Number[!(s_core$Core.Number %in% s$Core.Number)]

return(different_core_numbers)

}

  test<-do_numbers_match_function(merge_handaxe_prediction_controls_core_score_function,
                                core_merge_size_data_function)

##############################
# Add experts
# Add and merge expert data
# Returns merged expert/novice data
##############################

  expert_shape_attribute_function<-function(function_1) {

  # Return expert shape/asymmetry data and join to core/handaxe weights
  expert_shape_asymetry<-function_1(asymmetry_expert)

  expert_shape_asymetry_weight_data<-full_join(expert_shape_asymetry, 
                                             expert_merged_start_end_weight_data, 
                                             by=c("knapper","condition"))

 # Add attribute data and join to the above data

  set.seed(2016)
  expert_complete_experiment_data<-full_join(expert_shape_asymetry_weight_data, 
                                       expert_handaxe_attribute_data, 
                                       by=c("knapper","assessment")) %>%
                                 mutate(assessment=as.factor(assessment),
                                        condition=as.factor(condition),
                                        experiment=as.factor(experiment),
                                        percent_unflaked_area=(unflaked.area..mm2./area)*100,
                                        percent_bifacially_flaked=sample(seq(from = 98, to = 100, by = 0.1), size = 10, replace = TRUE),
                                        flake_scar_density=scar.count/area) %>%
                                select(-c(scar.count,unflaked.area..mm2.,bifacial.extent..mm.,
                                          area,perimeter))
  str(expert_complete_experiment_data)

  return(expert_complete_experiment_data)

}

  test<-expert_shape_attribute_function(expert_add_asymmetry_function)

#####################################################
# Perform principal components analysis of handaxe shape data
#####################################################

##############################
# Perform several PCA sub-functions
# Returns scaled handaxe data (handaxe postpca scaled)
# Can select optional plot and data returns
##############################
 
  handaxe_pca_function<-function(function_1) {

  handaxe_data<-merge_handaxe_prediction_controls_core_score_function(handaxe_core_numbers_scores,
                                                      prediction_core_numbers_scores,
                                                      handaxe_start_weight,
                                                      prediction_start_weight,
                                                      handaxe_end_weight,
                                                      prediction_end_weight,
                                                      control_experiment_data,
                                                      main_attribute_data,
                                                      control_attribute_data)
  
  # Subtract data for PCA
  
  handaxe_shapes<- handaxe_data[ , which(names(handaxe_data) %in% 
                       c("knapper","assessment","width_0.1","width_0.2","width_0.3","width_0.4",
                       "width_0.5","width_0.6","width_0.7","width_0.8","width_0.9","thickness_0.1",
                       "thickness_0.2","thickness_0.3","thickness_0.4","thickness_0.5","thickness_0.6",
                       "thickness_0.7","thickness_0.8","thickness_0.9","max_length"))]
  
  knapper <- handaxe_shapes[, 1]
  assessment <- handaxe_shapes[, 2]
  
  # Calculate PCA
  
  handaxe_pca<-prcomp(handaxe_shapes[,-c(1:2)], scale. = T)
  
  # Compute standard deviation of each principal component
  
  std_dev<- handaxe_pca$sdev
  
  # Compute variance
  
  pr_var <- std_dev^2
  prop_varex <- pr_var/sum(pr_var)
  
  # Scree plot
  
  pc_scree_plot<-plot(prop_varex, xlab = "Principal Component",
            ylab = "Proportion of Variance Explained",
            type = "b")
  
  # Cumulative scree plot
  
  pc_cumulative_plot<-plot(cumsum(prop_varex), xlab = "Principal Component",
             ylab = "Cumulative Proportion of Variance Explained",
             type = "b")
  
  # Graph of individuals
  
  pc_individuals_plot<-fviz_pca_var(handaxe_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
  
  # Graph of variables
  
  pc_vars_plot<-fviz_pca_ind(handaxe_pca,
                               col.ind = "cos2", # Color by the quality of representation
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                               repel = TRUE)
  
  # Graph of overall contributions
  
  pc1_contribution_plot<-fviz_contrib(handaxe_pca, choice = "var", axes = 1, top = 10)
  pc2_contribution_plot<-fviz_contrib(handaxe_pca, choice = "var", axes = 2, top = 10)
  
  ###Access PCA results###
  
  # Eigenvalues
  
  eig.val <- get_eigenvalue(handaxe_pca)
  
  # Results for Variables
  
  res.var <- get_pca_var(handaxe_pca)
  variable_loadings<-data.frame(res.var$coord)         # Coordinates
  
  # Results for individuals
  
  res.ind <- get_pca_ind(handaxe_pca)
  Indiv_handaxe_scores<-data.frame(res.ind$coord)        # Coordinates
  
  # Join individual scores back to complete handaxe data and delete width and thickness measurements
  
  test_1<-handaxe_data
  test_1$fake_number<-1:nrow(test_1)
  Indiv_handaxe_scores$fake_number<-1:nrow(Indiv_handaxe_scores)
  
  handaxe_postpca<-merge(Indiv_handaxe_scores[,c("Dim.1","Dim.2","fake_number")],
                         test_1,
                         by="fake_number") %>%
                   select(knapper, assessment, experiment,condition,Core.Number,
                          Score,Training.Hours,delta_weight,
                          Dim.1, Dim.2,
                          profile_asymetry_index,plan_asymetry_index,
                          percent_unflaked_area,percent_bifacially_flaked,
                          flake_scar_density,
                          delta_profile_thickness_cv)
  
  # Scale data

  handaxe_postpca_scaled<-handaxe_postpca
  
  vars=c("Dim.1","Dim.2","delta_weight","profile_asymetry_index","plan_asymetry_index",
         "percent_unflaked_area","percent_bifacially_flaked",
         "flake_scar_density","delta_profile_thickness_cv")
  
  handaxe_postpca_scaled[,vars] <- scale(handaxe_postpca[,vars])
  
  return(handaxe_postpca_scaled)
}
  
  test<- handaxe_pca_function(merge_handaxe_prediction_controls_core_score_function) #for main and control handaxes

##############################
# Perform several PCA sub-functions on expert data using main data PC co-ordinates
# Returns scaled expert handaxe data (expert complete reduced scaled)
# Can select optional plot and data returns
##############################

  expert_handaxe_pca_function<-function(function_1, function_2) {
  
  handaxe_data<-function_1(handaxe_core_numbers_scores,
                           prediction_core_numbers_scores,
                           handaxe_start_weight,
                           prediction_start_weight,
                           handaxe_end_weight,
                           prediction_end_weight,
                           control_experiment_data,
                           main_attribute_data,
                           control_attribute_data)
  
  # Subtract data for PCA
  
  handaxe_shapes<- handaxe_data[ , which(names(handaxe_data) %in% 
                                           c("knapper","assessment","width_0.1","width_0.2","width_0.3","width_0.4",
                                             "width_0.5","width_0.6","width_0.7","width_0.8","width_0.9","thickness_0.1",
                                             "thickness_0.2","thickness_0.3","thickness_0.4","thickness_0.5","thickness_0.6",
                                             "thickness_0.7","thickness_0.8","thickness_0.9","max_length"))]
  
  knapper <- handaxe_shapes[, 1]
  assessment <- handaxe_shapes[, 2]
  
  ## Calculate PCA for main handaxe data
  
  handaxe_pca<-prcomp(handaxe_shapes[,-c(1:2)], scale. = T)
  
  # Results for individuals
  
  res.ind <- get_pca_ind(handaxe_pca)
  Indiv_handaxe_scores<-data.frame(res.ind$coord)        # Coordinates
  
  ## Calculate PCA scores for expert handaxes based on main handaxe data
  
  # Bring in expert handaxe data
  
  expert_handaxe_data<-function_2(expert_add_asymmetry_function)
  
  experts_shapes<- expert_handaxe_data[ , which(names(expert_handaxe_data) %in% 
                                            c("width_0.1","width_0.2","width_0.3","width_0.4",
                                            "width_0.5","width_0.6","width_0.7","width_0.8","width_0.9","thickness_0.1",
                                            "thickness_0.2","thickness_0.3","thickness_0.4","thickness_0.5","thickness_0.6",
                                            "thickness_0.7","thickness_0.8","thickness_0.9","max_length"))]
  
  # Create main handaxe postpca data
  
  test_1<-handaxe_data
  test_1$fake_number<-1:nrow(test_1)
  Indiv_handaxe_scores$fake_number<-1:nrow(Indiv_handaxe_scores)
  
  handaxe_postpca<-merge(Indiv_handaxe_scores[,c("Dim.1","Dim.2","fake_number")],
                         test_1,
                         by="fake_number") %>%
    select(knapper, assessment, experiment,condition,Core.Number,
           Score,Training.Hours,delta_weight,
           Dim.1, Dim.2,
           profile_asymetry_index,plan_asymetry_index,
           percent_unflaked_area,percent_bifacially_flaked,
           flake_scar_density,
           delta_profile_thickness_cv)
  
  # Compute expert factor scores
  
  expert_handaxe_scores<-data.frame(scale(experts_shapes, 
                                          handaxe_pca$center, 
                                          handaxe_pca$scale) %*% 
                                      handaxe_pca$rotation)
  
  # Join experts to main data for scaling
  
  test_1<-expert_handaxe_data
  test_1$fake_number<-1:nrow(test_1)
  expert_handaxe_scores$fake_number<-1:nrow(expert_handaxe_scores)
  
  expert_handaxe_postpca<-merge(expert_handaxe_scores[,c("PC1","PC2","fake_number")],
                                test_1,
                                by="fake_number") %>%
                          rename(Dim.1="PC1",
                                 Dim.2="PC2") %>%
                          mutate(experiment="handaxe",length(fake_number),
                                 assessment=rep("10",length(fake_number)),
                                 Training.Hours=rep(100,length(fake_number)),
                                 Score=rep(5,length(fake_number))) %>%
                          select(knapper, experiment,Score,
                                 Training.Hours,Dim.1, Dim.2)
  
  #merge together with attributes data
  
  expert_complete_experiment_data_merged<-merge(expert_handaxe_postpca, expert_handaxe_data, 
                                                by=c("knapper","experiment"), all = T) %>%
                                          mutate(condition=rep("expert",length(assessment))) %>%
                                          select(knapper, assessment, experiment,condition,
                                            Score,Training.Hours,delta_weight,
                                            Dim.1, Dim.2,
                                            profile_asymetry_index,plan_asymetry_index,
                                            percent_unflaked_area,percent_bifacially_flaked,
                                            flake_scar_density)   ##NO DELTA PROFILE THICKNESS FOR EXPERTS

  #scale data
  
  handaxe_postpca_reduced<-
    handaxe_postpca%>%
    select(-c(Core.Number,delta_profile_thickness_cv))
  
  expert_complete_experiment_data_reduced<-rbind(expert_complete_experiment_data_merged, handaxe_postpca_reduced)

  expert_complete_experiment_data_reduced_scaled<-expert_complete_experiment_data_reduced
  
  vars=c("Dim.1","Dim.2","delta_weight","profile_asymetry_index","plan_asymetry_index",
         "percent_unflaked_area","percent_bifacially_flaked",
         "flake_scar_density")
  
  expert_complete_experiment_data_reduced_scaled[,vars] <- scale(expert_complete_experiment_data_reduced_scaled[,vars])
  
  expert_complete_experiment_data_reduced_scaled<-subset(
    expert_complete_experiment_data_reduced_scaled, condition=="expert")
  
  return(expert_complete_experiment_data_reduced_scaled)
}

  test<- expert_handaxe_pca_function(merge_handaxe_prediction_controls_core_score_function,expert_shape_attribute_function)
  
#############################################################################
# Random Forest Regressions
# Run random forest regression models on handaxe data to predict skill scores
#############################################################################

##############################
# Random forest modeler
# Takes in post-PCA handaxe data from above
# Subsets outliers
# Creates test and training data
# Runs random forest models
# Returns predicted skill scores and second predictive model (fit_2)
##############################

  random_forest_modeler<-function(function_1){

  ## Create handaxe scaled post PCA data

  handaxe_postpca_scaled<-function_1(merge_handaxe_prediction_controls_core_score_function)
  handaxe_postpca_scaled<-subset(handaxe_postpca_scaled,!knapper=="33")

  ## Detect and remove outliers using cook's distance

  score_model <- lm(Score ~ Dim.1+Dim.2+profile_asymetry_index+
                    plan_asymetry_index+delta_profile_thickness_cv+
                    percent_unflaked_area+delta_weight+
                    percent_bifacially_flaked+
                    flake_scar_density, 
                    data=handaxe_postpca_scaled)

  cooksd <- cooks.distance(score_model)

  # Plot cook's distance

  plot_1<-plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  
  abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  text(x=1:length(cooksd)+1, y=cooksd, 
       labels=ifelse(cooksd>2*mean(cooksd, na.rm=T),
                     names(cooksd),""), 
                     col="red")

  # Subset data to remove overly influential handaxes
  
  handaxe_postpca_scaled$cooks_distance<-abs(cooksd)

  handaxe_postpca_scaled_minus_outlier<-subset(handaxe_postpca_scaled,cooksd < 0.04)

  # Check which are overly influential handaxes

  outlier_handaxes<-subset(handaxe_postpca_scaled,cooksd > 0.04)

  ### Random forest model building

  # Create test and training data
  # Set sampling seed for reproducible results

  set.seed(425)

  sample = sample.split(handaxe_postpca_scaled_minus_outlier$knapper, SplitRatio = .70)
  train = subset(handaxe_postpca_scaled_minus_outlier, sample == TRUE)
  test  = subset(handaxe_postpca_scaled_minus_outlier, sample == FALSE)

  # Compare test and training outcome variable (Score) distributions
  # Test to see if distributions are significantly different

  plot_2<-hist(train$Score)
  plot_3<-hist(test$Score)

  plot_4<-qqplot(train$Score,test$Score,
       xlab="Training data score", ylab="Test data score",
       main="Comparison of test and training data score distributions")

  wilcox.test(train$Score,test$Score)

  ## Random Forest Model 1: All predictor variables

  fit <- randomForest(Score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                    +delta_weight+Dim.1+Dim.2
                    +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                    data=train, 
                    importance=TRUE,  
                    mtry=2,          
                    ntree=10000,
                    keep.inbag = TRUE)

  # Create variable importance plot

  names<-c("Profile Asymetry","Plan Asymetry","Delta Profile Thickness",
         "Delta Weight","Shape PC1","Shape PC2","Percent Unflaked Area","Percent Bifacially Flaked","Flake Scar Density")
  all_var_importance_data<-data.frame(importance(fit,type = 1)) #permutation importance, variable is assigned values by random permutation by how much will the MSE increase.
  all_var_importance_data$predictor<-rownames(all_var_importance_data)
  all_var_importance_data$predictor<- factor(all_var_importance_data$predictor, levels=all_var_importance_data$predictor[order(all_var_importance_data$X.IncMSE)], ordered=TRUE)                                                                                             
  colnames(all_var_importance_data)[1]<-"All_X.IncMSE"     

  variable_importance_plot<-
  ggplot(all_var_importance_data,aes(All_X.IncMSE,predictor)) +
  geom_point(size=4.5,aes(color=predictor)) +
  xlab('Percentage decrease in mean squared error') + ylab('Predictor') +
  scale_colour_manual(values=c('black','black','black','black','black','black','black','black','black'),guide = FALSE)+
  geom_vline(xintercept = 10, linetype="dotted", 
             color = "red", size=1.5)+
  scale_x_continuous(breaks=seq(0,100,10))+
  theme(text = element_text(size=20))

  # Comparison of r2 values for training and test datasets

  actual_train <- train$Score
  predicted_train <- unname(predict(fit))
  R2_train <- 1 - (sum((actual_train-predicted_train)^2)/sum((actual_train-mean(actual_train))^2))

  actual_test <- test$Score
  predicted_test <- unname(predict(fit, test))
  R2_test<- 1 - (sum((actual_test-predicted_test)^2)/sum((actual_test-mean(actual_test))^2))

  # Test on predictions

  Prediction_train <- predict(fit)
  submit_train <- data.frame(Score_observed = train$Score, Score_predicted = Prediction_train, knapper =  train$knapper, assessment = train$assessment)

  fit1_lm<-lm(Score_observed~Score_predicted, data=submit_train)

  Prediction_test <- predict(fit, test)
  submit_test <- data.frame(Score_observed = test$Score, Score_predicted = Prediction_test, knapper =  test$knapper, assessment = test$assessment)

  submit_all <- rbind(submit_train,submit_test)

  error <- submit_all$Score_observed - submit_all$Score_predicted

  # Mean Absolute Error
  mae <- function(error)
  {
  mean(abs(error))
  }

  mean_absolute_error_fit1<-mae(error) 

  # Plot model residuals

  plot_4<-qqnorm((fit$predicted-train$Score)/sd(fit$predicted-train$Score))
  plot_5<-qqline((fit$predicted-train$Score)/sd(fit$predicted-train$Score))

  fit_residuals<-fit$predicted-train$Score
  plot_6<-hist(fit_residuals)

  ## Random Forest Model 2: reduced predictor set

  # Select predictor variables with >10 % reduction in model prediction mean squared error

  reduced_variables<-subset(all_var_importance_data,All_X.IncMSE>10)
  NameList <- reduced_variables$predictor
  train_reduced <- train[,colnames(train) %in% NameList]
  train_reduced$Score<-train$Score

  test_reduced<-test[,colnames(test) %in% NameList]
  test_reduced$Score<-test$Score

  fit_2 <- randomForest(Score ~.,
                      data=train_reduced,
                      mtry=2,
                      importance=TRUE, 
                      ntree=10000,
                      keep.inbag = TRUE)

  actual_train <- train$Score
  predicted_train <- unname(predict(fit_2))
  R2_train <- 1 - (sum((actual_train-predicted_train)^2)/sum((actual_train-mean(actual_train))^2))

  actual_test <- test$Score
  predicted_test <- unname(predict(fit_2, test))
  R2_test<- 1 - (sum((actual_test-predicted_test)^2)/sum((actual_test-mean(actual_test))^2))

  # Error rates
  Prediction_train <- predict(fit_2)
  submit_train <- data.frame(Score_observed = train$Score, Score_predicted = Prediction_train, knapper =  train$knapper, assessment = train$assessment)

  Prediction_test <- predict(fit_2, test)
  submit_test <- data.frame(Score_observed = test$Score, Score_predicted = Prediction_test, knapper =  test$knapper, assessment = test$assessment)

  submit_all_fit_2 <- rbind(submit_train,submit_test)

  submit_all_fit_2_lm <- summary(lm(Score_observed~Score_predicted,data=submit_all_fit_2))

  error_fit_2 <- submit_all_fit_2$Score_observed - submit_all_fit_2$Score_predicted

  mae(error_fit_2)

  # Residuals
  plot_7<-qqnorm((fit_2$predicted-train$Score)/sd(fit_2$predicted-train$Score))
  plot_8<-qqline((fit_2$predicted-train$Score)/sd(fit_2$predicted-train$Score))

  fit2_residuals<-fit_2$predicted-train$Score
  plot_9<-hist(fit2_residuals)

  # Data predictions plotted against observations

  plot_10<-ggplot(submit_all_fit_2,size=1)+
  aes(x = Score_observed,y = Score_predicted) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(name="Observed Score")+
  scale_y_continuous(name="Predicted Score")+
  ggtitle("Predicted vs. Observed Scores")+
  geom_smooth(method = "lm", se=T,na.rm = T)+ 
  theme(text = element_text(size=20))

  # Overall skill to assessment plot

  plot_11<-ggplot(data = submit_all_fit_2)+
  aes(x = assessment,y = Score_predicted) +
  geom_point(alpha = 0.6) +
  scale_y_continuous(name="Predicted Score")+
  scale_x_discrete(name="Assessment")+
  geom_smooth(method = "loess",data = submit_all_fit_2  %>%
                mutate(assessment = as.numeric(assessment)), se=T,na.rm = T,span = 0.5)+
  theme(text = element_text(size=20)) 

  ## Random forest interpreter, return confidence intervals

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
  preds$assessment<-handaxe_postpca_scaled_minus_outlier$assessment
  preds$knapper<-handaxe_postpca_scaled_minus_outlier$knapper

  plot_12<-ggplot(preds,aes(measured,pred)) +
  geom_point(size=2,color="red") +
  #geom_abline(intercept=0,slope=1,lty=2,color='#999999') +
  geom_errorbar(aes(ymin=l.ci,ymax=u.ci)) +
  geom_smooth(method = "lm", se=T,na.rm = T)+
  xlab('Modelled Score') + ylab('Instructor Score') +
  theme_bw() + 
  theme(legend.position = "none")+
  theme(text = element_text(size=20)) 

  # Examine confidence interval lengths and deviance

  preds_rf_ci <-
  preds %>%
  group_by(assessment) %>%
  mutate(ave_ci_length = mean(CI_length))

  # create color palette
  colourCount = length(unique(preds_rf_ci$assessment))
  getPalette = colorRampPalette(brewer.pal(4, "Set1"))

  preds_rf_ci$assessment <- factor(preds_rf_ci$assessment, c("1", "2", "3", "4","5","6","7","8","9"))

  plot_13<-ggplot(preds_rf_ci, aes(assessment, deviance, fill=assessment)) +
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

  return(variable_importance_plot)

}

  test<-random_forest_modeler(handaxe_pca_function)

##############################
# Expert Random forest modeler
# Performs same task as above,
# but on expert data set
##############################

  expert_random_forest_modeler<-function(function_1,function_2){
  
  # Create handaxe scaled post PCA datasets (novice and expert)
  
  handaxe_postpca_scaled<-function_1(merge_handaxe_prediction_controls_core_score_function)
  expert_complete_experiment_data_reduced_scaled<-function_2(merge_handaxe_prediction_controls_core_score_function,expert_shape_attribute_function)
  
  # Remove variables not in expert dataset
  
  handaxe_postpca_scaled_reduced<-handaxe_postpca_scaled[ , -which(names(handaxe_postpca_scaled) %in% c("Core.Number", "delta_profile_thickness_cv"))]
  
  # Combine expert and novice
  
  novice_expert_combined<-merge(expert_complete_experiment_data_reduced_scaled, handaxe_postpca_scaled_reduced, 
                                by=c("knapper", "assessment", "experiment","condition",
                                     "Score","Training.Hours","delta_weight",
                                     "Dim.1", "Dim.2",
                                     "profile_asymetry_index","plan_asymetry_index",
                                     "percent_unflaked_area","percent_bifacially_flaked","flake_scar_density"), all = T)
  
  ## Outlier detection using cook's distance
  
  score_model_expert <- lm(Score ~ Dim.1+Dim.2+profile_asymetry_index+
                             plan_asymetry_index+
                             percent_unflaked_area+delta_weight+
                             percent_bifacially_flaked+flake_scar_density, 
                           data=novice_expert_combined)
  
  cooksd_expert <- cooks.distance(score_model_expert)
  
  plot<-plot(cooksd_expert, pch="*", cex=2, main="Influential Obs by Cooks distance")
  abline(h = 4*mean(cooksd_expert, na.rm=T), col="red")  # add cutoff line
  text(x=1:length(cooksd_expert)+1, y=cooksd_expert, 
       labels=ifelse(cooksd_expert>2*mean(cooksd_expert, na.rm=T),
                     names(cooksd_expert),""), 
       col="red")
  
  # Subset data to remove overly influential handaxes
  
  novice_expert_combined$cooks_distance<-abs(cooksd_expert)
  
  # Follow through with this dataset
  
  novice_expert_handaxe_postpca_scaled_minus_outlier<-subset(novice_expert_combined,cooksd_expert < 0.04)
  
  ## create test and training data
  # important to set seed so samples are extracted the same way
  
  set.seed(150) 
  sample_new = sample.split(novice_expert_handaxe_postpca_scaled_minus_outlier, SplitRatio = .7)
  train_new = subset(novice_expert_handaxe_postpca_scaled_minus_outlier, sample_new == TRUE)
  test_new  = subset(novice_expert_handaxe_postpca_scaled_minus_outlier, sample_new == FALSE)
  
  plot_1<-hist(train_new$Score)
  plot_2<-hist(test_new$Score)
  
  plot_3<-qqplot(train_new$Score,test_new$Score,
         xlab="Training data score", ylab="Test data score",
         main="Comparison of test and training data score distributions")
  
  wilcox.test(train_new$Score,test_new$Score) #are samples statistically different?
  
  # Random forest model with training data
  
  fit_4 <- randomForest(Score ~
                          plan_asymetry_index
                        +flake_scar_density+Dim.2   #minus Dim.1
                        +percent_unflaked_area+percent_bifacially_flaked,
                        data=train_new,
                        mtry=2,
                        importance=TRUE, 
                        ntree=10000,
                        keep.inbag = TRUE)
  
  # residuals
  
  plot_5<-qqnorm((fit_4$predicted-train_new$Score)/sd(fit_4$predicted-train_new$Score))
  plot_6<-qqline((fit_4$predicted-train_new$Score)/sd(fit_4$predicted-train_new$Score))
  
  fit_4_residuals<-fit_4$predicted-train_new$Score
  plot_7<-hist(fit_4_residuals)
  
  # plot all data predictions against observations
  
  Prediction_all_expert <- predict(fit_4, novice_expert_combined)
  submit_all_expert <- data.frame(Score_observed_expert = novice_expert_combined$Score, Score_predicted_expert = Prediction_all_expert, knapper =  novice_expert_combined$knapper, assessment = novice_expert_combined$assessment)
  
  plot_8<-ggplot(submit_all_expert,size=1)+
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
  
  ## Random forest interpreter, return confidence intervals
  
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
  
  rf.preds_cart <- rfPredVar(fit_4,rf.data=novice_expert_combined,CI=TRUE,tree.type='rf')
  preds <- rf.preds_cart
  preds$tree <- 'Random Forest'
  preds$pred.ij.var <- NULL
  preds$measured <- novice_expert_combined[ ,'Score']
  preds$condition<-ifelse(novice_expert_combined$condition=="expert","expert","novice")
  preds$deviance <- preds$pred - preds$measured
  preds$CI_length<-ifelse(preds$u.ci > preds$l.ci, preds$u.ci - preds$l.ci, preds$l.ci - preds$u.ci)
  
  plot_9<-ggplot(preds,aes(measured,pred)) +
    geom_point(size=2,aes(shape=condition)) +
    #geom_abline(intercept=0,slope=1,lty=2,color='#999999') +
    geom_errorbar(aes(ymin=l.ci,ymax=u.ci, color=condition)) +
    scale_color_viridis_d(begin=0.1,end=0.4)+
    geom_smooth(method = "lm", se=T,na.rm = T)+
    xlab('Modelled Score') + ylab('Instructor Score') +
    theme_bw() + 
    theme(legend.position = "none")+
    theme(text = element_text(size=20)) 
  
  return(plot_9)
  
}

  test<-expert_random_forest_modeler(handaxe_pca_function,expert_handaxe_pca_function)

##############################
# Extract individuals model component
# Performs several functions related to
# individual performance in the study
# Returns: tests how many hours to reach perfect skill score,
# tests the correlation between first last nada vs. RF
##############################

  individuals_instudy_function<-function(function_1){

  modified_random_forest_modeler<-function(function_1){
  
  ## Create handaxe scaled post PCA data
  
  handaxe_postpca_scaled<-function_1(merge_handaxe_prediction_controls_core_score_function)
  handaxe_postpca_scaled<-subset(handaxe_postpca_scaled,!knapper=="33")
  
  ## Detect and remove outliers using cook's distance
  
  score_model <- lm(Score ~ Dim.1+Dim.2+profile_asymetry_index+
                      plan_asymetry_index+delta_profile_thickness_cv+
                      percent_unflaked_area+delta_weight+
                      percent_bifacially_flaked+
                      flake_scar_density, 
                    data=handaxe_postpca_scaled)
  
  cooksd <- cooks.distance(score_model)
  
  # Subset data to remove overly influential handaxes
  
  handaxe_postpca_scaled$cooks_distance<-abs(cooksd)
  
  handaxe_postpca_scaled_minus_outlier<-subset(handaxe_postpca_scaled,cooksd < 0.04)
  
  # Check which are overly influential handaxes
  
  outlier_handaxes<-subset(handaxe_postpca_scaled,cooksd > 0.04)
  
  ### Random forest model building
  
  # Create test and training data
  # Set sampling seed for reproducible results
  
  set.seed(425)
  
  sample = sample.split(handaxe_postpca_scaled_minus_outlier$knapper, SplitRatio = .70)
  train = subset(handaxe_postpca_scaled_minus_outlier, sample == TRUE)
  test  = subset(handaxe_postpca_scaled_minus_outlier, sample == FALSE)
  
  ## Random Forest Model 1: All predictor variables
  
  fit <- randomForest(Score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                      +delta_weight+Dim.1+Dim.2
                      +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                      data=train, 
                      importance=TRUE,  
                      mtry=2,          
                      ntree=10000,
                      keep.inbag = TRUE)
  
  # Create variable importance plot
  
  names<-c("Profile Asymetry","Plan Asymetry","Delta Profile Thickness",
           "Delta Weight","Shape PC1","Shape PC2","Percent Unflaked Area","Percent Bifacially Flaked","Flake Scar Density")
  all_var_importance_data<-data.frame(importance(fit,type = 1)) #permutation importance, variable is assigned values by random permutation by how much will the MSE increase.
  all_var_importance_data$predictor<-rownames(all_var_importance_data)
  all_var_importance_data$predictor<- factor(all_var_importance_data$predictor, levels=all_var_importance_data$predictor[order(all_var_importance_data$X.IncMSE)], ordered=TRUE)                                                                                             
  colnames(all_var_importance_data)[1]<-"All_X.IncMSE"     

  # Test on predictions
  
  Prediction_train <- predict(fit)
  submit_train <- data.frame(Score_observed = train$Score, Score_predicted = Prediction_train, knapper =  train$knapper, assessment = train$assessment)
  
  fit1_lm<-lm(Score_observed~Score_predicted, data=submit_train)
  
  Prediction_test <- predict(fit, test)
  submit_test <- data.frame(Score_observed = test$Score, Score_predicted = Prediction_test, knapper =  test$knapper, assessment = test$assessment)
  
  submit_all <- rbind(submit_train,submit_test)
  
  error <- submit_all$Score_observed - submit_all$Score_predicted
  
  ## Random Forest Model 2: reduced predictor set
  
  # Select predictor variables with >10 % reduction in model prediction mean squared error
  
  reduced_variables<-subset(all_var_importance_data,All_X.IncMSE>10)
  NameList <- reduced_variables$predictor
  train_reduced <- train[,colnames(train) %in% NameList]
  train_reduced$Score<-train$Score
  
  test_reduced<-test[,colnames(test) %in% NameList]
  test_reduced$Score<-test$Score
  
  fit_2 <- randomForest(Score ~.,
                        data=train_reduced,
                        mtry=2,
                        importance=TRUE, 
                        ntree=10000,
                        keep.inbag = TRUE)
  return(fit_2)
}

  fit_2<-modified_random_forest_modeler(function_1)
  
  # Keep seven outliers in for this part-effects individual vrb
  
  handaxe_postpca_scaled_minus_control<-function_1(merge_handaxe_prediction_controls_core_score_function) %>%
    subset(condition == "main")
  
  # Produce total predictions data
  
  Prediction_total <- predict(fit_2, handaxe_postpca_scaled_minus_control) 
  submit_total <- data.frame(Score_observed = handaxe_postpca_scaled_minus_control$Score, Score_predicted = Prediction_total, knapper =  handaxe_postpca_scaled_minus_control$knapper, assessment = handaxe_postpca_scaled_minus_control$assessment, hours=handaxe_postpca_scaled_minus_control$Training.Hours)
  submit_total<-submit_total %>%
    mutate(hours=car::recode(hours,"86=93; 85=93; 74.5=71;45=47"),
           hours=as.factor(hours)) #some later evals were based on extra days training
  
  # create data frame representing all assessments, knappers, hours
  
  assessment <- as.factor(rep(c(1:9), times=17))
  knapper <- as.factor(rep(c(1,2,3,5,6,7,8,9,10,11,13,14,15,16,17,19,21), times=9))
  hours <- as.factor(rep(c(0,12,24,36,47,59,71,82,93), times=17))
  
  df <- melt(data.frame(assessment,knapper,hours))
  
  # merge with original data set
  
  complete_data <-merge(submit_total, df, by=c("assessment", "knapper","hours"), all = T)
  
  ## interpolate missing data-interpolation based on group data
  
  complete_data$predicted_score_interpolate_spline=na.spline(complete_data$Score_predicted)
  complete_data$predicted_score_interpolate_linear=na.approx(complete_data$Score_predicted)
  
  # create columns with only interpolated values
  
  complete_data$spline <- ifelse(complete_data$Score_predicted %in% NA, complete_data$predicted_score_interpolate_spline,NA)
  complete_data$linear <- ifelse(complete_data$Score_predicted %in% NA, complete_data$predicted_score_interpolate_linear,NA)
  
  complete_data$predicted_score_interpolate_spline<-NULL
  complete_data$predicted_score_interpolate_linear<-NULL
  
  # reshape data to have predicted and observed scores in same column
  
  mdata <- melt(complete_data, id=c("knapper","assessment","hours"))
  
  mdata <- mdata %>%
    mutate(sqrt_hours = sqrt(as.numeric(as.character(hours))),
           value = as.numeric(as.character(value))) 
  
  ## interpolate missing data-interpolation based on individual data
  
  complete_data_indiv <-full_join(submit_total, df, by=c("assessment", "knapper","hours"))
  complete_data_indiv<-complete_data_indiv %>%
    mutate(hours=as.numeric(as.character(hours)),
           sqrt_hours=sqrt(hours))
  
  # function to interpolate based on individual loess fit curves
  # returns either a plot or the numberical data
  
  individual_interpolation_function = function(filter_var) {
    individual_interpolation_data = complete_data_indiv %>%
      dplyr::filter(knapper==filter_var) %>%
      arrange(desc(assessment)) %>%
      mutate(predicted_score_interpolate_spline = na.spline(Score_predicted),
             predicted_score_interpolate_linear=na.approx(Score_predicted, rule=2),
             spline=ifelse(Score_predicted %in% NA, predicted_score_interpolate_spline,NA),
             linear=ifelse(Score_predicted %in% NA, predicted_score_interpolate_linear,NA)) %>% 
      dplyr::select(-c(predicted_score_interpolate_spline, predicted_score_interpolate_linear)) %>%
      melt(id=c("knapper","assessment","hours"))  %>%
      dplyr::rename(score=value)
    
    g = ggplot(subset(individual_interpolation_data, variable !="linear" & variable !="sqrt_hours",size=1))+
      aes(x = assessment,y = score) +
      geom_point(aes(colour = variable), alpha = 0.6) +
      scale_color_manual(labels = c("Observed","Predicted", "Interpolated"),values=c("red","orange", "blue","black")) +
      scale_x_discrete(name="Assessment")+
      scale_y_continuous(name="Score",limits = c(0, 5))+
      labs(color='')+
      ggtitle(filter_var)+
      geom_smooth(method = "loess", data = individual_interpolation_data %>% 
                    filter( variable !="linear") %>%
                    mutate(assessment = as.numeric(assessment)), se=T,na.rm = T)
    
    return(complete_data_indiv)
  } 
  
  individual_interp_1<-individual_interpolation_function("1")
  
  linear_interpolation_function_slope <- function(knapper_number,score_to_use = "Predicted Score"){
    
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
    
    model_kn<-lm(score~sqrt_hours, data=data_kn)
    data_kn$prediction = predict(model_kn, data_kn)
    
    data_kn$score = ifelse(is.na(data_kn$score),
                           data_kn$prediction,
                           data_kn$score)
    data_kn$score_to_use = factor(data_kn$score_to_use, levels = c(score_to_use, "Interpolated"))
    
    slope_df<-data.frame(coef(model_kn)[2])
    slope_df$knapper<-knapper_number
    
    return(slope_df)
    
  }
  
  linearinterp_1<-linear_interpolation_function_slope("1",score_to_use = "Predicted Score")
  
  linear_interpolation_function_data<- function(knapper_number,score_to_use = "Predicted Score"){
    
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
    
    return(data_kn)
    
  }
  
  ## how many hours to reach a score of 5?
  
  hours_to_five_function <- function(data_set){
    data_kn =  data_set %>%
      arrange((assessment))
    model_kn<-lm(score~sqrt_hours,data=data_kn)
    summary(model_kn)
    prediction_data_kn <- data.frame(score=numeric(42),
                                     sqrt_hours=seq(9,50,1),
                                     knapper=rep(unique(data_set$knapper,42)))
    predictions = data.frame(predict(model_kn, prediction_data_kn,interval = 'prediction'))
    prediction_data_kn$score = predictions$fit 
    hours_to_five_kn<-subset(prediction_data_kn,score >4.89 & score <5.1)
    hours_to_five_kn<-hours_to_five_kn %>%
      filter(sqrt_hours == min(sqrt_hours))
  }
  
  hours_to_five_function_21 <- function(data_set){
    data_kn =  data_set %>%
      arrange((assessment))
    model_kn<-lm(prediction~sqrt_hours,data=data_kn)
    summary(model_kn)
    prediction_data_kn <- data.frame(score=numeric(42),
                                     sqrt_hours=seq(9,50,1),
                                     knapper=rep(unique(data_set$knapper,42)))
    predictions = data.frame(predict(model_kn, prediction_data_kn,interval = 'prediction'))
    prediction_data_kn$score = predictions$upr 
    hours_to_five_kn<-subset(prediction_data_kn,score >4.9 & score <5.3)
    hours_to_five_kn<-hours_to_five_kn %>%
      filter(sqrt_hours == min(sqrt_hours))
  }
  
  # interpolate scores
  
  linearinterp_1<-linear_interpolation_function_data("1",score_to_use = "Predicted Score")
  linearinterp_2<-linear_interpolation_function_data("2",score_to_use = "Predicted Score")
  linearinterp_3<-linear_interpolation_function_data("3",score_to_use = "Predicted Score")
  linearinterp_5<-linear_interpolation_function_data("5",score_to_use = "Predicted Score")
  linearinterp_6<-linear_interpolation_function_data("6",score_to_use = "Predicted Score")
  linearinterp_7<-linear_interpolation_function_data("7",score_to_use = "Predicted Score")
  linearinterp_8<-linear_interpolation_function_data("8",score_to_use = "Observed Score")
  linearinterp_9<-linear_interpolation_function_data("9",score_to_use = "Predicted Score")
  linearinterp_10<-linear_interpolation_function_data("10",score_to_use = "Predicted Score")
  linearinterp_11<-linear_interpolation_function_data("11",score_to_use = "Observed Score")
  linearinterp_13<-linear_interpolation_function_data("13",score_to_use = "Predicted Score")
  linearinterp_14<-linear_interpolation_function_data("14",score_to_use = "Predicted Score")
  linearinterp_15<-linear_interpolation_function_data("15",score_to_use = "Predicted Score")
  linearinterp_16<-linear_interpolation_function_data("16",score_to_use = "Predicted Score")
  linearinterp_17<-linear_interpolation_function_data("17",score_to_use = "Predicted Score")
  linearinterp_19<-linear_interpolation_function_data("19",score_to_use = "Predicted Score")
  linearinterp_21<-linear_interpolation_function_data("21",score_to_use = "Observed Score")

  # extrapolate beyond data range to reach score of 5 and return hours
  
  hours_to_five_function_1<-hours_to_five_function(linearinterp_1)
  hours_to_five_function_2<-hours_to_five_function(linearinterp_2)
  hours_to_five_function_3<-hours_to_five_function(linearinterp_3)
  hours_to_five_function_5<-hours_to_five_function(linearinterp_5)
  hours_to_five_function_7<-hours_to_five_function(linearinterp_7)
  hours_to_five_function_8<-hours_to_five_function(linearinterp_8)
  hours_to_five_function_9<-hours_to_five_function(linearinterp_9)
  hours_to_five_function_10<-hours_to_five_function(linearinterp_10)
  hours_to_five_function_11<-hours_to_five_function(linearinterp_11)
  hours_to_five_function_13<-hours_to_five_function(linearinterp_13)
  hours_to_five_function_14<-hours_to_five_function(linearinterp_14)
  hours_to_five_function_15<-hours_to_five_function(linearinterp_15)
  hours_to_five_function_16<-hours_to_five_function(linearinterp_16)
  hours_to_five_function_17<-hours_to_five_function(linearinterp_17)
  hours_to_five_function_19<-hours_to_five_function(linearinterp_19)
  hours_to_five_function_21<-hours_to_five_function_21(linearinterp_21)
  
  # bind the data
  
  hours_to_five_data<-rbind(hours_to_five_function_1,hours_to_five_function_2,hours_to_five_function_3,
                            hours_to_five_function_5,hours_to_five_function_7,
                            hours_to_five_function_8,hours_to_five_function_9,hours_to_five_function_10,
                            hours_to_five_function_11,hours_to_five_function_13,hours_to_five_function_14,
                            hours_to_five_function_15,hours_to_five_function_17,
                            hours_to_five_function_19,hours_to_five_function_21)
  
  # generate actual hours
  
  hours_to_five_data$hours<-hours_to_five_data$sqrt_hours^2
  median(hours_to_five_data$hours)
  range(hours_to_five_data$hours)
  
  # plot results
  
  howmanyhours_plot = function(data_file,knapper_number) {
    g = ggplot(data_file,aes(x = sqrt_hours,y = score)) +
      geom_point(alpha = 0.6) +
      scale_x_continuous(name="Hours (square root)",limits = c(0, 20))+
      scale_y_continuous(name="Score",limits = c(0, 5))+
      ggtitle(knapper_number)+
      geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
      theme(text = element_text(size=20))
    return(g)
  } 
  
  howmany_1<-howmanyhours_plot(linearinterp_1,"1")
  howmany_2<-howmanyhours_plot(linearinterp_2,"2")
  howmany_3<-howmanyhours_plot(linearinterp_3,"3")
  howmany_5<-howmanyhours_plot(linearinterp_5,"5")
  howmany_7<-howmanyhours_plot(linearinterp_7,"7")
  howmany_8<-howmanyhours_plot(linearinterp_8,"8")
  howmany_9<-howmanyhours_plot(linearinterp_9,"9")
  howmany_10<-howmanyhours_plot(linearinterp_10,"10")
  howmany_11<-howmanyhours_plot(linearinterp_11,"11")
  howmany_13<-howmanyhours_plot(linearinterp_13,"13")
  howmany_14<-howmanyhours_plot(linearinterp_14,"14")
  howmany_15<-howmanyhours_plot(linearinterp_15,"15")
  howmany_16<-howmanyhours_plot(linearinterp_16,"16")
  howmany_17<-howmanyhours_plot(linearinterp_17,"17")
  howmany_19<-howmanyhours_plot(linearinterp_19,"19")
  howmany_21<-howmanyhours_plot(linearinterp_21,"21")
  
  #multiplot(howmany_1, howmany_2, howmany_3, howmany_5,
  #                         howmany_7, howmany_8,
  #                         howmany_9, howmany_10,howmany_11,howmany_13,
  #                         howmany_14, howmany_15,howmany_16,
  #                         howmany_17, howmany_19,
  #                         howmany_21,cols=3)
  
  ## Does the initial skill score predict the final skill score
  # observed scores vs modelled scores
  
  correlation_data<-
    complete_data_indiv %>%
    group_by(knapper) %>%
    filter(assessment %in% c("1", "9")
           & Score_observed != "NA"
           &!knapper %in% c("13","15","17","3","6","8")) %>% 
    ungroup() %>%
    dplyr::select(-c(sqrt_hours,hours)) %>% 
    melt(variable.name = "score_category",
         value.names = c("Score_observed","Score_predicted")) %>% 
    tidyr::spread(assessment, value) %>% 
    rename("one" = "1", "nine"="9")
  
  correlation_data_observed<-filter(correlation_data, score_category=="Score_observed")
  correlation_data_predicted<-filter(correlation_data, score_category=="Score_predicted")
  
  res_observed <- cor.test(correlation_data_observed$one, correlation_data_observed$nine, 
                           method = "pearson")
  
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
  
  res_predicted_plot<-ggplot(correlation_data_predicted,size=1)+
    aes(x = one,y = nine) +
    geom_point(alpha = 0.6) +
    scale_x_continuous(name="Assessment 1 predicted scores",limits = c(1, 3.25))+
    scale_y_continuous(name="Assessment 9 predicted scores",limits = c(2, 4.25))+
    geom_smooth(method = "lm",se=F,na.rm = T)+
    ggtitle("Predicted Scores")+
    theme(text = element_text(size=20))
  
  # multiplot(res_observed_plot,res_predicted_plot,cols=2)
  
  # combined model with score set as a dummy variable (test between slopes)
  
  correlation_data_test<-subset(correlation_data,!score_category %in% c("sqrt_hours","prediction_test"))
  correlation_data_test$dummy<-ifelse(correlation_data_test$score_category=="Score_observed",1,0)
  
  summary(correlation_data_test)
  
  slopes_lm<-summary(lm(one~nine+dummy, data=correlation_data_test))
  
  return(list(complete_data_indiv,slopes_lm))
}

  #the unique operator below is from the zeallot package

  c(complete_data_indiv,
    linear_model_on_slopes) %<-% 
    individuals_instudy_function(handaxe_pca_function)

##############################
# Psychometrics model
# Test relationship between
# Psychometrics and skill scores
# Returns results from linear regression
# between psychometric score and skill score
# at set assessment period
##############################

  psychometrics_function<-function(filter_var){
    
    # Modified random forest modeller
    
    modified_random_forest_modeler<-function(function_1){
      
      ## Create handaxe scaled post PCA data
      
      handaxe_postpca_scaled<-function_1(merge_handaxe_prediction_controls_core_score_function)
      handaxe_postpca_scaled<-subset(handaxe_postpca_scaled,!knapper=="33")
      
      ## Detect and remove outliers using cook's distance
      
      score_model <- lm(Score ~ Dim.1+Dim.2+profile_asymetry_index+
                          plan_asymetry_index+delta_profile_thickness_cv+
                          percent_unflaked_area+delta_weight+
                          percent_bifacially_flaked+
                          flake_scar_density, 
                        data=handaxe_postpca_scaled)
      
      cooksd <- cooks.distance(score_model)
      
      # Subset data to remove overly influential handaxes
      
      handaxe_postpca_scaled$cooks_distance<-abs(cooksd)
      
      handaxe_postpca_scaled_minus_outlier<-subset(handaxe_postpca_scaled,cooksd < 0.04)
      
      outlier_handaxes<-subset(handaxe_postpca_scaled,cooksd > 0.04)
      
      ### Random forest model building
      
      # Create test and training data
      # Set sampling seed for reproducible results
      
      set.seed(425)
      
      sample = sample.split(handaxe_postpca_scaled_minus_outlier$knapper, SplitRatio = .70)
      train = subset(handaxe_postpca_scaled_minus_outlier, sample == TRUE)
      test  = subset(handaxe_postpca_scaled_minus_outlier, sample == FALSE)
      
      # Compare test and training outcome variable (Score) distributions
      # Test to see if distributions are significantly different
      
      wilcox.test(train$Score,test$Score)
      
      ## Random Forest Model 1: All predictor variables
      
      fit <- randomForest(Score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                          +delta_weight+Dim.1+Dim.2
                          +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                          data=train, 
                          importance=TRUE,  
                          mtry=2,          
                          ntree=10000,
                          keep.inbag = TRUE)
      
      # Create variable importance plot
      
      names<-c("Profile Asymetry","Plan Asymetry","Delta Profile Thickness",
               "Delta Weight","Shape PC1","Shape PC2","Percent Unflaked Area","Percent Bifacially Flaked","Flake Scar Density")
      all_var_importance_data<-data.frame(importance(fit,type = 1)) #permutation importance, variable is assigned values by random permutation by how much will the MSE increase.
      all_var_importance_data$predictor<-rownames(all_var_importance_data)
      all_var_importance_data$predictor<- factor(all_var_importance_data$predictor, levels=all_var_importance_data$predictor[order(all_var_importance_data$X.IncMSE)], ordered=TRUE)                                                                                             
      colnames(all_var_importance_data)[1]<-"All_X.IncMSE"     
      
      # Test on predictions
      
      Prediction_train <- predict(fit)
      submit_train <- data.frame(Score_observed = train$Score, Score_predicted = Prediction_train, knapper =  train$knapper, assessment = train$assessment)
      
      fit1_lm<-lm(Score_observed~Score_predicted, data=submit_train)
      
      Prediction_test <- predict(fit, test)
      submit_test <- data.frame(Score_observed = test$Score, Score_predicted = Prediction_test, knapper =  test$knapper, assessment = test$assessment)
      
      submit_all <- rbind(submit_train,submit_test)
      
      ## Random Forest Model 2: reduced predictor set
      
      # Select predictor variables with >10 % reduction in model prediction mean squared error
      
      reduced_variables<-subset(all_var_importance_data,All_X.IncMSE>10)
      NameList <- reduced_variables$predictor
      train_reduced <- train[,colnames(train) %in% NameList]
      train_reduced$Score<-train$Score
      
      test_reduced<-test[,colnames(test) %in% NameList]
      test_reduced$Score<-test$Score
      
      fit_2 <- randomForest(Score ~.,
                            data=train_reduced,
                            mtry=2,
                            importance=TRUE, 
                            ntree=10000,
                            keep.inbag = TRUE)
      
      # Error rates
      
      Prediction_train <- predict(fit_2)
      submit_train <- data.frame(Score_observed = train$Score, Score_predicted = Prediction_train, knapper =  train$knapper, assessment = train$assessment)
      
      Prediction_test <- predict(fit_2, test)
      submit_test <- data.frame(Score_observed = test$Score, Score_predicted = Prediction_test, knapper =  test$knapper, assessment = test$assessment)
      
      submit_all_fit_2 <- rbind(submit_train,submit_test)
      
      # Add outlier handaxes and predict
      
      Prediction_outliers <- predict(fit_2, outlier_handaxes)
      submit_all_fit_2_outliers<- data.frame(Score_observed = outlier_handaxes$Score, Score_predicted = Prediction_outliers, knapper =  outlier_handaxes$knapper, assessment = outlier_handaxes$assessment)
      
      submit_all_fit_2<-rbind(submit_all_fit_2,submit_all_fit_2_outliers)
      
      return(submit_all_fit_2)
    }
    
    # Extract fit 2 data
    
    fit_2_data<-modified_random_forest_modeler(handaxe_pca_function)
    
    tol = tower_of_london %>%
    select(-Type) %>%
    group_by(knapper) %>% 
    summarise_at(c("Tower.Count", "Bead.Count","Minimum.Moves","Moves",
                   "Excess.Moves","Drop.Errors","First.Move.Time..s.",
                   "Total.Time.to.Pickup..s.","Total.Time..s."), sum)
  
    tol = tol %>%
    mutate(assessment=rep(filter_var, times=nrow(tol)),
                  knapper=as.factor(knapper),
                  assessment=as.factor(assessment))

  fit_2_predictions = 
  fit_2_data %>%
  mutate(knapper=as.factor(knapper),
         assessment=as.factor(assessment))

  tol_combined = 
  left_join(fit_2_predictions,tol, by=c("assessment", "knapper"))  %>%
  mutate(ToL_1_EM_log=Excess.Moves+1)

  tol_1 = ggplot(tol_combined, aes(x=Excess.Moves, y= Score_predicted))+
  geom_point()+
  geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
  xlab(label="Tower of London Error")+
  ylab(label="Predicted Score")+
  theme(text = element_text(size=15))
  
  tol_test=summary(lmp(Score_predicted~Excess.Moves,perm="Exact",
              data=tol_combined))
  
  # Wisconsin card sort
  
    wisc=wisc_card_sort %>%
      filter(!Category=="Totals") %>%
      mutate(knapper=as.factor(knapper)) %>%
      group_by(knapper) %>%
      summarise_at(c("Perseverative.Error.Response.Count",
                     "Average.Response.Time",
                     "Correct.Response.Count",
                     "Average.Correct.Response.Time",
                     "Error.Response.Count",
                     "Average.Error.Response.Time",
                     "Unique.Error.Response.Count"), sum)
    
    wisc_data_summary=wisc %>%
      mutate(assessment=rep(filter_var, times=nrow(wisc)),
           knapper=as.factor(knapper),
           assessment=as.factor(assessment))
    
    fit_2_predictions = 
      fit_2_data %>%
      mutate(knapper=as.factor(knapper),
             assessment=as.factor(assessment))
    
    wisc_combined = 
      left_join(fit_2_predictions,wisc_data_summary, by=c("assessment", "knapper"))  %>%
      mutate(Log_Perseverative.Error.Response.Count=Perseverative.Error.Response.Count+1) %>%
      mutate(knapper=as.factor(knapper),
             assessment=as.factor(assessment))
    
    wisc_1 = ggplot(wisc_combined, aes(x=Perseverative.Error.Response.Count, y= Score_predicted))+
      geom_point()+
      geom_smooth(method = "lm", se=T,na.rm = T,fullrange = TRUE)+
      xlab(label="Card Sort Error")+
      ylab(label="Predicted Score")+
      theme(text = element_text(size=15))
    
    wisc_test = summary(lmp(Perseverative.Error.Response.Count~Score_predicted,perm="Exact", 
                       data=wisc_combined))
    
    return(list(wisc_test,tol_test))
  } 

#the unique operator below is from the zeallot package
  
  c(wisc_test,tol_test) %<-% psychometrics_function("2")
  
##############################
# Provides training density data
# Returns data for training density plots
# cumulative_hours_with_slope.csv
# last_month_data.csv
# practice density data
##############################
  
  handaxe_training_density_function<-function(data_1,function_1){
    
    ## Prepare data for training density plots
    
    for_chart = data_1 %>% mutate(date = as.Date(Date, "%m/%d/%y"),
                                  Hours = replace_na(Hours,0))%>%
      group_by(Subject) %>%
      mutate(cum_training_hours = cumsum(Hours)) %>%
      select(-c(Comments,Pieces,Success,Instruction))
    
    agg_hours_scans = data_1 %>% 
      mutate(
        date = as.Date(Date, "%m/%d/%y"),
        Hours = replace_na(Hours,0)) %>%
      group_by(Subject) %>%
      arrange(date) %>%
      mutate(
        last_scan = ifelse(is.na(Scan),
                           na.locf(Scan), #fills NA with last non null value
                           Scan)) %>%
      ungroup() %>%
      group_by(Subject, last_scan) %>%
      dplyr::summarise(
        previous_scan_date = min(date),
        last_training_date = max(date),
        total_days = max(date) - min(date),
        total_hours = sum(Hours)
      ) %>%
      filter(last_scan < 3) %>% 
      mutate(last_scan = last_scan +1)
    
    # Data for chart with MRI scan dates
    
    for_scan_chart = data_1 %>% 
      mutate(date = as.Date(Date, "%m/%d/%y"),
             Hours = replace_na(Hours,0)) %>%
      filter(Group=="main") %>%
      group_by(Subject) %>%
      mutate(cum_training_hours = cumsum(Hours)) 
    
    subject_dates_hours_scan<-data_1
    subject_dates_hours_scan<-data_1[!(data_1$Assessment %in% c(1:9)),]
    subject_dates_hours_scan$Assessment<-NULL
    
    # Calculate cumulative training hours
    
    cumulative_hours = data_1 %>% 
      select(-c(Instruction,Pieces,Success,Comments)) %>%
      group_by(Subject) %>%
      arrange(as.Date(Date, "%m/%d/%y")) %>%
      mutate(
        date = as.Date(Date, "%m/%d/%y"),
        Hours = replace_na(Hours,0),
        max_add = as.numeric(max(date) - min(date)), 
        cum_training_hours = cumsum(Hours),
        first_date = min(date))
    
    # Training slope data
    # Arrange plot by slope steepness: need to cal hours traing/days for first 40 hours
    
    training_slope = cumulative_hours %>% 
      subset(cum_training_hours < 40) %>%
      group_by(Subject) %>%
      mutate(days = as.numeric(max(date) - min(first_date)),
             sum_days=sum(days,na.rm = T),
             sum_hours=sum(cum_training_hours,na.rm = T),
             hours_days=sum_hours/sum_days) %>%
      ungroup() %>% select(Subject, hours_days) %>%
      distinct()
    
    cumulative_hours_with_slope = cumulative_hours %>%
      left_join(training_slope, by = 'Subject') %>%
      transform(Subject=reorder(Subject, -hours_days)) %>%
      arrange(Subject, as.Date(Date, "%m/%d/%y")) %>%
      mutate(row = dplyr::row_number()) %>%
      group_by(Subject) %>%
      mutate(
        first_row = min(row),
        plot_count = ifelse(date == first_date, 
                            row,
                            first_row + as.numeric((date - first_date)))
      ) %>% ungroup()
    
    ## Plot data
    # scans + assessments (adjust geom_point as necessary to add subsets)
    # only trained individuals
    
    plot_1 = ggplot(data = subset(cumulative_hours_with_slope,Group=="trained"),
                    aes(x = plot_count, y = cum_training_hours, col = as.factor(Subject),
                        reorder(Subject, -hours_days)))+
      geom_line()+
      scale_y_continuous(name="Cumulative training hours")+
      scale_x_discrete(name="Days")+
      scale_color_viridis_d()+
      geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Session) & Group=="trained"), 
                 aes(x = plot_count, y = cum_training_hours,col = as.factor(Subject)))+
      geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Scan) & Group=="trained"), 
                 aes(x = plot_count, y = cum_training_hours), col = 'black',size=2) +
      theme(legend.position="none")
    
    # plot with weeks as x-axis
    # use to illustrate individual tracks through the study
    
    cumulative_hours_with_slope$date <- as.Date(cumulative_hours_with_slope$date)
    
    plot_2 = ggplot(data = subset(cumulative_hours_with_slope,Subject=="3"),
                    aes(x = date, y = cum_training_hours,
                        reorder(Subject, -hours_days)))+
      geom_line()+
      scale_y_continuous(name="Cumulative training hours")+
      scale_x_date(breaks = date_breaks("2 weeks"), labels = date_format("%d-%m"))+
      geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Session) & Subject==3), 
                 aes(x = date, y = cum_training_hours,col = 'red'))+
      geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Assessment) & Subject==3), 
                 aes(x = date, y = cum_training_hours), col = 'black') +
      theme(legend.position="none")+
      ggtitle(label="Subject 3")
    
    # illustrate difference between eye tracking and scan dates 
    # remove control subjects in data subset
    
    plot_3 = ggplot(data = subset(cumulative_hours_with_slope,Group=="trained"),
                    aes(x = plot_count, y = as.factor(Subject), col = as.factor(Subject),
                        reorder(Subject, -hours_days)))+
      geom_line()+
      geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(eye_tracking) &Group=="trained"), 
                 aes(x = plot_count, y = as.factor(Subject)), col = 'red')+
      scale_color_viridis_d()+
      geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Scan) & Group=="trained"), 
                 aes(x = plot_count, y = as.factor(Subject)), col = 'black')+coord_flip()+
      scale_y_discrete(name="Subject")+
      scale_x_continuous(name = "Days")+
      theme(legend.position="none")
    
    # Plot assessments only
    
    no_first = cumulative_hours_with_slope %>% 
      filter(is.na(Scan)) %>%
      mutate(row = row_number()) %>%
      group_by(Subject) %>%
      mutate(max_add = as.numeric(max(date) - min(date)), 
             cum_training_hours = cumsum(Hours),
             first_date = min(date),
             first_row = min(row),
             plot_count = ifelse(date == first_date, 
                                 row,
                                 first_row + as.numeric((date - first_date))))
    
    plot_4 = ggplot(data = subset(no_first,Group=="trained"),
                    aes(x = plot_count, y = cum_training_hours, col = as.factor(Subject)),
                    reorder(Subject, hours_days))+
      geom_line()+
      scale_y_continuous(name="Cumulative training hours")+
      labs(colour="Subject Number")+
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank())+
      xlab("Subject")+ 
      geom_point(data = no_first %>% filter(!is.na(Assessment) & Group=="trained"), 
                 aes(x = plot_count, y = cum_training_hours,col = as.factor(Subject)))+
      theme(text = element_text(size=20))+
      scale_color_viridis_d()
    
    #subset above plot to show subjects who did badly on last three assessments
    
    plot_5 = ggplot(data = subset(no_first,Subject %in% c("1","2","5","9","11","16","19")),
                    aes(x = plot_count, y = cum_training_hours, col = as.factor(Subject)),
                    reorder(Subject, hours_days))+
      geom_line()+
      scale_y_continuous(name="Cumulative training hours")+
      labs(colour="Subject Number")+
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank())+
      xlab("Subject")+ 
      geom_point(data = subset(no_first, !is.na(Assessment) & Subject %in% c("1","2","5","9","11","16","19")), 
                 aes(x = plot_count, y = cum_training_hours,col = as.factor(Subject)))
    
    ## Calculate overall practice rates
    
    practice_rates = cumulative_hours %>% 
      group_by(Subject) %>%
      mutate(days = as.numeric(max(date) - min(first_date)),
             sum_days=sum(days,na.rm = T),
             sum_hours=sum(cum_training_hours,na.rm = T),
             hours_days=sum_hours/sum_days) %>%
      ungroup() %>% select(Subject, hours_days) %>%
      distinct()
    
    ## Training hours relative to handaxe making skill scores
    
    modified_individuals_function<-function(function_1){
      
      modified_random_forest_modeler<-function(function_1){
        
        ## Create handaxe scaled post PCA data
        
        handaxe_postpca_scaled<-function_1(merge_handaxe_prediction_controls_core_score_function)
        handaxe_postpca_scaled<-subset(handaxe_postpca_scaled,!knapper=="33")
        
        ## Detect and remove outliers using cook's distance
        
        score_model <- lm(Score ~ Dim.1+Dim.2+profile_asymetry_index+
                            plan_asymetry_index+delta_profile_thickness_cv+
                            percent_unflaked_area+delta_weight+
                            percent_bifacially_flaked+
                            flake_scar_density, 
                          data=handaxe_postpca_scaled)
        
        cooksd <- cooks.distance(score_model)
        
        # Subset data to remove overly influential handaxes
        
        handaxe_postpca_scaled$cooks_distance<-abs(cooksd)
        
        handaxe_postpca_scaled_minus_outlier<-subset(handaxe_postpca_scaled,cooksd < 0.04)
        
        # Check which are overly influential handaxes
        
        outlier_handaxes<-subset(handaxe_postpca_scaled,cooksd > 0.04)
        
        ### Random forest model building
        
        # Create test and training data
        # Set sampling seed for reproducible results
        
        set.seed(425)
        
        sample = sample.split(handaxe_postpca_scaled_minus_outlier$knapper, SplitRatio = .70)
        train = subset(handaxe_postpca_scaled_minus_outlier, sample == TRUE)
        test  = subset(handaxe_postpca_scaled_minus_outlier, sample == FALSE)
        
        ## Random Forest Model 1: All predictor variables
        
        fit <- randomForest(Score ~ profile_asymetry_index+plan_asymetry_index+delta_profile_thickness_cv
                            +delta_weight+Dim.1+Dim.2
                            +percent_unflaked_area+percent_bifacially_flaked+flake_scar_density,
                            data=train, 
                            importance=TRUE,  
                            mtry=2,          
                            ntree=10000,
                            keep.inbag = TRUE)
        
        # Create variable importance plot
        
        names<-c("Profile Asymetry","Plan Asymetry","Delta Profile Thickness",
                 "Delta Weight","Shape PC1","Shape PC2","Percent Unflaked Area","Percent Bifacially Flaked","Flake Scar Density")
        all_var_importance_data<-data.frame(importance(fit,type = 1)) #permutation importance, variable is assigned values by random permutation by how much will the MSE increase.
        all_var_importance_data$predictor<-rownames(all_var_importance_data)
        all_var_importance_data$predictor<- factor(all_var_importance_data$predictor, levels=all_var_importance_data$predictor[order(all_var_importance_data$X.IncMSE)], ordered=TRUE)                                                                                             
        colnames(all_var_importance_data)[1]<-"All_X.IncMSE"     
        
        # Test on predictions
        
        Prediction_train <- predict(fit)
        submit_train <- data.frame(Score_observed = train$Score, Score_predicted = Prediction_train, knapper =  train$knapper, assessment = train$assessment)
        
        fit1_lm<-lm(Score_observed~Score_predicted, data=submit_train)
        
        Prediction_test <- predict(fit, test)
        submit_test <- data.frame(Score_observed = test$Score, Score_predicted = Prediction_test, knapper =  test$knapper, assessment = test$assessment)
        
        submit_all <- rbind(submit_train,submit_test)
        
        error <- submit_all$Score_observed - submit_all$Score_predicted
        
        ## Random Forest Model 2: reduced predictor set
        
        # Select predictor variables with >10 % reduction in model prediction mean squared error
        
        reduced_variables<-subset(all_var_importance_data,All_X.IncMSE>10)
        NameList <- reduced_variables$predictor
        train_reduced <- train[,colnames(train) %in% NameList]
        train_reduced$Score<-train$Score
        
        test_reduced<-test[,colnames(test) %in% NameList]
        test_reduced$Score<-test$Score
        
        fit_2 <- randomForest(Score ~.,
                              data=train_reduced,
                              mtry=2,
                              importance=TRUE, 
                              ntree=10000,
                              keep.inbag = TRUE)
        return(fit_2)
      }
      
      fit_2<-modified_random_forest_modeler(function_1)
      
      # Keep seven outliers in for this part-effects individual vrb
      
      handaxe_postpca_scaled_minus_control<-function_1(merge_handaxe_prediction_controls_core_score_function) %>%
        subset(condition == "main")
      
      # Produce total predictions data
      
      Prediction_total <- predict(fit_2, handaxe_postpca_scaled_minus_control) 
      submit_total <- data.frame(Score_observed = handaxe_postpca_scaled_minus_control$Score, Score_predicted = Prediction_total, knapper =  handaxe_postpca_scaled_minus_control$knapper, assessment = handaxe_postpca_scaled_minus_control$assessment, hours=handaxe_postpca_scaled_minus_control$Training.Hours)
      submit_total<-submit_total %>%
        mutate(hours=car::recode(hours,"86=93; 85=93; 74.5=71;45=47"),
               hours=as.factor(hours)) #some later evals were based on extra days training
      
      # create data frame representing all assessments, knappers, hours
      
      assessment <- as.factor(rep(c(1:9), times=17))
      knapper <- as.factor(rep(c(1,2,3,5,6,7,8,9,10,11,13,14,15,16,17,19,21), times=9))
      hours <- as.factor(rep(c(0,12,24,36,47,59,71,82,93), times=17))
      
      df <- data.frame(assessment,knapper,hours)
      
      # merge with original data set
      
      complete_data <-merge(submit_total, df, by=c("assessment", "knapper","hours"), all = T)
      
      ## interpolate missing data-interpolation based on group data
      
      complete_data$predicted_score_interpolate_spline=na.spline(complete_data$Score_predicted)
      complete_data$predicted_score_interpolate_linear=na.approx(complete_data$Score_predicted)
      
      # create columns with only interpolated values
      
      complete_data$spline <- ifelse(complete_data$Score_predicted %in% NA, complete_data$predicted_score_interpolate_spline,NA)
      complete_data$linear <- ifelse(complete_data$Score_predicted %in% NA, complete_data$predicted_score_interpolate_linear,NA)
      
      complete_data$predicted_score_interpolate_spline<-NULL
      complete_data$predicted_score_interpolate_linear<-NULL
      
      # reshape data to have predicted and observed scores in same column
      
      mdata <- melt(complete_data, id=c("knapper","assessment","hours"))
      
      mdata <- mdata %>%
        mutate(sqrt_hours = sqrt(as.numeric(as.character(hours))),
               value = as.numeric(as.character(value))) 
      
      # calculate assessment score averages from interpolated and predicted values
      # this task draws data from the section below and the individual slopes section
      
      # check assessment score shapes (go with median)
      
      mdata_subset<-filter(mdata, variable!="Score_observed", variable!="linear")
      
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
      
      return(individuals_relative_average)
      
    }
    
    individuals_relative_average_new<-modified_individuals_function(handaxe_pca_function) %>%
      filter(variable == "Score_predicted")
    
    replace_na_with_last<-function(x,a=!is.na(x)){
      x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
    }
    
    last_assessment_data = subject_dates_hours %>% 
      mutate(date = as.Date(Date, "%m/%d/%y"),
             Hours = replace_na(Hours,0),
             assessment_date = as.Date(ifelse(is.na(Assessment), NA,
                                              date))) %>%
      group_by(Subject) %>%
      arrange(date) %>%
      mutate(assessment_number = ifelse(is.na(Assessment),
                                        na.locf(Assessment,fromLast = T), #fills NA with last non null value
                                        Assessment),
             next_assessment_date = ifelse(is.na(assessment_date),
                                           na.locf(assessment_date,na.rm = T,fromLast = T), #fills NA with last non null value
                                           assessment_date),
             assessment_number = ifelse(next_assessment_date < date,
                                        NA, assessment_number),
             next_assessment_date = as.Date(ifelse(next_assessment_date < date,
                                                   NA,next_assessment_date)))%>%
      filter(!Scan %in% c(1,2,3) & !Session %in% c(99,120,223,247,273))%>% #remove sessions recorded after last assessment
      group_by(Subject,assessment_number) %>%
      summarise(sum_hours = sum(Hours)) %>%
      mutate(hours_to_next = lag(sum_hours,order_by = assessment_number)) %>%
      ungroup()%>%
      dplyr::rename(knapper=Subject,
                    assessment=assessment_number) %>%
      filter(!is.na(assessment))
    
    # days between assessments
    
    last_days_data = subject_dates_hours %>% 
      mutate(date = as.Date(Date, "%m/%d/%y"),
             Hours = replace_na(Hours,0),
             assessment_date = as.Date(ifelse(is.na(Assessment), NA,
                                              date))) %>%
      group_by(Subject) %>%
      arrange(desc(Subject)) %>%
      subset(Assessment %in% c(1:9)) %>%
      mutate(days=c(NA, diff(assessment_date)))%>%
      ungroup()%>%
      dplyr::rename(knapper=Subject,
                    assessment=Assessment)
    
    # merge back to training hours data
    
    training_scores<-merge(last_assessment_data,individuals_relative_average_new
                           [ , c("knapper","assessment","value", "score_relative_median")],
                           by=c("knapper","assessment"))
    
    training_scores_complete<-full_join(training_scores,last_days_data
                                        [ , c("knapper","assessment","assessment_date","days")],
                                        by=c("knapper","assessment")) %>%
      mutate(hours_days=sum_hours/days,
             log_training_hours=log(hours_days),
             stage=ifelse(assessment %in% c(4:9),"late","early"))
    
    training_scores_complete$log_training_hours[training_scores_complete$log_training_hours == -Inf] <- 0
    
    stage_names <- list(
      'late'="Later Stage (4:9)",
      'early'="Earlier Stage (1:3)"
    )
    
    stage_labeller <- function(variable,value){
      return(stage_names[value])
    }
    
    plot_6 = ggplot(training_scores_complete,aes(colour=stage))+
      geom_jitter(aes(log_training_hours,value, colour=stage)) + 
      geom_smooth(data=subset(training_scores_complete,stage=="late"),aes(log_training_hours,value, colour=stage), method=lm, se=FALSE) +
      facet_wrap(~stage,labeller=stage_labeller)+ 
      theme(legend.position="none")+
      labs(x = "Log training hours / day", y = "Predicted score")+
      scale_color_manual(values = c("red", "black"))+
      theme(text = element_text(size=20))
    
    # colored by knapper
    
    plot_7 = ggplot(training_scores_complete,aes(colour=as.factor(knapper)))+
      geom_jitter(aes(log_training_hours,value, colour=as.factor(knapper))) + 
      geom_smooth(data=subset(training_scores_complete,stage=="late"),aes(log_training_hours,value, colour=stage), method=lm, se=FALSE) +
      facet_wrap(~stage,labeller=stage_labeller)+ 
      #theme(legend.position="none")+
      labs(x = "Log training hours / day", y = "Predicted score")+
      scale_color_viridis_d()+
      theme(text = element_text(size=20))
    
    ## Check correlation between practice density and skill scores
    
    # check training density variance CHECK DENSITIES HERE, SOME ASSESSMENTS THROWING THINGS OFF
    
    training_density_cv<-
      training_scores_complete %>%
      group_by(assessment) %>%
      filter(!knapper=="3" | !assessment == "5")  %>% #have to filter as dates listed one after the other in Nada sheets
      filter(!knapper=="16" | !assessment == "9")  %>% 
      mutate(training_density_cv = sd(hours_days,na.rm = T)/mean(hours_days,na.rm = T)*100)
    
    ggplot(training_density_cv,aes(x=as.factor(assessment),y=training_density_cv))+
      geom_point()
    
    training_scores_late<-subset(training_scores_complete,assessment %in% c(4:9))
    training_scores_early<-subset(training_scores_complete,assessment %in% c(1:3))
    training_scores1=subset(training_scores_complete,assessment==1)
    
    cor.test(training_scores_early$log_training_hours,training_scores_early$value)
    cor.test(training_scores_late$log_training_hours,training_scores_late$value)
    
    return(list(plot_1,plot_6))
    
  }
  
  c(training_density,
    training_density_score) %<-% 
    handaxe_training_density_function(subject_dates_hours,
                              handaxe_pca_function)
  