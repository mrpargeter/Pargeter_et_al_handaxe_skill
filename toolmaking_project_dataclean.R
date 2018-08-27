getwd()
setwd("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology")

######merge handaxe and prediction core numbers and scores######
handaxe_core_numbers_scores<-read.csv("handaxe_experiment_scores_core_number_09_26_17.csv", header = T, na.strings=c('',""))
str(handaxe_core_numbers_scores)

prediction_core_numbers_scores<-read.csv("prediction_experiment_scores_core_number.csv", header = T, na.strings=c('',""))
str(prediction_core_numbers_scores)

str(handaxe_core_numbers_scores)
handaxe_core_numbers_scores[5] <- lapply(handaxe_core_numbers_scores[5], as.factor)
handaxe_core_numbers_scores[1:2] <- lapply(handaxe_core_numbers_scores[1:2], as.factor)
handaxe_core_numbers_scores$Core.Number = paste('h', handaxe_core_numbers_scores$Core.Number, sep='_')

str(prediction_core_numbers_scores)
prediction_core_numbers_scores[5] <- lapply(prediction_core_numbers_scores[5], as.factor)
prediction_core_numbers_scores[1:2] <- lapply(prediction_core_numbers_scores[1:2], as.factor)
prediction_core_numbers_scores$Core.Number = paste('p', prediction_core_numbers_scores$Core.Number, sep='_')

merged_experiment_data<-rbind(prediction_core_numbers_scores, handaxe_core_numbers_scores)

merged_experiment_data$condition<-rep("main",length(merged_experiment_data$Core.Number))
colnames(merged_experiment_data)[colnames(merged_experiment_data)=="Experiment"]<-"experiment"

##############Core weights####################
#Bring in start analysis files
handaxe_start_weight<-read.csv("handaxe_start_weights.csv", header = T, na.strings=c('',""))
handaxe_start_weight$Core.Number = paste('h', handaxe_start_weight$Core.Number, sep='_')
handaxe_start_weight$experiment<-rep("handaxe",length(handaxe_start_weight$Core.Number))

prediction_start_weight<-read.csv("prediction_start_weights.csv", header = T, na.strings=c('',""))
prediction_start_weight$Core.Number = paste('p', prediction_start_weight$Core.Number, sep='_')
prediction_start_weight$experiment<-rep("prediction",length(prediction_start_weight$Core.Number))

#Bring in end analysis files
handaxe_end_weight<-read.csv("handaxe_end_weights.csv", header = T, na.strings=c('',""))
handaxe_end_weight$Core.Number = paste('h', handaxe_end_weight$Core.Number, sep='_')

prediction_end_weight<-read.csv("prediction_end_weights.csv", header = T, na.strings=c('',""))
prediction_end_weight$Core.Number = paste('p', prediction_end_weight$Core.Number, sep='_')

#Merge handaxe and prediction core start weights

merged_start_weight_data<-rbind(handaxe_start_weight, prediction_start_weight)

#Merge handaxe and prediction core end weights

merged_end_weight_data<-rbind(handaxe_end_weight, prediction_end_weight)
colnames(merged_end_weight_data)[colnames(merged_end_weight_data)=="Experiment"]<-"experiment"

#Merge handaxe and prediction core start and end weights

merged_start_end_weight_data<-merge(merged_start_weight_data, merged_end_weight_data, by=c("Core.Number", "experiment"))

colnames(merged_start_end_weight_data)[3]<-"Core.start.weight"

merged_start_end_weight_data$condition<-rep("main",length(merged_start_end_weight_data$Core.Number))

#Merge with pre-experiment handaxe data above
merged_experiment_weight_data<-merge(merged_experiment_data, merged_start_end_weight_data, by=c("Knapper","Assessment","experiment","condition"), all = T)

merged_experiment_weight_data$Core_weight<-NULL
merged_experiment_weight_data$Core.Number.y<-NULL
merged_experiment_weight_data$Core.starting.weight<-NULL
colnames(merged_experiment_weight_data)[colnames(merged_experiment_weight_data)=="Core.Number.x"]<-"Core.Number"

write.csv(merged_experiment_weight_data,"merged_experiment_weight_data.csv")

##############control experiment handaxe and core data####################
#Bring in analysis file

control_experiment_data<-read.csv("control_experiment_data_09_27_17.csv", header = T, na.strings=c('',""))
str(control_experiment_data)
control_experiment_data[,c(1:2, 4)] <- lapply(control_experiment_data[,c(1:2, 4)], as.factor)

control_experiment_data$Core.Number <- ifelse(control_experiment_data$Experiment == "handaxe",
                                              paste("h",control_experiment_data$Core.Number,sep = "_"),
                                              paste("p",control_experiment_data$Core.Number,sep = "_"))

colnames(control_experiment_data)[colnames(control_experiment_data)=="Experiment"]<-"experiment"
colnames(control_experiment_data)[colnames(control_experiment_data)=="Condition"]<-"condition"
                         
#Merge handaxe analysis file with data from above 

complete_handaxe_experiment_data<-merge(merged_experiment_weight_data, control_experiment_data, 
                                by=c("Knapper","Assessment","Score","Core.Number","condition","experiment","Core.start.weight","Core.end.weight"), all = T)

colnames(complete_handaxe_experiment_data)[colnames(complete_handaxe_experiment_data)=="Knapper"]<-"knapper"
colnames(complete_handaxe_experiment_data)[colnames(complete_handaxe_experiment_data)=="Assessment"]<-"assessment"

str(complete_handaxe_experiment_data)

complete_handaxe_experiment_data[5] <- lapply(complete_handaxe_experiment_data[5], as.factor)
library(plyr)
complete_handaxe_experiment_data$condition <- revalue(complete_handaxe_experiment_data$condition, c("Control"="control"))

#Merge shape and experiment data
getwd()
complete_handaxe_experiment_shape_data<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/Handaxe shape data/complete_handaxe_experiment_shape_data.csv",header = T, na.strings=c('',""))

complete_experiment_data<-merge(complete_handaxe_experiment_shape_data, complete_handaxe_experiment_data, 
                                by=c("knapper","assessment","condition","experiment"), all = T)

colnames(complete_experiment_data)

complete_experiment_data$X<-NULL

#calculate delta columns and delete start and end weight columns
complete_experiment_data$delta_weight<-complete_experiment_data$Core.end.weight/complete_experiment_data$Core.start.weight*100
complete_experiment_data$delta_weight[ complete_experiment_data$delta_weight > 100 ] <- 100

complete_experiment_data$Core.start.weight<-NULL
complete_experiment_data$Core.end.weight<-NULL

#restructure dataframe
detach("package:raster", unload=TRUE)
library(dplyr)

complete_experiment_data<-dplyr::select(complete_experiment_data, knapper, assessment, experiment,condition,
                                 Training.Hours,Score,Core.Number,delta_weight,
                                 width_0.1,width_0.2,width_0.3,width_0.4,width_0.5,width_0.6,
                                 width_0.7,width_0.8,width_0.9,
                                 thickness_0.1,thickness_0.2,thickness_0.3,thickness_0.4,thickness_0.5,thickness_0.6,           
                                 thickness_0.7,thickness_0.8,thickness_0.9,
                                 handaxe_profile_thickness_cv,profile_asymetry_index,plan_asymetry_index,
                                 area_picture,perimeter,max_length, max_width,max_thickness)

#delete rows for subjects who withdrew

complete_experiment_data<-complete_experiment_data[!(complete_experiment_data$knapper=="12" & complete_experiment_data$assessment=="1"),]
complete_experiment_data<-complete_experiment_data[!(complete_experiment_data$knapper=="14" & complete_experiment_data$assessment=="3"),]
complete_experiment_data<-complete_experiment_data[!(complete_experiment_data$knapper=="20" & complete_experiment_data$assessment=="1"),]
complete_experiment_data<-complete_experiment_data[!(complete_experiment_data$knapper=="20" & complete_experiment_data$assessment=="2"),]
complete_experiment_data<-complete_experiment_data[!(complete_experiment_data$knapper=="4" & complete_experiment_data$assessment=="1"),]

#####Add control and regular handaxe attribute data######
handaxe_attribute_data<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Handaxe attributes/Attributes_data/Attribute data excel sheets/handaxe attribute data_01_09_18.csv",header = T, na.strings=c('',""))
control_attribute_data<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Handaxe attributes/Attributes_data/Attribute data excel sheets/control_handaxe_attribute_data_04_18_18.csv",header = T, na.strings=c('',""))

attribute_data<-merge(handaxe_attribute_data,control_attribute_data,all=T)

all_experiment_data<-merge(complete_experiment_data, attribute_data, 
                                by=c("knapper","assessment"), all = T)

all_experiment_data$Training.Hours <- ifelse(all_experiment_data$Training.Hours.x %in% NA,all_experiment_data$Training.Hours.y,all_experiment_data$Training.Hours.x)

all_experiment_data$Training.Hours.x<-NULL
all_experiment_data$Training.Hours.y<-NULL

all_experiment_data$area_box<-(all_experiment_data$max_length*all_experiment_data$max_width*all_experiment_data$max_thickness)*2+(all_experiment_data$max_length*all_experiment_data$max_thickness)*2+(all_experiment_data$max_thickness*all_experiment_data$max_width)*2
all_experiment_data$percent_unflaked_area<-all_experiment_data$unflaked.area_mm2/all_experiment_data$area_picture*100
all_experiment_data$percent_bifacially_flaked<-(all_experiment_data$bifacial.extent_mm/all_experiment_data$perimeter)*100
all_experiment_data$flake_scar_density<-(all_experiment_data$scar.count/all_experiment_data$area_box)

all_experiment_data$scar.count<-NULL
all_experiment_data$unflaked.area_mm2<-NULL
all_experiment_data$bifacial.extent_mm<-NULL

all_experiment_data$area_picture<-NULL
all_experiment_data$area_box<-NULL
all_experiment_data$perimeter<-NULL

#delete rows for control subjects who were not scored
#first subset to create datasheet for Erin to have the scores
all_experiment_data_to_subset<-all_experiment_data
all_experiment_data_controls_not_scored<-subset(all_experiment_data_to_subset,knapper %in% c("28","33","35","37") & experiment == "handaxe")
all_experiment_data_controls_not_scored$Score<-rep("1",times=nrow(all_experiment_data_controls_not_scored))
write.csv(all_experiment_data_controls_not_scored,"all_experiment_data_controls_not_scored.csv")

#then remove controls for rest of the analysis
all_experiment_data<-all_experiment_data[!(all_experiment_data$knapper=="28" & all_experiment_data$assessment=="1"),]
all_experiment_data<-all_experiment_data[!(all_experiment_data$knapper=="29" & all_experiment_data$assessment=="1"),]
all_experiment_data<-all_experiment_data[!(all_experiment_data$knapper=="30" & all_experiment_data$assessment=="1"),]
all_experiment_data<-all_experiment_data[!(all_experiment_data$knapper=="33" & all_experiment_data$assessment=="1"),]
all_experiment_data<-all_experiment_data[!(all_experiment_data$knapper=="35" & all_experiment_data$assessment=="1"),]
all_experiment_data<-all_experiment_data[!(all_experiment_data$knapper=="37" & all_experiment_data$assessment=="1"),]
all_experiment_data<-all_experiment_data[!(all_experiment_data$knapper=="38" & all_experiment_data$assessment=="1"),]

write.csv(all_experiment_data,"all_experiment_data.csv")

#####add core profile width CV data####

core_profile_measurements_reshape<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/Handaxe shape data/core_profile_measurements_reshape.csv")

myvars <- c("Core.Number","core_profile_thickness_cv")
core_thickness_cv<- core_profile_measurements_reshape[myvars]

#merge core profile CV data to handaxe database

library(plyr)
all_experiment_data=join(all_experiment_data,core_thickness_cv,by=c('Core.Number'))

str(all_experiment_data)

#create delta CV column and remove old CV columns

all_experiment_data$core_profile_thickness_cv<-as.numeric(as.character(all_experiment_data$core_profile_thickness_cv))
all_experiment_data$handaxe_profile_thickness_cv<-as.numeric(as.character(all_experiment_data$handaxe_profile_thickness_cv))

all_experiment_data$delta_profile_thickness_cv<-all_experiment_data$core_profile_thickness_cv-all_experiment_data$handaxe_profile_thickness_cv

all_experiment_data$handaxe_profile_thickness_cv<-NULL
all_experiment_data$core_profile_thickness_cv<-NULL

###subset to work with handaxes only
library(dplyr)
complete_experiment_data_handaxe_main<-filter(all_experiment_data, experiment == "handaxe")

colnames(all_experiment_data)

write.csv(complete_experiment_data_handaxe_main,"complete_experiment_data_handaxe_main.csv")
