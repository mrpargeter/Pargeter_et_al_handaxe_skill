##############Core profile width data##############

profile_filenames<-list.files("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/Handaxe shape data/Cores/Profile", pattern="*.txt")

open_read_cores<-function(filename, foldername){
  open_file<-read.delim(paste(foldername,"/",filename,sep=""),header=F,sep='_')
  colnames(open_file)<-c("variable","measurement_point","measurement")
  open_file$Core.Number<-gsub(".txt","",filename)
  return(open_file)
}

#Combine txt files for each recording set

core_profile_measurements<-do.call(rbind,lapply(profile_filenames,open_read_cores,"/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/Handaxe shape data/Cores/Profile"))

#rename width to thickness for profile and uppercase to lowercase for plan
library(dplyr)
core_profile_measurements$variable<-recode_factor(core_profile_measurements$variable, Width = "thickness")

#Convert from cm to mm
core_profile_measurements$shape_width_mm<-core_profile_measurements$measurement*10
core_profile_measurements$measurement<-NULL
core_profile_measurements$Core.Number<-as.numeric(core_profile_measurements$Core.Number)

#add designator to core number
core_profile_measurements$Core.Number = paste('h', core_profile_measurements$Core.Number, sep='_') #these are all handaxe cores

#create combined measurement point, measurement type column
core_profile_measurements$measurement_point = paste(core_profile_measurements$variable, core_profile_measurements$measurement_point, sep="_")

#reshape dataset
library(reshape2)
core_profile_measurements_reshape <- dcast(core_profile_measurements, ... ~ measurement_point, value.var="shape_width_mm")

core_profile_measurements_reshape$variable <-NULL

#calculate thickness CV

library(raster)
core_profile_measurements_reshape$core_profile_thickness_cv<-apply(core_profile_measurements_reshape[,c(2:10)], 1, cv)

###########code to test to see if core numbers match with handaxe datasets##########

s<-subset(complete_experiment_data, condition %in% "main" & experiment %in% "handaxe")
s<-data.frame(s$Core.Number)
s$s.Core.Number<-factor(s$s.Core.Number)
s_core<-data.frame(unique(core_profile_measurements$Core.Number))
s_core$unique.core_profile_measurements.Core.Number.<-factor(s_core$unique.core_profile_measurements.Core.Number.)

levels(s$s.Core.Number)
levels(s_core$unique.core_profile_measurements.Core.Number.)

s$s.Core.Number[!(s$s.Core.Number %in% s_core$unique.core_profile_measurements.Core.Number.)]
s_core$unique.core_profile_measurements.Core.Number.[!(s_core$unique.core_profile_measurements.Core.Number. %in% s$s.Core.Number)]

n_occur <- data.frame(table(s$s.Core.Number))

############################

##############Control Core profile width data##############

control_profile_filenames<-list.files("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/Handaxe shape data/Control_handaxes/Profile", pattern="*.txt")

open_read_cores<-function(filename, foldername){
  open_file<-read.delim(paste(foldername,"/",filename,sep=""),header=F,sep='_')
  colnames(open_file)<-c("variable","measurement_point","measurement")
  open_file$Core.Number<-gsub(".txt","",filename)
  return(open_file)
}

#Combine txt files for each recording set

control_core_profile_measurements<-do.call(rbind,lapply(control_profile_filenames,open_read_cores,"/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/Handaxe shape data/Control_handaxes/Profile"))

#rename width to thickness for profile and uppercase to lowercase for plan
library(dplyr)
control_core_profile_measurements$variable<-recode_factor(control_core_profile_measurements$variable, Width = "thickness")

#Convert from cm to mm
control_core_profile_measurements$shape_width_mm<-control_core_profile_measurements$measurement*10
control_core_profile_measurements$measurement<-NULL
control_core_profile_measurements$Core.Number<-as.numeric(control_core_profile_measurements$Core.Number)

#add designator to core number
control_core_profile_measurements$Core.Number = paste('h', control_core_profile_measurements$Core.Number, sep='_') #these are all handaxe cores

#create combined measurement point, measurement type column
control_core_profile_measurements$measurement_point = paste(control_core_profile_measurements$variable, control_core_profile_measurements$measurement_point, sep="_")

#reshape dataset
library(reshape2)
control_core_profile_measurements_reshape <- dcast(control_core_profile_measurements, ... ~ measurement_point, value.var="shape_width_mm")

control_core_profile_measurements_reshape$variable <-NULL

#calculate thickness CV

library(raster)
control_core_profile_measurements_reshape$core_profile_thickness_cv<-apply(control_core_profile_measurements_reshape[,c(2:10)], 1, cv)

###########code to test to see if control core numbers match with control handaxe datasets##########
all_experiment_data<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/all_experiment_data.csv",header = T, na.strings=c('',""))

s<-subset(all_experiment_data, condition %in% "control" & experiment %in% "handaxe")
s<-data.frame(s$Core.Number)
s$s.Core.Number<-factor(s$s.Core.Number)
s_core<-data.frame(unique(control_core_profile_measurements$Core.Number))
s_core$unique.control_core_profile_measurements.Core.Number.<-factor(s_core$unique.control_core_profile_measurements.Core.Number.)

levels(s$s.Core.Number)
levels(s_core$unique.control_core_profile_measurements.Core.Number.)

s$s.Core.Number[!(s$s.Core.Number %in% s_core$unique.core_profile_measurements.Core.Number.)]
s_core$unique.control_core_profile_measurements.Core.Number.[!(s_core$unique.control_core_profile_measurements.Core.Number. %in% s$s.Core.Number)]

n_occur <- data.frame(table(s$s.Core.Number))

#####Combine control and regular handaxes together

core_profile_measurements_reshape<-merge(core_profile_measurements_reshape,control_core_profile_measurements_reshape, all=T )

write.csv(core_profile_measurements_reshape,"core_profile_measurements_reshape.csv")
