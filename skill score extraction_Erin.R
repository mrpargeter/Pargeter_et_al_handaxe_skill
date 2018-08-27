#######Nada's score for preceeding assessment/first assessment

library(survival)
library(reshape2)
library(fANCOVA)
library(tidyr)
library(dplyr)
library(zoo)

subject_6 <- cumulative_hours_with_slope %>% filter(Subject ==6)
subject_6 <- subject_6 %>% filter(date < '2015-06-26')

df_proper_6 <- rbind(cumulative_hours_with_slope %>% filter(Subject != 6),
                    subject_6)

test = df_proper_6 %>% 
  mutate(
    date = as.Date(Date, "%m/%d/%y"),
    Hours = replace_na(Hours,0),
    scan_date = as.Date(ifelse(is.na(Scan), NA,
                               date)),
    assessment_date=as.Date(ifelse(is.na(Assessment), NA,
                                   date))) %>%
  group_by(Subject) %>%
  arrange(desc(date)) %>%
  mutate(
    scan_number = ifelse(is.na(Scan),  #PROBLEM HERE: na.locf(na.rm = F, fromLast = TRUE): does not work here, but works for training_session_dates data
                         na.locf(Scan), #fills NA with last non null value
                         Scan),
    next_scan_date = as.Date(ifelse(is.na(scan_date),
                                    na.locf(scan_date), #fills NA with last non null value
                                    scan_date))) %>%
  ungroup()

hours_scans<-test

colnames(hours_scans)[which(names(hours_scans) == "Assessment")] <- "assessment"
colnames(hours_scans)[which(names(hours_scans) == "Subject")] <- "knapper"

scores<-merge(hours_scans,handaxe_postpca_scaled[ , c("knapper","assessment","Score","Training.Hours")],
              by=c("knapper","assessment"),all=T)

##nada score scan 1
nada_score_1<-scores %>%
  subset(assessment == 1)%>%
  mutate(nada_score_1=Score)%>%
  select(c(knapper,assessment,cum_training_hours,nada_score_1,scan_number))

##nada score scan 2
scores_reduced_scan2<-scores %>%
  filter(Scan == 2 | (is.na(Scan) & scan_number == 2)) %>%
  select(knapper,assessment,scan_date,assessment_date,cum_training_hours,scan_number,Score)%>%
  group_by(knapper)%>%
  filter(assessment_date == max(assessment_date, na.rm = T))%>%
  mutate(nada_score_2=Score)%>%
  select(c(knapper,assessment,cum_training_hours,nada_score_2,scan_number))

scores_reduced_scan3<-scores %>%
  filter(Scan == 3 | (is.na(Scan) & scan_number == 3)) %>%
  select(knapper,assessment,scan_date,assessment_date,cum_training_hours,scan_number,Score)%>%
  group_by(knapper)%>%
  filter(assessment_date == max(assessment_date, na.rm = T))%>%
  mutate(nada_score_3=Score)%>%
  select(c(knapper,assessment,cum_training_hours,nada_score_3,scan_number))

nada_scores_complete<-merge(nada_score_1,scores_reduced_scan2,
                            by=c("knapper","assessment","cum_training_hours","scan_number"),all=T)
nada_scores_complete<-merge(nada_scores_complete,scores_reduced_scan3,
                            by=c("knapper","assessment","cum_training_hours","scan_number"),all=T)

###merge main and outlier handaxes
##need scores for outlier handaxes (check outlier_handaxes file for numbers)
##run fit_2 to predict from outlier handaxes
##extract values into submit_all_fit_2_outliers file
##code is in model building r script
model_dates_outliers<-merge(submit_all_fit_2,submit_all_fit_2_outliers,by=c("knapper","assessment","Score_predicted","Score_observed"),all=T)

#then merge in control handaxes dietz didn't score, but we have predictions for
#not model sub-components though-no NADA scores to do that
model_dates_outliers<-merge(model_dates_outliers,submit_all_fit_2_control_not_scored,by=c("knapper","assessment","Score_predicted","Score_observed"),all=T)

#####merge with hours scans data
model_dates_complete<-merge(hours_scans,model_dates_outliers,by=c("knapper","assessment"),all=T)

##model score scan 1
model_score_1<-model_dates_complete %>%
  subset(assessment == 1)%>%
  mutate(model_score_1=Score_predicted)%>%
  select(c(knapper,assessment,cum_training_hours,model_score_1,scan_number))

##model score scan 2
model_scores_reduced_scan2<-model_dates_complete %>%
  filter(Scan == 2 | (is.na(Scan) & scan_number == 2)) %>%
  select(knapper,assessment,scan_date,assessment_date,next_scan_date,scan_number,cum_training_hours,Score_predicted)%>%
  group_by(knapper)%>%
  filter(assessment_date == max(assessment_date, na.rm = T))%>%
  mutate(model_score_2=Score_predicted)%>%
  select(c(knapper,assessment,cum_training_hours,next_scan_date,assessment_date,scan_number,model_score_2))

##model score scan 3
model_scores_reduced_scan3<-model_dates_complete %>%
  subset(Group=="trained")%>% #filters untrained who have ass 1 scores associated with scan 3
  filter(Scan == 3 | (is.na(Scan) & scan_number == 3)) %>%
  select(knapper,assessment,scan_date,assessment_date,scan_number,next_scan_date,cum_training_hours,Score_predicted)%>%
  group_by(knapper)%>%
  filter(assessment_date == max(assessment_date, na.rm = T))%>%
  mutate(model_score_3=Score_predicted)%>%
  select(c(knapper,assessment,cum_training_hours,next_scan_date,scan_number,model_score_3))

model_scores_complete<-merge(model_score_1,model_scores_reduced_scan2,
                            by=c("knapper","assessment","cum_training_hours","scan_number"),all=T)
model_scores_complete<-merge(model_scores_complete,model_scores_reduced_scan3,
                            by=c("knapper","assessment","cum_training_hours","next_scan_date","scan_number"),all=T)

#########combine nada and model scores
nada_model_scores_combined<-merge(model_scores_complete,nada_scores_complete,
                                  by=c("knapper","assessment","cum_training_hours","scan_number"),all=T)

#########combine nada and model scores with previous months practice hours
last_month_data_test<-last_month_data
colnames(last_month_data_test)[which(names(last_month_data_test) == "Subject")] <- "knapper"

nada_model_scores_combined<-merge(nada_model_scores_combined,last_month_data_test[,c("knapper","previous_hours","next_scan_date","scan_number")],
                                  by=c("knapper","next_scan_date","scan_number"),all.x=T)

colnames(nada_model_scores_combined)[which(names(nada_model_scores_combined) == "previous_hours")] <- "previous_month_practice_hours"

#####Individual scores relative to group medians
nada_model_scores_combined_median<-merge(individuals_relative_average_1_5_6_7_9_spread,nada_model_scores_combined,by=c("knapper","assessment"),all.y=T)

#####Practice density since last scan
last_days_scans_data_test<-last_days_scans_data
last_days_scans_data_test$scan_number<-last_days_scans_data_test$Scan

nada_model_scores_combined_median_density<-merge(last_days_scans_data_test[,c("knapper","days","scan_number")],nada_model_scores_combined_median,by=c("knapper","scan_number"),all.y = T)

nada_model_scores_combined_median_density<-nada_model_scores_combined_median_density %>%
  mutate(days=ifelse(assessment==1,0,days),
         scan_number=ifelse(assessment==1,1,scan_number),
         practice_density_since_last_scan=cum_training_hours/days)%>% 
  dplyr::rename(days_since_last_scan = days)

#remove subjects 18 and 20-not included in lithic analysis
nada_model_scores_combined_median<-subset(nada_model_scores_combined_median,!knapper %in% c("18","20"))

######Interpolated values from each knapper's linear curve

#extract info from existing extrapolated scores data
test<-subset(nada_model_scores_combined_median, select=c("knapper","assessment","scan_number","cum_training_hours"))
test<-na.omit(test)
test$sqrt_hours<-sqrt(test$cum_training_hours)

#merge with individual data and keep sqrt hours from test dataset (contains hours at specific scan)
complete_data_indiv_new<-merge(test,complete_data_indiv[,c("Score_predicted","knapper","assessment")],by=c("knapper","assessment"),all=T)

complete_data_indiv_new<-complete_data_indiv_new %>%
  mutate(sqrt_hours=sqrt(cum_training_hours))

individual_interpolations <- function(knapper_number){
  
  data_kn =  complete_data_indiv %>% subset(knapper %in% knapper_number)
  model_kn<-lm(Score_predicted~sqrt_hours, data=data_kn)
  data_kn_new = complete_data_indiv_new %>% subset(knapper %in% knapper_number)
  data_kn_new$linear_interpolated_score = predict(model_kn, data_kn_new)
  
  return(data_kn_new)  }

one<-individual_interpolations("1")
two<-individual_interpolations("2")
three<-individual_interpolations("3")
five<-individual_interpolations("5")
six<-individual_interpolations("6")
seven<-individual_interpolations("7")
eight<-individual_interpolations("8")
nine<-individual_interpolations("9")
ten<-individual_interpolations("10")
eleven<-individual_interpolations("11")
thirteen<-individual_interpolations("13")
fourteen<-individual_interpolations("14")
fifteen<-individual_interpolations("15")
sixteen<-individual_interpolations("16")
seventeen<-individual_interpolations("17")
nineteen<-individual_interpolations("19")
twenty_one<-individual_interpolations("21")

individual_interpolated_scores<-rbind(one,two,three,five,six,seven,eight,nine,ten,eleven,
                                      thirteen,fourteen,fifteen,sixteen,seventeen,
                                      nineteen,twenty_one)

nada_model_scores_combined_median_interpolated<-merge(nada_model_scores_combined_median,individual_interpolated_scores[,c("knapper","assessment","scan_number","linear_interpolated_score")],by=c("knapper","assessment","scan_number"),all.x = T)

######Interpolated values from each knapper's loess fit curve

complete_data_indiv_new<-complete_data_indiv_new %>%
   mutate(hours=cum_training_hours)

individual_loess_interpolations <- function(knapper_number){
  
  data_loess_kn =  complete_data_indiv %>% subset(knapper %in% knapper_number)
  model_loess_kn<-loess(Score_predicted~hours, data=data_loess_kn,span = 0.65,na.action = na.exclude)
  data_loess_kn_new = complete_data_indiv_new %>% subset(knapper %in% knapper_number)
  data_loess_kn_new$loess_interpolated_score = predict(model_loess_kn, data_loess_kn_new)
  
  return(data_loess_kn_new)  }

individual_loess_interpolations <- function(knapper_number){
  
  data_loess_kn =  complete_data_indiv %>% subset(knapper %in% knapper_number)
  model_loess_kn<-loess(Score_predicted~hours, data=data_loess_kn,span = 0.9,na.action = na.exclude)
  data_loess_kn_new = complete_data_indiv_new %>% subset(knapper %in% knapper_number)
  data_loess_kn_new$loess_interpolated_score = predict(model_loess_kn, data_loess_kn_new)
  
  data_loess_kn_new<-
    data_loess_kn_new %>%
    filter(!is.na(loess_interpolated_score))
  
  return(data_loess_kn_new)  }

one_loess<-individual_loess_interpolations('1')
two_loess<-individual_loess_interpolations('2')
three_loess<-individual_loess_interpolations('3')
five_loess<-individual_loess_interpolations('5')
six_loess<-individual_loess_interpolations('6')
seven_loess<-individual_loess_interpolations('7')
eight_loess<-individual_loess_interpolations('8')   #too few observations
nine_loess<-individual_loess_interpolations('9')
ten_loess<-individual_loess_interpolations('10')
eleven_loess<-individual_loess_interpolations('11')
thirteen_loess<-individual_loess_interpolations('13')
fourteen_loess<-individual_loess_interpolations('14')
fifteen_loess<-individual_loess_interpolations('15')
sixteen_loess<-individual_loess_interpolations('16')
seventeen_loess<-individual_loess_interpolations('17')
nineteen_loess<-individual_loess_interpolations('19')
twentyone_loess<-individual_loess_interpolations('21')

individual_loess_interpolated_scores<-rbind(one_loess,two_loess,three_loess,five_loess,six_loess,seven_loess,eight_loess,
                                            nine_loess,ten_loess,eleven_loess,thirteen_loess,fourteen_loess,fifteen_loess,
                                            sixteen_loess,seventeen_loess,nineteen_loess,twentyone_loess)

#to check individual line fits
lw1 <- loess(Score_predicted ~ hours,data=data_5,span = 0.9,na.action = na.exclude)
plot(Score_predicted ~ hours,data=data_5,pch=19,cex=1)
j <- order(data_5$hours)
lines(data_5$hours[j],lw1$fitted[j],col="red",lwd=3)

#to check optimal spans:have to do by subset dataframes
my.loess<-loess.as(data_8$Score_predicted,data_8$hours,degree = 1,criterion = c("aicc","gcv")[2],user.span = NULL,plot=F)
summary(my.loess)

#merge data
nada_model_scores_combined_median_interpolated_loess<-merge(nada_model_scores_combined_median_interpolated,
                                                            individual_loess_interpolated_scores
                                                            [,c("knapper","assessment","scan_number","loess_interpolated_score")],all.x = T)

#cannot reassign untrained scan number based on assessment number: they had assessments after scan 3
#so have to subset then rejoin trained and untrained groups
nada_model_scores_combined_median_interpolated_loess_trained<-
  nada_model_scores_combined_median_interpolated_loess %>%
  subset(!knapper %in% c("22","23","24","25","26","27","28","31","32","33","34","35","37"))%>%
  mutate(scan_number=as.factor(ifelse(assessment=='1','1',scan_number)))

untrained_to_join<-
  nada_model_scores_combined_median_interpolated_loess %>%
  subset(knapper %in% c("22","23","24","25","26","27","28","31","32","33","34","35","37"))%>%
  mutate(scan_number=as.factor(scan_number))

nada_model_scores_combined_median_interpolated_loess_joined<-
  full_join(y = nada_model_scores_combined_median_interpolated_loess_trained, x = untrained_to_join)

##########Nada score model sub-components
#the combined models file from models_nada_score R code file
all_models_test<-all_models

all_models_test<-subset(all_models_test,!score_component =="all")

all_models_test<-all_models_test %>%
  select(c(knapper,assessment,Score_predicted,score_component))

test_components<-dcast(all_models_test,knapper+assessment ~ score_component,value.var = "Score_predicted")

#add outliers for Erin
all_models_merged_outliers<-rbind(all_models_outliers_components,test_components)

nada_model_scores_combined_median_interpolated_loess_components<-merge(all_models_merged_outliers,nada_model_scores_combined_median_interpolated_loess_joined,by=c("knapper","assessment"),all.y = T)

###Now add last remaining metrics
#Slope
nada_model_scores_combined_median_interpolated_loess_components_slopes<-merge(individual_slopes[,c("slope","knapper")],nada_model_scores_combined_median_interpolated_loess_components,by="knapper",all.y = T)

#Wisc Card Sort
nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc<-merge(psychometric_card_sort[,c("knapper","Card_sort_1_PE_start")],nada_model_scores_combined_median_interpolated_loess_components_slopes,by="knapper",all.y = T)

#TOL
nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc_tol<-merge(psychometric[,c("knapper","ToL_1_EM_start")],nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc,by="knapper",all.y = T)

#Assessment two
model_score_assessment_2<-model_dates_complete %>%
  subset(assessment == 2)%>%
  mutate(model_score_assessment_2=Score_predicted) %>%
  select(c(knapper,model_score_assessment_2)) %>%
  filter(!knapper=="18")

nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc_tol_two<-merge(model_score_assessment_2,nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc_tol,by="knapper",all.y = T)

#Assessment three
model_score_assessment_3<-model_dates_complete %>%
  subset(assessment == 3)%>%
  mutate(model_score_assessment_3=Score_predicted) %>%
  select(c(knapper,model_score_assessment_3)) %>%
  filter(!knapper=="18")

nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc_tol_two_three<-merge(model_score_assessment_3,nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc_tol_two,by="knapper",all.y = T)

######simplify columns
nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc_tol_two_three<-
  nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc_tol_two_three %>%
  mutate(model_scores=coalesce(model_score_1, model_score_2,model_score_3),
         nada_scores=coalesce(nada_score_1, nada_score_2,nada_score_3),
         score_relative_median=coalesce(score_relative_median_one, score_relative_median_five,score_relative_median_six,score_relative_median_seven,score_relative_median_nine))

colnames(nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc_tol_two_three)

toolmaking_scores<-
  nada_model_scores_combined_median_interpolated_loess_components_slopes_wisc_tol_two_three %>%
  select(knapper,assessment,scan_number,cum_training_hours,previous_month_practice_hours,slope,outcomes,perceptual_motor,strategic,
         linear_interpolated_score,loess_interpolated_score,model_scores,nada_scores,score_relative_median,model_score_assessment_2,
         model_score_assessment_3,Card_sort_1_PE_start,ToL_1_EM_start)%>% 
  dplyr::rename(learning_slope=slope,
         outcomes_score = outcomes,
         perceptual_motor_score=perceptual_motor,
         strategic_score=strategic,
         wisconsin_card_sort_pe_start=Card_sort_1_PE_start,
         tol_em_start=ToL_1_EM_start)

str(toolmaking_scores)

######add practice rate in less than month prior to scan
testeroo <-scores %>% subset(!is.na(Scan))
scan_assessment<-left_join(toolmaking_scores %>% mutate(scan_number = as.numeric(scan_number)),
                           testeroo %>% select("knapper","Scan","scan_date"), 
                           by = c('knapper' = 'knapper', 'scan_number' = 'Scan')) %>%
  left_join(scores %>% select("knapper","assessment","assessment_date"),
            by = c('knapper', 'assessment')) %>%
  select(knapper,assessment,scan_date,assessment_date)%>%
  subset(assessment %in% c(2:9))

scan_assessment$days <- as.numeric(scan_assessment$scan_date-scan_assessment$assessment_date)

dates_for_hours = scan_assessment %>% select(knapper, assessment_date, scan_date)

total_hours = full_join(scores %>% select(knapper, date, Hours), 
                        dates_for_hours, by = 'knapper') %>%
  filter(date >= assessment_date,  #filter to find days between assessment and scan
         date < scan_date) %>%
  group_by(knapper, assessment_date, scan_date) %>%
  summarise(
    cumulative_hours = as.numeric(sum(Hours))  #create cumulative hours variable
  )

scan_assessment_with_hours = left_join(scan_assessment,
                                       total_hours,
                                       by = c('knapper', 'assessment_date', 'scan_date'))

scan_assessment_with_hours$practice_hours_within_scan_month<-scan_assessment_with_hours$cumulative_hours/scan_assessment_with_hours$days

toolmaking_scores_clean<-merge(toolmaking_scores,scan_assessment_with_hours[,c("knapper","assessment","practice_hours_within_scan_month")],by=c("knapper","assessment"),all.x = T)

#order by knapper and assessment
toolmaking_scores_clean<- toolmaking_scores_clean[with(toolmaking_scores_clean, order(knapper, assessment)), ]

write.csv(toolmaking_scores_clean,"toolmaking_scores_clean.csv")

pairs(~learning_slope+outcomes_score+perceptual_motor_score+strategic_score+linear_interpolated_score+loess_interpolated_score+
        model_scores+nada_scores+score_relative_median+model_score_assessment_3+model_score_assessment_2+
        wisconsin_card_sort_pe_start+tol_em_start,data=toolmaking_scores_clean, 
      main="Simple Scatterplot Matrix")

########new data erin######
##days between scans
str(subject_dates_hours)

last_days_scan_data = subject_dates_hours %>% 
  mutate(date = as.Date(Date, "%m/%d/%y"),
         Hours = replace_na(Hours,0),
         scan_date = as.Date(ifelse(is.na(Scan), NA,
                                   date),origin="1970-01-01")) %>%
  group_by(Subject) %>%
  subset(Scan %in% c(1:3)) %>%
  arrange(order(Subject,Scan)) %>%
  mutate(days=c(NA, diff(scan_date)))%>%
  dplyr::rename(days_since_last_scan=days)%>%
  select(c(Subject,Scan,days_since_last_scan))

last_15day_scan_data = subject_dates_hours %>% 
  mutate(
    date = as.Date(Date, "%m/%d/%y"),
    Hours = replace_na(Hours,0),
    scan_date = as.Date(ifelse(is.na(Scan), NA,
                              date),origin="1970-01-01")) %>%
  
  group_by(Subject) %>%
  arrange(date) %>%
  mutate(
    Scan = ifelse(is.na(Scan),
                      na.locf(Scan,na.rm = F, fromLast = TRUE), #fills NA with last non null value
                  Scan),
    next_scan_date = as.Date(ifelse(is.na(scan_date),
                                   na.locf(scan_date, na.rm = F, fromLast = TRUE), #fills NA with last non null value
                                   scan_date),origin="1970-01-01")) %>%
  filter(date >= next_scan_date - as.difftime(15, unit="days")) %>%
  select(Subject,Scan,Hours,next_scan_date) %>%
  group_by(Subject,next_scan_date,Scan) %>%
  summarise(
    previous_15days_hours_training = sum(Hours)
  )

erin_days_data<-merge(last_days_scan_data,last_15day_scan_data[,c("Subject","Scan","previous_15days_hours_training")],by=c("Subject","Scan"))

write.csv(erin_days_data,"erin_days_data.csv")
