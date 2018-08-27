library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(zoo)

subject_dates_hours<-read.csv("/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/training times/dates_hours_overall_only_enrolled.csv")

for_chart = subject_dates_hours %>% mutate(date = as.Date(Date, "%m/%d/%y"),
                                           Hours = replace_na(Hours,0))%>%
                                    group_by(Subject) %>%
                                    mutate(cum_training_hours = cumsum(Hours))

agg_hours_scans = subject_dates_hours %>% 
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
  summarise(
    previous_scan_date = min(date),
    last_training_date = max(date),
    total_days = max(date) - min(date),
    total_hours = sum(Hours)
  ) %>%
  filter(last_scan < 3) %>% 
  mutate(last_scan = last_scan +1)

#scan_chart
for_scan_chart = subject_dates_hours %>% 
             mutate(date = as.Date(Date, "%m/%d/%y"),
                    Hours = replace_na(Hours,0)) %>%
            filter(Group=="main") %>%
            group_by(Subject) %>%
            mutate(cum_training_hours = cumsum(Hours)) 

subject_dates_hours_scan<-subject_dates_hours
subject_dates_hours_scan<-subject_dates_hours[!(subject_dates_hours_scan$Assessment %in% c(1:9)),]
subject_dates_hours_scan$Assessment<-NULL

##### calculate cumulative training hours
cumulative_hours = subject_dates_hours %>% 
  select(-c(Instruction,Pieces,Success,Comments)) %>%
  group_by(Subject) %>%
  arrange(as.Date(Date, "%m/%d/%y")) %>%
  mutate(
      date = as.Date(Date, "%m/%d/%y"),
      Hours = replace_na(Hours,0),
      max_add = as.numeric(max(date) - min(date)), 
      cum_training_hours = cumsum(Hours),
      first_date = min(date))
  
###### to arrange plot by slope steepness: need to cal hours traing/days for first 40 hours
##### use last days data created further down

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
  mutate(row = row_number()) %>%
  group_by(Subject) %>%
  mutate(
    first_row = min(row),
    plot_count = ifelse(date == first_date, 
                 row,
                 first_row + as.numeric((date - first_date)))
  ) %>% ungroup()

############scans + assessments + eye tracking (or just add geom_point as necessary-subset below, can remove)
standard_date<-ggplot(data = subset(cumulative_hours_with_slope,Subject=="11"),
       aes(x = plot_count, y = cum_training_hours, col = as.factor(Subject),
          reorder(Subject, -hours_days)))+
  geom_line()+
  scale_y_continuous(name="Cumulative training hours")+
  scale_x_discrete(name="Days")+
  scale_color_viridis_d()+
  geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Session) & Subject==11), 
            aes(x = plot_count, y = cum_training_hours,col = as.factor(Subject)))+
  geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Scan) & Subject==11), 
             aes(x = plot_count, y = cum_training_hours), col = 'black',size=2) +
  theme(legend.position="none")

###plot with weeks as x-axis
library(scales)

cumulative_hours_with_slope$date <- as.Date(cumulative_hours_with_slope$date)

ggplot(data = subset(cumulative_hours_with_slope,Subject=="7"),
       aes(x = date, y = cum_training_hours,
           reorder(Subject, -hours_days)))+
  geom_line()+
  scale_y_continuous(name="Cumulative training hours")+
  scale_x_date(breaks = date_breaks("2 weeks"), labels = date_format("%d-%m"))+
  geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Session) & Subject==7), 
             aes(x = date, y = cum_training_hours,col = 'red'))+
  geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Scan) & Subject==7), 
             aes(x = date, y = cum_training_hours), col = 'black') +
  theme(legend.position="none")+
  ggtitle(label="Subject 7")
  
##illustrate difference between eye tracking and scan dates #REMOVE CONTROL SUBJECTS IN GEOM_POINT FILTER
ggplot(data = subset(cumulative_hours_with_slope,Group=="main"),
       aes(x = plot_count, y = as.factor(Subject), col = as.factor(Subject),
           reorder(Subject, -hours_days)))+
  geom_line()+
  geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(eye_tracking)), 
             aes(x = plot_count, y = as.factor(Subject)), col = 'red')+
  scale_color_viridis_d()+
  geom_point(data = cumulative_hours_with_slope %>% filter(!is.na(Scan)), 
             aes(x = plot_count, y = as.factor(Subject)), col = 'black')+coord_flip()+
  scale_y_discrete(name="Subject")+
  scale_x_continuous(name = "Days")+
  theme(legend.position="none")
  
###assessments only
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

  ggplot(data = no_first,
   aes(x = plot_count, y = cum_training_hours, col = as.factor(Subject)),
   reorder(Subject, hours_days))+
  geom_line()+
  scale_y_continuous(name="Cumulative training hours")+
  labs(colour="Subject Number")+
  theme(axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  xlab("Subject")+ 
  geom_point(data = no_first %>% filter(!is.na(Assessment)), 
                         aes(x = plot_count, y = cum_training_hours,col = as.factor(Subject)))+
    theme(text = element_text(size=20))+
    scale_color_viridis_d()
  
#subset above plot to show subjects who did badly on last three assessments
  ggplot(data = subset(no_first,Subject %in% c("1","2","5","9","11","16","19")),
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

#####overall practice rates
  practice_rates = cumulative_hours %>% 
    group_by(Subject) %>%
    mutate(days = as.numeric(max(date) - min(first_date)),
           sum_days=sum(days,na.rm = T),
           sum_hours=sum(cum_training_hours,na.rm = T),
           hours_days=sum_hours/sum_days) %>%
    ungroup() %>% select(Subject, hours_days) %>%
    distinct()
  
###training in the last month

last_month_data = subject_dates_hours %>% 
  mutate(
    date = as.Date(Date, "%m/%d/%y"),
    Hours = replace_na(Hours,0),
    scan_date = as.Date(ifelse(is.na(Scan), NA,
                       date))) %>%
  group_by(Subject) %>%
  arrange(date) %>%
  mutate(
    scan_number = ifelse(is.na(Scan),
                         na.locf(Scan,na.rm = F, fromLast = TRUE), #fills NA with last non null value
                         Scan),
    next_scan_date = as.Date(ifelse(is.na(scan_date),
                       na.locf(scan_date, na.rm = F, fromLast = TRUE), #fills NA with last non null value
                       scan_date))) %>%
  filter(date >= next_scan_date %m-% months(1)) %>%
  select(Subject,Assessment,scan_number,Hours,next_scan_date) %>%
  group_by(Subject, next_scan_date, scan_number) %>%
  summarise(
    previous_hours = sum(Hours)
  )

write.csv(last_month_data,"/Users/stoutlab/Google Drive/Toolmaking Project/Pargeter work folder/Toolmaking Stats_Pargeter/Language of Technology/Time series/last_month_data.csv")

######Training hours relative to skill scores######

individuals_relative_average_new<-subset(individuals_relative_average, variable == "Score_predicted")

replace_na_with_last<-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

str(last_assessment_data)

last_assessment_data = subject_dates_hours %>% 
  mutate(date = as.Date(Date, "%m/%d/%y"),
    Hours = replace_na(Hours,0),
    assessment_date = as.Date(ifelse(is.na(Assessment), NA,
                       date))) %>%
  group_by(Subject) %>%
  arrange(date) %>%
  mutate(
    assessment_number = ifelse(is.na(Assessment),
                               na.locf(Assessment,fromLast = T), #fills NA with last non null value
                         Assessment),
    next_assessment_date = ifelse(is.na(assessment_date),
                                          na.locf(assessment_date,fromLast = T), #fills NA with last non null value
                                    assessment_date),
    assessment_number = ifelse(next_assessment_date < date,
                                  NA,
                               assessment_number),
    next_assessment_date = as.Date(ifelse(next_assessment_date < date,
                                  NA,
                                  next_assessment_date)))%>%
  filter(!Scan %in% c(1,2,3) & !Session %in% c(99,120,223,247,273))%>% #remove sessions recorded after last assessment
  group_by(Subject,assessment_number) %>%
  summarise(sum_hours = sum(Hours)) %>%
  mutate(hours_to_next = lead(sum_hours, order_by = assessment_number)) %>%
  ungroup()%>%
  dplyr::rename(knapper=Subject,
         assessment=assessment_number) %>%
  filter(!is.na(assessment))

##days between assessments

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

##days between scans
last_days_scans_data = subject_dates_hours %>% 
  mutate(date = as.Date(Date, "%m/%d/%y"),
         Hours = replace_na(Hours,0),
         scan_date = as.Date(ifelse(is.na(Scan), NA,
                                          date))) %>%
  group_by(Subject) %>%
  subset(Scan %in% c(1:3)) %>%
  arrange(order(Subject,Scan)) %>%
  mutate(days=c(NA, diff(scan_date)))%>%
  ungroup()%>%
  dplyr::rename(knapper=Subject,
                assessment=Assessment)

##merge back to training hours data
training_scores<-merge(last_assessment_data,individuals_relative_average_new[ , c("knapper","assessment","value", "score_relative_median")],by=c("knapper","assessment"))
training_scores<-merge(training_scores,last_days_data[ , c("knapper","assessment","assessment_date","days")],by=c("knapper","assessment"))

training_scores$hours_days<-training_scores$hours_to_next/training_scores$days
training_scores$log_training_hours<-log(training_scores$hours_days)
training_scores$stage<-ifelse(training_scores$assessment %in% c(4:9),"late","early")

training_scores_late<-subset(training_scores,assessment %in% c(4:9)) #effect of practice is NULL before assessment 4
training_scores_early<-subset(training_scores,assessment %in% c(1:3)) #effect of practice is NULL before assessment 4

stage_names <- list(
  'late'="Later Stage (4:9)",
  'early'="Earlier Stage (1:3)"
)

stage_labeller <- function(variable,value){
  return(stage_names[value])
}

ggplot(training_scores,aes(colour=stage))+
  geom_jitter(aes(log_training_hours,value, colour=stage)) + 
  geom_smooth(aes(log_training_hours,value, colour=stage), method=lm, se=FALSE) +
  facet_wrap(~stage,labeller=stage_labeller)+ 
  theme(legend.position="none")+
  labs(x = "Log training hours / day", y = "Predicted score")+
  scale_color_manual(values = c("red", "black"))+
  theme(text = element_text(size=20))

cor.test(training_scores_early$log_training_hours,training_scores_early$value)
cor.test(training_scores_late$log_training_hours,training_scores_late$value)
