summary_data<-
  no_first %>%
  subset(Assessment == 9) %>%
  group_by(Subject) %>%
  summarise(max_training = max(cum_training_hours, na.rm = TRUE),
            median_training = median(cum_training_hours, na.rm = TRUE),
            min_training = min(cum_training_hours, na.rm = TRUE))

test<-subject_dates_hours
test$Session<-as.factor(test$Session)
test$Subject<-as.factor(test$Subject)

summary_data<-
  test %>%
  group_by(Session) %>%
  arrange(Session) %>%
  dplyr::summarise(count = n_distinct(Subject))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


summary_data_count<- 
  summary_data %>%
  na.omit(Session) %>%
  mutate(mean=mean(count),
         max=max(count),
         median=median(count),
         mode=getmode(count))
    