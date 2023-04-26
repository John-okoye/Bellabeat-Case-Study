library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(ggpubr)
library(patchwork)

#Importing Data sets
dailyActivity_merged <- read_csv("Raw Data/dailyActivity_merged.csv")
sleepDay_merged <- read_csv("Raw Data/sleepDay_merged.csv")
weightLogInfo_merged <- read_csv("Raw Data/weightLogInfo_merged.csv")

#Saving Data sets to Data frames
Activity <- dailyActivity_merged
Sleep <- sleepDay_merged
Weight <- weightLogInfo_merged

#To get columns relevant for analysis
Activity <- Activity %>% select(Id, ActivityDate, Calories, TotalSteps, TotalDistance, SedentaryMinutes)
  head(Activity)

Sleep <- Sleep %>% select(Id, SleepDay, TotalMinutesAsleep, TotalTimeInBed)
  head(Sleep)

Weight <- Weight %>% select(Id, Date, WeightKg, BMI)
  head(Weight)

#To get number of unique customer ID
n_unique(Activity$Id)
n_unique(Sleep$Id)
n_unique(Weight$Id)

#To check for duplicates and NA values in the data set
sum(duplicated(Activity)) 
sum(duplicated(Sleep))
sum(duplicated(Weight))

sum(is.na(Activity))
sum(is.na(Sleep))
sum(is.na(Weight))

#To drop NA entries and duplicates 
Activity <- Activity %>%
  distinct() %>% 
  drop_na()

Sleep <- Sleep %>%
  distinct() %>% 
  drop_na()

Weight <- Weight %>% 
  distinct()

#To change column name and date format for the date columns 
Activity <- Activity %>% 
  rename(date = ActivityDate) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))
head(Activity)

Sleep <- Sleep %>%
  rename(date = SleepDay) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p"))
head(Sleep)

Weight <- Weight %>%
  rename(date = Date) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p"))
head(Weight)

#To summarize the data set
Activity_Avg <- Activity %>% select(Id, Calories, TotalSteps, TotalDistance, SedentaryMinutes) %>% 
  group_by(Id) %>% 
  summarize(avg_calories = mean(Calories), avg_steps = mean(TotalSteps), 
            avg_distance = mean(TotalDistance), sedentary_avg = mean(SedentaryMinutes))
head(Activity_Avg)

Sleep_Avg <- Sleep %>% select(Id, TotalMinutesAsleep, TotalTimeInBed) %>% 
  group_by(Id) %>% 
  summarize(avg_timeinbed = mean(TotalTimeInBed), avg_min_asleep = mean(TotalMinutesAsleep))
head(Sleep_Avg)

Weight_Avg<- Weight %>% select(Id, WeightKg, BMI) %>%
  group_by(Id) %>% 
  summarize(avg_weight = mean(WeightKg), avg_BMI = mean(BMI))
head(Weight_Avg)

#To merge the data sets for ease of analyzing 
Daily_activity_sleep <- merge(Activity, Sleep, by=c ("Id", "date")) %>% 
  mutate(weekday = weekdays(date))
head(Daily_activity_sleep)

#To get daily insights
weekday_summary <- Daily_activity_sleep %>%
  mutate(sedentary_hours= SedentaryMinutes / 60, 
         hours_asleep = TotalMinutesAsleep / 60, hours_in_bed = TotalMinutesAsleep / 60)
head(weekday_summary)

weekday_summary$weekday <-ordered(weekday_summary$weekday, 
  levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))


weekday_summary <-weekday_summary%>%
  group_by(weekday) %>%
  summarize (calories = mean(Calories), steps = mean(TotalSteps), 
             distance = mean(TotalDistance), sedentary_hours = mean(sedentary_hours),
             hours_asleep = mean(hours_asleep), hours_in_bed = mean(hours_in_bed))
head(weekday_summary)

#Visualizations for daily calories consumption  
ggarrange (
  ggplot(weekday_summary) +
    geom_col(mapping = aes(weekday, calories), fill = ("#12a4d9")) +
    labs(title = "Calories consumption on weekdays", x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)),
  ggplot(weekday_summary) +
    geom_col(mapping = aes(weekday, hours_asleep), fill = ("#6b7b8c")) + 
    geom_hline(yintercept = 8) +
    labs(title = "Hours asleep per weekday", x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
)  
  
#Daily steps and distance
ggarrange (
  ggplot(weekday_summary) +
    geom_col(mapping = aes(x = weekday, y = steps), fill = "#85e0e0") +
    geom_hline(yintercept = 8533) +
    labs(title = "Daily steps per weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)),
  ggplot(weekday_summary) +
    geom_col(mapping = aes(x = weekday, y = distance), fill = "#6b7b8c")+
    labs(title = "Distance per weekday (km)", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
)

#Relationships
ggarrange(
  ggplot(Daily_activity_sleep, aes(x= TotalSteps, y= Calories))+
    geom_jitter() +
    geom_smooth(color = "red") + 
    labs(title = "Daily steps vs Calories", x = "Daily steps", y= "Calories") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14)), 
  ggplot(Daily_activity_sleep, aes(x= SedentaryMinutes, y=Calories))+
    geom_jitter() +
    geom_smooth(color = "red") + 
    labs(title = "Sedentary Minutes vs Calories", x = "Sedentary Minutes", y= "Calories") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14))
)

#Daily use
Daily_use <- Daily_activity_sleep %>%
  group_by(Id) %>%
  summarize(days_used=n())%>%
  mutate(usage_level = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  ))
head(Daily_use)

#Percentage of total
daily_use_perc <- Daily_use %>% 
  group_by(usage_level) %>% 
  summarize(level_totals = n()) %>% 
  mutate(total = sum(level_totals)) %>%
  group_by(usage_level) %>% 
  summarize(total_perc = level_totals/ total) %>%
  mutate(labels = scales::percent(total_perc))
head(daily_use_perc)

#Visual of daily use 
daily_use_perc %>%
  ggplot(aes(x="",y=total_perc, fill=usage_level)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#004d99","#3399ff","#cce6ff"),
                    labels = c("High use - 21 to 31 days",
                               "Moderate use - 11 to 20 days",
                               "Low use - 1 to 10 days"))+
  labs(title="Daily use of smart device")


 



