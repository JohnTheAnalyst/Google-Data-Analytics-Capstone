case_1
#setting up the environment
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(DataExplorer)
library(readxl)
library(rpivotTable)
library(dlookr)

setwd("~/Data Analytics/Google Data Analyst Certificate/8.Capestone/Case Studies/Case 1/2020-11-to-2021-10-divvy-tripdata/XLSX-File")

#importing data and assigning the correct data type.
nov_2020 <- read_excel("2020-11-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
dec_2020 <- read_excel("2020-12-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
jan_2021 <- read_excel("2021-01-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
feb_2021 <- read_excel("2021-02-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
mar_2021 <- read_excel("2021-03-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
apr_2021 <- read_excel("2021-04-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
may_2021 <- read_excel("2021-05-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
jun_2021 <- read_excel("2021-06-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
jul_2021 <- read_excel("2021-07-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
aug_2021 <- read_excel("2021-08-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
sep_2021 <- read_excel("2021-09-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))
oct_2021 <- read_excel("2021-10-divvy-tripdata.xlsx", col_types = c("text", "text", "date", 
                                                                    "date", "text", "text", "text", "text", 
                                                                    "numeric", "numeric", "numeric", 
                                                                    "numeric", "text"))

#Confirming that the set of data.frames are row-bindable.
#Calls compare_df_cols()and returns TRUE if there are no miss-matching rows.

compare_df_cols_same(nov_2020, dec_2020, jan_2021, feb_2021, mar_2021, apr_2021, 
                     may_2021, jun_2021, jul_2021, aug_2021, sep_2021, oct_2021,
  bind_method = c("bind_rows", "rbind"),
  verbose = TRUE
)

#Now that the columns are proved to be consistent, the next step is to merge them into a single data-frame.

trips_data <- bind_rows(nov_2020, dec_2020, jan_2021, feb_2021, mar_2021, apr_2021, 
                                 may_2021, jun_2021, jul_2021, aug_2021, sep_2021, oct_2021)

#Now the data is ready for Clean up.
glimpse(trips_data)
introduce(trips_data)

#create new columns

##ride_duration in minutes, converted to numeric data type and rounded to 2 decimal places
trips_data$ride_duration <- difftime(trips_data$ended_at, trips_data$started_at, units="mins")
trips_data$ride_duration <- round(as.numeric(trips_data$ride_duration),2)

#day_of_week
trips_data$day_of_week <- wday(trips_data$started_at, label = TRUE, abbr = FALSE, week_start = getOption("lubridate.week.start", 1))

#year_month, converted to factor data type
trips_data$year_month <- factor(format(as.Date(trips_data$started_at), "%Y-%m"))

#starting_hour, converted to factor data type
trips_data$starting_hour <- factor(hour(trips_data$started_at))

#renaming existing columns meaningfully
colnames(trips_data)

#rideable_type to bike_type
trips_data <- trips_data %>% 
  rename(bike_type = rideable_type)

#member_casual to user_type
trips_data <- trips_data %>% 
  rename(user_type = member_casual)

#recoding docked_bike to classic_bike

#there is only 2 bike types(electric_bike and docked_bike), 
#after some research to check that the data makes sense given the knowledge of the business its evident that all electric_bikes can 
#Use the ebike's locking cable to secure it at an e-station or simply dock it at a divvy station 
#in the other hand the Classic bikes can't be locked at an e-stations they can only be docked at 
#divvy station from there comes the name docked_bike which can be misleading. for this reason we will
#recode it to classic bike

Another reasoning(reanalyse according to the code result)

ftable(trips_data$user_type, trips_data$year_month, trips_data$bike_type)

#From September to November 2020 there were only two rideable types, 
#docked_bike and electric_bike. At the beginning of December 2020 new rideable type is introducted, 
#the classic_bike, and a distinction is made between the docked_bike and the classic_bike.
#Since the beginning of 2021, casual riders can use classic and docked bikes, while annual members only classic bikes.
#Therefore, we can not conclude a preference for one rideable type over another between casuals and members. 
#Later on I will convert all docked bikes into classics and compare preferences for classic or electric bikes.

trips_data_v1 <- trips_data %>% 
  mutate(bike_type = recode(bike_type, "docked_bike" = "classic_bike"))

trips_data_v1 %>%
  count(bike_type)

#Inspecting the data-frame 
head(trips_data_v1)
skim_without_charts(trips_data_v1)

#By observing the data summary we can notice the following problems that need to be addressed:
  
#number of unique ride_id doesn't match the number of rows which tells me that there is some duplicated rows based on the ride_id
#simply does not make sense having negative ride_duration or numbers less than 1 min
#there is missing values (NA) in the start_station_name, start_station_id, end_station_name, end_station_id,end_lat and end_lng columns


#remove duplicates 
#Each row represents one observation/trip, 
#there is a total of 5,378,834 rows, but only 5,378,625 unique ride_id values,
#meaning there are 209 rows of duplicated data to be removed
trips_data_v2 <- trips_data_v1[!duplicated(trips_data_v1$ride_id), ] 

#verification
nrow(trips_data_v1) - nrow(trips_data_v2)

#given the business knowledge the following should be removed:

#trips with negative ride length (the end of the trip precedes the start)
trips_data_v2 %>%
  count(ride_duration <= 0)

#trips with ride length between 0 and 1 min (potentially false starts or users trying to re-dock a bike to ensure it was secure)
trips_data_v2 %>%
  count(ride_duration >= 0 & ride_duration < 1)

# trips associated with Divvy test and repair stations
unique(trips_data_v2$start_station_name)

test_and_repair_stations <- c("Base - 2132 W Hubbard Warehouse",
              "DIVVY CASSETTE REPAIR MOBILE STATION",
              "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)",
              "WATSON TESTING - DIVVY",
              "WEST CHI-WATSON")

unique(trips_data_v2$end_station_id)

test_and_repair_stations_id <- c("DIVVY 001",
            "DIVVY CASSETTE REPAIR MOBILE STATION",
            "Hubbard Bike-checking (LBS-WH-TEST)")

#removing the above mentioned trips
trips_to_remove <- trips_data_v2 %>%
  filter(start_station_name %in% test_and_repair_stations |
           end_station_name %in% test_and_repair_stations |
           start_station_id %in% test_and_repair_stations_id |
           end_station_id %in% test_and_repair_stations_id |
           ride_duration <1)

nrow(trips_to_remove)

trips_data_v2 <- setdiff(trips_data_v2, trips_to_remove)

nrow(trips_data_v2)


#Inspecting closely the observations with missing values(NA) left
plot_missing(trips_data_v2)

#create a NA data-frame for a close look of the data and better evaluation
trips_data_NA <- trips_data_v2 %>% 
  filter(is.na(start_station_name) | 
           is.na(end_station_name) | 
           is.na(start_station_id) | 
           is.na(end_station_id) |
           is.na(end_lat) |
           is.na(end_lng))

#remove trips with no end_lat and end_lng and no end_station name/id(considered as lost or stolen)
trips_to_remove_1 <- trips_data_v2 %>%
       filter(is.na(end_station_name) & 
                is.na(end_station_id) &
                is.na(end_lat) &
                is.na(end_lng))

trips_data_v2 <- setdiff(trips_data_v2, trips_to_remove_1)

#summary of missing Values retained
trips_data_keptNA <- trips_data_v2 %>% 
  filter(is.na(start_station_name) | 
           is.na(end_station_name) | 
           is.na(start_station_id) | 
           is.na(end_station_id) |
           is.na(end_lat) |
           is.na(end_lng))

table(trips_data_keptNA$year_month, trips_data_keptNA$user_type)

#The reason for missing values here is the fact that electric bikes can be parked outside of the stations within a service area.
#You can find more information here.It is strange to see rides taken with classic bikes in this subset since classic bikes should be parked only at the stations
#(note: improper parking may incur a fee).

to be worked on

#Checking if any station has changed its name at some point.
all_trips_v2 %>% 
  group_by(start_station_id, year_month) %>%
  summarise(n_distinct_start_station_name = n_distinct(start_station_name), .groups = 'drop') %>% 
  filter(n_distinct_start_station_name > 1)

start_id <- c("13074", "13099", "13300", "19", "26", "317", "332", "351", "503",
              "625", "631", "704", "709", "725", "E011", "LF-005", "TA1305000039",
              "TA1306000029", "TA1307000041", "TA1309000039", "TA1309000049")

all_trips_v2 %>%
  filter(start_station_id %in% start_id) %>% 
  group_by(start_station_id, start_station_name) %>%
  summarise(min_datetime = min(started_at), max_datetime = max(started_at), count = n(), .groups = 'drop' ) %>% 
  arrange(start_station_id, min_datetime)

#For example, in July 2021 Chicago renamed the iconic Lake Shore Drive to honor its city's 'founder' Jean Baptiste Point DuSable. 
#It is now known as DuSable Lake Shore Drive. You can read the story here. I will now recode old station names in the start_station_name column with new ones.
#This step is necessary if we want to get an accurate list of the most popular stations.

all_trips_v2 <- all_trips_v2 %>% 
  mutate(start_station_name = recode(start_station_name, 
                                     "Broadway & Wilson Ave" = "Broadway & Wilson - Truman College Vaccination Site",
                                     "Halsted St & 18th St" = "Halsted St & 18th St (Temp)",
                                     "Lake Shore Dr & Monroe St" = "DuSable Lake Shore Dr & Monroe St",
                                     "Throop (Loomis) St & Taylor St" = "Throop St & Taylor St",
                                     "McClurg Ct & Illinois St" = "New St & Illinois St",
                                     "Burling St (Halsted) & Diversey Pkwy (Temp)" = "Burling St & Diversey Pkwy",
                                     "Drake Ave & Fullerton Ave" = "St. Louis Ave & Fullerton Ave",
                                     "Malcolm X College" = "Malcolm X College Vaccination Site",
                                     "Lake Shore Dr & North Blvd" = "DuSable Lake Shore Dr & North Blvd",
                                     "Marshfield Ave & Cortland St" = "Elston Ave & Cortland St",
                                     "Lake Shore Dr & Ohio St" = "DuSable Lake Shore Dr & Ohio St",
                                     "Lake Shore Dr & Wellington Ave" = "DuSable Lake Shore Dr & Wellington Ave",
                                     "Lake Shore Dr & Diversey Pkwy" = "DuSable Lake Shore Dr & Diversey Pkwy",
                                     "Lake Shore Dr & Belmont Ave" = "DuSable Lake Shore Dr & Belmont Ave"))

#Repating previous step for the end_station_name column.

all_trips_v2 <- all_trips_v2 %>% 
  mutate(end_station_name = recode(end_station_name, 
                                   "Broadway & Wilson Ave" = "Broadway & Wilson - Truman College Vaccination Site",
                                   "Halsted St & 18th St" = "Halsted St & 18th St (Temp)",
                                   "Lake Shore Dr & Monroe St" = "DuSable Lake Shore Dr & Monroe St",
                                   "Throop (Loomis) St & Taylor St" = "Throop St & Taylor St",
                                   "McClurg Ct & Illinois St" = "New St & Illinois St",
                                   "Burling St (Halsted) & Diversey Pkwy (Temp)" = "Burling St & Diversey Pkwy",
                                   "Drake Ave & Fullerton Ave" = "St. Louis Ave & Fullerton Ave",
                                   "Malcolm X College" = "Malcolm X College Vaccination Site",
                                   "Lake Shore Dr & North Blvd" = "DuSable Lake Shore Dr & North Blvd",
                                   "Marshfield Ave & Cortland St" = "Elston Ave & Cortland St",
                                   "Lake Shore Dr & Ohio St" = "DuSable Lake Shore Dr & Ohio St",
                                   "Lake Shore Dr & Wellington Ave" = "DuSable Lake Shore Dr & Wellington Ave",
                                   "Lake Shore Dr & Diversey Pkwy" = "DuSable Lake Shore Dr & Diversey Pkwy",
                                   "Lake Shore Dr & Belmont Ave" = "DuSable Lake Shore Dr & Belmont Ave"))

#Analysis & Share
trips_data_v3 <- trips_data_v2
skim_without_charts(trips_data_v3)


#number of rides
#total rides per month by user type
total_rides_per_month <- trips_data_v3 %>%
  group_by(user_type, year_month) %>% 
  summarise(total_rides = n(), .groups = 'drop') %>% 
  mutate(percentage = total_rides/sum(total_rides)*100)

#plot
options(repr.plot.width = 12, repr.plot.height = 7)

ggplot(data = total_rides_per_month, mapping =  aes(x = year_month, y = total_rides, fill = user_type)) + 
  geom_col(color = "black", position = "dodge", width = 0.6) + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("purple", "#7E9BB4FF")) +
  ylab("Number of rides") + 
  xlab("Month of the year") +
  ggtitle("Total Rides Per Month By User Type", subtitle = "November 2020 - October 2021 (1 year)") +
  theme_minimal()

#total rides per day of week by user type
total_rides_per_day_of_week <- trips_data_v3 %>%
  group_by(user_type, day_of_week) %>% 
  summarise(total_rides = n()) %>% 
  mutate(percentage = total_rides/sum(total_rides)*100)

#plot
options(repr.plot.width = 9, repr.plot.height = 7)

ggplot(total_rides_per_day_of_week, aes(x = day_of_week, y = total_rides, fill = user_type)) + 
  geom_col(colour = "black", width = 0.6, position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("purple", "#7E9BB4FF")) +
  ylab("Number of rides") + 
  xlab("Day of the week") +
  ggtitle("Total Rides Per Day of The Week By User Type", subtitle = "November 2020 - October 2021 (1year)") +
  theme_minimal()

#total rides per hour
total_rides_per_st_hour <- trips_data_v3 %>%
  group_by(user_type, starting_hour) %>% 
  summarise(total_rides = n()) %>% 
  mutate(percentage = total_rides/sum(total_rides)*100)

#plot
options(repr.plot.width = 11, repr.plot.height = 7)  

ggplot(total_rides_per_st_hour, aes(x = starting_hour, y = total_rides, color = user_type, group = user_type)) +
  geom_line(size=1.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("purple", "#7E9BB4FF")) +
  ylab("Number of rides") +
  xlab("Starting hours") +
  ggtitle("Total Rides Per Starting Hour By User Type", subtitle = "November 2020 - October 2021 (1year)") +
  theme_bw()

#average ride duration
trips_data_v3 %>%                            
  group_by(user_type) %>% 
  summarize(min = min(ride_duration),
            q1 = quantile(ride_duration, 0.25),
            median = median(ride_duration),
            mean = mean(ride_duration),
            q3 = quantile(ride_duration, 0.75),
            max = max(ride_duration))

#Average Ride Length by Day of the Week
avg_ride_duration_per_day_of_week <- trips_data_v3 %>%
  group_by(user_type, day_of_week) %>% 
  summarise(avg_ride_duration = mean(ride_duration), .groups = 'drop')

#plot
options(repr.plot.width = 9, repr.plot.height = 7)

ggplot(avg_ride_duration_per_day_of_week, aes(x = day_of_week, y = avg_ride_duration, fill = user_type)) + 
  geom_col(colour = "black", width = 0.6, position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("purple", "#7E9BB4FF")) +
  ylab("Average ride duration in minutes") + 
  xlab("Day of the week") +
  ggtitle("Average Ride Duration Per Day Of The Week By User Type In Minutes", subtitle = "November 2020 - October 2021 (1year)") +
  theme_minimal()

#Average Ride Duration per Starting Hour
avg_ride_duration_per_st_hour <- trips_data_v3 %>%
  group_by(user_type, starting_hour) %>% 
  summarise(avg_ride_duration = mean(ride_duration), .groups = 'drop')

#plot
options(repr.plot.width = 11, repr.plot.height = 7)  

ggplot(avg_ride_duration_per_st_hour, aes(x = starting_hour, y = avg_ride_duration, color = user_type)) +
  geom_line(aes(group = user_type), size=1.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("purple", "#7E9BB4FF")) +
  ylab("Average ride duration in minutes") +
  xlab("Starting hour") +
  ggtitle("Average Ride Duration Per Starting Hour By User Type In Minutes", subtitle = "November 2020 - October 2021 (1year)") +
  theme_bw()

#bike type usage
prop.table(table(trips_data_v3$user_type, trips_data_v3$bike_type), margin=1)*100

#monthly usage
bike_type_usage_per_month <- trips_data_v3 %>%
  group_by(user_type, year_month, bike_type) %>% 
  summarise(total_rides = n()) %>% 
  mutate(percentage = total_rides/sum(total_rides)*100)

#plot
options(repr.plot.width = 15, repr.plot.height = 7)

ggplot(bike_type_usage_per_month, aes(x = year_month, y = total_rides, fill = bike_type)) +
  geom_col(color = "black", width=0.7, position = "fill") +
  facet_wrap(~user_type) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(breaks = c("classic_bike", "electric_bike"), 
                    values=c("#A2ACB4FF", "#0062B4FF")) +
  ylab("bike_type usage") +
  xlab("Month of the year") +
  ggtitle("Bike Type Usage per Month", subtitle = "November 2020 - October 2021 (1year)")


#people who incurred extra fees (who incurs more extra fees m/c)
#m45,c30(daypass=only_classicbike 3h)

#tableau

#Top 20 most popular starting stations for casuals
top_st_station_for_casual <- trips_data_v3 %>% 
  filter(!is.na(start_station_name), user_type == "casual") %>% 
  group_by(user_type, start_station_name,start_lat, start_lng) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(-number_of_rides) %>% 
  head(20)

#Top 20 most popular starting stations for members
top_st_station_for_member <- trips_data_v3 %>% 
  filter(!is.na(start_station_name), user_type == "member") %>% 
  group_by(user_type, start_station_name, start_lat, start_lng) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(-number_of_rides) %>% 
  head(n=20)

top_start_stations <- bind_rows(top_st_station_for_casual, top_st_station_for_member)

write.csv(top_start_stations,"top_start_stations.csv", row.names = FALSE)

#Bike type usage per user type and bike type
bike_type_usage_per_user_type_and_bike_type <- trips_data_v3 %>%
  group_by(user_type, bike_type) %>% 
  summarise(total_rides = n()) %>% 
  mutate(percentage = total_rides/sum(total_rides)*100)

write.csv(bike_type_usage_per_user_type_and_bike_type,"bike_type_usage_per_user_type_and_bike_type.csv", row.names = FALSE)

#Bike usage per user type
bike_usage_per_user_type <- trips_data_v3 %>%
  group_by(user_type) %>% 
  summarise(total_rides = n()) %>% 
  mutate(percentage = total_rides/sum(total_rides)*100)

write.csv(bike_type_usage_per_user_type,"bike_type_usage_per_user_type.csv", row.names = FALSE)

#The following two visualizations are made in Tableu Public using "top_start_fin" 
#dataset created in the previous step.

ftable(trips_data$year_month, trips_data$bike_type)

trips_data %>%
  group_by(bike_type) %>% 
  summarise(total_rides = n()) %>% 
  mutate(percentage = total_rides/sum(total_rides)*100)

top_st_station_for_casual <- trips_data_v3 %>% 
  filter(!is.na(start_station_name), user_type == "casual") %>% 
  group_by(user_type, start_station_name,start_lat, start_lng,start_station_id) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(-number_of_rides) %>% 
  head(20)

avg_ride_duration <- trips_data_v3 %>%
  group_by(user_type) %>% 
  summarise(avg_ride_duration = mean(ride_duration), .groups = 'drop')
