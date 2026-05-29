library(tidyverse)
library(lubridate)

########################################
# READ AND TRANSFORM THE DATA
########################################

# read one month of data
trips <- read_csv('201402-citibike-tripdata.csv', na = c("\\N", "NA"))

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# convert dates strings to dates
# trips <- mutate(trips, starttime = mdy_hms(starttime), stoptime = mdy_hms(stoptime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
f <- factor(trips$gender, 
            levels = c(0,1,2), 
            labels = c("Unknown","Male","Female"))
trips <- mutate(trips, gender=f)

########################################
# YOUR SOLUTIONS BELOW
########################################

# count the number of trips (= rows in the data frame)
nrow(trips)

# find the earliest and latest birth years (see help for max and min to deal with NAs)
summarize(trips, earliest_birth_year=min(birth_year,na.rm=TRUE), latest_birth_year=max(birth_year,na.rm=TRUE))

# use filter and grepl to find all trips that either start or end on broadway
filter(trips, grepl("Broadway", start_station_name) | grepl("Broadway", end_station_name))

# do the same, but find all trips that both start and end on broadway
filter(trips, grepl("Broadway", start_station_name) & grepl("Broadway", end_station_name))

# find all unique station names
n_distinct(trips['start_station_name'])
n_distinct(trips['end_station_name'])

# count the number of trips by gender, the average trip time by gender, and the standard deviation in trip time by gender
# do this all at once, by using summarize() with multiple arguments
summarize(group_by(trips,gender), 
        num_trips = n(),
        mean_trip_time = mean(tripduration),
        sd_trip_time = sd(tripduration))

# find the 10 most frequent station-to-station trips
# idea: group_by station, get count, arrange, get top 10
summarize(group_by(trips, start_station_name, end_station_name),
        count = n()) %>%
        arrange(desc(count)) %>%
        head(10)

# find the top 3 end stations for trips starting from each start station
summarize(group_by(trips, start_station_name, end_station_name), count = n()) %>% 
        slice_max(order_by=count,n=3)

# find the top 3 most common station-to-station trips by gender
# idea/thoughts: for each gender, want top 3 trips
summarize(group_by(trips,gender,start_station_name,end_station_name), count=n()) %>% 
        group_by(gender) %>% 
        slice_max(order_by=count,n=3)

# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)
# idea: base it on starttime, just extract the date
# floor_date(x, "day")
trips1 <- trips %>%
        mutate(ymd=floor_date(starttime, "day"))
# str(trips1) confirms ymd contains values in the format "2014-02-01" "2014-02-01" ...
summarize(group_by(trips1, ymd), count=n()) %>%
        arrange(desc(count))
# matches result from citibike.sh

# compute the average number of trips taken during each of the 24 hours of the day across the entire month
# what time(s) of day tend to be peak hour(s)?
# idea: would floor_date(x,"hour") be helpful here // Octopilot -> hour() is more useful
trips2 <- trips %>%
        mutate(hour_of_day=hour(starttime))
summarize(group_by(trips2,hour_of_day), count=n()) %>%
        arrange(desc(count))
