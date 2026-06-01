########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load("trips.RData")

########################################
# plot trip data
########################################
# plot the distribution of trip times across all rides (compare a histogram vs. a density plot)
ggplot(trips, aes(x=tripduration)) +
    scale_x_log10(label=comma) +
    geom_histogram(bins=30)

ggplot(trips, aes(x=tripduration)) +
    scale_x_log10(label=comma) +
    geom_density()

# plot the distribution of trip times by rider type indicated using color and fill (compare a histogram vs. a density plot)
# user type
ggplot(trips, aes(x=tripduration, color=usertype, fill=usertype)) + 
    scale_x_log10(label=comma) +
    scale_y_log10(label=comma) + 
    geom_histogram(bins=30)

ggplot(trips, aes(x=tripduration, color=usertype, fill=usertype)) + 
    scale_x_log10(label=comma) + 
    geom_density(alpha=0.5)

# plot the total number of trips on each day in the dataset
# envisioning x axis being the day, and the count/num of trips being the y axis
ggplot(trips, aes(x=ymd)) + 
    geom_histogram() +
    xlab('Number of Trips per Day')
    # want to fix y-axis, don't want scientific notation

# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)
# get age from 2014-birth_year
ggplot(trips, aes(x=(2014-birth_year), color=gender, fill=gender)) + 
    scale_y_log10(label=comma) +
    geom_histogram(alpha=0.5) +
    xlab('Age')

# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the pivot_wider() function to reshape things to make it easier to compute this ratio
# (you can skip this and come back to it tomorrow if we haven't covered pivot_wider() yet)
# age = x axis, gender ratio is the y axis // need to compute that -- save for weekend/monday
trips %>%
    mutate(age=2014-birth_year) %>%
    filter(gender != "Unknown") %>%
    group_by(gender,age) %>%
    summarize(count = n()) %>%
    pivot_wider()


########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)
ggplot(weather, aes(x=ymd, y=tmin)) +
    geom_line()

# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the pivot_longer() function for this to reshape things before plotting
# (you can skip this and come back to it tomorrow if we haven't covered reshaping data yet)

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
trips_with_weather %>%
    group_by(ymd,tmin) %>%
    summarize(trips_per_day = n()) %>%
    ggplot(aes(x=tmin,y=trips_per_day)) +
        geom_point()

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
trips_with_weather %>%
    mutate(sub_precip=(prcp>.1)) %>%
    group_by(ymd,tmin,sub_precip) %>%
    summarize(trips_per_day = n()) %>%
    ggplot(aes(x=tmin,y=trips_per_day, color=sub_precip)) +
        geom_point()
        

# add a smoothed fit on top of the previous plot, using geom_smooth
trips_with_weather %>%
    mutate(sub_precip=(prcp>.1)) %>%
    group_by(ymd,tmin,sub_precip) %>%
    summarize(trips_per_day = n()) %>%
    ggplot(aes(x=tmin,y=trips_per_day, color=sub_precip)) +
        geom_point() +
        geom_smooth()

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
trips %>%
        mutate(hour_of_day=hour(starttime)) %>%
        group_by(ymd,hour_of_day) %>%
        summarize(count = n()) %>%
        group_by(hour_of_day) %>%
        summarize(avg_num_trips=mean(count), sd_num_trips=sd(count)) %>%

# plot the above
trips %>%
        mutate(hour_of_day=hour(starttime)) %>%
        group_by(ymd,hour_of_day) %>%
        summarize(count = n()) %>%
        group_by(hour_of_day) %>%
        summarize(avg_num_trips=mean(count), sd_num_trips=sd(count)) %>%
        ggplot(aes(x=hour_of_day, y=avg_num_trips, ymin = avg_num_trips-sd_num_trips, ymax=avg_num_trips+sd_num_trips)) +
            geom_pointrange()

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
# use color to show day of the week?
trips %>%
        mutate(hour_of_day=hour(starttime), day_of_week=wday(ymd, label=TRUE)) %>%
        group_by(ymd,hour_of_day, day_of_week) %>%
        summarize(count = n()) %>%
        group_by(day_of_week, hour_of_day) %>%
        summarize(avg_num_trips=mean(count), sd_num_trips=sd(count)) %>%
        ggplot(aes(x=hour_of_day, y=avg_num_trips, color=day_of_week, ymin = avg_num_trips-sd_num_trips, ymax=avg_num_trips+sd_num_trips)) +
            geom_pointrange() +
            facet_wrap(~ day_of_week)