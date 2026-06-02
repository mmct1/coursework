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
    geom_histogram(bins=30) +
    labs(x="Trip Duration",y="Count",title="Trip Times Across All Rides")


ggplot(trips, aes(x=tripduration)) +
    scale_x_log10(label=comma) +
    geom_density() +
    labs(x="Trip Duration",y="Density",title="Trip Times Across All Rides")


# plot the distribution of trip times by rider type indicated using color and fill (compare a histogram vs. a density plot)
# user type
ggplot(trips, aes(x=tripduration, color=usertype, fill=usertype)) + 
    scale_x_log10(label=comma) +
    scale_y_log10(label=comma) + 
    geom_histogram(bins=30) +
    labs(x="Trip Duration",y="Count",title="Trip Times by Rider Type") +
    facet_wrap(~usertype)

ggplot(trips, aes(x=tripduration, color=usertype, fill=usertype)) + 
    scale_x_log10(label=comma) + 
    geom_density() +
    labs(x="Trip Duration",y="Density",title="Trip Times by Rider Type") +
    facet_wrap(~usertype)

# plot the total number of trips on each day in the dataset
# envisioning x axis being the day, and the count/num of trips being the y axis
trips%>%
    group_by(ymd) %>%
    summarize(count = n()) %>%
    ggplot(aes(x=ymd,y=count)) + 
        geom_line() +
        scale_y_continuous() +
        labs(x="Date", y="Count",title="Number of Trips per Day")

# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)
# get age from 2014-birth_year
# filtering age above a certain number would probably be useful
ggplot(trips, aes(x=(2014-birth_year), color=gender, fill=gender)) + 
    geom_histogram() +
    scale_y_log10(label=comma) +
    labs(x="Age", y="Count",title="Number of Trips by Age and Gender") +
    facet_wrap(~gender)

# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the pivot_wider() function to reshape things to make it easier to compute this ratio
# (you can skip this and come back to it tomorrow if we haven't covered pivot_wider() yet)
trips %>%
    mutate(age=2014-birth_year) %>%
    filter(gender != "Unknown") %>%
    group_by(age,gender) %>%
    summarize(count = n()) %>%
    pivot_wider(names_from=gender, values_from=count) %>%
    mutate(ratio=Male/Female) %>%
    ggplot(aes(x=age,y=ratio)) +
        geom_point() +
        geom_smooth() +
        labs(x="Age",y="Ratio (Male:Female)",title="Ratio of Male to Female Trips by Age")

########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)
ggplot(weather, aes(x=ymd, y=tmin)) +
    geom_line() +
    labs(x="Date",y="Minimum Temp",title="Minimum Temperature by Day")

# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the pivot_longer() function for this to reshape things before plotting
# (you can skip this and come back to it tomorrow if we haven't covered reshaping data yet)
weather %>%
    pivot_longer(names_to="max_or_min", values_to="temp", 5:6) %>%
    ggplot(aes(x=ymd, y=temp, color=max_or_min, group=max_or_min)) +
        geom_line() +
        scale_x_date(date_breaks='2 month') +
        labs(x="Date",y="Temperature",title="Minimum and Maximum Temperature by Day")


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
        geom_point() +
        labs(x="Minimum Temperature",y="Trips per Day",title="Minimum Temperature vs. Trips per Day")


# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
# mean(trips_with_weather$prcp) outputs 0.1013884, so defining anything greater as substantial
trips_with_weather %>%
    mutate(substantial_precip=(prcp>.101)) %>%
    group_by(ymd,tmin,substantial_precip) %>%
    summarize(trips_per_day = n()) %>%
    ggplot(aes(x=tmin,y=trips_per_day, color=substantial_precip)) +
        geom_point()+
        facet_wrap(~substantial_precip) +
        labs(x="Minimum Temp",y="Trips per Day",title="Minimum Temperature vs. Trips per Day", subtitle="Accounting for Precipitation")
           

# add a smoothed fit on top of the previous plot, using geom_smooth
trips_with_weather %>%
    mutate(substantial_precip=(prcp>.101)) %>%
    group_by(ymd,tmin,substantial_precip) %>%
    summarize(trips_per_day = n()) %>%
    ggplot(aes(x=tmin,y=trips_per_day, color=substantial_precip)) +
        geom_point()+
        facet_wrap(~substantial_precip) +
        labs(x="Minimum Temp",y="Trips per Day",title="Minimum Temperature vs. Trips per Day", subtitle="Accounting for Precipitation") +
        geom_smooth()

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
trips %>%
        mutate(hour_of_day=hour(starttime)) %>%
        group_by(ymd,hour_of_day) %>%
        summarize(count = n()) %>%
        group_by(hour_of_day) %>%
        summarize(avg_num_trips=mean(count), sd_num_trips=sd(count))

# plot the above
trips %>%
        mutate(hour_of_day=hour(starttime)) %>%
        group_by(ymd,hour_of_day) %>%
        summarize(count = n()) %>%
        group_by(hour_of_day) %>%
        summarize(avg_num_trips=mean(count), sd_num_trips=sd(count)) %>%
        ggplot(aes(x=hour_of_day, y=avg_num_trips, ymin = avg_num_trips-sd_num_trips, ymax=avg_num_trips+sd_num_trips)) +
            geom_pointrange() +
            labs(x="Hour of Day",y="Avg Number of Trips",title="Avgerage Number of Trips by Hour of Day ")

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
            facet_wrap(~ day_of_week) +
            labs(x="Hour of Day",y="Avg Number of Trips",title="Avgerage Number of Trips by Hour of Day ")