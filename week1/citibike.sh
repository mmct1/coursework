#!/bin/bash
#
# add your solution after each of the 10 comments below
# use 201402-citibike-tripdata.csv

# count the number of unique stations
cut -d, -f5 201402-citibike-tripdata.csv | sort | uniq | wc -l #based on start station
# cut -d, -f9 201402-citibike-tripdata.csv | sort | uniq | wc -l #based on end station
# both output 330

# count the number of unique bikes
cut -d, -f12 201402-citibike-tripdata.csv | sort | uniq | wc -l
# 5700

# count the number of trips per day
# can get the date from start time or stop time, what if a trip goes between days?
cut -d, -f2 201402-citibike-tripdata.csv | cut -d ' ' -f1 | sort | uniq -c
cut -d, -f3 201402-citibike-tripdata.csv | cut -d ' ' -f1 | sort | uniq -c
# slighty different #, 1st looks at start time, second looks at stop
# looking only @ start time for the next 2 questions

# find the day with the most rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -d ' ' -f1 | sort | uniq -c | sort -n -r | head -1
# 13816 rides on 2014-02-02

# find the day with the fewest rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -d ' ' -f1 | sort | uniq -c | sort -n | head -5
# 876 rides on 2014-02-13

# find the id of the bike with the most rides
cut -d, -f12 201402-citibike-tripdata.csv | sort | uniq -c | sort -n -r | head -5
# 130 rides with bike id 20837

# count the number of rides by gender and birth year
cut -d, -f14,15 201402-citibike-tripdata.csv | sort | uniq -c | sort -n -r
# most common group w 7348 rides is men born in 1984

# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
cut -d, -f5 201402-citibike-tripdata.csv | grep '.*[0-9].*&.*[0-9].*' | wc -l
# 90549 trips

# compute the average trip duration
# basically, want to sum #s in column 1 (tripduration)
# and divide by length of the file
awk -F, '{sum += $1} END {print sum/NR}' 201402-citibike-tripdata.csv 