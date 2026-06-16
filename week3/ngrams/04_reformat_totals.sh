#!/bin/bash

# reformat total counts in googlebooks-eng-all-totalcounts-20120701.txt to a valid csv
#   use tr, awk, or sed to convert tabs to newlines
#   write results to total_counts.csv

# cat googlebooks-eng-all-totalcounts-20120701.txt | tr '\t' '\n' > total_counts.csv
# below is probably an easier/better way to do this
sed 's/\t/\n/g' googlebooks-eng-all-totalcounts-20120701.txt >  total_counts.csv