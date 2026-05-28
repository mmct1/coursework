# Think about how to write a musical_pairs.sh script to determine your programming 
# partner each day. We want the script to do the following:

# Produce a (pseudo)random pairing of 6 groups of 2 people who get to work together 
# each day on pair programming assignments
# Any one of us should be able to run the script and get the same pairing on a given 
# day (i.e., as long as our computers agree on the year/month/day)

# It's interesting to think about how we might avoid repeated pairs from one day 
# to the next, but for a first cut (and maybe final cut) version of the script you
# can ignore that issue

# use day as random seed

ls ./students *.txt | tr -d '.txt' | awk 'BEGIN{srand(date "+%d")} {print rand(), $0}'| sort -n | cut -d' ' -f2-
