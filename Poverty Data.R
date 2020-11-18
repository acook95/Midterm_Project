library(readr)
library(tidyr)
library(dplyr)

## READ THE DATA
poverty <- read_csv("poverty.csv")
elections <- read_csv("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")

## DATA CLEANING
poverty <- pivot_longer(data = poverty, 
                        cols = c(total_est, aian_est, api_est, asian_est, black_est, 
                                 hisp_est, nhisp_est, nhopi_est, nhwhite_est, nnhwhite_est, 
                                 other_est, othermore_est, twomore_est, white_est), 
                        names_to = "race_ethnicity", values_to = "poverty_rate")

# not sure how to make this work--but maybe I don't need it?
# poverty <- pivot_longer(data = poverty, 
#                         cols = c(total_se, aian_se, api_se, asian_se, black_se, hisp_se, 
#                                  nhisp_se, nhopi_se, nhwhite_se, nnhwhite_se, other_se, 
#                                  othermore_se, twomore_se, white_se), 
#                         names_to = "race/eth error", values_to = "standard error")


poverty1 <- poverty %>% select(geoid, name, year, race_ethnicity, poverty_rate)


## POVERTY DATA EDA

# might need to remove api_est since it isn't represented in many states
api <- poverty1 %>% filter(race_ethnicity=="api_est")


