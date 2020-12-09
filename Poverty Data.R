library(readr)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(rstanarm)
library(lme4)

## READ THE DATA
poverty <- read_csv("poverty.csv")
elections <- read_csv("elections.csv")
# elections1 <- read_csv("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")

## POVERTY DATA CLEANING

# make columns into values under new variable race_ethnicity
poverty <- pivot_longer(data = poverty, 
                        cols = c(total_est, aian_est, api_est, asian_est, black_est, 
                                 hisp_est, nhisp_est, nhopi_est, nhwhite_est, nnhwhite_est, 
                                 other_est, othermore_est, twomore_est, white_est), 
                        names_to = "race_ethnicity", values_to = "poverty_rate")

# remove standard errors, remove NAs, and rename state column
poverty1 <- poverty %>% select(name, year, race_ethnicity, poverty_rate)
poverty1 <- na.omit(poverty1)
poverty1 <- poverty1 %>% rename(state = name)

# might need to remove api_est and/or others since it isn't represented in many states/years
api <- poverty1 %>% filter(race_ethnicity=="api_est")
aian <- poverty1 %>% filter(race_ethnicity=="aian_est") 

# filter to include only a few racial/ethnic groups for sake of simplicity/interpretation
poverty_final <- poverty1 %>% filter(race_ethnicity == "aian_est" | race_ethnicity == "asian_est" | race_ethnicity == "black_est" | race_ethnicity == "hisp_est" | race_ethnicity == "white_est")

## ELECTIONS DATA CLEANING

# filter for only years similar to the poverty dataset, select only relevant columns, remove NAs
elections <- elections %>% filter(year >=2008)
elections <- elections %>% select(-c(office, notes, version, writein, state_cen, state_ic, state_fips, state_po))
elections <- na.omit(elections)

# create new variable vote percentage
elections$vote_pct <- round((elections$candidatevotes / elections$totalvotes) * 100, 2)

# filter for only major parties (all other parties had relatively very few votes)
elections <- elections %>% filter(party == "republican" | party == "democrat")

# not sure how to create a variable to indicate the winner of the state
# elections$winner <- ifelse(subset(elections, party == "republican")$vote_pct > 50 & subset(elections, party == "democrat")$vote_pct < 50, "republican", "democrat")
elections$winner <- ifelse(elections$vote_pct >= 49.4, 1, 0)

## POVERTY DATA EDA

# 2009 poverty rates, all states
ggplot(data = subset(poverty_final, year == 2009), aes(x = race_ethnicity, y = poverty_rate)) + 
  geom_col(aes(fill = race_ethnicity)) + facet_wrap(vars(state))

# 2017 poverty rates, all states
ggplot(data = subset(poverty_final, year == 2017), aes(x = race_ethnicity, y = poverty_rate)) + 
  geom_col(aes(fill = race_ethnicity)) + facet_wrap(vars(state))

# Alabama poverty rates, all years
ggplot(data = subset(poverty_final, state == "Alabama"), 
       aes(x = year, y = poverty_rate, color = race_ethnicity)) + 
  geom_path() + xlim(2009, 2017)

# Illinois poverty rates, all years
ggplot(data = subset(poverty_final, state == "Illinois"), 
       aes(x = year, y = poverty_rate, color = race_ethnicity)) + 
  geom_path() + xlim(2009, 2017)

# poverty rates all states, all years
ggplot(data = poverty_final, aes(x = year, y = poverty_rate, color = race_ethnicity)) + 
  geom_path() + facet_wrap(vars(state))

## ELECTIONS DATA EDA

# 2008 vote pct, all states
ggplot(data = subset(elections, year == 2008), aes(x = party, y = vote_pct)) + 
  geom_col(aes(fill = party)) + facet_wrap(vars(state))

# 2012 vote pct, all states
ggplot(data = subset(elections, year == 2012), aes(x = party, y = vote_pct)) + 
  geom_col(aes(fill = party)) + facet_wrap(vars(state))

# 2016 vote pct, all states
ggplot(data = subset(elections, year == 2016), aes(x = party, y = vote_pct)) + 
  geom_col(aes(fill = party)) + facet_wrap(vars(state))

# Alabama vote pct, all years
ggplot(data = subset(elections, state == "Alabama"), 
       aes(x = year, y = vote_pct, color = party)) + geom_path()

# all states vote pct, all years
ggplot(data = elections, aes(x = year, y = vote_pct, color = party)) + 
  geom_path() + facet_wrap(vars(state))

## DATA MERGING

data_merge <- left_join(poverty_final, elections, by = c("state", "year"))


## MERGED DATA EDA

# summarize across years
group_by_state <- data_merge %>% group_by(state, race_ethnicity) %>% summarize(poverty_rate = mean(poverty_rate))

# povert
ggplot() + geom_point(data = group_by_state, aes(x = race_ethnicity, y = poverty_rate, color = state))

# summarize across states
group_by_race <- data_merge %>% group_by(race_ethnicity, party, year) %>% summarize(poverty_rate = mean(poverty_rate), vote_pct = mean(vote_pct))

ggplot() + geom_point(data = subset(group_by_race, year == 2016), aes(x = race_ethnicity, y = vote_pct, color = party))
ggplot() + geom_point(data = subset(group_by_race, year == 2012), aes(x = race_ethnicity, y = vote_pct, color = party))

ggplot() + geom_point(data = data_merge, aes(x = year, y = vote_pct, color = party)) + 
  facet_wrap(vars(data_merge$race_ethnicity))

## MODELING

fit1 <- stan_glm(poverty_rate ~ race_ethnicity + state, data = data_merge, refresh = 0)
pp_check(fit1)  
plot(fitted(fit1), resid(fit1))

fit2 <- stan_glmer(poverty_rate ~ race_ethnicity + state + vote_pct + (1|year), data = data_merge)
pp_check(fit2)
plot(fitted(fit2), resid(fit2))

fit3 <- stan_glmer(poverty_rate ~ race_ethnicity + (1|year), data = data_merge)
pp_check(fit3)
plot(fitted(fit3), resid(fit3))

fit4 <- stan_glm(vote_pct ~ race_ethnicity + state, data = data_merge, refresh = 0)
pp_check(fit4)
plot(fitted(fit4), resid(fit4))

fit5 <- stan_glmer(poverty_rate ~ race_ethnicity + (1|state), data = data_merge)
pp_check(fit5)
plot(fitted(fit5), resid(fit5))

fit6 <- stan_glm()


## CITATIONS

citation("ggplot2")
citation("readr")
citation("tidyr")
citation("dplyr")
citation("magrittr")
citation("rstanarm")
citation("lme4")

