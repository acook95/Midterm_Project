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
poverty_final <- poverty1 %>% filter(race_ethnicity == "aian_est" | race_ethnicity == "asian_est" | race_ethnicity == "black_est" | race_ethnicity == "hisp_est" | race_ethnicity == "white_est" | race_ethnicity == "total_est")

## ELECTIONS DATA CLEANING

# filter for only years similar to the poverty dataset, select only relevant columns, remove NAs
elections <- elections %>% filter(year >=2009)
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
  geom_path() + xlim(2009, 2017) + 
  scale_x_continuous(name = waiver(), breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  ggtitle("Poverty Rates in Alabama by Year and Race") +
  scale_color_discrete(name = "Race/Ethnicity", labels = c("Native Am.", "Asian", "Black", "Hispanic", "Total", "White"))

# New York poverty rates, all years
ggplot(data = subset(poverty_final, state == "New York"), 
       aes(x = year, y = poverty_rate, color = race_ethnicity)) + 
  geom_path() + xlim(2009, 2017) +
  scale_x_continuous(name = waiver(), breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  ggtitle("Poverty Rates in New York by Year and Race") +
  scale_color_discrete(name = "Race/Ethnicity", labels = c("Native Am.", "Asian", "Black", "Hispanic", "Total", "White"))

# poverty rates all states, all years
ggplot(data = poverty_final, aes(x = year, y = poverty_rate, color = race_ethnicity)) + 
  geom_path() + facet_wrap(vars(state)) +
  scale_x_continuous(name = waiver(), breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  ggtitle("Poverty Rates by Year and Race") +
  scale_color_discrete(name = "Race/Ethnicity", labels = c("Native Am.", "Asian", "Black", "Hispanic", "Total", "White")) +
  theme(axis.text.x = element_text(angle = 90))


## ELECTIONS DATA EDA

# 2012 vote pct, all states
ggplot(data = subset(elections, year == 2012), aes(x = party, y = vote_pct)) + 
  geom_col(aes(fill = party)) + facet_wrap(vars(state)) + 
  scale_fill_discrete(type = c("blue", "red")) + ylab(label = "Vote Share %")

# 2016 vote pct, all states
ggplot(data = subset(elections, year == 2016), aes(x = party, y = vote_pct)) + 
  geom_col(aes(fill = party)) + facet_wrap(vars(state)) + 
  scale_fill_discrete(type = c("blue", "red")) + ylab(label = "Vote Share %")

# Alabama vote pct
ggplot(data = subset(elections, state == "Alabama"), 
       aes(x = year, y = vote_pct)) + geom_col(aes(fill = party)) + 
  scale_fill_discrete(type = c("blue", "red")) + ylab(label = "Vote Share %") +
  ggtitle("Vote Share Percentage for 2012 and 2016 Presidential Elections in Alabama") + 
  scale_x_continuous(name = waiver(), breaks = c(2012, 2016)) 

# New York vote pct
ggplot(data = subset(elections, state == "New York"), 
       aes(x = year, y = vote_pct)) + geom_col(aes(fill = party)) + 
  scale_fill_discrete(type = c("blue", "red")) + ylab(label = "Vote Share %") +
  ggtitle("Vote Share Percentage for 2012 and 2016 Presidential Elections in New York") + 
  scale_x_continuous(name = waiver(), breaks = c(2012, 2016)) 


# all states vote pct, all years
ggplot(data = elections, aes(x = year, y = vote_pct)) + geom_col(aes(fill = party)) + 
  scale_fill_discrete(type = c("blue", "red")) + ylab(label = "Vote Share %") +
  ggtitle("Vote Share Percentage for 2012 and 2016 Presidential Elections") + 
  scale_x_continuous(name = waiver(), breaks = c(2012, 2016)) + facet_wrap(vars(state))


## DATA MERGING

data_merge <- left_join(poverty_final, elections, by = c("state", "year"))


## MERGED DATA EDA

# summarize across years
group_by_state <- subset(data_merge, party == "republican") %>% group_by(state, year) %>% summarize(poverty_rate = mean(poverty_rate), vote_pct = mean(vote_pct))

# poverty rate vs. republican vote share
ggplot() + geom_point(data = group_by_state, aes(x = poverty_rate, y = vote_pct, color = factor(year))) +
  ggtitle("Poverty Rate vs. Vote Share for Republican Candidate") + xlab("Poverty Rate") + ylab("Vote Share %")

## Plots tried out but not informative for research question--ignore these ##
# summarize across states
group_by_race <- data_merge %>% group_by(race_ethnicity, party, year) %>% summarize(poverty_rate = mean(poverty_rate), vote_pct = mean(vote_pct))

ggplot() + geom_point(data = subset(group_by_race, year == 2016), aes(x = race_ethnicity, y = vote_pct, color = party))
ggplot() + geom_point(data = subset(group_by_race, year == 2012), aes(x = race_ethnicity, y = vote_pct, color = party))

ggplot() + geom_point(data = data_merge, aes(x = year, y = vote_pct, color = party)) + 
  facet_wrap(vars(data_merge$race_ethnicity))

ggplot() + geom_point(data = subset(data_merge, party == "republican"), aes(x = poverty_rate, y = vote_pct, color = race_ethnicity)) +
  ggtitle("Poverty Rate vs. Vote Share for Republican Candidate") + 
  ylab("Vote Share %") + facet_wrap(vars(race_ethnicity)) +
  scale_color_discrete(name = "Race/Ethnicity", labels = c("Native Am.", "Asian", "Black", "Hispanic", "Total", "White"))

ggplot() + geom_point(data = subset(data_merge, party == "republican"), aes(x = poverty_rate, y = vote_pct, color = race_ethnicity)) +
  ggtitle("Poverty Rate vs. Vote Share for Republican Candidate") + 
  ylab("Vote Share %") +
  scale_color_discrete(name = "Race/Ethnicity", labels = c("Native Am.", "Asian", "Black", "Hispanic", "Total", "White"))



## MODELING

fit1 <- stan_glm(poverty_rate ~ race_ethnicity + state, data = data_merge, refresh = 0)
print(fit1)
pp_check(fit1)  
plot(fitted(fit1), resid(fit1))

fit2 <- stan_glmer(poverty_rate ~ race_ethnicity + state + vote_pct + (1|year), data = data_merge)
pp_check(fit2)
plot(fitted(fit2), resid(fit2))

fit3 <- stan_glmer(poverty_rate ~ race_ethnicity + (1|year), data = data_merge)
pp_check(fit3)
plot(fitted(fit3), resid(fit3))

fit4 <- stan_glm(vote_pct ~ race_ethnicity + state, data = subset(data_merge, party == "republican"), refresh = 0)
pp_check(fit4)
plot(fitted(fit4), resid(fit4))

# final model1
model1 <- stan_glmer(poverty_rate ~ race_ethnicity + (1|state), data = data_merge, refresh = 0)
print(model1)
fixef(model1)
ranef(model1)
pp_check(model1)
plot(fitted(model1), resid(model1), xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot for Model 1" )

fit6 <- lmer(poverty_rate ~ race_ethnicity + (1|state), data = data_merge)
print(fit6)

fit7 <- stan_glmer(vote_pct ~ race_ethnicity + (1|state), data = subset(data_merge, party == "republican"), refresh = 0)
fit7
pp_check(fit7)
plot(fitted(fit7), resid(fit7))

fit8 <- stan_glmer(vote_pct ~ poverty_rate + (1|state), data = subset(data_merge, party == "republican"))
fit8
pp_check(fit8)
plot(fitted(fit8), resid(fit8))

fit9 <- stan_glm(vote_pct ~ poverty_rate + state, data = subset(data_merge, party == "republican"))
fit9
pp_check(fit9)
plot(fitted(fit9), resid(fit9))

#final model2
model2 <- stan_glm(vote_pct ~ poverty_rate, data = group_by_state, refresh = 0)
print(model2)
pp_check(model2)
plot(fitted(model2), resid(model2), xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot for Model 2")

fit11 <- stan_glmer(vote_pct ~ poverty_rate + (1 + poverty_rate |state), data = group_by_state)
fit11
summary(fit11)
pp_check(fit11)
plot(fitted(fit11), resid(fit11))

## CITATIONS

citation("ggplot2")
citation("readr")
citation("tidyr")
citation("dplyr")
citation("magrittr")
citation("rstanarm")
citation("lme4")

