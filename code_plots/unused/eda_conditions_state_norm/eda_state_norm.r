# Exploratory data analysis of data set "Conditions contributing.."

## Round 2 - (Deaths by state per 100000 persons in state)

### Steps ------------------------------------------------------------------------
# 1) Load "nst-est2019-alldata" [census state population data]
#      and state abbreviation-name crosswalk files

# 2) Merge census and crosswalk data on state name

# 3) Merge Conditions with census data on state abbreviation

# 4) Divide death counts by population by state

# 5) Adjust new death counts to deaths / 100000 persons

# 6) Plots!

### Reasoning ------------------------------------------------------------------
# I am bringing in Census data here to normalize each state's death
# by dividing by its most recent estimate population count

# Then I can format to deaths / 100000 persons (standard unit)

# Date Created:  Oct. 31, 2020

# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
library(ggplot2)

# Define %notin% operator for use later
`%notin%` <- Negate(`%in%`)


data_path <- '../../src_data/'
cond_fn <- 'Conditions_contributing_to_deaths_involving_coronavirus_disease_2019__COVID-19___by_age_group_and_state__United_States..csv'

# Concatenate base path with cond_fn
conditions_path <- file.path(data_path, cond_fn)

col_names <- c(
    'Data.as.of',
    'Start.Week',
    'End.Week',
    'State',
    'Condition.Group',
    'Condition',
    'ICD10.Codes',
    'Age.Group',
    'Covid19.Deaths',
    'Flag'
)

conditions_data <- read_csv(conditions_path,
                            col_names = col_names, skip = 1)

cond_types_fixed <- conditions_data %>%
    mutate(Data.as.of = as.Date(Data.as.of, "%m/%d/%Y"),
           Start.Week = as.Date(Start.Week, "%m/%d/%Y"),
           End.Week = as.Date(End.Week, "%m/%d/%Y"),
           State = factor(State),
           Condition.Group = factor(Condition.Group),
           Condition = factor(Condition),
           ICD10.Codes = factor(ICD10.Codes),
           Age.Group = factor(Age.Group),
           Flag = factor(Flag)) %>%
    rename(State.Abbrev = State) %>%
    drop_na(Covid19.Deaths)

# print(summary(cond_types_fixed))

ovr_age_deaths <- filter(cond_types_fixed,
                         State.Abbrev %notin% c('US', 'YC')) %>%
    group_by(State.Abbrev, Age.Group) %>%
    summarize(Total.Deaths = sum(Covid19.Deaths)) %>%
    droplevels()


excl_ages <- c('All Ages', 'Not stated')
ovr_age_deaths <- filter(ovr_age_deaths, Age.Group %notin% excl_ages) %>%
    droplevels()


# Import census data
census_fn <- 'nst-est2019-alldata.csv'
census_path <- file.path(data_path, census_fn)

census_state_pops <- read_csv(census_path) %>%
    select(c('SUMLEV',
             'REGION',
             'DIVISION',
             'STATE',
             'NAME',
             'POPESTIMATE2019')) %>%
    mutate(SUMLEV = factor(SUMLEV),
           REGION = factor(REGION, levels=c('1', '2', '3', '4', 'X')),
           DIVISION = factor(DIVISION),
           STATE = factor(STATE),
           NAME = factor(NAME)) %>%
    rename(Sum.Level = SUMLEV,
           Region = REGION,
           Division = DIVISION,
           State.Code = STATE,
           State.Name = NAME)


# sumlev 10 = United States overall
#        20 = pop by region
#        40 = pop by states
state_pops <- census_state_pops %>%
    filter(Sum.Level == '40') %>%
    droplevels()


# Import crosswalk file
xwalk_path <- '../src_data/crosswalks/'
state_xwalk_fn <- 'state_abbrv_name_xwalk.csv'
state_path <- file.path(xwalk_path, state_xwalk_fn)

state_xwalk <- read_csv(state_path) %>%
    mutate(State.Abbrev = factor(State.Abbrev),
           State.Name = factor(State.Name))


# Merge census data with state crosswalk data to get
# population by state abbreviation (same format as conditions data)
state_pops_merged <- merge(state_pops, state_xwalk,
                           by = 'State.Name', all.x = TRUE)


# Merge initial Conditions data with Census data
cond_census_merged <- merge(ovr_age_deaths, state_pops_merged,
                            by = 'State.Abbrev', all.x = TRUE)


# cond_census_merged$Region <- recode_factor(cond_census_merged$Region,
#     'Northeast' = '1',
#     'Midwest' = '2',
#     'South' = '3',
#     'West' = '4',
#     'Puerto.Rico' = 'X')

cond_census_merged <- cond_census_merged %>%
    group_by(Region, Age.Group) %>%
    summarize(Total.Deaths = sum(Total.Deaths),
              Total.Pop = sum(POPESTIMATE2019)) %>%
    mutate(Deaths.Per.State.Person = Total.Deaths / Total.Pop,
           Region.Name = factor(Region,
                                levels = c('1', '2', '3', '4', 'X'),
                                labels = c('Northeast',
                                           'Midwest',
                                           'South',
                                           'West',
                                           'Puerto.Rico'))) %>%
    droplevels()

print(summary(cond_census_merged))

# Death counts vs State by age group (ordered by death count)
cond_census_merged$Region.Name <- reorder(
    cond_census_merged$Region.Name,
    cond_census_merged$Deaths.Per.State.Person)


gg_state_deaths <- cond_census_merged %>%
    ggplot(mapping = aes(x = Region.Name, y = Deaths.Per.State.Person)) +
    geom_col(fill = 'red') +
    facet_grid( ~ Age.Group) +
    coord_flip() +
    scale_y_continuous(labels = function(y) {paste0(y*100)}) +
    labs(x = '', y = '',
         title = 'COVID-19 has disproportionately affected older Americans',
         subtitle = 'Death count per 100,000 persons by Region and Age Group') +
    theme(plot.subtitle = element_text(face = 'italic', size = 10))


pdf("plots/region_deaths_norm.pdf")
print(gg_state_deaths)

dev.off()
