# Cleaning and making choropleth USA map plots

# url: https://covid.cdc.gov/covid-data-tracker/#cases_casesper100klast7days

# Date Created: Nov 27, 2020

# Case rate / 100k over past 7 days by state and
# Death rate / 100k over past 7 days by state
# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(magrittr)
library(forcats)
library(ggplot2)
library(stringr)

# choropleth pcks
library(plotly)


# Define %notin% operator for use later
`%notin%` <- Negate(`%in%`)


# Set current working directory to this file's parent directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Clean data file location
data_path <- '../../src_data/'
covid_state_fn <- 'united_states_covid19_cases_and_deaths_by_state.csv'

# Concatenate base path with covid_state_fn
covid_state_path <- file.path(data_path, covid_state_fn)


covid_state_data <- read_csv(covid_state_path, skip = 3)

simp_states <- covid_state_data %>%
    select("State/Territory",
           "Case Rate per 100000 in Last 7 Days",
           "Death Rate per 100K in Last 7 Days") %>%
    rename(State.Name = "State/Territory",
           Cases.Per100k.Last7Days = "Case Rate per 100000 in Last 7 Days",
           Deaths.Per100k.Last7Days = "Death Rate per 100K in Last 7 Days") %>%
    mutate(State.Name = factor(State.Name)) %>%
    droplevels()

# print(summary(simp_states))


# ------------------------------------------------------------------------------
# Load State census population data
# state_census_fn <- 'state_census_pops/nst-est2019-alldata.csv'
# state_census_path <- file.path(data_path, state_census_fn)

# state_census <- read_csv(state_census_path)

# state_census_pop <- state_census %>%
#     rename(State = NAME,
#            Population = POPESTIMATE2019) %>%
#     mutate(State = factor(State)) %>%
#     filter(SUMLEV == '40')  %>%
#     select(State, Population)

# # print(summary(state_census_pop))


# # ------------------------------------------------------------------------------
# # Merge Covid state and state census data sets
# merged_covid_state <- merge(simp_states, state_census_pop,
#                             by = 'State')

# merged_covid_state <- merged_covid_state %>%
#     rename(State.Name = State)

# print(summary(merged_covid_state))


# ------------------------------------------------------------------------------
# Load state to state abbreviation crosswalk file
state_abbrv_xwalk_fn <- 'crosswalks/state_abbrv_name_xwalk.csv'
state_abbrv_xwalk_path <- file.path(data_path, state_abbrv_xwalk_fn)

state_abbrv_xwalk <- read_csv(state_abbrv_xwalk_path)

final_data <- merge(simp_states, state_abbrv_xwalk,
                    by = 'State.Name')

final_data <- final_data %>%
    mutate(State.Abbrev = factor(State.Abbrev)) %>%
    # remove Wash DC and Puerto Rico from map data
    filter(State.Abbrev %notin% c('DC', 'PR')) %>%
    droplevels()


final_data$hover <- with(
    final_data, paste(
    State.Name, '<br>',
    'CasesPer100k<br>Last7Days:',
    Cases.Per100k.Last7Days))

final_data$hoverD <- with(
    final_data, paste(
    State.Name, '<br>',
    'DeathsPer100k<br>Last7Days:',
    Deaths.Per100k.Last7Days))

# print(summary(final_data))



# ------------------------------------------------------------------------------
# Make choropleth maps

m <- list(
  l = 10,
  r = 50,
  b = 10,
  t = 50,
  pad = 0
)
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# Cases plot
# fig_cases <- plot_geo(final_data, locationmode = 'USA-states', offline=TRUE)
# fig_cases <- fig_cases %>% add_trace(
#     z = ~Cases.Per100k.Last7Days, text = ~hover, locations = ~State.Abbrev,
#     color = ~Cases.Per100k.Last7Days, colors = 'YlOrRd'
#   )
# fig_cases <- fig_cases %>% colorbar(title = "", tickfont = list(size = 18))
# fig_cases <- fig_cases %>% layout(
#     title = list(text = 'US COVID-19 Cases Per 100,000 Persons (Nov. 21-27)',
#                  font = list(color = '#000000', size = 24)),
#     geo = g,
#     margin = m
#   )

# orca(fig_cases, file='plots/usa_cases_map.png')

## Output top 15 states
cases_ord <- final_data %>%
    select(State.Name, Cases.Per100k.Last7Days) %>%
    arrange(-Cases.Per100k.Last7Days) %>%
    rename(Cases.Per100k = Cases.Per100k.Last7Days)

top10_cases <- cases_ord[1:10, ]
write_csv(top10_cases, 'cases_top10.csv')


# Deaths plot
fig_deaths <- plot_geo(final_data, locationmode = 'USA-states', offline=TRUE)
fig_deaths <- fig_deaths %>% add_trace(
    z = ~Deaths.Per100k.Last7Days, text = ~hoverD, locations = ~State.Abbrev,
    color = ~Deaths.Per100k.Last7Days, colors = 'YlOrRd'
  )
fig_deaths <- fig_deaths %>% colorbar(title = "", tickfont = list(size = 18))
fig_deaths <- fig_deaths %>% layout(
    title = list(text = 'US COVID-19 Deaths Per 100,000 Persons (Nov. 21-27)',
                 font = list(color = '#000000', size = 24)),
    geo = g,
    margin = m
  )

orca(fig_deaths, file='plots/usa_deaths_map.png')


## Output top 15 states
deaths_ord <- final_data %>%
    select(State.Name, Deaths.Per100k.Last7Days) %>%
    arrange(-Deaths.Per100k.Last7Days) %>%
    rename(Deaths.Per100k = Deaths.Per100k.Last7Days)

top10_deaths <- deaths_ord[1:10, ]
write_csv(top10_deaths, 'deaths_top10.csv')
