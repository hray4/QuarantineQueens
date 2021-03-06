# Data Quality Assessment of data set "Conditions contributing.."
# Date Created:  Nov. 2, 2020

library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
library(ggplot2)

# Define %notin% operator for use later
`%notin%` <- Negate(`%in%`)


data_path <- '../../src_data/'
filename <- 'Conditions_contributing_to_deaths_involving_coronavirus_disease_2019__COVID-19___by_age_group_and_state__United_States..csv'

# Concatenate base path with filename
conditions_fn <- file.path(data_path, filename)

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

conditions_data <- read_csv(conditions_fn, col_names = col_names, skip = 1)

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
    drop_na(Covid19.Deaths)



# Assessing data quality across 6 dimensions:
# 1. Completeness - requires that a particular column, element, or class of data
#    is populated and does not feature null values or values in place of null (e.g., N/A)
# 2. Uniqueness - are all the entities or attributes within a dataset unique?
# 3. Accuracy - refers to whether the data values stored for an object are the correct
#    values. To be correct, a data value must the right value and must be represented
#    in a consistent and unambiguous form. For ex, my birth date is December 13, 1941.
# 4. Atomicity -
# 5. Conformity - does the data conform to right conventions and standards?
# 6. Overall quality - overall assessment of data quality


# 1. Completeness
print(summary(cond_types_fixed))

# 3. Accuracy
us_conds <- filter(cond_types_fixed, State == 'US',
                   Age.Group %notin% c('Not stated', 'All Ages')) %>%
    droplevels()

gg_boxplot <- ggplot(data = us_conds) +
    geom_boxplot(aes(x = Age.Group, y = Covid19.Deaths)) +
    labs(x = '', y = '')

print(gg_boxplot)


# ovr_age_deaths <- filter(cond_types_fixed, State %notin% c('US', 'YC')) %>%
#     group_by(State, Age.Group) %>%
#     summarize(Total.Deaths = sum(Covid19.Deaths)) %>%
#     droplevels()


# excl_ages <- c('All Ages', 'Not stated')
# ovr_age_deaths <- filter(ovr_age_deaths, Age.Group %notin% excl_ages) %>%
#     droplevels()

# # x_age_groups <- unique(ovr_age_deaths$Age.Group)


# # Death counts vs Age group by state
# gg_age_deaths <- ovr_age_deaths %>%
#     ggplot(mapping = aes(x = Age.Group, y = Total.Deaths)) +
#     geom_col(fill = 'red') +
#     facet_wrap( ~ State, nrow = 7) +
#     scale_x_discrete(labels = x_age_groups)

# pdf("../plots/eda1/gg_age_deaths_states.pdf")
# print(gg_age_deaths)

# dev.off()


# # Death counts vs State by age group
# ovr_state_deaths <- ovr_age_deaths
# ovr_state_deaths$State <- reorder(ovr_state_deaths$State,
#                                   ovr_state_deaths$Total.Deaths)

# gg_state_deaths <- ovr_state_deaths %>%
#     ggplot(mapping = aes(x = State, y = Total.Deaths)) +
#     geom_col(fill = 'red') +
#     facet_grid( ~ Age.Group) +
#     coord_flip() +
#     scale_y_continuous(breaks = c(0, 10000, 20000),
#                        labels = function(x) {x/1000}) +
#     labs(x = '', y = '',
#            title = 'COVID-19 has disproportionately affected older Americans',
#            subtitle = 'Death counts (Thousands) by State and Age Group') +
#     theme(plot.subtitle = element_text(face = 'italic', size = 10))


# pdf("../plots/eda1/gg_state_deaths.pdf")
# print(gg_state_deaths)

# dev.off()
