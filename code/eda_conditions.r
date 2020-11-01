# Exploratory data analysis of data set "Conditions contributing.."

## Round 3 - (Deaths by condition)


# Date Created:  Nov. 1, 2020

# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
library(ggplot2)

# Define %notin% operator for use later
`%notin%` <- Negate(`%in%`)


data_path <- '../src_data/'
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
    filter(is.na(Flag)) %>%
    drop_na(Covid19.Deaths)

# print(summary(cond_types_fixed))


ovr_age_deaths <- filter(cond_types_fixed, State.Abbrev =='US') %>%
    group_by(Condition.Group, Age.Group) %>%
    summarize(Total.Deaths = sum(Covid19.Deaths) / 1000) %>%
    droplevels()

# Condition group 'Intentional .. ' is too long, add a newline in middle
ovr_age_deaths$Condition.Group <- recode_factor(ovr_age_deaths$Condition.Group,
    'Intentional and unintentional injury, poisoning, and other adverse events' =
    paste0('Intentional and unintentional injury,\n',
          'poisoning, and other adverse events'))


# All ages exploration
ovr_age_deaths <- filter(ovr_age_deaths, Age.Group == 'All Ages') %>%
    droplevels()


# Death counts vs Condition by age group (ordered by death count)
ovr_age_deaths$Condition.Group <- reorder(
    ovr_age_deaths$Condition.Group,
    ovr_age_deaths$Total.Deaths)


gg_conds_age <- ovr_age_deaths %>%
    ggplot(mapping = aes(x = Condition.Group, y = Total.Deaths)) +
    geom_col(fill = 'red', width = 0.75) +
    coord_flip() +
    theme_minimal() +
    labs(x = '', y = '',
         title = 'Respiratory diseases top the COVID-19 comorbidities list',
         subtitle = 'Death count (thousands) by Condition Group') +
    theme(plot.title = element_text(hjust = 7),
          plot.subtitle = element_text(face = 'italic', size = 9.5,
                                       hjust = -1.27),
          panel.grid.major.y = element_blank())


pdf("../plots/conds_deaths.pdf")
print(gg_conds_age)

dev.off()
