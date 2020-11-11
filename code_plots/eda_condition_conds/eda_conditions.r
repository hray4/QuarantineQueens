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


us_deaths <- filter(cond_types_fixed, State.Abbrev == 'US') %>%
    group_by(Condition.Group, Age.Group) %>%
    summarize(Total.Deaths = sum(Covid19.Deaths) / 1000) %>%
    droplevels()

# Condition group 'Intentional .. ' is too long, add a newline in middle
us_deaths$Condition.Group <- recode_factor(us_deaths$Condition.Group,
    'Intentional and unintentional injury, poisoning, and other adverse events' =
    paste0('Intentional and unintentional injury,\n',
          'poisoning, and other adverse events'))


# All ages exploration
all_age_us_deaths <- filter(us_deaths, Age.Group == 'All Ages') %>%
    droplevels()


# Death counts vs Condition by age group (ordered by death count)
all_age_us_deaths$Condition.Group <- reorder(
    all_age_us_deaths$Condition.Group,
    all_age_us_deaths$Total.Deaths)

# ALL AGES plot
gg_conds_all_ages <- all_age_us_deaths %>%
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


pdf("plots/conds_deaths_all_ages.pdf")
print(gg_conds_all_ages)

dev.off()



# Age breakdown exploration
us_deaths_by_age <- filter(us_deaths,
                            Age.Group %notin% c('Not stated', 'All Ages')) %>%
    droplevels()


# Death counts vs Condition by age group (ordered by death count)
us_deaths_by_age$Condition.Group <- reorder(
    us_deaths_by_age$Condition.Group,
    us_deaths_by_age$Total.Deaths)

# ALL AGES plot
gg_conds_age <- us_deaths_by_age %>%
    ggplot(mapping = aes(x = Condition.Group, y = Total.Deaths)) +
    geom_col(fill = 'red', width = 0.75) +
    facet_grid( ~ Age.Group) +
    coord_flip() +
    theme_bw() +
    labs(x = '', y = '',
         title = 'COVID-19 has disproportionately affected the elderly',
         subtitle = 'US Death count by Condition Group and Age Group') +
    theme(plot.title = element_text(hjust = -40),
          plot.subtitle = element_text(face = 'italic', size = 9.5,
                                       hjust = -1.56),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())


pdf("plots/conds_deaths_age.pdf")
print(gg_conds_age)

dev.off()
