# Exploring "AH Provisional Diabetes.." data set


# Date Created:  Nov. 4, 2020

# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
library(ggplot2)

# Define %notin% operator for use later
`%notin%` <- Negate(`%in%`)


data_path <- '../src_data/'
cond_fn <- 'AH_Provisional_Diabetes_Death_Counts__2020.csv'

# Concatenate base path with cond_fn
conditions_path <- file.path(data_path, cond_fn)

col_names <- c(
    'Data.as.of',
    'Death.Year',
    'Death.Month',
    'Age.Group',
    'Sex',
    'COVID19',
    'Diabetes.uc',
    'Diabetes.mc',
    'C19_Diabetes',
    'C19_Hypertensive.Diseases',
    'C19_Major.Cardio.Disease',
    'C19_Hypertensive_MCVD',
    'C19_Chronic.Lower.Resp.Disease',
    'C19_Kidney.Disease',
    'C19_Chronic.Liver.Disease_Cirrhosis',
    'C19_Obesity'
)

conditions_data <- read_csv(conditions_path,
                            col_names = col_names, skip = 1)

cond_types_fixed <- conditions_data %>%
    mutate(Data.as.of = as.Date(Data.as.of, "%m/%d/%Y"),
           Death.Year = factor(Death.Year),
           Death.Month = factor(Death.Month),
           Age.Group = factor(Age.Group),
           Sex = factor(Sex)) %>%
    filter(!is.na(Age.Group))

# print(summary(cond_types_fixed))


# Transform covid19 and condition deaths from wide to long tidy format
wide.deaths <- cond_types_fixed %>%
    select(Death.Month,
           COVID19,
           Diabetes.uc,
           Diabetes.mc,
           C19_Diabetes,
           C19_Hypertensive.Diseases,
           C19_Major.Cardio.Disease,
           C19_Hypertensive_MCVD,
           C19_Chronic.Lower.Resp.Disease,
           C19_Kidney.Disease,
           C19_Chronic.Liver.Disease_Cirrhosis,
           C19_Obesity) %>%
    gather(key = 'Condition', value = 'Deaths', -Death.Month) %>%
    mutate(Condition = factor(Condition)) %>%
    droplevels() %>%
    group_by(Death.Month, Condition) %>%
    summarize(Total.Deaths = sum(Deaths))


# print(summary(wide.deaths))b


# Death count over time (months) by condition
gg_deaths_time_cond <- wide.deaths %>%
    ggplot(mapping = aes(x = Death.Month, y = Total.Deaths, group = 1)) +
    geom_line(color = 'coral') +
    theme_minimal() +
    facet_wrap(Condition ~ ., ncol = 2) +
    labs(x = '', y = '',
         title = 'Covid19 and comorbid conditions death counts over time',
         subtitle = 'Jan - Sep 2020') +
    theme(plot.subtitle = element_text(face = 'italic', size = 9.5),
          panel.grid.major.x = element_blank())


pdf("plots/deaths_over_time_by_cond.pdf")
print(gg_deaths_time_cond)

dev.off()
