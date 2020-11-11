# Exploring "AH Provisional Diabetes.." data set

# Date Created: Nov.  4, 2020

# Date Updated: Nov. 11, 2020

# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
library(ggplot2)

# Define %notin% operator for use later
`%notin%` <- Negate(`%in%`)


# Set current working directory to this file's parent directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Source data location
data_path <- '../../src_data/'
cond_fn <- 'AH_Provisional_Diabetes_Death_Counts__2020.csv'

# Concatenate base path with cond_fn
conditions_path <- file.path(data_path, cond_fn)

col_names <- c(
    'Data.as.of',
    'Death.Year',
    'Death.Month',
    'Age.Group',
    'Sex',
    'COVID19',  # Deaths with COVID-19 (U071) as underlying or contributing cause
    'Diabetes.uc',  # Deaths with diabetes (E10–E14) as underlying cause
    'Diabetes.mc',  # Deaths with diabetes (E10–E14) as underlying or contributing cause
    'C19_Diabetes', # Deaths with COVID-19 (U071) and diabetes (E10–E14)
    'C19_Hypertensive.Disease', # Deaths with COVID-19 (U071) and hypertensive disease (I10–I15)
    'C19_Major.Cardio.Disease',  # Deaths with COVID-19 (U071) and major cardiovascular diseases (I00–I78)
    'C19_Hypertensive_MCVD',  # Deaths with COVID-19 (U071), hypertensive disease (I10–I15), and major cardiovascular diseases (I00–I78)
    'C19_Chronic.Lower.Resp.Disease', # Deaths with COVID-19 (U071) and chronic lower respiratory disease (J40–J47)
    'C19_Kidney.Disease',  # Deaths with COVID-19 (U071) and kidney disease (N00–N07, N17–N19, and N25–N27)
    'C19_Chronic.Liver.Disease_Cirrhosis', # Deaths with COVID-19 (U071) and chronic liver disease and cirrhosis (K70, K73–K74)
    'C19_Obesity'  # Deaths with COVID-19 (U071) and obesity (E66)
)

conditions_data <- read_csv(conditions_path, col_names = col_names, skip = 1)

age.group.excl <- c('50-64 years', '65-74 years', '75-84 years', '85+ years', 'Unknown age')

cond_types_fixed <- conditions_data %>%
    mutate(Data.as.of = as.Date(Data.as.of, "%m/%d/%Y"),
           Death.Year = factor(Death.Year),
           Death.Month = factor(Death.Month),
           Age.Group = factor(Age.Group),
           Sex = factor(Sex)) %>%
    filter(!is.na(Age.Group),
           Age.Group %notin% age.group.excl)

# print(summary(cond_types_fixed))
print(summary(cond_types_fixed$Age.Group))


# Transform covid19 and condition deaths from wide to long tidy format
wide.deaths <- cond_types_fixed %>%
    select(Death.Month,
           Age.Group,
           Sex,
           COVID19,
           Diabetes.uc,
           Diabetes.mc,
           C19_Diabetes,
           C19_Hypertensive.Disease,
           C19_Major.Cardio.Disease,
           C19_Hypertensive_MCVD,
           C19_Chronic.Lower.Resp.Disease,
           C19_Kidney.Disease,
           C19_Chronic.Liver.Disease_Cirrhosis,
           C19_Obesity) %>%
    gather(key = 'Condition', value = 'Deaths', COVID19:C19_Obesity) %>%
    mutate(Condition = factor(Condition)) %>%
    droplevels() %>%
    group_by(Death.Month, Age.Group, Sex, Condition) %>%
    summarize(Total.Deaths = sum(Deaths))


# print(summary(wide.deaths))
# print(summary(wide.deaths$Age.Group))


# Output cleaned data set
cleaned_data_fn <- '../../clean_data/ah_diabetes/diab_mon_age_sex_cond.csv'
write_csv(wide.deaths, cleaned_data_fn)


# Death count over time (months) by condition
# gg_deaths_time_cond <- wide.deaths %>%
#     ggplot(mapping = aes(x = Death.Month, y = Total.Deaths, group = 1)) +
#     geom_line(color = 'coral') +
#     theme_minimal() +
#     facet_wrap(Condition ~ ., ncol = 2) +
#     labs(x = '', y = '',
#          title = 'Covid19 and comorbid conditions death counts over time',
#          subtitle = 'Jan - Sep 2020') +
#     theme(plot.subtitle = element_text(face = 'italic', size = 9.5),
#           panel.grid.major.x = element_blank())


# pdf("plots/deaths_over_time_by_cond.pdf")
# print(gg_deaths_time_cond)

# dev.off()
