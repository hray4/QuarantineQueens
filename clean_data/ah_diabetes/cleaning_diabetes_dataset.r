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
    'COVID-19',  # Deaths with COVID-19 (U071) as underlying or contributing cause
    'Diabetes.Underlying.Cause',  # Deaths with diabetes (E10–E14) as underlying cause
    'Diabetes.Under.Contrib.Cause',  # Deaths with diabetes (E10–E14) as underlying or contributing cause
    'C19+Diabetes', # Deaths with COVID-19 (U071) and diabetes (E10–E14)
    'C19+Hypertensive.Disease', # Deaths with COVID-19 (U071) and hypertensive disease (I10–I15)
    'C19+MCVD',  # Deaths with COVID-19 (U071) and major cardiovascular diseases (I00–I78)
    'C19+Hypertensive+MCVD',  # Deaths with COVID-19 (U071), hypertensive disease (I10–I15), and major cardiovascular diseases (I00–I78)
    'C19+Chronic.Lower.Resp.Disease', # Deaths with COVID-19 (U071) and chronic lower respiratory disease (J40–J47)
    'C19+Kidney.Disease',  # Deaths with COVID-19 (U071) and kidney disease (N00–N07, N17–N19, and N25–N27)
    'C19+Chronic.Liver.Disease+Cirrhosis', # Deaths with COVID-19 (U071) and chronic liver disease and cirrhosis (K70, K73–K74)
    'C19+Obesity'  # Deaths with COVID-19 (U071) and obesity (E66)
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
# print(summary(cond_types_fixed$Age.Group))


# Transform covid19 and condition deaths from wide to long tidy format
wide.deaths <- cond_types_fixed %>%
    select("Death.Month",
           "Age.Group",
           "Sex",
           "COVID-19",
           # Diabetes.Underlying.Cause",
           # Diabetes.Under.Contrib.Cause",
           "C19+Diabetes",
           "C19+Hypertensive.Disease",
           "C19+MCVD",
           "C19+Hypertensive+MCVD",
           "C19+Chronic.Lower.Resp.Disease",
           "C19+Kidney.Disease",
           # C19+Chronic.Liver.Disease+Cirrhosis",  # not Strong evidence for severe illness
           "C19+Obesity") %>%
    gather(key = 'Condition', value = 'Deaths', "COVID-19":"C19+Obesity") %>%
    mutate(Condition = factor(Condition)) %>%
    droplevels() %>%
    group_by(Death.Month, Age.Group, Sex, Condition) %>%
    summarize(Total.Deaths = sum(Deaths))


# print(summary(wide.deaths))
# print(summary(wide.deaths$Age.Group))


# Output cleaned data set (overall)
cleaned_data_fn <- 'DIAB_month_age_sex.csv'
write_csv(wide.deaths, cleaned_data_fn)


group_sum <- function(groups, out_fn) {
    new_df <- wide.deaths %>%
        group_by_at(groups) %>%
        summarize(Total.Deaths = sum(Total.Deaths)) %>%
        droplevels()
    write_csv(new_df, out_fn)
    return(NULL)
}

# ------------------------------------------------------------------------------
# Age totals (stratified by sex and death month)
age_total_groups <- c("Death.Month", "Sex", "Condition")
age_total_fn <- 'DIAB_month_sex__age_totals.csv'
group_sum(age_total_groups, age_total_fn)


# ------------------------------------------------------------------------------
# Sex totals (stratified by age group and death month)
sex_total_groups <- c("Death.Month", "Age.Group", "Condition")
sex_total_fn <- 'DIAB_month_age__sex_totals.csv'
group_sum(sex_total_groups, sex_total_fn)


# ------------------------------------------------------------------------------
# Month totals (stratified by age group and sex)
mon_total_groups <- c("Sex", "Age.Group", "Condition")
mon_total_fn <- 'DIAB_sex_age__month_totals.csv'
group_sum(mon_total_groups, mon_total_fn)


# ------------------------------------------------------------------------------
# Month, Sex totals (stratified by age group only)
mon_sex_total_groups <- c("Age.Group", "Condition")
mon_sex_total_fn <- 'DIAB_age__sex_month_totals.csv'
group_sum(mon_sex_total_groups, mon_sex_total_fn)


# ------------------------------------------------------------------------------
# Month, Age totals (stratified by Sex only)
mon_age_total_groups <- c("Sex", "Condition")
mon_age_total_fn <- 'DIAB_sex__age_month_totals.csv'
group_sum(mon_age_total_groups, mon_age_total_fn)


# ------------------------------------------------------------------------------
# Sex, Age totals (stratified by Month only)
sex_age_total_groups <- c("Death.Month", "Condition")
sex_age_total_fn <- 'DIAB_month__age_sex_totals.csv'
group_sum(sex_age_total_groups, sex_age_total_fn)


# ------------------------------------------------------------------------------
# All totals (no stratification)
all_total_groups <- c("Condition")
all_total_fn <- 'DIAB_all_totals.csv'
group_sum(all_total_groups, all_total_fn)
