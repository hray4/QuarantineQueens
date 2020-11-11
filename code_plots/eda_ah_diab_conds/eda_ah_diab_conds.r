# Use cleaned data set from "AH Provisional Diabetes.." raw data to do analysis

# Date Created: Nov. 11, 2020

# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(magrittr)
library(forcats)
library(ggplot2)
library(RColorBrewer)


# Set current working directory to this file's parent directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Clean data file location
clean_data_path <- '../../clean_data/ah_diabetes/'
cond_fn <- 'diab_mon_age_sex_cond.csv'

# Concatenate base path with cond_fn
diab_path <- file.path(clean_data_path, cond_fn)


diab_data <- read_csv(diab_path, col_types = 'ffffi')


cond_sex_deaths <- diab_data %>%
    group_by(Condition, Sex) %>%
    summarize(Total.Deaths = sum(Total.Deaths)) %>%
    droplevels()


# Death counts vs Condition by Sex (ordered by death count)
cond_sex_deaths$Condition <- reorder(
    cond_sex_deaths$Condition,
    cond_sex_deaths$Total.Deaths)

# Output final plot data
write_csv(cond_sex_deaths, 'plot_cond_sex_data.csv')


# Make ggplot of Condition vs Deaths by Sex (fill)
gg_cond_sex <- cond_sex_deaths %>%
    ggplot(mapping = aes(x = Condition, y = Total.Deaths, fill = Sex)) +
    geom_col(position = 'dodge', width = 0.75) +
    xlab('') + ylab('') +
    ggtitle('Males have more total deaths than females for all conditions',
            subtitle = 'Thousands of Deaths by Condition and Sex (Jan - Sep 2020)') +
    scale_y_continuous(labels = function(y) {paste0(y/1000, 'k')}) +
    scale_fill_discrete(breaks = c("Male (M)", "Female (F)"),
                        labels = c("  Male", "  Female")) +
    coord_flip() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 14.25),
          plot.subtitle = element_text(face = 'italic', size = 9.5, hjust = -1.5),
          panel.grid.major.y = element_blank(),
          legend.position = c(0.8, 0.2),
          legend.title = element_blank())

pdf("plots/condition_sex.pdf")
print(gg_cond_sex)

dev.off()


# ------------------------------------------------------------------------------
# Condition vs Deaths by Age Group
cond_age_deaths <- diab_data %>%
    group_by(Condition, Age.Group) %>%
    summarize(Total.Deaths = sum(Total.Deaths)) %>%
    droplevels()


# Death counts vs Condition by age (ordered by death count)
cond_age_deaths$Condition <- reorder(
    cond_age_deaths$Condition,
    cond_age_deaths$Total.Deaths)

# Reorder age group levels (young to old now)
# cond_age_deaths$Age.Group <- fct_rev(cond_age_deaths$Age.Group)

# Output final plot data
write_csv(cond_age_deaths, 'plot_cond_age_data.csv')


# Make ggplot of Condition vs Deaths by age (fill)
gg_cond_age <- cond_age_deaths %>%
    ggplot(mapping = aes(x = Condition, y = Total.Deaths, fill = Age.Group)) +
    geom_col(position = 'dodge') +
    xlab('') + ylab('') +
    ggtitle('Diabetes and Covid 19 have hit older Americans the hardest',
            subtitle = 'Thousands of Deaths by Condition and Age Group (Jan - Sep 2020)') +
    scale_y_continuous(labels = function(y) {paste0(y/1000, 'k')}) +
    scale_fill_brewer(breaks = rev(unique(cond_age_deaths$Age.Group)),
                      palette = 'OrRd') +
    coord_flip() +
    # theme_minimal() +
    theme(plot.title = element_text(hjust = 28.5),
          plot.subtitle = element_text(face = 'italic', size = 9.5, hjust = -1.5),
          panel.grid.major.y = element_blank(),
          legend.position = c(0.8, 0.2),
          legend.title = element_blank(),
          axis.ticks = element_blank())
          # panel.background = element_rect(fill = 'gray90', color = 'white'))

pdf("plots/condition_age.pdf")
print(gg_cond_age)

dev.off()
