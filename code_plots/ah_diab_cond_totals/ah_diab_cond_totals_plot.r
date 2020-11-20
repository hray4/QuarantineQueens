# Use cleaned data set from "AH Provisional Diabetes.." raw data to do analysis

# Date Created: Nov. 19, 2020

# All conditions death count plot
# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(magrittr)
library(forcats)
library(ggplot2)
# library(RColorBrewer)


# Set current working directory to this file's parent directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Clean data file location
clean_data_path <- '../../clean_data/ah_diabetes/'
cond_fn <- 'DIAB_all_totals.csv'

# Concatenate base path with cond_fn
diab_path <- file.path(clean_data_path, cond_fn)


diab_data <- read_csv(diab_path, col_types = 'fi')


# Death counts vs Condition (ordered by death count)
diab_data$Condition <- reorder(
    diab_data$Condition, diab_data$Total.Deaths)

# Output final plot data
write_csv(diab_data, 'plot_cond_totals_data.csv')


# Make ggplot of Condition vs Deaths
gg_cond_totals <- diab_data %>%
    ggplot(aes(x = Condition, y = Total.Deaths)) +
    geom_col(width = 0.75, fill = 'firebrick4') +
    labs(x='', y='',
         title = 'Major Cardiovascular and Hypertensive Diseases lead in Covid-19 Deaths',
         subtitle = 'Thousands of Deaths by Underlying Condition (Jan - Sep 2020)',
         caption = '*MCVD = Major Cardiovascular Diseases') +
    scale_y_continuous(labels = function(y) {paste0(y/1000, 'k')},
                       expand = expand_scale(mult = c(0.01, 0.05))) +
    coord_flip() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 1.25),
          plot.subtitle = element_text(face = 'italic', size = 9.5, hjust = -2.15),
          panel.grid.major.y = element_blank())

pdf("plots/condition_total.pdf")
print(gg_cond_totals)

dev.off()
