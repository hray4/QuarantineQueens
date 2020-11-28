# Making ggplot of Occurence of underlying condition representation
# in COVID deaths

# Date Created: Nov 28, 2020
# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)


# Set current working directory to this file's parent directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Clean data file location
data_path <- '../../clean_data/excel_sims/'
uc_rep_deaths_fn <- 'uc_rep_deaths.csv'

# Concatenate base path with uc_rep_deaths_fn
uc_rep_deaths_path <- file.path(data_path, uc_rep_deaths_fn)

uc_rep_deaths  <- read_csv(uc_rep_deaths_path)

# ------------------------------------------------------------------------------
# Prepare data for plotting

uc_rep_deaths <- uc_rep_deaths %>%
    mutate(Underlying_Condition = factor(Underlying_Condition))

uc_rep_deaths$Underlying_Condition <- reorder(
    uc_rep_deaths$Underlying_Condition,
    uc_rep_deaths$COVID_Deaths_Rep_Perc)

# Output final plotting data
write_csv(uc_rep_deaths, 'plot_uc_rep_deaths.csv')


# Make ggplot of Underlying condition vs Death rep %
gg_uc_rep_death <- uc_rep_deaths %>%
    ggplot(aes(x=Underlying_Condition, y=COVID_Deaths_Rep_Perc)) +
    geom_col(color='gray90') +
    coord_flip() +
    theme_minimal()

print(gg_uc_rep_death)
