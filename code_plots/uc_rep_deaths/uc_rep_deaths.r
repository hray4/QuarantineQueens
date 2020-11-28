# Making ggplot of Occurence of underlying condition representation
# in COVID deaths

# Date Created: Nov 28, 2020
# ------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)
library(stringr)


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

under_conds_keep <- c(
    'Chronic lower respiratory diseases',
    'Ischemic heart disease',
    'Cardiac arrest',
    'Cardiac arrhythmia',
    'Heart failure',
    'Diabetes',
    'Obesity',
    'Renal failure')

uc_rep_deaths <- uc_rep_deaths %>%
    mutate(Underlying_Condition = factor(Underlying_Condition)) %>%
    filter(Underlying_Condition %in% under_conds_keep)

uc_rep_deaths$Underlying_Condition <- reorder(
    uc_rep_deaths$Underlying_Condition,
    uc_rep_deaths$COVID_Deaths_Rep_Perc)

# Output final plotting data
write_csv(uc_rep_deaths, 'plot_uc_rep_deaths.csv')


fmt_pct <- function(y) {
    return (ifelse(y == 0, '0%', y * 100))
}

# Make ggplot of Underlying condition vs Death rep %
gg_uc_rep_death <- uc_rep_deaths %>%
    ggplot(aes(x=Underlying_Condition, y=COVID_Deaths_Rep_Perc)) +
    geom_col(color='gray90', width = 0.6) +
    coord_flip() +
    labs(x='', y='',
         title = 'Underlying Condition Representation in COVID-19 Deaths (%)',
         caption = '*Only includes conditions with strong evidence for COVID-19 severe illness') +
    theme_minimal() +
    scale_x_discrete(labels = function(x) {str_wrap(x, width = 20)}) +
    scale_y_continuous(labels = function(y) {fmt_pct(y)}) +
    theme(panel.grid.major.y = element_blank(),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 16, hjust = 1.3),
          plot.caption = element_text(size = 12))

pdf("plots/gg_uc_rep_death.pdf")
print(gg_uc_rep_death)

dev.off()
# ggsave(filename = "plots/gg_us_rept_death.png",
#        plot = gg_uc_rep_death)
