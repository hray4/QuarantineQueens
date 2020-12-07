# Making plot of simulated patient deaths with Diabetes, Hypertensive, Obesity

# Date Created: Dec 7, 2020
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

# uc_rep_deaths  <- read_csv(uc_rep_deaths_path)

# ------------------------------------------------------------------------------
# Prepare data for plotting

# under_conds_keep <- c(
#     'Chronic lower respiratory diseases',
#     'Ischemic heart disease',
#     'Cardiac arrest',
#     'Cardiac arrhythmia',
#     'Heart failure',
#     'Diabetes',
#     'Obesity',
#     'Renal failure')

conditions <- c(
    'Influenza and pneumonia',
    'Respiratory failure',
    'Hypertensive diseases',
    'Adult respiratory distress syndrome',
    'Vascular and unspecified dementia',
    'Sepsis',
    'Cardiac arrest')

deaths_perc <- c(0.35, 0.28, 0.155, 0.115, 0.08, 0.075, 0.07)

uc_rep_deaths <- data.frame(
    Underlying_Condition = conditions,
    COVID_Deaths_Rep_Perc = deaths_perc)

uc_rep_deaths <- uc_rep_deaths %>%
    mutate(Underlying_Condition = factor(Underlying_Condition))
    # filter(Underlying_Condition %in% under_conds_keep)

uc_rep_deaths$Underlying_Condition <- reorder(
    uc_rep_deaths$Underlying_Condition,
    uc_rep_deaths$COVID_Deaths_Rep_Perc)

# Output final plotting data
write_csv(uc_rep_deaths, 'plot_uc_sim_deaths_diab.csv')


fmt_pct <- function(y) {
    return (ifelse(y == 0, '0%', y * 100))
}

# Make ggplot of Underlying condition vs Death rep %
gg_uc_sim_death_diab <- uc_rep_deaths %>%
    ggplot(aes(x=Underlying_Condition, y=COVID_Deaths_Rep_Perc, fill=1)) +
    geom_col(width = 0.7) +
    coord_flip() +
    labs(x='', y='',
         title = 'COVID-19 Deaths in Simulated Patients with Diabetes',
         subtitle = 'Condition Prevalence (%) paired with COVID-19 and Diabetes',
         caption = 'Source: 1,000 Simulated Patient Deaths') +
    theme_minimal() +
    scale_x_discrete(labels = function(x) {str_wrap(x, width = 20)}) +
    scale_y_continuous(labels = function(y) {fmt_pct(y)},
                       limits = c(0, 0.4),
                       # expand scale adjust padding between y axis labels and plot
                       expand = expand_scale(mult = c(0.01, 0.15))) +
    theme(panel.grid.major.y = element_blank(),
          axis.text = element_text(size = 14),
          legend.position = 'none',
          # If you change title or subtitle, you may need to play the horiz adjust
          # to line up the title and subtitle over the plot (left justified)
          plot.title = element_text(size = 16, hjust = 2.5),
          plot.subtitle = element_text(size = 12, face = 'italic', hjust = -10.25),
          plot.caption = element_text(size = 12))

pdf("plots/gg_uc_sim_death_diab.pdf")
print(gg_uc_sim_death_diab)

dev.off()
# ggsave(filename = "plots/gg_us_rept_death.png",
#        plot = gg_uc_rep_death)
