"""
Cleaning and making plots of ECDC_COVID-19_cases_worldwide.csv

url: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

Date Created: Nov 27, 2020

Time series animated plot of Cases and Death per 100k counts over time
from European CDC site
"""

import pandas as pd
import os
from typing import List, Dict
import matplotlib.pyplot as plt
from matplotlib.dates import DateFormatter
import matplotlib.animation as animation
import time
from datetime import datetime


def fmt_x_per_100k(df: pd.DataFrame) -> pd.DataFrame:
    """ Divide cases or deaths by us population * 100_000 to give
        format of cases/100k or deaths/100k result """
    dfc = df.copy()

    dfc['cases'] = dfc['cases'] / dfc['popData2019'] * 100_000
    dfc['deaths'] = dfc['deaths'] / dfc['popData2019'] * 100_000

    # Convert from daily to weekly date by aggregating by each week
    weekly_data = dfc.resample('W-Mon', label='right',
                               closed='right', on='dateRep') \
        .sum().reset_index().sort_values(by='dateRep')

    return weekly_data.iloc[:-1]


def get_plot_vars(df: pd.DataFrame, yVar: str) -> Dict[str, List[object]]:
    """ Split df into plotting variables """
    vars_dict = {}
    vars_dict['xs'] = df['dateRep']
    vars_dict['ys'] = df[yVar]

    return vars_dict


def update(num, x, y, line):
    line.set_data(x[:num+1], y[:num+1])

    if num == len(x)-1:
        time.sleep(2)

    return line,


def plot_path(vars_dict: Dict[str, object], plt_title: str, linecolor: str,
              gif_fn: str, jpg_fn: str) -> None:
    """ Plot path taken by person until intersection happens """

    xs = vars_dict['xs']
    ys = vars_dict['ys']

    with plt.style.context('ggplot'):
        fig, ax = plt.subplots(figsize=(10, 10))
        line, = ax.plot(xs, ys, color=linecolor, alpha=.5, linewidth=5)

        ax.xaxis.set_major_formatter(DateFormatter("%b"))
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda y, loc: f"{int(y):,}"))

        # parameters = {'axes.titlesize': 20,
                      # 'xtick.labelsize': 18,
                      # 'ytick.labelsize': 18}
        # plt.rcParams.update(parameters)
        ax.tick_params(axis='both', which='major', labelsize=18)

        plt.title(plt_title, fontsize = 20)

        plt.tick_params(
            axis='both',
            which='both',
            bottom=False,
            left=False)

        anim = animation.FuncAnimation(fig, update, len(xs), fargs=[xs, ys, line],
                                       interval=135, blit=True)

        anim.save(gif_fn, writer='Pillow', fps=5)

        plt.savefig(jpg_fn)
        # plt.show()



if __name__ == '__main__':
    os.chdir(os.path.dirname(__file__))

    # Filenames and paths
    data_path = '../../src_data/'

    daily_cases_fn = 'ECDC_COVID-19_cases_worldwide_20201127.csv'
    daily_cases_path = os.path.join(data_path, daily_cases_fn)

    # Program Begin
    ## Daily Cases
    daily_cases = pd.read_csv(daily_cases_path)
    usa_daily_cases = daily_cases[daily_cases['countriesAndTerritories'] == 'United_States_of_America'].copy()
    usa_daily_cases['dateRep'] = pd.to_datetime(usa_daily_cases['dateRep'],
                                                format='%d/%m/%Y')
    usa_daily_cases = usa_daily_cases[usa_daily_cases['dateRep'] >= '03/01/2020']

    cases_deaths = usa_daily_cases[['dateRep',
                                    'cases',
                                    'deaths',
                                    'popData2019']].copy()

    per_100k_fmted = fmt_x_per_100k(df=cases_deaths)
    cases = per_100k_fmted[['dateRep', 'cases']].copy()

    plot_path(vars_dict=get_plot_vars(df=cases, yVar='cases'),
              plt_title='US COVID-19 Weekly Cases Per 100,000 Persons, Mar-Nov 2020',
              linecolor='red',
              gif_fn='plots/cases_animation.gif',
              jpg_fn='plots/cases_animation.jpg')

    ## Daily Deaths
    deaths = per_100k_fmted[['dateRep', 'deaths']].copy()

    plot_path(vars_dict=get_plot_vars(df=deaths, yVar='deaths'),
              plt_title='US COVID-19 Weekly Deaths Per 100,000 Persons, Mar-Nov 2020',
              linecolor='black',
              gif_fn='plots/deaths_animation.gif',
              jpg_fn='plots/deaths_animation.jpg')


