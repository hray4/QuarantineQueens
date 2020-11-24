"""
Cleaning and making plot of Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State.csv

url: https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Week-Ending-D/r8kw-7aab

Date Created: Nov 22, 2020

Time series animated plot of Death counts over time
"""

import pandas as pd
import os
from typing import List
import matplotlib.pyplot as plt
from matplotlib.dates import DateFormatter
import matplotlib.animation as animation
import time
from datetime import datetime


def update(num, x, y, line):
    line.set_data(x[:num+1], y[:num+1])
    # line.axes.axis([-140, 65, -19, 219])

    if num == len(x)-1:
        time.sleep(2)

    return line,


def plot_path(xs: List[object], ys: List[int], mon: List[int]) -> None:
    """ Plot path taken by person until intersection happens """
    # int_x = intersect_pt.x
    # int_y = intersect_pt.y

    with plt.style.context('ggplot'):
        fig, ax = plt.subplots(figsize=(10, 10))
        line, = ax.plot(xs, ys, color='b', alpha=.5)

        ax.xaxis.set_major_formatter(DateFormatter("%b"))
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda y, loc: f"{int(y):,}"))

        # ax.plot(0, 0, 'r*', alpha=.7)
        # plt.annotate(
        #     text="Start search at (0, 0)",
        #     xy=(2, 2),
        #     xytext=(10, 10),
        #     arrowprops={
        #         'facecolor': 'black',
        #         'shrink': 0.1,
        #         'headlength': 7
        #     }
        # )

        plt.title("US COVID-19 Death Counts by Week")

        plt.tick_params(
            axis='both',
            which='both',
            bottom=False,
            left=False)
        # plt.grid(False)

        anim = animation.FuncAnimation(fig, update, len(xs), fargs=[xs, ys, line],
                                       interval=125, blit=True)

        # ax.plot(int_x, int_y, 'rX', alpha=.7)
        # plt.annotate(
        #     text=f"First point visited twice ({int_x}, {int_y})",
        #     xy=(int_x - 2, int_y + 2),
        #     xytext=(int_x - 11, int_y + 11),
        #     arrowprops={
        #         'facecolor': 'black',
        #         'shrink': 0.1,
        #         'headlength': 7
        #     },
        #     ha='right'
        # )

        # anim.save('plots/weekly_deaths_animation.gif', writer='Pillow', fps=5)

        # plt.savefig('plots/weekly_deaths_animation.jpg')
        # plt.show()



if __name__ == '__main__':
    # Filenames and paths
    data_path = '../../src_data/'
    weekly_deaths_fn = 'Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State.csv'
    weekly_deaths_path = os.path.join(data_path, weekly_deaths_fn)

    daily_cases_fn = 'ECDC_COVID-19_cases_worldwide.csv'
    daily_cases_path = os.path.join(data_path, daily_cases_fn)

    # Program Begin
    weekly_deaths = pd.read_csv(weekly_deaths_path)

    removed_suppress_flag = weekly_deaths[weekly_deaths['Footnote'].isna()] \
        .copy().drop_duplicates().reset_index(drop=True)


    simp_cols_usa = removed_suppress_flag.loc[
        removed_suppress_flag['State'] == 'United States',
        ['Start week', 'COVID-19 Deaths']].copy()


    # Format columns
    simp_cols_usa['COVID-19 Deaths'] = simp_cols_usa['COVID-19 Deaths'].apply(int)
    simp_cols_usa['Death Month'] = simp_cols_usa['Start week'].apply(lambda dt: int(dt[:2]))
    simp_cols_usa['Start week'] = pd.to_datetime(simp_cols_usa['Start week'],
                                                 format='%m/%d/%Y')
    simp_cols_usa['Death Month Diff'] = simp_cols_usa['Death Month'].diff()

    xs = simp_cols_usa['Start week']
    ys = simp_cols_usa['COVID-19 Deaths']
    mon = simp_cols_usa['Death Month']
    mon_chg = simp_cols_usa[simp_cols_usa['Death Month Diff'] != 0]

    ## Daily Cases
    daily_cases = pd.read_csv(daily_cases_path)
    usa_daily_cases = daily_cases[daily_cases['countriesAndTerritories'] == 'United_States_of_America'].copy()
    usa_daily_cases['dateRep'] = pd.to_datetime(usa_daily_cases['dateRep'],
                                                format='%d/%m/%Y')

    # print(usa_daily_cases.head())

    simp_usa_daily_cases = usa_daily_cases[['dateRep', 'cases']].copy()
    print(simp_usa_daily_cases.head())


    plot_path(xs=xs, ys=ys, mon=mon)


