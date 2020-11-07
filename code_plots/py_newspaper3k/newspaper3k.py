"""
Let's get the newspaper3k details for each dataset we're using

Date Created: Nov 6, 2020
"""

from newspaper import Article

def get_url_details(url: str) -> None:
    art = Article(url)

    art.download()
    art.parse()

    print(url[:50])
    print(f"Authors: {art.authors}")
    print(f"Publish date: {art.publish_date}")
    print(f"Article text: {art.text}")
    print(f"Length article text: {len(art.text)}\n")


url_1 = 'https://catalog.data.gov/dataset/conditions-contributing-to-deaths-involving-coronavirus-disease-2019-covid-19-by-age-group-7ee07'
get_url_details(url=url_1)


url_2 = 'https://catalog.data.gov/dataset/ah-provisional-diabetes-death-counts-2020-5e6ac'
get_url_details(url=url_2)
