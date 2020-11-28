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


# url_1 = 'https://data.cdc.gov/NCHS/Conditions-contributing-to-deaths-involving-corona/hk9y-quqm'
# get_url_details(url=url_1)


# url_2 = 'https://data.cdc.gov/NCHS/AH-Provisional-Diabetes-Death-Counts-2020/qdcb-uzft'
# get_url_details(url=url_2)

url_3 = 'https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/evidence-table.html'
get_url_details(url=url_3)
