import requests
from bs4 import BeautifulSoup
import pandas as pd
from datetime import datetime
import re
import sys
import io

# setting utf-8
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')


# function for get each page data
def get_page_data(url):
    response = requests.get(url, headers={'cookie': 'over18=1;'})
    response.encoding = 'utf-8'  # assign right coding
    soup = BeautifulSoup(response.text, 'html.parser')
    results = soup.select("div.title")
    page_data = []

    for item in results:
        title = item.text.strip()

        if(any(keyword in title for keyword in Travel_share)):

            # classification
            is_popspot_related = any(keyword in title for keyword in Tokyo_keywords)
            if is_popspot_related:
                classification = "Y"
            else:
                classification = "N"

            # scrap 
            a_tag = item.select_one("a")

            if a_tag:
                href = a_tag.get("href")
                href = "https://www.ptt.cc" + href
                print(href)

                # grab tag
                response = requests.get(href)
                soup = BeautifulSoup(response.text, 'lxml')

                # # catch time
                time_elements = soup.select('span.article-meta-value')
                post_datetime = "N/A"  
                if len(time_elements) > 3:
                    post_time = time_elements[3].text
                    if '.' in post_time:  # Check the data format if is YYYY.MM.DD
                        post_datetime = "NA"
                    else:
                        try:
                            post_datetime = datetime.strptime(post_time, '%a %b %d %H:%M:%S %Y')
                        except ValueError:
                            post_datetime = "Invalid date format"
                elif len(time_elements) == 3:
                    post_time = time_elements[2].text
                    if '.' in post_time:  # Check the data format if is YYYY.MM.DD
                        post_datetime = "NA"
                    else:
                        try:
                            post_datetime = datetime.strptime(post_time, '%a %b %d %H:%M:%S %Y')
                        except ValueError:
                            post_datetime = "Invalid date format"
    
            


                # catch push
                like = dislike = other = 0
                for i in soup.find_all('div', 'push'):
                    temp = i.find('span')
                    if temp is None:
                        break
                    else:
                        if temp.getText() == '推 ':
                            like += 1
                        elif temp.getText() == '噓 ':
                            dislike += 1
                        else:
                            other += 1
                total = like + dislike + other


                # catch content
                main_content = ""
                content_section = soup.find(id="main-content")
                if content_section:
                    # filter content 
                    for unwanted in content_section.find_all(['a', 'div', 'span']):
                        unwanted.decompose()

                    main_content = content_section.get_text(separator="\n", strip=True)


                # saving data
                page_data.append({'Title': title, 'Type': classification, 'Time': post_datetime,
                                    'Like': like, 'Dislike': dislike,
                                    'CommentNum': total, 'Content': main_content})
    return page_data


# keywords list
# https://statistics.jnto.go.jp/en/graph/#graph--inbound--prefecture--ranking

Tokyo_keywords = ["東京", "京都", "大阪"]
Travel_share = ["遊記"]

# setting page
start_url = "https://www.ptt.cc/bbs/Japan_Travel/index4649.html"
# total_pages = 732
total_pages = 3000
prev_post_datetime = 0

# scrapping data
all_data = []
for page in range(total_pages):
    page_data = get_page_data(start_url)
    #print(page_data)
    all_data.extend(page_data)
   # find "a" tag of last page
    prev_page_link = BeautifulSoup(requests.get(start_url).text, 'html.parser').find('a', class_='btn wide', string='‹ 上頁')
    print(prev_page_link)
   
    if prev_page_link:
        # find number part in "href" and minus one
        page_number = re.search(r'\d+', prev_page_link['href']).group()
        start_url = f"https://www.ptt.cc/bbs/Japan_Travel/index{int(page_number)}.html"
        #print(start_url, "1232123")
    else:
        print("Its the first one cannot get another one")
        break

# saving data to DataFrame
df = pd.DataFrame(all_data)

# writing DataFrame to Excel 
df.to_excel('data7.xlsx', index=False)
