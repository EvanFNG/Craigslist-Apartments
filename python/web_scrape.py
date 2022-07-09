import requests
import bs4
import numpy as np
import random
import time
import warnings
import pandas as pd

regions = ['austin', 'phoenix']

def scrape(region: str) -> None:

  response = requests.get(f'https://{region}.craigslist.org/search/apa?hasPic=1&min_price=&max_price=&availabilityMode=0&sale_date=all+dates')
  
  html_soup = bs4.BeautifulSoup(response.text, 'html.parser')
  
  posts = html_soup.find_all('li', class_ = 'result-row')
  
  results_num = html_soup.find('div', class_ = 'search-legend')
  results_total = int(results_num.find('span', class_ = 'totalcount').text)
  
  pages = np.arange(0, results_total + 1, 120)
  
  post_time = []
  post_hood = []
  post_title_texts = []
  n_beds = []
  sqft = []
  url = []
  price = []
  
  for page in pages:
  
      response = requests.get(f'https://{region}.craigslist.org/search/apa?s={page}&hasPic=1&availabilityMode=0')
  
      time.sleep(random.randint(1, 5))
  
      if response.status_code != 200:
          warnings.warn(f'Request: {response}; Status code: {response.status_code}')
      
      page_html = bs4.BeautifulSoup(response.text, 'html.parser')
  
      posts = page_html.find_all('li', class_ = 'result-row')
  
      for post in posts:
          if post.find('span', class_ = 'result-hood') is not None:
  
              # post date
              post_datetime = post.find('time', class_ = 'result-date')['datetime']
              post_time.append(post_datetime)
  
              # neighborhood
              hood = post.find('span', class_ = 'result-hood').text
              post_hood.append(hood)
  
              # title
              title = post.find('a', class_ = 'result-title hdrlnk')
              text = title.text
              post_title_texts.append(text)
  
              # URL
              p_link = title['href']
              url.append(p_link)
  
              # price
              p_price = int(post.a.text.strip().replace('$', '').replace(',', ''))
              price.append(p_price)
  
              if post.find('span', class_ = 'housing') is not None:
  
                  # if the first element is accidentally square footage
                  if 'ft2' in post.find('span', class_ = 'housing').text.split()[0]:
  
                      # make bedroom nan
                      bed_count = np.nan
                      n_beds.append(bed_count)
  
                      # make sqft first element
                      p_sqft = int(post.find('span', class_ = 'housing').text.split()[0][:-3])
                      sqft.append(p_sqft)
                  
                  elif len(post.find('span', class_ = 'housing').text.split()) > 2:
                      
                      bed_count = post.find('span', class_ = 'housing').text.replace('br', '').split()[0]
                      n_beds.append(bed_count)
  
                      p_sqft = int(post.find('span', class_ = 'housing').text.split()[2][:-3])
                      sqft.append(p_sqft)
  
                  elif len(post.find('span', class_ = 'housing').text.split()) == 2:
  
                      bed_count = post.find('span', class_ = 'housing').text.replace('br', '').split()[0]
                      n_beds.append(bed_count)
  
                      p_sqft = np.nan
                      sqft.append(p_sqft)
  
                  else:
  
                      bed_count = np.nan
                      n_beds.append(bed_count)
  
                      p_sqft = np.nan
                      sqft.append(p_sqft)
              else:
  
                  bed_count = np.nan
                  n_beds.append(bed_count)
  
                  p_sqft = np.nan
                  sqft.append(p_sqft)
  
  print('\n')
  
  print(f'{region} scrape complete.')
   
  df = pd.DataFrame(
      {
          'date_time': post_time,
          'town': post_hood,
          'title': post_title_texts,
          'price': price,
          'beds': n_beds,
          'sqft': sqft,
          'url': url
      }
  )
  
  df.to_csv(f'data/{region}_craigslist.csv', index = False)

for i in regions:
  scrape(i)
