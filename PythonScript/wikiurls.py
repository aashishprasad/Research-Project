#!/usr/bin/env python
# coding: utf-8

# In[37]:


import pandas as pd


# In[38]:


data = pd.read_csv("D:/DA/Semester_3/Research Project/Dataset/ign_video_games.csv")


# In[40]:


pip install wikipedia-api


# In[ ]:


pip install --default-timeout=100 future


# In[41]:


import wikipediaapi
wiki_wiki = wikipediaapi.Wikipedia('en')


# In[53]:


from pandas import DataFrame


# In[242]:


ctr = 0
ctr1 = 0
ctr2 = 0
flag = 0
for x in data['game_name']:
    ctr= ctr + 1
    print(ctr)
    if(ctr == 1048 or ctr == 1574):
        x = x.replace(":", "")
        print(x)
    elif ctr == 7482:
        x = x.replace("-h", "-H")
        print(x)

            
    page_py = wiki_wiki.page(x)
    if page_py.exists() : 
        ctr1= ctr1 + 1
        #print(page_py.fullurl)
        d = {'url': [page_py.fullurl]}
        df = pd.DataFrame(data=d)
        if flag == 0: 
            url_df = df
            flag = flag+1
            #print(ctr)
        else: 
            url_df = url_df.append(df)
            #print(ctr)
    else: 
        ctr2= ctr2 + 1
        d = {'url': ['NA']}
        df = pd.DataFrame(data=d)
        url_df = url_df.append(df)
        #print('NA')


# In[166]:


#print(page_py.canonicalurl)


# In[285]:


data.reset_index(drop=True, inplace=True)
url_df.reset_index(drop=True, inplace=True)
final_df = pd.concat([data['game_name'].reset_index(drop=True), url_df], axis=1)


# In[300]:


#final_df
data = final_df.to_csv("D:/DA/Semester_3/Research Project/Dataset/wiki_urls_video_games.csv")


# In[ ]:




