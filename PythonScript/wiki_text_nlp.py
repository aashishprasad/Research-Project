#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import nltk
from bs4 import BeautifulSoup
import string
from nltk.corpus import stopwords
from nltk.tokenize import RegexpTokenizer
from nltk.stem import WordNetLemmatizer
from nltk.stem.porter import PorterStemmer


# In[168]:


#Loading Data
df = pd.read_csv("D:/DA/Semester_3/Research Project/Dataset/game_data.csv")
df.shape


# In[166]:


#Initializing variables
#count = 0
a = 0
#creating an empty array
arr = []
for sentence in df['wiki_tbl']:
    #if loop to check if the data is not a string i.e. a null value
    if type(sentence) != str:
        #Drop the row if it is null
        df = df.drop([a],axis=0)
        #print(a)
        #appending the index which has been dropped to the arr variable
        arr.append(a)
        a = a + 1
        #after dropping the row skip the whole loop
        continue
#df = df.dropna()


# In[169]:


print(df['wiki_tbl'].head(5))
text = df['wiki_tbl']


# In[170]:


#text = df['game_text'].str.split("//", n=10, expand=True)
text.head()


# In[172]:


import re 
for i in range(len(df['wiki_tbl'])):
    if pd.notnull(df['wiki_tbl'][i]) : df['wiki_tbl'][i] = re.sub("[\(\[].*?[\)\]]", "", df['wiki_tbl'][i])


# In[173]:


def remove_html(text):
    soup = BeautifulSoup(text, 'lxml')
    html_free = soup.get_text()
    return html_free


# In[174]:


def remove_punctuation(text):
    no_punct = "".join([c for c in text if c not in string.punctuation])
    return no_punct


# In[175]:


#def remove_punctuation(text):
#    if pd.notnull(text) : no_punct = "".join([c for c in text if c not in string.punctuation])
#        return no_punct
#    else:
#        return None


# In[176]:


df['wiki_tbl'] = df['wiki_tbl'].apply(lambda x: remove_punctuation(x))
df['wiki_tbl'].head()


# In[177]:


#instantiate tokenizer
tokenizer = RegexpTokenizer(r'\w+')


# In[178]:


df['wiki_tbl'] = df['wiki_tbl'].apply(lambda x: tokenizer.tokenize(x.lower()))
df['wiki_tbl'].head(20)


# In[179]:


def remove_stopwords(text):
    words = [w for w in text if w not in stopwords.words('english')]
    return words


# In[180]:


df['wiki_tbl'] = df['wiki_tbl'].apply(lambda x: remove_stopwords(x))
df['wiki_tbl'].head(10)


# In[184]:


#instantiate stemmer
stemmer = PorterStemmer()


# In[185]:


def word_stemmer(text):
    stem_text = " ".join([stemmer.stem(i) for i in text])
    return stem_text


# In[186]:


df['wiki_tbl'] = df['wiki_tbl'].apply(lambda x: word_stemmer(x))


# In[187]:


df['wiki_tbl']


# In[188]:


df.shape


# In[189]:


df.to_csv("D:/DA/Semester_3/Research Project/Dataset/wiki_text_nlp.csv")

