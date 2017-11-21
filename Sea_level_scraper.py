import string
import csv
import datetime
import time
import requests
import bs4
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
import string

url1 = u"http://www.psmsl.org/data/obtaining/"
r1 = requests.get(url1)
soup1 = BeautifulSoup(r1.text,'html.parser')
table = soup1.find_all('table')[0]
rows= table.find_all('tr')

df = pd.DataFrame(columns=range(0,9), index = range(0,1507)) # I know the size 

#Get the head row
i = 0
columns = rows[i].find_all('th')
for j in range(0,9):
        df.iat[i,j] = columns[j].get_text()
        
#Fill in table with body row
for i in range(1,1507):
    columns = rows[i].find_all('td')
    for j in range(0,9):
        df.iat[i,j] = columns[j].get_text()
    
df = df[1:]
df.columns = ['Station_name','ID',"lat",'long','GlOSS_ID','Country','Date','Coastline','Station']

df_USA = df[df['Country']=='USA']

base_url = 'http://www.psmsl.org/data/obtaining/rlr.annual.data/%s.rlrdata'

def Get_SL(ID):
    url = (base_url % ID).replace(" ","")
    r=requests.get(url)
    soup = BeautifulSoup(r.text,'html.parser')
    string =soup.get_text()
    new_string = []
    j=0
    for i in range(0,len(string)):
        if string[i] == ';':
            new_element = (''.join(string[j:i])).strip()
            j=i+1
            if '000\n' in new_element:
                new_element = new_element.split()[1]
            new_string.append(new_element)
    table = pd.DataFrame()
    year=[]
    sl=[]
    y_n=[]
    for i in np.arange(0,len(new_string),3):
        year.append(new_string[i])
        sl.append(new_string[i+1])
        y_n.append(new_string[i+2])
        
    table['Years'] = year
    table['Sea_level_mm'] = sl
    table['Y/N'] = y_n
  
    return table

table = pd.DataFrame()
for i in range(0,df_USA.shape[0]):
    ID = df_USA.iloc[i]['ID']
    table0 = Get_SL(ID)
    table0['ID'] = [(df_USA.iloc[i]['ID']).strip()]*table0.shape[0]
    table0['Lat'] = [(df_USA.iloc[i]['lat']).strip()]*table0.shape[0]
    table0['Long']=[(df_USA.iloc[i]['long']).strip()]*table0.shape[0]
    table=pd.concat([table,table0],join='outer',axis=0)

table.to_csv("SL_table.csv")
    
