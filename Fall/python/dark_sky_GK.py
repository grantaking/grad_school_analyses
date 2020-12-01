import sys
import pandas as pd
sys.path.insert(1,'C:/Users/grant/Documents/MSA/Fall/python')
from forecastiopy import *

## API Key removed
api_key = 'xxx'

## Assignment to play around with dark sky and get the average min and max temperetures for a few cities
loc = [
    ["Anchorage, Alaska",61.2181, -149.9003],
    ["Buenos Aires, Argentina",-34.6037, -58.3816],
    ["Sao Jose dos Campos, Brazil",-23.2237, -45.9009],
    ["San Jose, Costa Rica",9.9281, -84.0907],
    ["Nanaimo, Canada",49.1659, -123.9401],
    ["Ningbo, China",29.8683, 121.5440],
    ["Giza, Egypt",30.0131, 31.2089],
    ["Mannheim, Germany",49.4875, 8.4660],
    ["Hyderabad, India",17.3850, 78.4867],
    ["Tehran, Iran",35.6892, 51.3890],
    ["Bishkek, Kyrgyzstan",42.8746, 74.5698],
    ["Riga, Latvia",56.9496, 24.1052],
    ["Quetta, Pakistan",30.1798, 66.9750],
    ["Warsaw, Poland",52.2297, 21.0122],
    ["Dhahran, Saudia Arabia",26.2361, 50.0393],
    ["Madrid, Spain",40.4168, -3.7038],
    ["Oldham, United Kingdom",53.5409, -2.1114],
]

data = {'City':[],
        'Min 1':[],'Max 1':[],
        'Min 2':[],'Max 2':[],
        'Min 3':[],'Max 3':[],
        'Min 4':[],'Max 4':[],
        'Min 5':[],'Max 5':[],
       }

for item in loc:
    city = item[0]
    lat = item[1]
    lon = item[2]
    
    weather = ForecastIO.ForecastIO(api_key, units=ForecastIO.ForecastIO.UNITS_SI, latitude=lat, longitude=lon)
    daily = FIODaily.FIODaily( weather )
    
    data['City'].append(city)
    
    for day in range(2,7):
        mincol = 'Min ' + str(day-1)
        maxcol = 'Max ' + str(day-1)
        
        val = daily.get(day)
        minval = val['temperatureMin']
        maxval = val['temperatureMax']
        
        data[mincol].append(minval)
        data[maxcol].append(maxval)
        
df = pd.DataFrame.from_dict(data)
df['Min Avg'] = df[['Min 1','Min 2','Min 3','Min 4','Min 5']].mean(axis=1).round(2)
df['Max Avg'] = df[['Max 1','Max 2','Max 3','Max 4','Max 5']].mean(axis=1).round(2)


df.to_csv(path_or_buf='temp.csv',index=False, float_format = '%.2f')
