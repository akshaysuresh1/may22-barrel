# Sample coe to query a weather API for district-level data

import urllib.parse
import urllib.request
import json
import time
import matplotlib.pyplot as plt
import pandas as pd
import csv
import codecs
import geopandas as gpd
from os import chdir as cd

path_to_data = '../Raw_data/India_districts2020.shp'
gdf = gpd.read_file(path_to_data)


API_KEY="NLDNET8VQKEHUEDYTBL4RGQVC"
LOCATION="Nainital"
#LOCATION = gdf.distname[0] 
UNIT_GROUP="in"
DATE_START = "2022-02-01"
DATE_END = "2022-02-03"

requestUrl = "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/"
requestUrl = requestUrl + urllib.parse.quote_plus(LOCATION) + "/" + DATE_START + "/" + DATE_END +  "?" 
requestUrl = requestUrl+"?unitGroup="+UNIT_GROUP+"&include=days&key=" + API_KEY+ "&contentType=json"


try: 
  ResultBytes = urllib.request.urlopen(requestUrl)
  # Parse the results as CSV
  CSVText = csv.reader(codecs.iterdecode(ResultBytes, 'utf-8'))
  # Parse the results as JSON
  jsonData = json.loads(ResultBytes.read().decode('utf-8'))
except urllib.error.HTTPError  as e:
  ErrorInfo= e.read().decode() 
  print('Error code: ', e.code, ErrorInfo)
  sys.exit()
except  urllib.error.URLError as e:
  ErrorInfo= e.read().decode() 
  print('Error code: ', e.code,ErrorInfo)
  sys.exit()


jsonData
