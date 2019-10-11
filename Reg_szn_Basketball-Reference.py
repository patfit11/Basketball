#######################################################################################################################
# This code is for webscaping from Basketball-Reference.com
# Created October 8, 2019
# Edited: October 10, 2019
#######################################################################################################################

# import the scraping tools
from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd
import os


#### NON-RECURSIVE #### but effective
# set the path
path="/Users/m/Desktop/Basketball/Projects/Seasons/2009-2010/Regular Season"
os.chdir(path)

# give the webpage to be scaped (Basketball Reference)
base_url = 'https://www.basketball-reference.com/leagues/NBA_2010_per_game.html'

# opening up connections, grabbing the page
my_html = urlopen(base_url)        
page_html = my_html.read() # off-load the content to a variable   
my_html.close() # close
# html parsing
soup = BeautifulSoup(page_html)

# find all the column headers
column_names = soup.findAll('tr', limit=2) # take this info from <div id="all_total_stats"...
# Extract the text we need into a list
headers = [th.getText() for th in column_names[0].findAll('th')]
headers = headers[1:] # remove the first column (ranks)

# Extract data from players rows
rows = soup.findAll('tr')[1:] # avoid the first header row
player_stats = [[td.getText() for td in rows[i].findAll('td')]
		for i in range(len(rows))]
stats = pd.DataFrame(player_stats, columns = headers)

stats.to_csv('Player__Per_Game09-10.csv')






#### NEED TO FIGURE OUT HOW TO TURN THESE INTO LOOPS ####
# cwd=os.getcwd()
# cwd

path="/Users/m/Desktop/Basketball/Projects/Seasons/2013-2014/Regular Season"
os.chdir(path)


# multiple NBA seasons at a time
year = ['2014']
pages = ['totals', 'per_game', 'per_minute', 'per_poss', 'advanced'] # stats to be culled
web_url = []
urls = []
soups = []
for i in year:	
	for j in pages:
		seasons = str("NBA_" + i + "_" + j)
		web_url.append(seasons)
		for s in range(len(web_url)):
			base_url = str("https://www.basketball-reference.com/leagues/" + web_url[s] + ".html")
			urls.append(base_url)
			for x in range(len(urls)):
				my_html = urlopen(urls[x]) #  opening up connections, grabbing the page     
				page_html = my_html.read() # off-load the content to a variable   
				my_html.close() # close
				soup = BeautifulSoup(page_html) # html parsing
				soups.append(soup)

				for u in range(len(soups)):
					# find all the column headers
					column_names = soup.findAll('tr', limit=2) # take this info from <div id="all_total_stats"...
					# Extract the text we need into a list
					headers = [th.getText() for th in column_names[0].findAll('th')]
					headers = headers[1:] # remove the first column (ranks)

					# Extract data from players rows
					rows = soup.findAll('tr')[1:] # avoid the first header row -> this is where the errors start
					player_stats = [[td.getText() for td in rows[i].findAll('td')]
						for x in range(len(rows))]
					stats = pd.DataFrame(player_stats, columns = headers)
					for n in range(len(pages)):
						filename = str("Player_" + pages[n])
						for m in range(len(filename)):
							stats.to_csv("filename" + m + ".csv")
















