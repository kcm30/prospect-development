from bs4 import BeautifulSoup
import requests

# grab player ids used in mls website URLs
def grab_ids(season, page):
    URL = "https://www.mlssoccer.com/stats/season?page=" + page + "&year="+season+"&group=g&sort=desc&order=MINS"
    result = requests.get(URL)
    content = result.content
    soup = BeautifulSoup(content, 'html.parser')

    player_links = soup.find_all('td', {'data-title':'Player'})
    mls_ids = [player.find('a')['href'].split("/",2)[2] for player in player_links]
    return mls_ids

# grab player stats for a given mls website id
def grab_player_stats(mls_id):
    URL = "https://www.mlssoccer.com/players/" + mls_id
    result = requests.get(URL)
    content = result.content
    soup = BeautifulSoup(content, 'html.parser')

    # check to make sure there is data to grab
    stats_table_list = soup.find_all('tbody')
    if len(stats_table_list) > 0:
        season_table = stats_table_list[0]
    else:
        print("Invalid ID: " + mls_id)
        return;
    
    ### Grab all the basic player info ######
    name_list = soup.find_all('div', 'name')
    if len(name_list) >= 1:
        name = name_list[0].text.split("\n",1)[1]
    else:
        name = mls_id

    age_list = soup.find_all('div', 'age')
    if len(age_list) >= 1:
        birthday = age_list[0].text.split("(", 1)[1][:-1]
    else:
        birthday = "NA"

    position_list = soup.find_all('span', 'position')
    if len(position_list) >= 1:
        position = position_list[0].string
    else:
        position = 'NA'

    size_list = soup.find_all('span', 'stat')
    if len(size_list) == 2:
        height = size_list[0].text
        weight = size_list[1].text
    else:
        height = 'NA'
        weight = 'NA'
    ##############################################
    player_info = [mls_id, name, position, height, weight, birthday]

    # parse season by season data
    rows = []
    for season in season_table.find_all('tr')[:-1]:
        if season.td.text != '2017':
            rows.append(player_info + [stat.text for stat in season.find_all('td')])

    return rows;
