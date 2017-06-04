import csv
import time
from scraping_functions import grab_ids
from scraping_functions import grab_player_stats

season_page = {'2007':14, '2008':16, '2009':14, '2010':16, '2011':19, '2012':21, '2013':21, '2014':21, '2015':21, '2016':21}

id_set = set()
# grab every id for a player who played a season between 2007 and 2016
for season in season_page.keys():
    for page in range(0, season_page[season]+1):
        id_set.update(grab_ids(season, str(page)))

# write the ids to csv
with open('mls_ids.csv', 'w') as f:
    writer = csv.writer(f)
    for val in list(id_set):
        writer.writerow([val])

with open('mls_ids.csv', 'r') as f:
    reader = csv.reader(f)
    id_list = list(reader)

HEADERS = ['mls_id', 'name', 'position', 'height', 'weight', 'birthday', 'year', 'club', 'gp', 'gs', 'g', 'mins', 'a', 'shts', 'sog', 'fc', 'off', 'y', 'r']
# write each season to csv
start = time.time()
with open('player_season_data.csv', 'w') as fi:
    writer = csv.writer(fi)
    writer.writerow(HEADERS)
    for player in id_list:
        print(player)
        stats = grab_player_stats(player[0])
        for row in stats:
            writer.writerow(row)

end = time.time()
print(end-start)
