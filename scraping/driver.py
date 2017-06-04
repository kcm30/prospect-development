from scraping_functions import grab_ids
from scraping_functions import grab_player_stats


grab_ids('2008', '4')

HEADERS = ['mls_id', 'name', 'position', 'height', 'weight', 'birthday', 'year', 'club', 'gp', 'gs', 'g', 'mins', 'a', 'shts', 'sog', 'fc', 'off', 'y', 'r']

grab_player_stats('matt-polster')
