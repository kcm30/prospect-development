import pandas as pd
from lifelines import KaplanMeierFitter
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

df = pd.read_csv("../person_level.csv") # returns a Pandas DataFrame

# kaplan-meier for all prospects
kmf = KaplanMeierFitter()
T = df['duration']
E = df['event']
kmf.fit(T, event_observed=E) # more succiently, kmf.fit(T,E)
print(kmf.survival_function_)
ax = kmf.plot()
ax.set_title('Survival Function for All Prospects')
ax.set_xlabel('Season')
ax.get_figure().savefig("overall_prospects.png")

# km for 2007-2011 vs 2012-2016
seasons = df['first.yr']
ix = (seasons >= 2012)
kmf.fit(T[~ix], E[~ix], label='2007-2011')
ax2 = kmf.plot()
print(kmf.survival_function_)
kmf.fit(T[ix], E[ix], label='2012-2016')
kmf.plot(ax=ax2)
print(kmf.survival_function_)
ax2.set_title('Survival Function by First Season in MLS')
ax2.set_xlabel('Season')
ax2.get_figure().savefig("by_year.png")

# km by position
pos = df['position']
fwd = (pos == 'Forward')
mid = (pos == 'Midfielder')
defense = (pos == 'Defender')
kmf.fit(T[fwd], E[fwd], label = 'Fwds')
ax3 = kmf.plot()
kmf.fit(T[mid], E[mid], label = "Mids")
kmf.plot(ax=ax3)
kmf.fit(T[defense], E[defense], label = "Defs")
kmf.plot(ax=ax3)
ax3.set_title("Survival Function by Position")
ax3.set_xlabel("Season")
ax3.get_figure().savefig("by_pos.png")

# km by team
fig = matplotlib.pyplot.gcf()
fig.set_size_inches(20.5, 14.5)
teams = df['club'].unique()

for i,team in enumerate(teams):
    ax4 = plt.subplot(6,4,i+1)
    ix = df['club'] == team
    kmf.fit( T[ix], E[ix], label=team )
    kmf.plot(ax=ax4, legend=False)
    plt.title(team)
    plt.xlim(0,6)
    plt.ylim(0,1.1)
    #if i==0:
    #    plt.ylabel('Frac. in power after $n$ years')
plt.tight_layout()

fig.savefig('by_team.png')
ax4.get_figure().savefig("by_team.png")

