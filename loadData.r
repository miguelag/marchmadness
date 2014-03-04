
setwd('/server/Temp/marchmadness/')

reg = read.csv('data/regular_season_results.csv')
teams = read.csv('data/teams.csv')
seasons = read.csv('data/seasons.csv')
results = read.csv('data/tourney_results.csv')
slots = read.csv('data/tourney_slots.csv')
seeds = read.csv('data/tourney_seeds.csv')

sample = read.csv('data/sample_submission.csv')
# What do the letters represent?
table(sapply(sample$id,function(x) substr(x, 1, 1)))
# The sample file has the lower numbered team first for each matchup
table(sapply(sample$id,function(x) substr(x, 7, 9)>substr(x,3,5)))

# Regular season results
# Get the stats from when each team was the winner
team_wins = with(reg,data.frame(table(wteam,wloc)))
names(team_wins) = c('id','wloc','wins')
team_wins$winsHome = (team_wins$wloc=='H')
team_wins$winsHome[team_wins$winsHome] <- team_wins$wins[team_wins$winsHome]*0.6
team_wins$winsAway = (team_wins$wloc=='A')
team_wins$winsAway[team_wins$winsAway] <- team_wins$wins[team_wins$winsAway]*1.4
team_wins$winsNeutral = (team_wins$wloc=='N')
team_wins$winsNeutral[team_wins$winsNeutral] <- team_wins$wins[team_wins$winsNeutral]*1.0
team_wins$winsWP = with(team_wins,winsHome+winsAway+winsNeutral)
# Get the stats from when each team was the loser
team_losses = with(reg,data.frame(table(lteam,wloc)))
names(team_losses) = c('id','wloc','losses')
team_losses$lossesHome = (team_losses$wloc=='A')
team_losses$lossesHome[team_losses$lossesHome] <- team_losses$losses[team_losses$lossesHome]*0.6
team_losses$lossesAway = (team_losses$wloc=='H')
team_losses$lossesAway[team_losses$lossesAway] <- team_losses$losses[team_losses$lossesAway]*1.4
team_losses$lossesNeutral = (team_losses$wloc=='N')
team_losses$lossesNeutral[team_losses$lossesNeutral] <- team_losses$losses[team_losses$lossesNeutral]*1.0
team_losses$lossesWP = with(team_losses,lossesHome+lossesAway+lossesNeutral)

# Merge the two
reg_team = merge(team_wins,team_losses,by='id')
# WP is a variable used to calculate RPI
# http://www.kaggle.com/c/march-machine-learning-mania/forums/t/6769/calculating-rpi-a-detailed-example
reg_team$WP = with(reg_team,winsWP/(winsWP+lossesWP))
reg_team$wp = with(reg_team,wins/(wins+losses))

head(reg)
with(reg, table(wteam, lteam))
