
# load packages to access alternative R commands

library(data.table)
library(ggplot2)

# load data from disk
# MLB data from 2010-2016

pitching <- as.data.table(read.csv('player_pitching.csv'))
lw <- as.data.table(read.csv('linear_weights.csv'))

# count BIP and find BABIP

pitching[, bip := ab - k - hr + sf]
pitching[, babip := (h - hr) / bip]

# link season data to next year data

pit_seasons <- merge(pitching, pitching[, list(pitcher_id, season = season-1, f_bip = bip, f_babip = babip)],
                      by=c('pitcher_id', 'season'))
pit_seasons[, harm_mean := 2/(1/bip + 1/f_bip)]

# very little consistency in pitcher BABIP!

qplot(data=pit_seasons[harm_mean >= 50], x=babip, y=f_babip) + geom_smooth(method='lm')
pit_seasons[harm_mean >= 50, cor(babip, f_babip)]

# so DIPS theory (defense-independent pitching statistics) asks how we can evaluate pitchers without
# depending on what happens on what happens when batters put the ball in play

# count BIP events

bip <- pitching[season == 2016, list(b1 = sum(h-b2-b3-hr), b2 = sum(b2), b3 = sum(b3), bip_out = sum(bip-h), bip = sum(bip))]

# what is RV of BIP?

bip_val <-  bip[, ( b1*lw[event == '1b', linear_weight]
                  + b2*lw[event == '2b', linear_weight]
                  + b3*lw[event == '3b', linear_weight]
                  + bip_out*lw[event == 'bip_out', linear_weight]) / bip ]

# subtract from linear weight values to get lw relative to bip

lw[event %in% c('hr','bb','hbp','k'), fip_constant := (linear_weight - bip_val)*9]

# usually round these to 13, 3, and -2

# FIP = (13*HR + 3*BB + 3*HBP - 2*K)/IP + const
# const = Lg ERA - (13*LgHR + 3*LgBB + 3*LgHBP - 2*LgK)/LgIP

