
# load packages to access alternative R commands

library(data.table)
library(ggplot2)

# load data from disk
# MLB data from 2010-2016

team_hit <- as.data.table(read.csv('team_hitting.csv'))
pl_hit <- as.data.table(read.csv('player_hitting.csv'))


# link season data to the next season

pl_hit[, rbi_pa := rbi/pa]
pl_hit <- merge(pl_hit, pl_hit[, list(player_id, season = season-1, f_runs = runs, f_pa = pa,
                            f_ab = ab, f_h = h, f_b2 = b2, f_b3 = b3, f_hr = hr,
                            f_bb = bb, f_hbp = hbp, f_sf = sf, f_tb = tb, f_rbi = rbi,
                            f_avg = avg, f_obp = obp, f_slg = slg, f_rbi_pa = rbi / pa)],
                by=c('player_id','season'), all.x=TRUE)

pl_hit[, harm_mean := 2/(1/pa + 1/f_pa)]
pl_hit$is_pitcher <- as.logical(pl_hit$is_pitcher)

# plot batting average over subsequent seasons

# this is called a scatter plot
qplot(data=pl_hit, x=avg, y=f_avg) + geom_abline()
qplot(data=pl_hit[harm_mean >= 50], x=avg, y=f_avg) + geom_abline()
qplot(data=pl_hit[harm_mean >= 50], x=avg, y=f_avg, colour=is_pitcher) + geom_abline()

# correlation is a concept defining how related 2 variables are

pl_hit[, rand := runif(nrow(pl_hit))]
qplot(data=pl_hit[harm_mean >= 50], x=avg, y=avg + (rand-0.5)/50) + geom_abline() + coord_fixed()
qplot(data=pl_hit[harm_mean >= 50], x=avg, y=avg + (rand-0.5)/2) + geom_abline() + coord_fixed()

# cor checks linear relationship

pl_hit[, list(cor_self = cor(avg, avg), cor_neg = cor(avg, -avg), cor_scale = cor(avg, avg/8), cor_self_small_noise = cor(avg, avg+(rand-0.5)/50), cor_self_large_noise = cor(avg, avg+(rand-0.5)/2))]
pl_hit[, list(avg_obp = cor(avg, obp))]

# next season correlations

pl_hit[harm_mean >= 50, list(cor_avg = cor(avg, f_avg), cor_obp = cor(obp, f_obp), cor_slg = cor(slg, f_slg))]

qplot(data=pl_hit[harm_mean >= 50][{!is_pitcher}], x=avg, y=f_avg) + geom_abline() + geom_smooth(method='lm')
qplot(data=pl_hit[harm_mean >= 50][{!is_pitcher}], x=obp, y=f_obp) + geom_abline() + geom_smooth(method='lm')



# on team level

qplot(data=team_hit, x=avg, y=runs) + geom_smooth(method='lm')
qplot(data=team_hit, x=obp, y=runs) + geom_smooth(method='lm')

pl_hit[, list(cor_avg = cor(avg, runs), cor_obp = cor(obp, runs), cor_slg = cor(slg, runs))]

# what about rbi? this is another stat that analysts say is important

qplot(data=pl_hit[harm_mean >= 50], x=rbi, y=f_rbi) + geom_abline()
qplot(data=pl_hit[harm_mean >= 250], x=rbi_pa, y=f_rbi_pa) + geom_abline()
qplot(data=pl_hit[harm_mean >= 250], x=slg, y=rbi_pa) + geom_abline()

pl_hit[harm_mean >= 50, list(cor_rbi = cor(rbi_pa, f_rbi_pa), cor_slg_rbi = cor(slg, rbi_pa), cor_slg_frbi = cor(slg, f_rbi_pa))]



