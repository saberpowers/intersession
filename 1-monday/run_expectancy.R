
# load packages to access alternative R commands

library(data.table)

# load data from disk

run_data <- as.data.table(read.csv('NL2016_scoring.csv'))
run_data[, base_st := paste0(runner_1b, runner_2b, runner_3b)]

# summarize by base-out state

summary <- run_data[, list(count = .N, runs = sum(batter_scores, runner_1b_scores, runner_2b_scores, runner_3b_scores, future_batter_runs)), by=c('outs','base_st')]

# runs / count; how many times is enough?

summary[, list(outs, base_st, count, re = runs/count)][order(-re)]

# which parks are contributing to 0 out, 001 situation?

run_data[base_st == '001'][outs == 0, .N, by='home_team'][order(-N)]

# do SF and CIN differ in how states translate to runs?

summary_sf_cin <- run_data[home_team %in% c('SF','CIN'), list(count = .N, runs = sum(batter_scores, runner_1b_scores, runner_2b_scores, runner_3b_scores, future_batter_runs)), by=c('outs','base_st','home_team')]
summary_sf_cin[outs == 0, list(home_team, outs, base_st, count, re=runs/count)][order(-re)]

# 0 out, 001 state has a 2.11 RE in CIN and a 1.38 RE in SF!!

