
# load packages to access alternative R commands

library(data.table)

# load data from disk

run_data <- as.data.table(read.csv('NL2016_runexp.csv'))
run_data[, base_st := paste0(runner_1b, runner_2b, runner_3b)]
run_data[, post_st := paste0(post_runner_1b, post_runner_2b, post_runner_3b)]
re <- as.data.table(read.csv('run_exp.csv'))
re[, base_st := paste0(runner_1b, runner_2b, runner_3b)]

# merge RE into NL 2016 data

run_data <- merge(run_data, re[, list(outs, base_st, re)], by=c('outs','base_st'), all.x=TRUE)
run_data <- merge(run_data, re[, list(post_outs = outs, post_st = base_st, post_re = re)], by=c('post_outs','post_st'), all.x=TRUE)

# how do sac bunts change base states?

sac_bunt_st <- run_data[event_type == 'Sac Bunt', list(.N, re = mean(re), post_re = mean(post_re+event_runs)), by=c('base_st','outs','post_st','post_outs')]
sac_bunt_st[, sum(N*(post_re-re))/sum(N)]

# why might we have to dig a little deeper?
# 1. classification of sac bunts -- we need to know that there's not a bias in the statistics recorded
# 2. win expectancy changes instead of run expectancy changes
# 3. run expectancy will be different based on who is batting

# are there any situations where we should be more inclined to bunt?

sac_bunt_st[, list(sample = sum(N), re_change = sum(N*(post_re-re))/sum(N)), by=c('base_st','outs')][order(-re_change)]
            

