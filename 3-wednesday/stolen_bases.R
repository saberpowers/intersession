
# load packages to access alternative R commands

library(data.table)

# load data from disk

run_data <- as.data.table(read.csv('NL2016_runners_going.csv'))
run_data[, base_st := paste0(runner_1b, runner_2b, runner_3b)]
run_data[, post_st := paste0(post_runner_1b, post_runner_2b, post_runner_3b)]
re <- as.data.table(read.csv('run_exp.csv'))
re[, base_st := paste0(runner_1b, runner_2b, runner_3b)]

# merge RE into NL 2016 data

run_data <- merge(run_data, re[, list(outs, base_st, re)], by=c('outs','base_st'), all.x=TRUE)
run_data <- merge(run_data, re[, list(post_outs = outs, post_st = base_st, post_re = re)], by=c('post_outs','post_st'), all.x=TRUE)
run_data[post_outs == 3, post_re := 0]

# how do stolen base attempts change the game state?

sb_st <- run_data[is_sb + is_cs > 0, list(.N, re = mean(re), post_re = mean(post_re + event_runs)), by='event_type']
sb_st[event_type %in% c('Stolen Base 2B','Caught Stealing 2B'), sum(N*(post_re-re))/sum(N)]

# What SB considerations is this analysis leaving out?
# 1. Score! Dave Roberts steal. Very fundamental here.
# 2. Hit+runs mixed in with SB attempts
# 3. batter isn't quite as important here, because we're not necessarily
# biased, but there could be considerations based on expected production

# how would we calculate a break even rate for this decision?

sb_st[event_type %in% c('Stolen Base 2B','Caught Stealing 2B'), list(event_type, value = post_re - re)]
# p * success_value + (1-p) * failure_value >= 0
# p * (success_value - failure_value) >= -failure_value
# p >= 0.404 / (0.166 + 0.404) >= .70877 ~ 70%

# but even more directly, we can just refer to the RE tables