library(tsibble)

# seq() and arithmetic
wk1 <- yearweek("2017 W50")
wk2 <- yearweek("2018 W12")
seq(from = wk1, to = wk2, by = 2)
wk1 + 0:9

# display formats
format(c(wk1, wk2), format = "%D")
is_53weeks(2015:2016)
is_53weeks(1969)
is_53weeks(1969, week_start = 7)