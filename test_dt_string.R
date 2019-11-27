library(data.table)


dt <- data.table(a=letters[sample(1:2, 200, replace=T)],
                 b=LETTERS[1:5],
                 ID=sample(1:300, 200, replace=T))

dt_summary <- dt[order(b)][, .(ad = paste0(a, collapse = ""), count = .N), by = ID]


dt_summary2 <- dt_summary[, .(count_n = .N), .(ad, count)][, quota := count_n/sum(count_n), .(count)]


dt_summary2[count == 2, -c("count_n", "count")][order(-quota)]
