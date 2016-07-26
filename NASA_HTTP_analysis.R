library(ggplot2)
library(gdata)
library(iptools)

raw_log <- read.table('../Projects/data/NASA_access_log_Jul95', fill=TRUE, nrows=100)
log <- subset(raw_log, select=c('V1', 'V4', 'V5', 'V6', 'V7', 'V8'))

log$V6 <- data.frame(do.call('rbind', strsplit(as.character(log$V6), " ", fixed=TRUE)))
log$V5 <- gsub(']', '', log$V5)
log$V4 <- gsub('\\[', '', log$V4)

names(log) <- c('host', 'timestamp', 'timezone', 'request', 'reply', 'reply_size')
names(log$request) <- c('method', 'path', 'status')

log$ip_address <- hostname_to_ip(as.character(log$host))