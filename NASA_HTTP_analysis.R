library(ggplot2)
library(gdata)
library(iptools)
library(rjson)

raw_log <- read.table('../Projects/data/NASA_access_log_Jul95', fill=TRUE, nrows=100)

log <- subset(raw_log, select=c('V1', 'V4', 'V5', 'V6', 'V7', 'V8'))
  
log$V6 <- data.frame(do.call('rbind', strsplit(as.character(log$V6), " ", fixed=TRUE)))
log$V5 <- gsub(']', '', log$V5)
log$V4 <- gsub('\\[', '', log$V4)
  
names(log) <- c('host', 'timestamp', 'timezone', 'request', 'reply', 'reply_size')
names(log$request) <- c('method', 'path', 'status')
  
ips <- character()
 
for(host in as.character(log$host)) {
  response <- hostname_to_ip(host)
    
  if(response == "Not resolved") {
    host <- gsub('^[^.]*\\.',  '', host)
    response <- hostname_to_ip(host)
  }
  
  ips <- c(ips, response)
}
  
log$ip_address <- ips



# log$ip_address <- hostname_to_ip(as.character(log$host))