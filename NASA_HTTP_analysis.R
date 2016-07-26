#
# Analysis of HTTP Traffic on NASA Server from JULY 1 -JULY 31 1995
#
# Citations:
#
# (For ggmap)
# D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161. URL
# http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
#


library(ggplot2)
library(ggmap)
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
    
    if(response == "Not resolved") {
      response = NA
    } else if (length(response[[1]]) > 1) {
      response = response[[1]][1]
    }
  }
  
  ips <- c(ips, response)
}
  
log$ip_address <- ips

freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    if(is.na(ip)) {
      return(NULL)
    } else {
      # a single IP address
      require(rjson)
      url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
      ret <- fromJSON(readLines(url, warn=FALSE))
      if (format == 'dataframe')
        ret <- data.frame(t(unlist(ret)))
      return(ret) 
    }
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}   

create.map <- function(lon, lat) {

  df <- data.frame(c(1:length(lat)))
  df$lat <- lat
  df$lon <- lon
    
  thamap <- get_map(location=c(lon=mean(as.numeric(as.vector(df$lon))), lat=mean(as.numeric(as.vector(df$lat)))), zoom=3, maptype="satellite", scale=2)
  
  ggmap(thamap) + geom_point(data=df, aes(x=as.numeric(as.vector(lon)), y=as.numeric(as.vector(lat)), fill="red", alpha=0.8), size=3, shape=21) + guides(fill=FALSE, alpha=FALSE, size=FALSE)
}

# log$ip_address <- hostname_to_ip(as.character(log$host))