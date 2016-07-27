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


main <- function() {
  n <- getrownum()

  raw_log <- read.table('../Projects/data/NASA_access_log_Jul95', fill=TRUE, nrows=n)
  
  clean_log <- clean(raw_log)
  
  clean_log$ip_address <- findips(clean_log$host, n)
  
  coords <- freegeoip(clean_log$ip_address, n)
  
  return(coords)
}


getrownum <- function() {
  n <- readline(prompt="Number of rows to compute: ")
  n <- as.integer(n)
  if(n > 20000 || n < 0 || is.na(n)) {
    print("Please enter an integer in the range of 0 to 20,000")
    n <- getrownum()
  }
  return(n)
}

clean <- function(rawlog) {
  
  # remove unnecessary columns of hyphens (V2 & V3) and rename columns
  log <- subset(rawlog, select=c('V1', 'V4', 'V5', 'V6', 'V7', 'V8'))
  names(log) <- c('host', 'timestamp', 'timezone', 'request', 'reply', 'reply_size')
  
  
  # split the request string into a sub data frame  
  log$request <- data.frame(do.call('rbind', strsplit(as.character(log$request), " ", fixed=TRUE)))
  names(log$request) <- c('method', 'path', 'status')
  
  
  # remove trailing and leading brackets in colums
  log$timezone <- gsub(']', '', log$timezone)
  log$timestamp <- gsub('\\[', '', log$timestamp)
  
  return(log)
}


findips <- function(hosts, n) {
  
  # vector for storing all found ips  
  ips <- character()
  
  # list of all ips and their hosts that were already found (memoization)
  found.ips <- list()
  
  print('Retrieving IPs')
  
  
  # Creates Progress bar for monitoring retrieval of ip addresses from hosts
  pb <- txtProgressBar(1, n, style=3)
  count <- 0
  
  
  # Finds the ip addresses that correspond to the host
  # if two addresses are founds, the first is taken,
  # if none are found a NULL value is added
  for(host in as.character(hosts)) {
    if(host %in% names(found.ips)) {
      response <- found.ips[host]
      
      ips <- c(ips, response)
    } else {
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
      
      if(length(response[[1]]) > 1) {
        response = response[[1]][1]
      }
      
      ips <- c(ips, response)
      found.ips[host] <- response
    }
    
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
  return(ips)
}

freegeoip <- function(ips, n)
{
  print("\nRetrieving Geo Coordinates")
  pb <- txtProgressBar(1, n, style=3)

  count <- 0

  lats <- character()
  lons <- character()
  
  
  for (ip in ips) {
    if (is.na(ip)) {
      lats <- c(lats, NULL)
      lons <- c(lons, NULL)
    } else {
      require(rjson)
      url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
      ret <- fromJSON(readLines(url, warn=FALSE))
      ret <- data.frame(t(unlist(ret)))
      lats <- append(lats, as.vector(ret$latitude[[1]]))
      lons <- c(lons, as.vector(ret$longitude[[1]]))
    }
    
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
  
  coords <- data.frame(c(1:length(lats)))
  coords$latitude <- lats
  coords$longitude <- lons
    
  return(coords)
}   

getloc <- function() {
  loc <- readline(prompt="Enter Location (i.e. United States, China, Europe): ")
  loc <- as.character(loc)
  if(is.na(loc)) {
    print("Please Enter A Valid Location")
    loc <- getloc()
  }
  return(loc)
}

create.map <- function(df) {
  
  loc <- getloc()

  thamap <- get_map(location=loc, zoom=3)
  
  ggmap(thamap) + geom_point(data=df, aes(x=as.numeric(as.vector(longitude)), y=as.numeric(as.vector(latitude)), fill="red", alpha=0.8), size=3, shape=21) + guides(fill=FALSE, alpha=FALSE, size=FALSE)
}

create.heat_map <- function() {
  ggmap(map_g, extent="device") + geom_density2d(data = x, aes(x=as.numeric(as.vector(longitude)), y=as.numeric(as.vector(latitude))), size=0.3) + stat_density2d(data=x, aes(x=as.numeric(as.vector(longitude)), y=as.numeric(as.vector(latitude)), fill=..level.., alpha=..level..), size=0.01, bins=16, geom="polygon") + scale_fill_gradient(low="green", high="red") + scale_alpha(range=c(0, 0.3), guide=FALSE)
}

# log$ip_address <- hostname_to_ip(as.character(log$host))

coords <- main()