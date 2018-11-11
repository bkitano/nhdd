## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

## ------------------------------------------------------------------------
# library for sample_n
library('dplyr')
library('leaps')

folder <- 'geos'
files <- dir(folder, pattern=".csv")
alldf <- list()
for (i in 1:length(files)) {
  student <- gsub("([^_]*)_.*.csv", "\\1", files[i])
  alldf[[student]] <- read.csv(paste0(folder, "/", files[i]), as.is=TRUE)
}

dd <- read.csv('final_nhh.csv', as.is = TRUE)
d <- sample_n(dd, 5000)

# turn each sale into its own observation
df <- data.frame(matrix(nrow = 1, ncol = 16))
insert.cols <- c('pid', 'yearbuilt', 'totval', 'bedrooms', 'bathrooms', 'halfbaths', 'landval', 'zone', 'acres', 'exval', 'pctgood', 'sqft', 'garagesqft', "saleprice", "saledate", "saleowner")
colnames(df) <- insert.cols

for (i in seq(1:nrow(d)) ) {
  # for each sale in row
  for (sale in 1:5) {
    # create new row with the right data
    access.cols <- c('pid', 'yearbuilt', 'totval', 'bedrooms', 'bathrooms', 'halfbaths', 'landval', 'zone', 'acres', 'exval', 'pctgood', 'sqft', 'garagesqft', paste(c("saleprice", "saledate", "saleowner"), sale, sep = ""))
    
    r_ <- d[i, access.cols]
    names(r_)
    
    colnames(r_) <- insert.cols
    df <- rbind(df, r_)
    
  }
}

# turn the dates into separate cols via lubridate
library('lubridate')
lbd_helper1 <- mdy(df$saledate)
df$salemonth <- as.factor(month(lbd_helper1))
df$saleyear <- year(lbd_helper1)


## ------------------------------------------------------------------------
# drop the na sales ones
df.na <- df[-which(is.na(df$saledate)),]

df.na$saleprice <- as.numeric(df.na$saleprice)

# drop the ones in which the sale price was 0
df.sale <- df.na[-which(df$saleprice == 0), ]

## ------------------------------------------------------------------------
# add latitude and longitude data
adf <- data.frame(matrix(ncol=3))
colnames(adf) <- c('pid', 'latitude', 'longitude')
for (d in alldf) {
  newdf <- data.frame(d$pid)
  colnames(newdf) <- c('pid')
  newdf$latitude <- ifelse('latitude' %in% colnames(d), d$latitude, d$lat)
  newdf$longitude <- ifelse('longitude' %in% colnames(d), d$longitude, d$long)
  adf <- rbind(adf, newdf)
}

# some people messed up the lat's and long's 
# get all the ones where the latitude is 
df.loc <- merge(adf, df.sale, by = 'pid')

## ------------------------------------------------------------------------
# separate into commercial vs residential units
df.com <- df.loc[which(df.loc$bedrooms == 0), ]
df.res <- df.loc[which(df.loc$bedrooms != 0), ]

# get rid of all property transfers
df.res.s <- df.res[-which(df.res$saleprice == 0), ]

## ------------------------------------------------------------------------
# get average price for zone by each year

# dataframe to store stuff
avg.nghbd.price <- data.frame(
  matrix(
    ncol = length(unique(df.res.s$saleyear)),
    nrow = length(unique(df.res.s$zone))
    )
  )
colnames(avg.nghbd.price) <- as.character(unique(df.res.s$saleyear))
rownames(avg.nghbd.price) <- unique(df.res.s$zone)

for (zone in unique(df.res.s$zone) ) {
  for (year in unique(df.res.s$saleyear)) {
    m <- mean(df.res.s$saleprice[which(df.res.s$saleyear == year & df.res.s$zone == zone)], na.rm = TRUE)
    avg.nghbd.price[zone, as.character(year)] <- m
  }
  
}

## ------------------------------------------------------------------------
graphics.off()
par(mar=c(1,1,1,1))
plot(log(avg.nghbd.price['RS2',]))

