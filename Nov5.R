library('dplyr')
library('leaps')

# model housing prices
dd <- read.csv('final_nhh.csv', as.is = TRUE)
d <- sample_n(dd, 2000)

# turn each sale into its own observation
df <- data.frame(matrix(nrow = 1, ncol = 15))
insert.cols <- c('pid', 'yearbuilt', 'totval', 'bedrooms', 'bathrooms', 'halfbaths', 'landval',
                 'acres', 'exval', 'pctgood', 'sqft', 'garagesqft', "saleprice", "saledate", "saleowner")
colnames(df) <- insert.cols

for (i in seq(1:nrow(d)) ) {
  # for each sale in row
  for (sale in 1:5) {
    # create new row with the right data
    access.cols <- c('pid', 'yearbuilt', 'totval', 'bedrooms', 'bathrooms', 'halfbaths', 'landval',
                     'acres', 'exval', 'pctgood', 'sqft', 'garagesqft', paste(c("saleprice", "saledate", "saleowner"), sale, sep = ""))
    
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

STOPYEAR <- 2018

# buying homes means the sale price is > 0; otherwise people are just exchanging houses
dfbuy <- df[which(df$saleprice > 0), ]

# when were people buying homes the most?
hist(dfbuy$saleyear[which(dfbuy$saleyear > 1950)], breaks = c(seq(1950,STOPYEAR,1)))
df1950 <- df[which(dfbuy$saleyear > 1950),]

# margin
df1950$margin <- df1950$saleprice - df1950$totval

# total market liquidity per year
year.stats <- data.frame()
years <- seq(1950, STOPYEAR, 1)
for (i in seq(1, length(years))) {
  
  # year
  year.stats[i, 'year'] <- years[i]
  
  # total market value
  total.volume <- sum(df1950$saleprice[which(df1950$saleyear == years[i])])
  year.stats[i, 'volume'] <- total.volume
  
  # average price of home sold
  avg.price <- mean(df1950$saleprice[which(df1950$saleyear == years[i])])
  year.stats[i, 'avgPrice'] <- ifelse(is.nan(avg.price), 0, avg.price)
  
  # total number of homes sold
  total.homes <- sum(df1950$saleyear == years[i], na.rm = TRUE)
  year.stats[i, 'totalSold'] <- total.homes
  
  # how does totval differ from saleprice?
  average.margin <- mean(df1950$margin[which(df1950$saleyear == years[i])])
  year.stats[i, 'margin'] <- average.margin
}

par(mfrow=c(2,2)) # all plots on one page 

# sum of all sales
plot(years, year.stats$volume)
lines(years, year.stats$volume)

# average cost
plot(years, year.stats$avgPrice)
lines(years, year.stats$avgPrice)

# total number of houses sold
plot(years, year.stats$totalSold)
lines(years, year.stats$totalSold)

# average profit margin
plot(years, year.stats$margin)
lines(years, year.stats$margin)

# interest rates, volumes, neighborhoods, which neighborhoods were most hit

