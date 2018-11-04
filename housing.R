library('dplyr')
library('leaps')

# model housing prices
dd <- read.csv('final_nhh.csv', as.is = TRUE)
d <- sample_n(dd, 2000)

# turn each sale into its own observation
df <- data.frame(matrix(nrow = 1, ncol = 14))
insert.cols <- c('yearbuilt', 'totval', 'bedrooms', 'bathrooms', 'halfbaths', 'landval',
                                     'acres', 'exval', 'pctgood', 'sqft', 'garagesqft', "saleprice", "saledate", "saleowner")
colnames(df) <- insert.cols

for (i in seq(1:nrow(d)) ) {
  # for each sale in row
  for (sale in 1:5) {
    # create new row with the right data
    access.cols <- c('yearbuilt', 'totval', 'bedrooms', 'bathrooms', 'halfbaths', 'landval',
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

# mixed effects model since houses are not individual
library('lme4')
