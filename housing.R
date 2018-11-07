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

# mixed effects model since houses are not individual
library('lme4')
mixed.model1 <- lmer(saleprice 
                         ~ 
                         + yearbuilt 
                         + totval 
                         + bedrooms 
                         + bathrooms 
                         + halfbaths 
                         + landval
                         + acres 
                         + exval 
                         + pctgood 
                         + sqft 
                         + garagesqft 
                         + saleyear
                         + (1 | pid), data = df)

# extract coefficients
coefs <- data.frame(coef(summary(mixed.model1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs[which(coefs$p.z < .005), ]

# Estimate   Std..Error   t.value          p.z
# (Intercept) -8.767003e+06 1.192499e+06 -7.351794 1.956213e-13
# totval      -8.190341e-02 1.127452e-02 -7.264473 3.745892e-13
# landval      5.924022e-01 7.579730e-02  7.815610 5.551115e-15
# acres        3.340762e+05 1.285902e+04 25.979908 0.000000e+00
# exval        1.543221e+00 2.297978e-01  6.715562 1.873435e-11
# saleyear     4.414074e+03 5.671072e+02  7.783491 7.105427e-15

# Question: when is the best time to buy a home?

# when were people buying homes the most?
df <- df[which(df$saleyear > 1970 && !is.na(df$saleprice)), ]
hist(df$saleyear[which(df$saleyear > 1970)], breaks = c(seq(1970,2020,1)))

# change in cost per square foot of real estate in nh
