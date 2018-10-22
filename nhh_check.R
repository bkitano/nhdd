###############################################################################
# October 15, 2018
###############################################################################

# Checking submissions

folder <- "nhh"
files <- dir(folder, pattern=".csv")


alldf <- list()
for (i in 1:length(files)) {
  student <- gsub("([^_]*)_.*.csv", "\\1", files[i])
  alldf[[student]] <- read.csv(paste0(folder, "/", files[i]), as.is=TRUE)
}

cat("Dimension/colname check: \n")
scraped_cols <- c("pid", "yearbuilt", "location", 
                  "totval", "bedrooms", "bathrooms",
                  "halfbaths")
dims <- sapply(alldf, dim)
print(dims)




sapply(alldf, function(x) identical(names(x),
                                    scraped_cols))
print(sapply(alldf, names))


# some manual fixes to let us proceed
colnames(alldf$kodavid)[3] <- "location"
colnames(alldf$malikprateek)[4] <- "totval"
alldf$kitanobrian <- NULL


# check pids
pids <- sapply(alldf, function(x) x$pid)
head(pids)

for (j in c(1:2, 4, 8)) {
  alldf[[j]] <- alldf[[j]][order(as.numeric(alldf[[j]][,1])),]
}

## YOUR TURN:

# Cycling through each of the columns that are scraped, identify what
# discrepancies exist in your solutions. Try to code cleanly, avoiding
# repetition of code where unnecessary.

# Here's something to get you started:

thiscol <- scraped_cols[5]
thiscol
allsols <- sapply(alldf, function(df) df[,thiscol])
table(allsols)

rowsds <- apply(allsols, 1, sd, na.rm=TRUE)
summary(rowsds)
hist(rowsds)

which.max(rowsds)
allsols[22858,]





# Now, organize into groups of 2. Write a script to reconcile all of the columns and
# output a **cleaned up data frame** with all variables. Even if
# you're not plugged in, you should be providing advice (or even coding
# in tandem on your computer).

# You will likely stumble upon some differences that are due to
# choices made in coding the data. There's not necessarily a single
# correct way to code the data, but we should settle on a collective
# choice for each of these discrepancies. If you have identified one
# such coding discrepancy, bring it to my attention, and let's discuss
# as a class.


# New variables to scrape
# - 'multibuilding': TRUE/FALSE (is this a property with multiple buildings?)
# - 'zone': land use zone (e.g. RS1, RS2, ...)
# - 'neighborhood': neighborhood (like 0200, which should not be a number)
# - 'landval': appraised value of the land
# - 'acres': land size in acres
# - 'exval': the sum of the value of any extra features
# - 'address': Owner address

# We discussed multibuilding properties... what to do with them? Sum up their 
# values (bedrooms, baths, etc.) in addition to flaggin them.

### Example record for pid 2334:
# - 'multibuilding': FALSE
# - 'zone': 'RS2'
# - 'neighborhood': '0200'
# - 'landval': 54400
# - 'acres': 0.17
# - 'exval': 3700
# - 'address': '30 UPSON TER, NEW HAVEN, CT 06512'
