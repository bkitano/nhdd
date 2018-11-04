###############################################################################
# October 29, 2018
###############################################################################

##
# Today (until 2pm):
# 
# In pairs, begin to reconcile everyone's submissions (in zipped folder).
# Aim to get a single data frame with the "correct" data by 2pm. 
# Then (or talk to me if you finish this part before 2pm), 
# we'll discuss the next part -- some analysis. 

folder = "/Users/bkitano/Desktop/Classes/Fall_2018/S&DS 625/NHHD/nhh3"
files <- dir(folder, pattern=".csv")

alldf <- list()
for (i in 1:length(files)) {
  student <- gsub("([^_]*)_.*.csv", "\\1", files[i])
  alldf[[student]] <- read.csv(paste0(folder, "/", files[i]), as.is=TRUE)
}

dims <- sapply(alldf, dim)
print(dims)

alldf$malikprateek <- NULL
alldf$kodavid <- NULL
alldf$chancolleen <- NULL

salescols <- c("saledate1"   , 
 "saleprice1",    "saleowner1" ,   "saledate2"   ,  "saleprice2" ,   "saleowner2"  , 
 "saledate3" ,    "saleprice3" ,   "saleowner3"  ,  "saledate4"  ,   "saleprice4"  , 
 "saleowner4",    "saledate5"  ,   "saleprice5"  ,  "saleowner5" )

# reconciling sales data
thiscol <- "saleprice1"
thiscol
allsols <- sapply(alldf, function(df) df[,thiscol])
rowsds <- apply(allsols, 1, sd, na.rm=TRUE)

w <- which(rowsds != 0)
length(w)
allsols[w,]
pids[w]

# remove shuheng's sale prices, then check for differences
salespricegoodies <- c("changallen", "kimclaire", "kitanobrian", "tsuijenkin", "zhangjingwen")

thiscol <- "saleprice1"
thiscol
allsols <- sapply(alldf[salespricegoodies], function(df) df[,thiscol])
rowsds <- apply(allsols, 1, sd, na.rm=TRUE)

w <- which(rowsds != 0)
length(w)
allsols[w,]

# all the salesprice1 are reconciled
for (colname in c("saleprice1", "saleprice2", "saleprice3", "saleprice4", "saleprice5")) {
  thiscol <- colname
  thiscol
  allsols <- sapply(alldf[salespricegoodies], function(df) df[,thiscol])
  rowsds <- apply(allsols, 1, sd, na.rm=TRUE)
  
  w <- which(rowsds != 0)
  print(length(w))
  print(allsols[w,])
}

# all the sale prices are reconciled

# ----- SALE OWNER ------
# reconciling sales data
thiscol <- "saleowner1"
thiscol
allsols <- sapply(alldf, function(df) df[,thiscol])

# distinct values per row (should all be 1)
print(table(apply(allsols, 1, function(y) length(unique(y)))))

# get the ones with more than one owner in the row
for (i in 1:length(allsols)) {
  row <- allsols[i,]
  if(length(unique(row)) > 1) {
    print(row)
  }
}

# replace &amp; with & in brian and jingwen's stuff
alldf$kitanobrian$saleowner1 <- gsub("(&amp;)", "&", alldf$kitanobrian$saleowner1)
alldf$zhangjingwen$saleowner1 <- gsub("(&amp;)", "&", alldf$zhangjingwen$saleowner1)

# check again
thiscol <- "saleowner1"
thiscol
allsols <- sapply(alldf, function(df) df[,thiscol])

# distinct values per row (should all be 1)
print(table(apply(allsols, 1, function(y) length(unique(y)))))

# get the ones with more than one owner in the row
for (i in 1:length(allsols)) {
  row <- allsols[i,]
  if(length(unique(row)) > 1) {
    print(row)
  }
}

# jenkin replaces the &amp; with a space, so remove jenkin
saleownergoodies <- c("changallen", "kimclaire", "kitanobrian", "wangshuheng", 'zhangjingwen' )

# check again
thiscol <- "saleowner1"
thiscol
allsols <- sapply(alldf[saleownergoodies], function(df) df[,thiscol])

# distinct values per row (should all be 1)
print(table(apply(allsols, 1, function(y) length(unique(y)))))

# get the ones with more than one owner in the row
for (i in 1:length(allsols)) {
  row <- allsols[i,]
  if(length(unique(row)) > 1) {
    print(row)
  }
}

# wangshuheng and zhangjingwen didn't deal with commas correctly, remove them
saleownergoodies <- c("changallen", "kimclaire", "kitanobrian" )

# check again
thiscol <- "saleowner1"
thiscol
allsols <- sapply(alldf[saleownergoodies], function(df) df[,thiscol])

# distinct values per row (should all be 1)
print(table(apply(allsols, 1, function(y) length(unique(y)))))

# get the ones with more than one owner in the row
for (i in 1:length(allsols)) {
  row <- allsols[i,]
  if(length(unique(row)) > 1) {
    print(row)
  }
}

# need to trim white space in kimclaire, changallen
alldf$changallen$saleowner1 <- trimws(alldf$changallen$saleowner1)
alldf$kimclaire$saleowner1 <- trimws(alldf$kimclaire$saleowner1)

# check again
thiscol <- "saleowner1"
thiscol
allsols <- sapply(alldf[saleownergoodies], function(df) df[,thiscol])

# distinct values per row (should all be 1)
print(table(apply(allsols, 1, function(y) length(unique(y)))))

# all ownername1 reconciled

# now do it over all the sales dates
for (colname in c("saleowner1", "saleowner2", "saleowner3", "saleowner4", "saleowner5")) {
  thiscol <- colname
  thiscol
  allsols <- sapply(alldf[saleownergoodies], function(df) df[,thiscol])
  rowsds <- apply(allsols, 1, sd, na.rm=TRUE)
  
  w <- which(rowsds != 0)
  print(length(w))
  print(allsols[w,])
}

# ------ SALE DATE ------
# reconciling sales data
thiscol <- "saledate1"
thiscol
allsols <- sapply(alldf, function(df) df[,thiscol])

# distinct values per row (should all be 1)
print(table(apply(allsols, 1, function(y) length(unique(y)))))

# get the ones with more than one date in the row
for (i in 1:length(allsols)) {
  row <- allsols[i,]
  if(length(unique(row)) > 1) {
    print(row)
  }
}

# brian and jingwen are wrong, so remove them
salesdategoodies <- c("changallen", "kimclaire", "tsuijenkin", "wangshuheng") 

# check again
thiscol <- "saledate1"
thiscol
allsols <- sapply(alldf[salesdategoodies], function(df) df[,thiscol])

# distinct values per row (should all be 1)
print(table(apply(allsols, 1, function(y) length(unique(y)))))

# get the ones with more than one date in the row
for (i in 1:length(allsols)) {
  row <- allsols[i,]
  if(length(unique(row)) > 1) {
    print(row)
  }
}

# go with NA over & or "", so remove allen and jenkin
salesdategoodies <- c("kimclaire", "wangshuheng") 

# check again
thiscol <- "saledate1"
thiscol
allsols <- sapply(alldf[salesdategoodies], function(df) df[,thiscol])

# distinct values per row (should all be 1)
print(table(apply(allsols, 1, function(y) length(unique(y)))))

# now do it over all the sales dates
for (colname in c("saledate1", "saledate2", "saledate3", "saledate4", "saledate5")) {
  thiscol <- colname
  thiscol
  allsols <- sapply(alldf[salesdategoodies], function(df) df[,thiscol])
  rowsds <- apply(allsols, 1, sd, na.rm=TRUE)
  
  w <- which(rowsds != 0)
  print(length(w))
  print(allsols[w,])
}

# all the sales dates are reconciled

# ----- submitting the columns ------
df <- data.frame(alldf[[1]]$pid)
colnames(df) <- c("pid")

# dates
for (colname in c("saledate1", "saledate2", "saledate3", "saledate4", "saledate5")) {
  # get one of the resolved dfs
  name <- salesdategoodies[1]
  gooddf <- alldf[[name]]
  df[colname] <- gooddf[colname]
}

# owners
for (colname in c("saleowner1", "saleowner2", "saleowner3", "saleowner4", "saleowner5")) {
  name <- saleownergoodies[1]
  gooddf <- alldf[[name]]
  df[colname] <- gooddf[colname]
}

# prices
for (colname in c("saleprice1", "saleprice2", "saleprice3", "saleprice4", "saleprice5")) {
  name <- salespricegoodies[1]
  gooddf <- alldf[[name]]
  df[colname] <- gooddf[colname]
}

df <- df[, c(1,2,7,12,3,8,13,4,9,14,5,10,15,6,11,16)]

write.csv(df, "sale_cols.csv", row.names = FALSE)
