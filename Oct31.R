###############################################################################
# October 31, 2018
###############################################################################
 
# Merging everyone's reconciled files


folder <- "nhh_partial"
files <- dir(folder, pattern=".csv")


alldf <- list()
for (i in 1:length(files)) {
  student <- gsub("([^_]*)_.*.csv", "\\1", files[i])
  alldf[[student]] <- read.csv(paste0(folder, "/", files[i]), as.is=TRUE)
}

pids <- sapply(alldf, function(x) x$pid)
sapply(pids, length)
pids <- sapply(alldf[-1], function(x) x$pid)
head(pids)
pids_sd <- apply(pids, 1, sd)
w <- which(pids_sd != 0)
head(pids[w,])

alldf$changallen <- alldf$changallen[order(alldf$changallen$pid),]


group_cols <- c("yearbuilt", "landval", "saledate1", "sqft", "occupancy")


for (grp in 1:4) {
  cat("Checking group: ", grp, "\n")
  w <- which(sapply(alldf, function(df) group_cols[grp] %in% colnames(df)))
  
  # dimension check
  dims <- sapply(alldf[w], dim)
  print(dims)
  
  if (grp == 1) {
    multibuilding <- alldf[[w[1]]]$multibuilding
  }
  scraped_cols <- colnames(alldf[[w[1]]])
  for (col in scraped_cols) {
    cat("  Checking col: ", col, "\n")
    allsols <- sapply(alldf[w], function(df) df[!multibuilding,col])
    rownames(allsols) <- order(alldf[[2]]$pid[!multibuilding])
    if (is.numeric(allsols)) {
      rowsds <- apply(allsols, 1, sd, na.rm=TRUE)
      cat("      % Row SD non-0:", mean(rowsds > 0, na.rm = TRUE), "\n")
      cat("         ")
      if  (mean(rowsds > 0, na.rm = TRUE) > 0) {
        print(head(allsols[which(rowsds > 0),]))
      }
    } else {
      cat("      Distrib of Distinct Values per Entry:\n")
      cat("         ")
      print(table(apply(trimws(allsols), 1, function(y) length(unique(y)))))
    }
    cat("      Summary of Missingness:\n")
    print(apply(is.na(allsols), 2, sum))
  }
}


# issues:
# saleprice5 one off
# saleowner5 one off
# saledate5 lots off
# saleprice4 two fff
# saleowner4 two off
# saledate4 lots off
# saleprice3 4 off
# saleowner3 4 off
# saledate3 lots off
# saleprice2 2 off
# saleowner2 2 off
# saledate2 lots off
# saleprice1 ok
# saleowner1 ok
# saledate1 ok

jenkin <- alldf[['tsuijenkin']]
brian <- alldf[['kitanobrian']]

# ----- resolving saleprice -----
table(brian$saleprice1 - jenkin$saleprice1) # all of our values are the same
table(brian$saleprice2 - jenkin$saleprice2) # all of our values are the same
table(brian$saleprice3 - jenkin$saleprice3) # all of our values are the same
table(brian$saleprice4 - jenkin$saleprice4) # all of our values are the same
table(brian$saleprice5 - jenkin$saleprice5) # all of our values are the same

# fixing the NA's
sp2 <- which(is.na(brian$saleprice2) != is.na(jenkin$saleprice2))
print(brian$pid[sp2])
print(brian$saleprice2[sp2])
print(jenkin$saleprice2[sp2])

sp3 <- which(is.na(brian$saleprice3) != is.na(jenkin$saleprice3))
print(brian$pid[sp3])
print(brian$saleprice3[sp3])
print(jenkin$saleprice3[sp3])

sp4 <- which(is.na(brian$saleprice4) != is.na(jenkin$saleprice4))
print(brian$pid[sp4])
print(brian$saleprice4[sp4])
print(jenkin$saleprice4[sp4])

sp5 <- which(is.na(brian$saleprice5) != is.na(jenkin$saleprice5))
print(brian$pid[sp5])
print(brian$saleprice5[sp5])
print(jenkin$saleprice5[sp5])

# going with jenkin
df = data.frame(jenkin$pid)
colnames(df) <- c("pid")
df$saleprice1 <- jenkin$saleprice1
df$saleprice2 <- jenkin$saleprice2
df$saleprice3 <- jenkin$saleprice3
df$saleprice4 <- jenkin$saleprice4
df$saleprice5 <- jenkin$saleprice5
df$saleowner1 <- jenkin$saleowner1
df$saleowner2 <- jenkin$saleowner2
df$saleowner3 <- jenkin$saleowner3
df$saleowner4 <- jenkin$saleowner4
df$saleowner5 <- jenkin$saleowner5
df$saledate1 <- brian$saledate1
df$saledate2 <- brian$saledate2
df$saledate3 <- brian$saledate3
df$saledate4 <- brian$saledate4
df$saledate5 <- brian$saledate5
write.csv(df, "reconciled.csv", row.names = FALSE)

# ----- OWNER -----
brian$saleowner2 <- trimws(brian$saleowner2)
which(brian$saleowner2 != jenkin$saleowner2)
which(trimws(brian$saleowner3) != jenkin$saleowner3)
which(trimws(brian$saleowner4) != jenkin$saleowner4)
which(trimws(brian$saleowner5) != jenkin$saleowner5)
brian$pid[8315]
