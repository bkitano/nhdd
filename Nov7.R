###################################################################
# November 7, 2018
###################################################################
x <- read.csv("loc_for_geocode.csv", as.is = TRUE)

# Individual work:
# Begin geocoding for your block of addresses (your netid will be
# next to your pids). 

pids <- x$pid[which(x$ggmap == 'bkk6')]

# Required submission: a .csv of either 456x rows containing 4 cols:
  
# * 'pid'
# * 'latitude'
# * 'longitude'
# * 'source': ggmap or Census



# Helpful hints for geocoding:

# ggmap usage:
library(ggmap)
geocode("24 Hillhouse Ave., New Haven, CT", source = "dsk")

df <- read.csv("final_nhh.csv", as.is = TRUE)
df.gg <- df[ which( df$pid %in% pids ), ]

cc <- data.frame(matrix(ncol = 4))
colnames(cc) <- c('pid', 'latitude', 'longitude', 'source')

for (e in df.gg$pid) {
  row <- data.frame(matrix(ncol = 4))
  colnames(row) <- c('pid', 'latitude', 'longitude', 'source')
  address <- df.gg$address[which(df.gg$pid == e)]
  lon.lat <- geocode(address, source = "dsk")
  row$longitude <- lon.lat$lon
  row$latitude <- lon.lat$lat
  row$source <- 'ggmap'
  row$pid <- e
  cc <- rbind(cc, row)
}

write.csv(cc, 'bkk6_latlon.csv', row.names = FALSE)

# ====== don't need this ========

# Census usage:
library(httr)
url <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form"


# the API can only handle up to 10000 requests
# id must start from 1 (as far as I can tell)
addresses <- data.frame(id = 1, location = '24 Hillhouse Ave.',
                        city = 'New Haven', 
                        state = 'CT',
                        zip = ' ')

# have to write out these 1 addresses into a .csv file
f <- tempfile(fileext = ".csv") # creates a temporary file connection somewhere
write.csv(addresses, f, row.names=FALSE, col.names = NULL)

# the API call
req <- POST(url, body=list(addressFile = upload_file(f),
                           benchmark = "Public_AR_Census2010",
                           vintage = "Census2010_Census2010"),
            encode = "multipart")

length(content(req, "text", encoding = "UTF-8"))

# easier to write the output into a .csv file and then read it in
outfile <- tempfile(fileext = ".csv")
writeLines(content(req, "text", encoding = "UTF-8"), outfile)


v <- read.csv(outfile, header=FALSE, as.is=TRUE)
head(v)
# I'm pulling in some column headers from the API document
colnames(v) <- c("id", "address", "match1", "match2", "address_match",
                 "latlong", "lineid")
head(v)

table(v$match1)
table(v$match2)

# Upload your .csv to Canvas whenever you are ready. I'll share
# the bundle of latitude/longitude files with the class after
# class.



## For Monday
# In your groups, prepare no more than 5 presentation slides 
# (using RMarkdown)
# summarizing your research question, EDA, analysis, and 
# conclusions. 

# Plan on a 5-minute presentation. Not everyone will get a chance to present. 
# The sample() function will be used to select presenters.


