#################################################################
# October 10, 2018
#################################################################

# Last class, you downloaded a New Haven Housing dataset. Within a folder,
# you have data named as 'pid.html', where 'pid' is a numeric identifier for
# the parcel (property).

x <- scan("newdata/600.html", what="", sep="\n")

thisline <- grep("MainContent_ctl01_lblYearBuilt", x)
thisline

x[thisline]

temp <- gsub("<[^<>]*>", "", x[thisline])
temp
as.numeric(temp)

# Today: continue scrape.
# Due at Monday noon, submit a .csv file with exactly 7 columns and 27,373 rows + a row header:
# * 'pid': the parcel id, numeric format, increasing order
# * 'yearbuilt' 
# * 'location': raw address of the property (not the owner's address)
# * 'totval': 2016 total appraisal value
# * 'bedrooms': number of bedrooms
# * 'bathrooms': number of bathrooms
# * 'halfbaths': number of half-bathrooms


# Example: for parcel number 600, the `pid` would be 600,
# the `location` would be "108 HYDE ST", the `yearbuilt` 
# would be 1910, `totval` would be 266690, `bedrooms` would be 8, `bathrooms`
# would be 3, and `halfbaths` would be 1.

# You will submit both a .csv and a .R script for the data processing.
# We will review your script for good coding style and efficiency.

# You should do some error checking by spot checking a range of properties. 
# Don't just assume code that works for one property will work for all.
