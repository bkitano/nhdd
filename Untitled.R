library(magrittr)

path = "~/Desktop/Classes/Fall_2018/S&DS 625/newdata"
file.names <- dir(path, pattern =".html")
df <- data.frame()
pb <- txtProgressBar(style = 3)

pids <- c()

getRealEstateData <- function(i) {
  df <- data.frame()
  # out.file <- rbind(out.file, file)
  url <- file.names[i]
  
  x <- scan(url, what="", sep="\n", quiet = TRUE)
  
  # --------- PID ---------- ok
  pid <- strsplit(url, "[.]")[[1]][1]
  pids <- cbind(pid, pids)
  
  # --------- YEAR BUILT ---------- ok
  yearbuiltline <- grep("MainContent_ctl01_lblYearBuilt", x)
  if(length(yearbuiltline) != 0) {
    rawyearbuiltstring <- gsub("<[^<>]*>", "", x[yearbuiltline])
    yearbuilt <- as.numeric(rawyearbuiltstring)
  } else {
    yearbuilt <- NA
  }
      
  # ------------ LOCATION ------------- ok
  locationline <- grep("MainContent_lblLocation", x)
  if(length(locationline) != 0) {
    rawlocationlinestring <- gsub("<[^<>]*>", "", x[locationline])
    location <- trimws(rawlocationlinestring)
  } else {
    location <- NA
  }

  # ----------- TOTVAL ---------- ok
  appraisalline <- grep("MainContent_lblGenAppraisal", x)
  rawappraisalstring <- gsub("<[^<>]*>", "", x[appraisalline])
  totval <- as.numeric(gsub("[$,]", "", rawappraisalstring[2]))
  
  # ---------- BEDROOMS ----------- not ok
  # returns all the matching lines
  totalbedroomslines <- grep("<td>(Total\\s)*B[e]*dr(oo)*ms[:]*</td>", x, ignore.case=TRUE)
  
  # returns all the matching strings
  totalbedroomsstrings <- gsub("<[^<>]*>", "", x[totalbedroomslines]) %>% trimws()
  
  # instead of splitting at the colon, search for digits in the line 33
  if( length(totalbedroomsstrings) != 0 ) {
    
    # finds the start of digits
    digitFinder <- regexpr("\\d", totalbedroomsstrings)
    
    # cuts the left side of the string off, leaving numbers in the front
    digitStart <- substr(totalbedroomsstrings, digitFinder, nchar(totalbedroomsstrings))

    # cuts the right side of the string off, leaving just numbers
    digitEndFinder <- regexpr("\\s", digitStart)
    
    if(digitEndFinder != -1) {
      digits <- substr(digitStart, 0, digitEndFinder) %>% trimws()
      beds <- sum(as.numeric(digits))
    } else {
      beds <- sum(as.numeric(digitStart))
    }
  } else {
    beds <- NA
  }
  
  # ---------- BATHROOMS ---------- not ok
  totalbathroomsline <- grep("<td>(Total\\s)*B[a]*th(r)*(oo)*(m)*s[:]*</td>", x, ignore.case=TRUE )
  totalbathroomsstring <- gsub("<[^<>]*>", "", x[totalbathroomsline]) %>% trimws()
  
  # instead of splitting at the colon, search for digits in the line 33
  if( length(totalbathroomsstring) != 0 ) {
    digitFinder <- regexpr("\\d", totalbathroomsstring)
  
    # cuts the back part of the string off, leaving numbers in the front
    digitStart <- substr(totalbathroomsstring, digitFinder, nchar(totalbathroomsstring))
      
    digitEndFinder <- regexpr("\\s", digitStart)
    if(digitEndFinder != -1) {
      baths <- sum(as.numeric(substr(digitStart, 0, digitEndFinder) %>% trimws()))
    } else {
      baths <- sum(as.numeric(digitStart))
    }
  } else {
    baths <- NA
  }
  
  # ---------- HALFBATHS ------------
  totalhbathroomsline <- grep("<td>Total Half Baths:</td>", x, ignore.case=TRUE)
  totalhbathroomsstring <- gsub("<[^<>]*>", "", x[totalhbathroomsline]) %>% trimws()
  
  if( length(totalhbathroomsstring) != 0 ) {
    digitFinder <- regexpr("\\d", totalhbathroomsstring)
    
    # cuts the back part of the string off, leaving numbers in the front
    digitStart <- substr(totalhbathroomsstring, digitFinder, nchar(totalhbathroomsstring))
    
    digitEndFinder <- regexpr("\\s", digitStart)
    if(digitEndFinder != -1) {
      hbaths <- sum(as.numeric(substr(digitStart, 0, digitEndFinder) %>% trimws()))
    } else {
      hbaths <- sum(as.numeric(digitStart))
    }
  } else {
    hbaths <- NA
  }
  
  # - 'multibuilding': TRUE/FALSE (is this a property with multiple buildings?)
  # ------ MULTIBUILDING -------
  buildings_lines <- grep("MainContent_ctl(\\d)*_lblHeading", x, ignore.case = TRUE)
  multibuilding <- (length(buildings_lines) > 1)
  
  # - 'zone': land use zone (e.g. RS1, RS2, ...)
  # ------- ZONE -------
  zoneline <- grep("MainContent_lblZone", x, ignore.case=TRUE)
  zonestring <- gsub("<[^<>]*>|(Zone)", "", x[zoneline]) %>% trimws()
  zone <- ifelse( length(zonestring) != 0, zonestring, NA)
  
  # - 'neighborhood': neighborhood (like 0200, which should not be a number)
  # ------- NEIGHBORHOOD -------
  nbhdline <- grep("MainContent_lblNbhd", x, ignore.case=TRUE)
  nbhdstring <- gsub("<[^<>]*>|(Neighborhood)", "", x[nbhdline]) %>% trimws()
  neighborhood <- ifelse( length(nbhdstring) != 0, nbhdstring, NA)
  
  # - 'landval': appraised value of the land
  # ------- LANDVAL -------- remove the commas
  landvaltableline <- grep("MainContent_grdCurrentValueAppr", x, ignore.case = TRUE)
  landvaltablestring <- gsub( "<[^<>]*>" , "", x[landvaltableline+6]) %>% trimws()
  if(length(landvaltablestring) > 0) {
    numbers_in_line <- strsplit(landvaltablestring, "[$]")[[1]]
    landval <- ifelse( length(numbers_in_line) > 0, numbers_in_line[3], NA )
  } else {
    landval <- NA
  }

  # - 'acres': land size in acres
  # ------- ACRES --------
  acresline <- grep("MainContent_lblLndAcres", x, ignore.case = TRUE)
  acresstring <- gsub("<[^<>]*>|(Size \\(Acres\\))", "", x[acresline]) %>% trimws()
  acres <- ifelse(length(acresstring) > 0, as.numeric(acresstring), NA)
  
  # - 'exval': the sum of the value of any extra features
  # ------- EXVAL --------
  # finds the start of the table
  exvaltableopentag <- grep("MainContent_grdXf", x, ignore.case = TRUE)
  
  if(length(exvaltableopentag) > 0) {
    
    # need to find where the table closes
    hasFoundTableCloseTag <- FALSE
    lines_ahead <- 1
    
    while(!hasFoundTableCloseTag) {
      if( length(grep("(\\s)*</table>(\\s)*", x[exvaltableopentag + lines_ahead], ignore.case = TRUE)) > 0) {
        hasFoundTableCloseTag <- TRUE
      } else {
        lines_ahead <- lines_ahead + 1
      }
    }
    
    costs <- 0
    
    for( line in exvaltableopentag:(exvaltableopentag+lines_ahead)) {
      cleanedline <- trimws(x[line])
      if(length(grep("[$]", cleanedline)) > 0) {
        costLine <- strsplit(cleanedline, "[$]")[[1]][2]
        costString <- strsplit(costLine, "(</td>)")[[1]][1]
        costs <- costs + as.numeric( gsub(",", "", costString))
      }
    }
  } else {
    costs <- NA
  }
  
  # - 'address': Owner address
  # -------- OWNER ADDRESS ---------
  owner_address_line <- grep("MainContent_lblAddr1", x, ignore.case = TRUE)
  
  owner_address_string <- gsub("<br>", ", ", x[owner_address_line]) %>% trimws()
  owner_address_stringf <- gsub("<[^<>]*>|(Address)*", "", owner_address_string)
  
  owner_address <- ifelse(length(owner_address_stringf) > 0, owner_address_stringf, NA)
  
  # put everything in the dataframe
  df[1,'pid'] <- pid
  df[1,'yearbuilt' ] <- yearbuilt
  df[1,'location'] <- location
  df[1,'totval'] <- totval
  df[1,'bedrooms'] <- beds
  df[1,'bathrooms'] <- baths
  df[1,'halfbaths'] <- hbaths
  df[1, 'multibuilding'] <- multibuilding
  df[1, 'zone'] <- zone
  df[1, 'neighborhood'] <- neighborhood
  df[1, 'landval'] <- landval
  df[1, 'acres'] <- acres
  df[1, 'exval'] <- costs
  df[1, 'address'] <- owner_address
  
  return(df)
}

x = length(file.names)
z <- 0
for (i in 1:x) {
  df <- rbind(df, getRealEstateData(i))
  if(i %% ceiling(x/100) == 0) {
    z <- z + 1
    setTxtProgressBar(pb, z/100)
  }
}

# order them by number
df_ordered <- df[order(as.numeric(df[,1])),]

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

getRealEstateData(61)

write.csv(df_ordered, file = "nh_real_estate.csv", row.names = FALSE)
