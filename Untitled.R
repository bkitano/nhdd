library(magrittr)

path = "/Users/bkitano/Desktop/Classes/Fall_2018/S&DS 625/NHHD/newdata"
file.names <- dir(path, pattern=".html")
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
  
  # ---------- BEDROOMS ----------- not ok (too high by ~120)
  # returns all the matching lines
  totalbedroomslines <- grep("T[o]*t[a]*l(\\s)*B[e]*dr(oo)*ms", x, ignore.case=TRUE)
  
  # returns all the matching strings
  totalbedroomsstrings <- gsub("<[^<>]*>", "", x[totalbedroomslines]) %>% trimws()
  
  # instead of splitting at the colon, search for digits in the line 33
  if( length(totalbedroomsstrings) != 0 ) {
    digits <- gsub("\\D", "", totalbedroomsstrings)
    beds <- sum(as.numeric(digits))
  } else {
    beds <- NA
  }
  
  # ---------- BATHROOMS ---------- not ok
  totalbathroomsline <- grep("T[o]*t[a]*l(\\s)*B[a]*th(r)*(oo)*(m)*s", x, ignore.case=TRUE )
  totalbathroomsstring <- gsub("<[^<>]*>", "", x[totalbathroomsline]) %>% trimws()
  
  # instead of splitting at the colon, search for digits in the line 33
  if( length(totalbathroomsstring) != 0 ) {
    baths <- sum(as.numeric(gsub("\\D","",totalbathroomsstring)))
  } else {
    baths <- NA
  }
  
  # ---------- HALFBATHS ------------
  totalhbathroomsline <- grep("T[o]*t[a]*l H[a]*lf B[a]*ths", x, ignore.case=TRUE)
  totalhbathroomsstring <- gsub("<[^<>]*>", "", x[totalhbathroomsline]) %>% trimws()
  
  if( length(totalhbathroomsstring) != 0 ) {
    hbaths <- sum(as.numeric(gsub("\\D", "", totalhbathroomsstring)))
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
    landvalstring <- ifelse( length(numbers_in_line) > 0, numbers_in_line[3], NA )
    landval <- as.numeric(gsub("[,]","",landvalstring))
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
  
  # - 'saledate1', 'saleprice1', 'saleowner1',
  #   'saledate2', 'saleprice2', 'saleowner2', etc.:
  #   sale history (up to the 5 most recent; most recent first)
  # ------ SALE HISTORY -------
  # finds the start of the table
  salehistoryline <- grep("MainContent_grdSales", x, ignore.case = TRUE)
  
  if(length(salehistoryline) > 0) {
    
    # need to find where the table closes
    hasFoundTableCloseTag <- FALSE
    lines_ahead <- 1
    
    while(!hasFoundTableCloseTag) {
      if( length(grep("(\\s)*</table>(\\s)*", x[salehistoryline + lines_ahead], ignore.case = TRUE)) > 0) {
        hasFoundTableCloseTag <- TRUE
      } else {
        lines_ahead <- lines_ahead + 1
      }
    }
    
    nthRecord <- 1
    salehistory <- data.frame()

    coltitlesnoindex <- c("saleowner", "saleprice", "saledate")
    
    for( line in salehistoryline:(salehistoryline+lines_ahead)) {
      cleanedline <- trimws(x[line])
      
      # look at each line with td tags
      if(length(grep("<td\\s*(.*)>(.*)</td>", cleanedline)) > 0) {
        coltitleswindex <- paste(coltitlesnoindex, nthRecord, sep="")
        
        # split up the tds tag line into individual td
        tds <- strsplit(cleanedline, "><")
        for(i in 1:3) {
          j <- c(1,2,6)
          data <- trimws(gsub("[<]*(td.*)>(.*)<(/td.*)[>]*", "\\2", tds[[1]][j[i]]))
          salehistory[1, coltitleswindex[i]] <- ifelse(j[i]==2, as.numeric(gsub("\\D", "", data)), data)
        }
        nthRecord <- nthRecord + 1
      }
      # print(salehistory)
    }
    
    while(nthRecord <= 5) {
      coltitleswindex <- paste(coltitlesnoindex, nthRecord, sep="")
      for(i in 1:3) {
        salehistory[1, coltitleswindex[i]] <- NA
      }
      nthRecord <- nthRecord + 1
    }
    
    salehistory <- salehistory[1, 1:15]
    
  } else {
    print("no sales data")
    # no sales table
    salehistory <- NA
  }
  
  
  # - 'sqft': living area in square feet
  # ------- SQFT --------
  sqft_line <- grep("MainContent_ctl01_lblBldArea", x, ignore.case = TRUE)
  if( length(x[sqft_line]) != 0 ) {
    sqftstring <- gsub("MainContent_ctl01_lblBldArea", "", x[sqft_line])
    sqft <- sum(as.numeric(gsub("\\D", "", sqftstring)))
  } else {
    sqft <- NA
  }
  
  # - 'pctgood': building percent good
  # ------- PCTGOOD -------
  pctgoodline <- grep('MainContent_ctl01_lblPctGood', x, ignore.case = TRUE)
  if( length(x[pctgoodline]) != 0 ) {
    pctgoodstring <- gsub("MainContent_ctl01_lblPctGood", "", x[pctgoodline])
    pctgood <- mean(as.numeric(gsub("\\D", "", pctgoodstring)))
  } else {
    pctgood <- NA
  }  
  
  
  # - 'style', 'model', 'occupancy', 'actype', 'bathstyle', 'kstyle':
  #      style, model, occupancy, A/C Type, bath style, kitchen style
  # -------- STYLE --------
  styleline <- grep("<td>Style[:]*</td>", x, ignore.case = TRUE)[1]
  stylestring <- x[styleline] %>% trimws()
  style_ <- gsub("<td>(Style|STYLE)[:]*</td><td>(.*)</td>", "\\2", stylestring) %>% trimws()
  style <- ifelse(style_ == "" || style_ == "NA", NA, style_)
  
  # -------- MODEL --------
  modelline <- grep("<td>Model[:]*</td>", x, ignore.case = TRUE)[1]
  modelstring <- x[modelline] %>% trimws()
  model_ <- gsub("<td>(Model|MODEL)[:]*</td><td>(.*)</td>", "\\2", modelstring) %>% trimws()
  model <- ifelse(model_ == "" || model_ == "NA", NA, model_)
  
  # -------- OCCUPANCY --------
  occupancyline <- grep("<td>Occupancy[:]*</td>", x, ignore.case = TRUE)[1]
  occupancystring <- x[occupancyline] %>% trimws()
  occupancy_ <- gsub("<td>(Occupancy|OCCUPANCY)[:]*</td><td>(.*)</td>", "\\2", occupancystring) %>% trimws()
  occupancy <- ifelse(occupancy_ == "" || occupancy_ == "NA", NA, occupancy_)
  
  # -------- ACTYPE --------
  actypeline <- grep("<td>AC Type[:]*</td>", x, ignore.case = TRUE)[1]
  actypestring <- x[actypeline] %>% trimws()
  actype_ <- gsub("<td>(AC Type|AC TYPE)[:]*</td><td>(.*)</td>", "\\2", actypestring) %>% trimws()
  actype <- ifelse(actype_ == "" || actype_ == "NA", NA, actype_)
  
  # -------- BATHSTYLE --------
  bathstyleline <- grep("<td>Bath Style[:]*</td>", x, ignore.case = TRUE)[1]
  bathstylestring <- x[bathstyleline] %>% trimws()
  bathstyle_ <- gsub("<td>(Bath Style|BATH STYLE)[:]*</td><td>(.*)</td>", "\\2", bathstylestring) %>% trimws()
  bathstyle <- ifelse(bathstyle_ == "" || bathstyle_ == "NA", NA, bathstyle_)
  
  # -------- KSTYLE --------
  kstyleline <- grep("<td>Kitchen Style[:]*</td>", x, ignore.case = TRUE)[1]
  kstylestring <- x[kstyleline] %>% trimws()
  kstyle_ <- gsub("<td>(Kitchen Style|KITCHEN STYLE)[:]*</td><td>(.*)</td>", "\\2", kstylestring) %>% trimws()
  kstyle <- ifelse(kstyle_ == "" || kstyle_ == "NA", NA, kstyle_)
  
  # - 'garagesqft': area of garage in square feet
  # ------ GARAGESQFT -------
  # finds the start of the table
  outbuildingsline <- grep("MainContent_grdOb", x, ignore.case = TRUE)
  
  if(length(outbuildingsline) > 0) {
    
    # need to find where the table closes
    hasFoundTableCloseTag <- FALSE
    lines_ahead <- 1
    
    while(!hasFoundTableCloseTag) {
      if( length(grep("(\\s)*</table>(\\s)*", x[outbuildingsline + lines_ahead], ignore.case = TRUE)) > 0) {
        hasFoundTableCloseTag <- TRUE
      } else {
        lines_ahead <- lines_ahead + 1
      }
    }
    
    totalgsqft <- 0
    for( line in outbuildingsline:(outbuildingsline+lines_ahead)) {
      cleanedline <- trimws(x[line])
      
      # look at each line with td tags
      if(length(grep("(GARAGE)", cleanedline)) > 0) {
        
        # split up the tds tag line into individual td
        tds <- strsplit(cleanedline, "><")
        gsqft <- as.numeric(gsub("\\D", "",tds[[1]][5]))
        totalgsqft <- totalgsqft + gsqft
      }
    }
    
    # print(totalgsqft)
  } else {
    print("no outbuildings data")
    # no sales table
    totalgsqft <- 0
  }
  
  
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
  df <- cbind( df, salehistory)
  df[1, 'pctgood'] <- pctgood
  df[1, 'sqft'] <- sqft
  df[1, 'style'] <- style
  df[1, 'model'] <- model
  df[1, 'occupancy'] <- occupancy
  df[1, 'actype'] <- actype
  df[1, 'bathstyle'] <- bathstyle
  df[1, 'kstyle'] <- kstyle
  df[1, 'garagesqft'] <- totalgsqft
  # cbind sale history
  return(df)
}

x = length(file.names)
# x = 100
z <- 0
setwd("~/Desktop/Classes/Fall_2018/S&DS 625/NHHD/newdata")

for (i in 1:x) {
  df <- rbind(df, getRealEstateData(i))
  if(i %% ceiling(x/100) == 0) {
    z <- z + 1
    setTxtProgressBar(pb, z/100)
  }
}

# order them by number
df_ordered <- df[order(as.numeric(df[,1])),]

# When you are done, scrape the last batch of variables below (due next Monday):
# - 'saledate1', 'saleprice1', 'saleowner1',
#   'saledate2', 'saleprice2', 'saleowner2', etc.:
#   sale history (up to the 5 most recent; most recent first)
# - 'sqft': living area in square feet
# - 'pctgood': building percent good
# - 'style', 'model', 'occupancy', 'actype', 'bathstyle', 'kstyle':
#      style, model, occupancy, A/C Type, bath style, kitchen style
# - 'garagesqft': area of garage in square feet


### Example record for pid 2334:
# - 'saledate1': '11/30/2017', 'saleprice1': '0', 'saleowner1': 'PIETRUSZCA DIANE',
#   'saledate2': '01/15/2014', 'saleprice2': '0', 'saleowner2': 'PIETRUSZCA RAYMOND S',
#   'saledate3': NA, 'saleprice3': 0, 'saleowner3': 'PIETRUSZKA RAYMOND S & JANE C'
# - 'sqft': 2100
# - 'pctgood': 78
# - 'style': "Colonial"
# - 'model': "Single Family"
# - 'occupancy': 1
# - 'actype': "None"
# - 'bathstyle': "Average"
# - 'kstyle': "Average"
# - 'garagesqft': 0

getRealEstateData(2334)

write.csv(df_ordered, file = "nh3.csv", row.names = FALSE)
