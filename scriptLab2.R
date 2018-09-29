# R sciript for Lab2

#############################
## Part I
#############################

## 1. Set your working directory to Lab2
setwd('your/path/to/Lab2')

## 2. Load packag
# Install rgbif from CRAN
install.packages("rgbif")
# Or install the development version from GitHub
devtools::install_github("ropensci/rgbif")

# Load packages
library(rgbif)
library(maptools)

# GBIF account information
username <- 'your user name'
password <- 'your password'
email    <- 'your email'

## 3. GBIF data overview
# Number of occurrence records in GBIF.
occ_count()
# Number of records in China
occ_count(country='CN') 
# Use supported query parameter combinations
occ_count(country='US', georeferenced=TRUE, basisOfRecord='OBSERVATION')


## 4. Get GBIF data
# Lookup names in the GBIF backbone taxonomy
spe.inf  <- name_backbone(name = 'Abies lasiocarpa', kingdom='plants')
taxonkey <- spe.inf$speciesKey

# Or you can use a simple and quick way
name_suggest(q='Abies lasiocarpa',limit = 5)

# Search for GBIF occurrences
# Use occ.search() to obtain small dataset (number < 200,000)
occ.search <- occ_search(taxonKey = taxonkey, limit = 200)
tbl.search <- occ.search$data
class(tbl.search); dim(tbl.search)
# Use occ_download() to obtain big dataset
# You must input your username and password of GBIF account
down.spe <- occ_download(paste0('taxonKey = ',taxonkey), user = username, 
                         pwd = password,email = email)
# Download information
print(down.spe)

# Download zip file to work directory
# After running the previous line of code, you have to wait a short while to download the data. 
# Because GBIF needs to prepare the zip file you need in the background.
occ.dl <- occ_download_get(down.spe, overwrite = T, path = '.')

# Import data set
tbl.dl <- occ_download_import(occ.dl)
# World map data in maptools package
data(wrld_simpl)

# Plot map and points
plot(wrld_simpl,col="light grey",xlim=c(-180,180), ylim=c(-90,90), axes=TRUE)
points(tbl.dl$decimalLongitude, tbl.dl$decimalLatitude, col='red', pch=20)
# Try to view some photos of genus Abies
# Note: search data using the name directly is also allowed 
res <- occ_search(scientificName = "Abies", mediaType = 'StillImage',
                  return = "media", limit= 3)
gbif_photos(res)


## 5. Download data within certain study area
# load packages
library(rgeos) # read shapefile data
library(rgdal) # wkt use
# Read USA boundary polygons
usa.shp <- readOGR('usaBoundary.shp')
class(usa.shp)
# Transfer the shapefile to wkt format 
usa.wkt <- writeWKT(usa.shp)
# Get data
occ.usa <- occ_search(taxonKey = taxonkey,geometry = usa.wkt, geom_big = "bbox",limit=1000)

# Plot distribution map
plot(wrld_simpl,col="light grey",xlim=c(-180,180), ylim=c(-90,90), axes=TRUE)
plot(usa.shp,col="yellow",add=T)
points(occ.usa$data$decimalLongitude,occ.usa$data$decimalLatitude, col='red', pch=20)

## 6. Complex queries
# complex example with many predicates
occ.ex <- occ_download("taxonKey = 2685313",
                       "basisOfRecord = HUMAN_OBSERVATION,OBSERVATION,MACHINE_OBSERVATION",
                       "country = US","hasCoordinate = true",
                       "year >= 1970","year <= 2015",
                       "month >= 3","month <= 8",
                       user = username,pwd = password,
                       email = email)


#############################
## Part II
#############################

# Format conversion
df.occ <- as.data.frame(tbl.dl)
class(df.occ)

# Extract useful variables
# Pay attention to variable "hasCoordinate" values if subset() return empty data.frame
df.use <- subset(df.occ,hasCoordinate == 'true', 
                 select = c('gbifID','decimalLatitude','decimalLongitude',
                            'countryCode','year','scientificName'))
names(df.use) <- gsub(names(df.use),pattern = 'decimalLatitude',replacement = 'Lat')
names(df.use) <- gsub(names(df.use),pattern = 'decimalLongitude',replacement = 'Lon')
head(df.use)

## 1. Reshape your data.frame
# load package
library(reshape2) 

# melt: reshape data.frame from wide to long format
# Convert all variables excepet gbifID
df.melt1 <- melt(df.use, id.vars = "gbifID",variable.name = "variable", 
                 value.name = "value")
head(df.melt1)
dim(df.melt1)

# Select two variables as id.vars
df.melt2 <- melt(df.use, id.vars = c("gbifID","year"),
                 variable.name = "variable", value.name = "value")
head(df.melt2)
dim(df.melt2)

# dcast: reshape data.frame from long to wide format
# Convert the data back to original state
df.cast1 <- dcast(df.melt1, gbifID ~ variable)
head(df.cast1)
dim(df.cast1)
df.cast2 <- dcast(df.melt2, gbifID + year ~ variable)
head(df.cast2)
dim(df.cast2)


## 2. Separating and uniting
# Unite: Unite multiple columns into one
# Separator characrer: “,”
df.unite <- unite(df.use,Coordinate,Lat,Lon,sep = ",")
head(df.unite)

# Separate: Separate one column into multiple columns
df.seprate <- separate(df.unite,Coordinate,c('Lat','Lon'),sep = ",")
head(df.seprate)


## 3. Missing values
library(VIM)
# Transfer empty string "" to missing value NA
df.use$countryCode[which(df.use$countryCode == "")] <- NA

# Missing value patterns
aggr(df.use,prop = F,numbers=T)

# Missing value processing
# Replace NA of year with the average year
year_mean <- ceiling(mean(df.use$year, na.rm = T))
# Replace NA of country Code with the mode of countryCode
country_mode <- df.use$countryCode[which.max(table(df.use$countryCode))]
# New data.frame pattern
df.pro <- replace_na(df.use, replace = list(year = year_mean, countryCode = country_mode))
aggr(df.pro,prop = F,numbers=T)


## 4.	String processing

# (1) Regular expression
# load package
library(stringr)
example.use <- "1. A small sentence. - 2. Another tiny sentence."

# Extract a substring of a string
str_extract(example.use, "small")
str_extract(example.use, "SMALL") # Default: character matching is case sensitive
str_extract(example.use, "big")

str_extract(example.use, "2")
str_extract(example.use, "ˆ2")
str_extract_all(example.use, "sentence.$")
str_extract_all(example.use, "tiny|sentence")

str_extract(example.use, "sm.ll")
str_extract(example.use, "sm[a-p]ll")

# [[:alpha:]]{3} matches any three alphabetic characters
str_extract(example.use, "s[[:alpha:]]{3}l")

str_extract(example.use, "A.+sentence")
str_extract(example.use, "A.+?sentence")

str_extract_all(example.use, "\\.")

str_extract_all(example.use, "\\w+")

# (2) The stringr package
# select a string
spe.name <- df.use$scientificName[1]
print(spe.name)
# String length
str_length(spe.name)
# Detect pattern
str_detect(spe.name, "Abies")
# Combine strings
str_c(spe.name,spe.name,sep = ";")
# Split string
str_split(spe.name," ")
# Count number of "a" in the string
str_count(spe.name,"a")


## 5. Other useful functions
# load package
library(plyr)
# count combinations in df.use, select year and countryCode as grouping variables
df.count <- count(df.use,c('year','countryCode'))
# Sort by frequency
df.order <- df.count[order(df.count$freq,decreasing = T),]
head(df.order)
