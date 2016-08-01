load("workspace.RData")

library(dplyr)
library(RCurl)
library(RJSONIO)

url <- function(address, return.call = "json") {
  root <- "https://maps.googleapis.com/maps/api/place/autocomplete/"
  key = "AIzaSyAUaf4m_QsQqDG-LrK1ADNl06wJGcg8eag"
  u <- paste0(root, return.call, "?input=", address, "&location=0,0", "&radius=20000000", "&types=establishment", "&key=", key)
  return(URLencode(u))
}



geoCode <- function(address, verbose = FALSE) {
  if(verbose) cat(address, "\n)")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc, simplify = FALSE)
  if (x$status == "OK") {
    nazwy <- rep(NA, length(x$predictions))
    for (i in 1:length(x$predictions)) {
      nazwy[i] <- c(x$predictions[[i]]$description)
    }
    return(nazwy)
    Sys.sleep(0.5)
  } else {
    return(c(NA))
  }
}

powiazane <- list()

for (i in wsp2[, "firma"]) {
  # print(i)
  powiazane[[i]] <- geoCode(i)
}

powiazane2 <- powiazane[1:129]


# ZNAJDZ ICH GEOLOKALIZACJE ----------------------------------------------

# powinienem uzyc geocoding api
# https://console.developers.google.com/apis/api/geocoding_backend/overview?project=linear-aviary-138408&hl=PL&duration=P30D
# zamienia adresy na geolokalizacje

# http://maps.googleapis.com/maps/api/geocode/

url <- function(address, return.call = "json") {
  root <- "https://maps.googleapis.com/maps/api/geocode/"
  key = "AIzaSyAUaf4m_QsQqDG-LrK1ADNl06wJGcg8eag"
  u <- paste0(root, return.call, "?address=", address, "&key=", key)
  return(URLencode(u))
}



geoCode <- function(address, verbose = FALSE) {
  if(verbose) cat(address, "\n)")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc, simplify = FALSE)
  if (x$status == "OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lon <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lon, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA))
  }
}

powiazane.unlist <- unlist(powiazane)

wsp3 <- data.frame(lon = rep(NA, 483), lat = rep(NA, 483), adr = powiazane.unlist)
braki <- which(is.na(wsp3$adr))
wsp3 <- wsp3[-braki, ]
wsp3$adr <- as.character(wsp3$adr)

for (i in 1:465) {
  geoc <- geoCode(wsp3$adr[i])
  wsp3[i, 1] <- geoc[1]
  wsp3[i, 2] <- geoc[2]
}

wsp3$lon <- as.numeric(wsp3$lon)
wsp3$lat <- as.numeric(wsp3$lat)



