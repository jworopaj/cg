library(dplyr)
library(rvest)
library(RgoogleMaps)
library(data.table)
library(RCurl)
library(RJSONIO)



# POBIERZ DANE Z WIKI -----------------------------------------------------

url <- "https://en.wikipedia.org/wiki/List_of_companies_of_Belgium"

dane <- url %>%
  read_html() %>%
  html_nodes("li") %>%
  html_text()

firmy <- dane[15:144]
firmy <- regmatches(firmy, regexpr(", ", firmy), invert = TRUE)
# firmy <- strsplit(firmy, ", ")
firmy <- t(as.data.table(firmy))



# POBIERZ LAT LONG Z GOOGLE -----------------------------------------------


url <- function(address, return.call = "json") {
  root <- "https://maps.googleapis.com/maps/api/place/textsearch/"
  key = "AIzaSyAUaf4m_QsQqDG-LrK1ADNl06wJGcg8eag"
  u <- paste0(root, return.call, "?query=", address, "&key=", key, "&components=country:BE")
  return(URLencode(u))
}

#####
# CO ZAWIERA POJEDYNCZE WYSZUKANIE
u <- url("Aurubis Belgium")
doc <- getURL(u)
x <- fromJSON(doc, simplify = FALSE)
x <- data.frame(unlist(x))
write.csv(x, file = "Aurubis-Belgium.csv")
#####


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
    return(c(NA, NA, NA, NA))
  }
}

# test
# geoCode("Aurubis Belgium") # dziala
# geoCode("Aurubis Helicopter")
# geoCode("Libeco-Lagae Belgium")

wsp2 <- data.frame(lon = rep(NA, 130), lat = rep(NA, 130), adr = rep(NA, 130), firma = firmy[1:130, 1])

for (i in 1:130) {
  geoc <- geoCode(paste(firmy[i, 1], "Belgium"))
  wsp2[i, 1] <- geoc[1]
  wsp2[i, 2] <- geoc[2]
  wsp2[i, 3] <- geoc[3]
}

wsp2$lon <- as.numeric(wsp2$lon)
wsp2$lat <- as.numeric(wsp2$lat)

# zapisz koncowa baze z lat long firm z wiki
write.csv(wsp2, "wspolrzedne-firm.csv", row.names = FALSE)


# ZNAJDZ POWIAZANE  -------------------------------------------------------

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

#usun braki w powiazanych
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


# zapisz koncowa baze z lat long firm (powiazanych) z wiki
write.csv(wsp3, "wspolrzedne-firm-powiazanych.csv", row.names = FALSE)




# SPRWADZ API PLACES ------------------------------------------------------



# NEARBYSEARCH ------------------------------------------------------------

url <- function(address, return.call = "json") {
  root <- "https://maps.googleapis.com/maps/api/place/nearbysearch/"
  key = "AIzaSyAUaf4m_QsQqDG-LrK1ADNl06wJGcg8eag"
  u <- paste0(root, return.call, "?location=", address, "&rankby=distance", "&name=Aurubis", "&key=", key)
  return(URLencode(u))
}

wspol <- as.name("51.1773286,4.8795559") # Aurubis
u <- url(wspol)
doc <- getURL(u)
x <- fromJSON(doc, simplify = FALSE)
nearbysearch <- data.frame(unlist(x))




# TEXTSEARCH --------------------------------------------------------------

url <- function(address, return.call = "json") {
  root <- "https://maps.googleapis.com/maps/api/place/textsearch/"
  key = "AIzaSyAUaf4m_QsQqDG-LrK1ADNl06wJGcg8eag"
  u <- paste0(root, return.call, "?query=", address, "&radius=50000", "&key=", key)
  return(URLencode(u))
}

u <- url("Aurubis Belgium")
doc <- getURL(u)
x <- fromJSON(doc, simplify = FALSE)
textsearch <- data.frame(unlist(x))



# RADARSEARCH -------------------------------------------------------------

url <- function(address, return.call = "json") {
  root <- "https://maps.googleapis.com/maps/api/place/radarsearch/"
  key = "AIzaSyAUaf4m_QsQqDG-LrK1ADNl06wJGcg8eag"
  u <- paste0(root, return.call, "?location=", address, "&radius=50000", "&keyword=pharmaceutical", "&key=", key)
  return(URLencode(u))
}

wspol <- as.name("51.1773286,4.8795559") # Aurubis
u <- url(wspol)
doc <- getURL(u)
x <- fromJSON(doc, simplify = FALSE)
radarsearch <- data.frame(unlist(x))

