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



# POBIERZ ADRES Z GOOGLE --------------------------------------------------

#geocode("aurubis belgium", source = "dsk")


# POBIERZ LAT LONG Z GOOGLE -----------------------------------------------


# url <- function(address, return.call = "json" ) {
#   root <- "https://maps.googleapis.com/maps/api/geocode/"
#   key = "AIzaSyAUaf4m_QsQqDG-LrK1ADNl06wJGcg8eag"
#   u <- paste0(root, return.call, "?address=", address, "&components=country:BE", "&key=", key)
#   return(URLencode(u))
# }
# 


url <- function(address, return.call = "json") {
  root <- "https://maps.googleapis.com/maps/api/place/textsearch/"
  key = "AIzaSyAUaf4m_QsQqDG-LrK1ADNl06wJGcg8eag"
  u <- paste0(root, return.call, "?query=", address, "&key=", key, "&components=country:BE")
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
    return(c(NA, NA, NA, NA))
  }
}

# test
geoCode("Aurubis Belgium") # dziala

wsp2 <- data.frame(lon = rep(NA, 10), lat = rep(NA, 10), adr = rep(NA, 10), firma = firmy[1:10, 1])

for (i in 1:10) {
  geoc <- geoCode(paste(firmy[i, 1], "Belgium"))
  wsp2[i, 1] <- geoc[1]
  wsp2[i, 2] <- geoc[2]
  wsp2[i, 3] <- geoc[3]
}





geoCode("Aurubis")
geoCode
geoCode("Watertorenstraat 35, 2250 Olen")


#  ------------------------------------------------------------------------



getDocNodeVal=function(doc, path)
{
  sapply(getNodeSet(doc, path), function(el) xmlValue(el))
}


gGeoCode=function(str)
{
  library(XML)
  u=paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
  doc = xmlTreeParse(u, useInternal=TRUE)
  str=gsub(' ','%20',str)
  lat=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lat")
  lng=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lng")
  list(lat = lat, lng = lng)
}

# test
gGeoCode("Watertorenstraat 35, 2250 Olen")





#  ------------------------------------------------------------------------



library(ggmap)
library(ggplot2)

geocode("Aurubis", output = "more")
geocode("Belgium, Aurubis")
geocode("Jana Pawla Poznan")
geocode("Watertorenstraat 35, 2250 Olen")
geocode("Aurubis")
wsp <- data.frame(lon = rep(NA, 130), lat = rep(NA, 130))

for (i in 1:130) {
  geoc <- geocode(firmy[i, 1])
  wsp[i, 1] <- geoc[1]
  wsp[i, 2] <- geoc[2]
}


qmap("Belgium", zoom = 5) + geom_point(data = wsp, aes(x = lon, y = lat), color = "red")

qmap("Belgium", zoom = 18) + geom_point(aes(x = 4.469936, y = 50.503887), color = "red") # w adresie google jest najpierw druga wartosc

qmap("Belgium", zoom = 8) + geom_point(aes(x = 4.8795559, y = 51.1773286), color = "red") # w adresie google jest najpierw druga wartosc

qmap("Belgium", zoom = 10) + geom_point(aes(x = 4,        y = 50.83333,   color = "red") # w adresie google jest najpierw druga wartosc

qmap("Aurubis Belgium", source = "cloudmade")
