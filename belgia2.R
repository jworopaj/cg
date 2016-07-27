
# WCZYTAJ DANE ------------------------------------------------------------

load("workspace.RData")

head(wsp2)

library(dplyr)
library(rvest)
library(RgoogleMaps)
library(data.table)
library(RCurl)
library(RJSONIO)
library(openxlsx)

write.xlsx(wsp2, "wspolrzedne-firm.xlsx")

shell("zip")

write.csv(wsp2, "wspolrzedne-firm.csv", row.names = FALSE)



library(WikipediR)

page_metadata <- page_info("en","wikipedia", page = "List of companies of Belgium")
links <- page_links("en","wikipedia", page = "List of companies of Belgium", limit = 500)

str(links, 5)
str(links)
links$query$pages$`285254`$links

firmy2 <- as.character()
for (i in links$query$pages$`285254`$links) {
  firmy2 <- append(firmy2, i$title)
}

ind1 <- grep("Template", firmy2)
ind2 <- grep("List of", firmy2)
firmy2 <- firmy2[-c(ind1, ind2)]


strony <- page_info("nl", "wikipedia", page = "Jonckheere")
