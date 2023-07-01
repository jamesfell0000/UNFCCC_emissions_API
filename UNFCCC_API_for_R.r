library(httr)
library(jsonlite)
library(rrapply)
library(dplyr)

#Get cookies first:
get_cookies <- function() {
UNFCCC_headers <- c("User-Agent" = "Mozilla/5.0 (X11; CrOS x86_64 14541.0.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36","Accept" = "*/*","Accept-Encoding" = "gzip, deflate, br","Accept-Language" = "en-US,en;q=0.9")
r <- GET("https://di.unfccc.int/api/parties/annexOne", add_headers(.headers = UNFCCC_headers))
ck <- cookies(r)$value
return(ck)
}


#Get a list of all the parties
get_parties <- function() {
#Get annex one countries, just as a reference table
UNFCCC_headers <- c("User-Agent" = "Mozilla/5.0 (X11; CrOS x86_64 14541.0.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36","Accept" = "*/*","Accept-Encoding" = "gzip, deflate, br","Accept-Language" = "en-US,en;q=0.9")
if (exists('ck')==FALSE) {ck <- get_cookies()}
r <- GET("https://di.unfccc.int/api/parties/annexOne", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
annex_1_parties <- fromJSON(content(r, "text"))
annex_1_parties <- annex_1_parties[[3]][[2]]
colnames(annex_1_parties)[which(names(annex_1_parties) == "id")] <- "partyId"
colnames(annex_1_parties)[which(names(annex_1_parties) == "code")] <- "partyCode"
colnames(annex_1_parties)[which(names(annex_1_parties) == "name")] <- "partyDesc"

#Get non annex one countries, just as a reference table
r <- GET("https://di.unfccc.int/api/parties/nonAnnexOne", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
non_annex_1_parties <- fromJSON(content(r, "text"))
non_annex_1_parties <- non_annex_1_parties[[3]][[1]]
colnames(non_annex_1_parties)[which(names(non_annex_1_parties) == "id")] <- "partyId"
colnames(non_annex_1_parties)[which(names(non_annex_1_parties) == "code")] <- "partyCode"
colnames(non_annex_1_parties)[which(names(non_annex_1_parties) == "name")] <- "partyDesc"
non_annex_1_parties$noData <- NULL

#create a parties dataframe:
parties <- rbind(annex_1_parties, non_annex_1_parties)
return(parties)
}


#Get years, just as a reference table
get_years <- function () {
UNFCCC_headers <- c("User-Agent" = "Mozilla/5.0 (X11; CrOS x86_64 14541.0.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36","Accept" = "*/*","Accept-Encoding" = "gzip, deflate, br","Accept-Language" = "en-US,en;q=0.9")
if (exists('ck')==FALSE) {ck <- get_cookies()}
r <- GET("https://di.unfccc.int/api/years/single", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
years <- fromJSON(content(r, "text"))
years <- years[[1]]
colnames(years)[which(names(years) == "id")] <- "yearId"
colnames(years)[which(names(years) == "name")] <- "yearDesc"
return(years)
}

#GET A LIST OF THE VARIABLE IDs THAT YOU NEED FOR A QUERY:
get_variableIDs <- function () {
UNFCCC_headers <- c("User-Agent" = "Mozilla/5.0 (X11; CrOS x86_64 14541.0.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36","Accept" = "*/*","Accept-Encoding" = "gzip, deflate, br","Accept-Language" = "en-US,en;q=0.9")
if (exists('ck')==FALSE) {ck <- get_cookies()}
#Get categories, just as a reference table
r <- GET("https://di.unfccc.int/api/dimension-instances/category", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
json_content<-content(r)
melted_content <- rrapply(json_content, how = "melt")
names <- melted_content %>% filter_all(any_vars(. %in% c('name')))
IDs <- melted_content %>% filter_all(any_vars(. %in% c('id')))
categories <- as.data.frame(cbind(IDs$L1,IDs$value,names$value))
colnames(categories)[which(names(categories) == "V1")] <- "partyType"
colnames(categories)[which(names(categories) == "V2")] <- "categoryId"
colnames(categories)[which(names(categories) == "V3")] <- "categoryDesc"


#Get classifications, just as a reference table
r <- GET("https://di.unfccc.int/api/dimension-instances/classification", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
classifications <- fromJSON(content(r, "text"))
classifications <- as.data.frame(classifications$annexOne)
colnames(classifications)[which(names(classifications) == "id")] <- "classificationId"
colnames(classifications)[which(names(classifications) == "name")] <- "classificationDesc"

#Get measures, just as a reference table
r <- GET("https://di.unfccc.int/api/dimension-instances/measure", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
measures <- fromJSON(content(r, "text"))
measures_annexOne <- do.call("rbind",list(measures[["annexOne"]][["children"]][[1]],measures[["annexOne"]][["children"]][[2]],measures[["annexOne"]][["children"]][[3]],measures[["annexOne"]][["children"]][[4]],measures[["annexOne"]][["children"]][[5]]))
measures_nonAnnexOne <- do.call("rbind",list(measures[["nonAnnexOne"]][["children"]][[1]],measures[["nonAnnexOne"]][["children"]][[2]],measures[["nonAnnexOne"]][["children"]][[3]],measures[["nonAnnexOne"]][["children"]][[4]],measures[["nonAnnexOne"]][["children"]][[5]]))
measures_cad <- measures[["cad"]]
measures_cadCP2 <- measures[["cadCP2"]]
measures <- do.call("rbind",list(measures_annexOne,measures_nonAnnexOne,measures_cad,measures_cadCP2))
measures <- measures[!duplicated(measures), ]

colnames(measures)[which(names(measures) == "id")] <- "measureId"
colnames(measures)[which(names(measures) == "name")] <- "measureDesc"

#Get gases, just as a reference table
r <- GET("https://di.unfccc.int/api/dimension-instances/gas", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
gases <- fromJSON(content(r, "text"))
gases <- as.data.frame(gases$annexOne)
colnames(gases)[which(names(gases) == "id")] <- "gasId"
colnames(gases)[which(names(gases) == "name")] <- "gasDesc"

#Get units, just as a reference table
r <- GET("https://di.unfccc.int/api/conversion/fq", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
units <- fromJSON(content(r, "text"))
units <- as.data.frame(units$units)
colnames(units)[which(names(units) == "id")] <- "unitId"
colnames(units)[which(names(units) == "name")] <- "unitDesc"

#Get variableIDs:
r <- GET("https://di.unfccc.int/api/variables/fq/annexOne", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
variableIDs_annexOne <- as.data.frame(fromJSON(content(r, "text")))

r <- GET("https://di.unfccc.int/api/variables/fq/nonAnnexOne", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
variableIDs_nonAnnexOne <- as.data.frame(fromJSON(content(r, "text")))

variableIDs <- rbind(variableIDs_annexOne,variableIDs_nonAnnexOne)
variableIDs <- variableIDs[!duplicated(variableIDs), ]


#Merge category IDs into variableIDs:
variableIDs <- merge(variableIDs, categories, by="categoryId", all.x=TRUE)
variableIDs <- merge(variableIDs, classifications, by="classificationId", all.x=TRUE)
variableIDs <- merge(variableIDs, measures, by="measureId", all.x=TRUE)
variableIDs <- merge(variableIDs, gases, by="gasId", all.x=TRUE)
variableIDs <- merge(variableIDs, units, by="unitId", all.x=TRUE)

#Reorder the columns for variableID:
col_order <- c("variableId", "partyType", "categoryDesc", "classificationDesc", "measureDesc", "gasDesc", "unitDesc","categoryId", "classificationId", "measureId", "gasId", "unitId")
variableIDs <- variableIDs[, col_order]

return(variableIDs)
}

#And a function here to get a variable id:
#get_variableID <- function(category,classification,measure,gas,unit) {
#	variableID <- variableIDs$variableId[variableIDs$categoryId==category & variableIDs$classificationId==classification & #variableIDs$measureId==measure & variableIDs$gasId==gas & variableIDs$unitId==unit]
#	return(variableID)
#}



flex_query <- function (parties_list,years_list,variables_list) {
UNFCCC_headers <- c("User-Agent" = "Mozilla/5.0 (X11; CrOS x86_64 14541.0.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36","Accept" = "*/*","Accept-Encoding" = "gzip, deflate, br","Accept-Language" = "en-US,en;q=0.9")
if (exists('ck')==FALSE) {ck <- get_cookies()}
r <- POST("https://di.unfccc.int/api/records/flexible-queries", body = list("variableIds" = variables_list, "partyIds" = parties_list, "yearIds" = years_list),encode = "json", set_cookies(.cookies = ck), add_headers(.headers = UNFCCC_headers))
results<-as.data.frame(fromJSON(content(r,"text")))

#Now make the results human readable by merging in the descriptions for the variableId and the partyId and the yearId
parties<-get_parties()
results <- merge(results, parties, by="partyId", all.x=TRUE)
years<-get_years()
results <- merge(results, years, by="yearId", all.x=TRUE)
variableIDs<-get_variableIDs()
results <- merge(results, variableIDs, by="variableId", all.x=TRUE)
col_order <- c("partyDesc", "partyCode", "yearDesc", "categoryDesc", "classificationDesc", "measureDesc", "gasDesc", "unitDesc", "numberValue", "stringValue")
results <- results[, col_order]
return(results)
}

##EXAMPLE OF QUERY:
##First define what parties, years and variables:
#parties_list = list(36) #A list of parties. You can get these IDs by running parties<-get_parties()
#years_list = list(58,59,60,61,62) #A list of years. You can get these IDs by running years<-get_years()
#variables_list = list(200187) #A list of variable IDs. You can get these IDs by running variableIDs<-get_variableIDs()
##Then run the query:
#query_results <- flex_query(parties_list,years_list,variables_list)
