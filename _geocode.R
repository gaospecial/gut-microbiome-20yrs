#' # 使用 Google API 解析地址
#'
#' 保存 JSON 文件到 RData 中

library(rjson)

# 注册 Google API key（临时）
google_key <- "AIzaSyCcTKF_kF6QnZnNFWr4aVx6mAWfYVDKEIQ"

# google API url base
google_url_base <- "https://maps.googleapis.com/maps/api/geocode/json?"


affiliation <- read.csv("data-raw/affiliation/Affiliations.csv")

last_sentence <- function(x, sep=",", last=5){
	list <- strsplit(x,split=sep,fixed=TRUE)
	list <- lapply(list, function(l){
		len <- length(l)
		if (len <= last){
			return(l)
		}
		if (len > last){
			return(l[(len-last+1):len])
		}
	})
	result <- unlist(lapply(list, function(l)paste0(l,collapse=sep)))
	return(result)
}

google_geocode_url <- function(address){
	# trim address
	address <- trimws(address)
	address <- gsub(address,pattern=" +",replacement="+")
	url <- paste0(google_url_base,"address=",address,"&key=",google_key)
	if(length(url)>1) url <- unlist(lapply(url,function(x) URLencode(enc2utf8(x))))
	if(length(url)==1) url <- URLencode(enc2utf8(url))
	return(url)
}

uniq_address <- trimws(unique(last_sentence(as.character(affiliation$address),last = 2)))
google_api_url <- google_geocode_url(uniq_address)

message(paste0(length(uniq_address), " address to resolve with Google Geocoding API."))

write.csv(uniq_address,file = "data-raw/affiliation/uniq_address.csv")

#
# json_list <- lapply(google_api_url, function(url) try(fromJSON(file=url)))
