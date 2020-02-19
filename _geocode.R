#' # 使用 Google API 解析地址
#'
#' 首先将地址列表和解析程序上传到已经部署完开发环境的外网服务器上，然后
#' 在外网服务器上运行解码程序，调用 Google Geocoding API 解析作者地址，
#' 保存解析结果（JSON格式）到 RDS 中。
#'
#' 解析结果下载到本地，进行分析和可视化。

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
# saveRDS(json_list,file = "json_list.RDS")

json_list <- readRDS("data-raw/affiliation/json_list.RDS")

success <- lapply(json_list, function(x){
   return(x$status)
})

#' 解析结果如下所示。
#'
#' 外网访问真好，压根不存在失去响应的情况。

unlist(success) %>% table()
##     OK ZERO_RESULTS
##   5379           23

#' 如何处理纷繁复杂的作者地址？
#'
#' 选项一：针对每一篇文章中所有列出的地址，使用 Google Geocoding API 解析，解析成功的，
#' 使用解析结果中的 `formatted_address` 替代原有地址，这一操作可视为对**地址字符串进行标准化**。
#' 与此同时，将解析得到的坐标点与地址对应起来。~~坐标点可以用来评估地点的唯一性，即可设一个阈值，~~
#' ~~当坐标位置接近时，视为同一个研究机构（这个操作不能区分开相邻的不同研究机构）。~~
#'
#'
#' ## 结果分析
#'
#' ZERO_RESULTS 说明没有任何地址返回，而成功的解析结果则可能会有多个不同结果。
#' 针对于解析成功的，统计结果数目。
#'
count_result <- function(object){
  if (hasName(object,"status")) status <- object$status
  if (status == "ZERO_RESULTS"){
    return(0)
  } else if (status == "OK"){
    return(length(object$results))
  } else {
    return(NA)
  }
}

result_len <- unlist(lapply(json_list, count_result))
barplot(table(result_len),xlab = "结果数目",ylab = "数量",
        main = "同一地址存在解析出多个结果的情形")

#' 看一下出现多个结果的地址有哪些。

idx <- which(result_len>1)
ambiguous_address <- data.frame(id=idx,address=uniq_address[idx],nResult=result_len[idx])
DT::datatable(ambiguous_address,
              filter = "top")

#' # 画地图
#'
#' ## 获取坐标
#'
#' 简便起见，针对同一地址解析出多个结果的情况，我们只取其第一个。
#'

# 获取经纬度
geocode_results_geometry_location <- function(object){
  if (hasName(object,"status")) status <- object$status
  if (status == "ZERO_RESULTS"){
    return(NA)
  } else if (status == "OK"){
    return(object$results[[1]]$geometry$location)
  } else {
    return(NA)
  }
}

# 获取国家
geocode_results_address_country <- function(object){
  if (hasName(object,"status")) status <- object$status
  if (status == "ZERO_RESULTS"){
    return(NA)
  } else if (status == "OK"){
    components <- object$results[[1]]$address_components
    is_country <- lapply(components, function(x){
       types <- x$types
       any(types %in% "country")
    })

    id <- which(unlist(is_country)==TRUE)
    if (length(id)<1) return(NA)
    components[[id]]$long_name
  } else {
    return(NA)
  }
}

location <- lapply(json_list, geocode_results_geometry_location)
location <- do.call("rbind",location) %>% data.frame()

country <- unlist(lapply(json_list, geocode_results_address_country))

if(nrow(location) == length(uniq_address)) {
  address <- data.frame(id = 1:nrow(location),
                        address = uniq_address,
                        lat = unlist(location$lat),
                        lng = unlist(location$lng),
                        country=country)
}

#' ## 画地图
#'
#' 首先是一副点图。每个点都是一个位置。
world <- map_data('world')
world <- world[world$region != "Antarctica", ]
world <- world %>% left_join(address %>%
                               group_by(country) %>%
                               summarise(n=n()) %>%
                               ungroup() %>%
                               mutate(region=ifelse(country=="United States","USA",country)))

ggplot()  +
  geom_map(aes(long,lat,group=group,map_id=region),
           color="lightblue",
           fill="grey",
           alpha=1/3,
           map = world,
           data = world) +
  geom_point(aes(lng,lat),data=address,alpha=1/5,size=1) +
  # geom_density2d(aes(lng,lat),data = address) +
  theme_void() +
  theme(plot.background = element_blank())


#' 按国家统计
cols <- brewer.pal(5,name = "Blues")
ggplot()  +
  geom_map(aes(long,lat,group=group,map_id=region,fill=n),
           color="lightblue",
           map = world,
           data = world) +
  scale_fill_gradient(low = cols[[1]],high = cols[[5]],na.value = "white",trans="log10") +
  # geom_point(aes(lng,lat),data=address,alpha=1/5,size=1) +
  # geom_density2d(aes(lng,lat),data = address) +
  theme_void() +
  theme(plot.background = element_blank())
