yandex_geosearch_bb <- function(x, coord_left_low, coord_right_up, apikey, export = F) {
require(jsonlite)
require(openxlsx)
#First, prepare request phrase and convert coordinates from 'lat, lon' into 'lon, lat' format
  request <- URLencode(enc2utf8(x))
  coord1 <- unlist(strsplit(trimws(coord_left_low), split = ","))
  coord1 <- paste(coord1[2], coord1[1], sep = ",")
  coord2 <- unlist(strsplit(trimws(coord_right_up), split = ","))
  coord2 <- paste(coord2[2], coord2[1], sep = ",")
  
#Combine a complete url for request  
  first_part <- "https://search-maps.yandex.ru/v1/?apikey="
  url_compl <- gsub(" ", "", paste(first_part, apikey, "&text=", request, "&type=biz&lang=ru_RU&", "bbox=", coord1, "~", coord2, "&results=500", collapse = ""))

#Obtain a request results in json format  
  full_req <- suppressWarnings(fromJSON(paste(readLines(url_compl, encoding = 'UTF-8'), collapse="")))

#Desired results are stored in element, called 'feautures'. Here we take from there only name, address, url and coordinates of an object 
  req_data <- full_req$features
  if (length(req_data) < 1) {
    return('Nothing to show')
  }
  prop <- req_data$properties
  geo <- req_data$geometry
  vec_geo <- unlist(geo$coordinates)
  geo_df <- data.frame(lat = vec_geo[seq(2,length(vec_geo), by = 2)], lon = vec_geo[seq(1,length(vec_geo), by = 2)])
  
#Combine our vectors in a dataframe and save it as csv if 'export' argument was set to 'TRUE'
  total <- cbind(prop$name, prop$description, ifelse(is.null(prop$CompanyMetaData$url) == TRUE, rep(NA, length(prop$name)), prop$CompanyMetaData$url), geo_df)
  colnames(total) <- c("Name", "Address", "URL", "Lat", "Lon")
  if (export == TRUE) {
    write.xlsx(total, 'request_api_result.xlsx')
  }
  return(total)
}