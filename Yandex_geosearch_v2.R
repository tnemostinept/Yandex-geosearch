yandex_geosearch_bb <- function(x, coord_left_low, coord_right_up, apikey, export = F) {
require(jsonlite)
  request <- paste(unlist(strsplit(x, split = " ")), collapse = "%20")
  coord1 <- unlist(strsplit(coord_left_low, split = ", "))
  coord1 <- paste(coord1[2], coord1[1], sep = ",")
  coord2 <- unlist(strsplit(coord_right_up, split = ", "))
  coord2 <- paste(coord2[2], coord2[1], sep = ",")
  first_part <- "https://search-maps.yandex.ru/v1/?apikey="
  url_compl <- gsub(" ", "", paste(first_part, apikey, "&text=", request, "&type=biz&lang=ru_RU&", "bbox=", coord1, "~", coord2, "&results=500", collapse = ""))
  full_req <- fromJSON(paste(readLines(url_compl, encoding = "UTF-8"), collapse=""))
  req_data <- full_req$features
  prop <- req_data$properties
  geo <- req_data$geometry
  vec_geo <- unlist(geo$coordinates)
  geo_df <- data.frame(lat = vec_geo[seq(2,length(vec_geo), by = 2)], lon = vec_geo[seq(1,length(vec_geo), by = 2)])
  total <- cbind(prop$name, prop$description, prop$CompanyMetaData$url, geo_df)
  colnames(total) <- c("Name", "Address", "URL", "Lat", "Lon")
  if (export == TRUE) {
    write.csv(total, 'request_api_result.csv')
  }
  return(total)
}