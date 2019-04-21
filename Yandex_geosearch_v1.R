yandex_geosearch <- function(x) {
  require(jsonlite)
  require(urltools)
  request <- unlist(strsplit(x, split = " "))
  russian_text <- url_encode(paste(request, collapse = "%20"))
  first_part <- "https://search-maps.yandex.ru/v1/?apikey=2b3cdec3-9ea3-4479-a389-e61e995fda2b&text="
  second_part <- "&type=biz&lang=ru_RU&results=500"
  url <- paste(first_part, russian_text, second_part, collapse = "")
  site <- fromJSON(paste(readLines(url, encoding = "UTF-8"), collapse=""))
  x <- lapply(site, data.frame)
  z <- site$features
  prop <- z$properties
  geo <- z$geometry
  vec_geo <- unlist(geo$coordinates)
  geo_df <- data.frame(lat = vec_geo[seq(2,length(vec_geo), by = 2)], lon = vec_geo[seq(1,length(vec_geo), by = 2)])
  total <- cbind(prop$name,prop$description,geo_df)
  colnames(total) <- c("Название", "Адрес", "Широта", "Долгота")
  return(total)
}