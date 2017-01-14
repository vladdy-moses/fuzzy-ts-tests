# Подключаем необходимые библиотеки.
library(httr)
library(jsonlite)

# model - D,T,S.
salxApi <- function(X, Y, model, actualCount = 10, forecastCount = 10) {
  ROW = data.frame(X,Y)
  url = paste("http://salx.pw/api/FModel/", model, sep = "")
  
  body = list(
    Name = unbox("VladdyTS"),
    ROW = ROW,
    order = unbox(1),
    ForecastCount = unbox(10),
    ActualCount = unbox(10)
  )
  response = POST(url = url, encode = "json", body = body)
  content = content(response, "text")
  result = fromJSON(content)
  return(result)
}