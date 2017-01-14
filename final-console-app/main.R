# Подключаем необходимые библиотеки.
library(httr)
library(jsonlite)

# Настраиваем основные параметры
tsCategoryLength = 10 # количество временных рядов в каждой категории
elementsInTs = 100 # количество элементов во временном ряде
outputFile = "output.csv" # имя файла, куда будут записываться результаты
#outputSourceDir = "./ts-categories/" # директория, куда будут записываться значения временных рядов

# Действия
needTSRegen = TRUE # генерация временных рядов
needModelsExec = TRUE # вычисление SMAPE различных моделей
needResultsCalc = TRUE # расчёт результатов
needResultsSave = TRUE # сохранение результатов

# Генератор временных рядов.
tsGen <- function(count = 100, seasonLoop = 10, trendCoef = 0, seasonCoef = 0, noiseCount = 0.1, noiseCoef = 0) {
  result = numeric(count)
  noise = rnorm(count, sd = noiseCount)
  season = rnorm(seasonLoop)
  
  # add trend
  for(pt in 1:count) {
    currentVal = trendCoef * pt / count
    result[pt] = result[pt] + currentVal
  }
  
  # add season
  for(pt in 1:count) {
    seasonPt = pt %% seasonLoop + 1
    currentCoef = 1 + seasonCoef * pt / count
    result[pt] = result[pt] + season[seasonPt] * currentCoef
  }
  
  # add noise
  for(pt in 1:count) {
    currentCoef = 1 + noiseCoef * pt / count
    result[pt] = result[pt] + noise[pt] * currentCoef
  }
  
  return(result)
}

# model - D,T,S.
salxApi <- function(X, Y, model, grade = 1, actualCount = 10, forecastCount = 10) {
  ROW = data.frame(X,Y)
  url = paste("http://salx.pw/api/FModel/", model, sep = "")
  
  body = list(
    Name = unbox("VladdyTS"),
    ROW = ROW,
    order = unbox(grade),
    ForecastCount = unbox(forecastCount),
    ActualCount = unbox(actualCount)
  )
  response = POST(url = url, encode = "json", body = body)
  content = content(response, "text")
  result = fromJSON(content)
  return(result)
}

# Заполнение SMAPE-ов одной модели
fillModel <- function(lst, model, grade) {
  mapeVector = c()
  for (idx in 1:tsCategoryLength) {
    result = NULL
    result = salxApi(1:elementsInTs, lst[[idx]], model, grade = grade)
    mapeVector = c(mapeVector, result["MSE_e"][[1]])
  }
  return(mapeVector)
}

# Заполнение SMAPE-ов моделей.
fillModels <- function(tsCategory) {
  tsCategory$models = NULL
  
  tsCategory$models$D1$mapes = fillModel(tsCategory$data, "D", 1)
  tsCategory$models$D2$mapes = fillModel(tsCategory$data, "D", 2)
  tsCategory$models$D3$mapes = fillModel(tsCategory$data, "D", 3)
  
  tsCategory$models$T1$mapes = fillModel(tsCategory$data, "T", 1)
  tsCategory$models$T2$mapes = fillModel(tsCategory$data, "T", 2)
  tsCategory$models$T3$mapes = fillModel(tsCategory$data, "T", 3)
  
  tsCategory$models$S1$mapes = fillModel(tsCategory$data, "S", 1)
  tsCategory$models$S2$mapes = fillModel(tsCategory$data, "S", 2)
  tsCategory$models$S3$mapes = fillModel(tsCategory$data, "S", 3)
  
  return(tsCategory)
}

if (needTSRegen) {
  tsCategories = NULL
  
  for (idx in 1:tsCategoryLength) {
    tsCategories$TS01$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 0, seasonLoop = 1, noiseCount = 0)
    tsCategories$TS02$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = -1, seasonLoop = 1, noiseCount = 0)
    tsCategories$TS03$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 1, seasonLoop = 1, noiseCount = 0)
    
    tsCategories$TS04$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 0, seasonLoop = 10, noiseCount = 0)
    tsCategories$TS05$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 0, seasonLoop = 10, seasonCoef = -0.5, noiseCount = 0)
    tsCategories$TS06$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 0, seasonLoop = 10, seasonCoef = 2, noiseCount = 0)
    
    tsCategories$TS07$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 0, seasonLoop = 1, noiseCount = 0.1)
    tsCategories$TS08$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 0, seasonLoop = 1, noiseCount = 0.1)
    tsCategories$TS09$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 0, seasonLoop = 1, noiseCount = 0.1, noiseCoef = 2)
    
    tsCategories$TS10$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 1, seasonLoop = 1, noiseCount = 0.1)
    tsCategories$TS11$data[[idx]] <- tsGen(count = elementsInTs, trendCoef = 1, seasonLoop = 10, noiseCount = 0.1)
  }
}

if (needModelsExec) {
  for (idx in 1:length(tsCategories)) {
    tsCategories[[idx]] = fillModels(tsCategories[[idx]])
  }
}

if (needResultsCalc) {
  resultMatrix = c();
  resultMatrixCols = length(tsCategories[[1]]$models) * 2
  resultMatrixColNames = c()
  for (idx in 1:length(tsCategories)) {
    for (idx2 in 1:length(tsCategories[[idx]]$models)) {
      resultModelElem = tsCategories[[idx]]$models[[idx2]]
      resultModelElem$mean = mean(resultModelElem$mapes)
      resultModelElem$sd = sd(resultModelElem$mapes)
      tsCategories[[idx]]$models[[idx2]] = resultModelElem
      
      resultMatrix = c(resultMatrix, resultModelElem$mean)
      resultMatrix = c(resultMatrix, resultModelElem$sd)
      if (length(resultMatrixColNames) < resultMatrixCols) {
        resultMatrixColNames = c(resultMatrixColNames, paste(names(tsCategories[[idx]]$models)[[idx2]], "M"))
        resultMatrixColNames = c(resultMatrixColNames, paste(names(tsCategories[[idx]]$models)[[idx2]], "S"))
      }
    }
  }
  resultMatrix = matrix(resultMatrix, ncol = resultMatrixCols, byrow = TRUE)
  rownames(resultMatrix) <- names(tsCategories)
  colnames(resultMatrix) <- resultMatrixColNames
}

if (needResultsSave) {
  write.table(resultMatrix, file = outputFile, sep = ";", col.names = NA)
}