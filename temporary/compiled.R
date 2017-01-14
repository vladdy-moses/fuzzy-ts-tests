simpleList <- list(); # t(n)s(0,0)n(0,0)
downTrendList <- list(); # t(f,-1)s(0,0)n(0,0)
upTrendList <- list(); # t(g,1)s(0,0)n(0,0)

simpleWithSeasonList <- list(); # t(n)s(1,0)n(0,0)
downSeasonList <- list(); # t(n)s(1,-0.5)n(0,0)
upSeasonList <- list(); # t(n)s(1,2)n(0,0)

simpleWithNoiseList <- list(); # t(n)s(0,0)n(0.1,0)
downNoiseList <- list(); # t(n)s(0,0)n(0.1,0.1)
upNoiseList <- list(); # t(n)s(0,0)n(0.1,2)

complexNoSeasonList <- list(); # t(1)s(0)n(0.1)
complexWithSeasonList <- list(); # t(1)s(10)n(0.1)

for (idx in 1:100) {
  simpleList[[idx]] <- tsGen(trendCoef = 0, seasonLoop = 1, noiseCount = 0)
  downTrendList[[idx]] <- tsGen(trendCoef = -1, seasonLoop = 1, noiseCount = 0)
  upTrendList[[idx]] <- tsGen(trendCoef = 1, seasonLoop = 1, noiseCount = 0)
  
  simpleWithSeasonList[[idx]] <- tsGen(trendCoef = 0, seasonLoop = 10, noiseCount = 0)
  downSeasonList[[idx]] <- tsGen(trendCoef = 0, seasonLoop = 10, seasonCoef = -0.5, noiseCount = 0)
  upSeasonList[[idx]] <- tsGen(trendCoef = 0, seasonLoop = 10, seasonCoef = 2, noiseCount = 0)
  
  simpleWithNoiseList[[idx]] <- tsGen(trendCoef = 0, seasonLoop = 1, noiseCount = 0.1) ##????
  downNoiseList[[idx]] <- tsGen(trendCoef = 0, seasonLoop = 1, noiseCount = 0.1)
  upNoiseList[[idx]] <- tsGen(trendCoef = 0, seasonLoop = 1, noiseCount = 0.1, noiseCoef = 2)
}

fillMape <- function(list, modelType) {
  mapeVector = c()
  for (idx in 1:100) {
    result = NULL
    result = salxApi(1:100, list[[idx]], modelType)
    mapeVector = c(mapeVector, result["MSE_e"][[1]])
  }
  return(mapeVector)
}

fillMapes <- function(list) {
  result = list()
  result["D"][[1]] = fillMape(list, "D")
  result["T"][[1]] = fillMape(list, "T")
  result["S"][[1]] = fillMape(list, "S")
  return(result)
}

print(Sys.time())

simpleMape = fillMapes(simpleList)
downTrendMape = fillMapes(downTrendList)
upTrendMape = fillMapes(upTrendList)

print(Sys.time())

simpleWithSeasonMape = fillMapes(simpleWithSeasonList)
downSeasonMape = fillMapes(downSeasonList)
upSeasonMape = fillMapes(upSeasonList)

print(Sys.time())

simpleWithNoiseMape = fillMapes(simpleWithNoiseList)
downNoiseMape = fillMapes(downNoiseList)
upNoiseMape = fillMapes(upNoiseList)

print(Sys.time())

complexNoSeasonMape = fillMapes(complexNoSeasonList)
complexWithSeasonMape = fillMapes(complexWithSeasonList)

print(Sys.time())