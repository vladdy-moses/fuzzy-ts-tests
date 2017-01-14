#генератор временных рядов
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
