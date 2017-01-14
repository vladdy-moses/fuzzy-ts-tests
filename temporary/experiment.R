test <- function(lst = list()) {
  print(mean(lst["D"][[1]]))
  print(mean(lst["T"][[1]]))
  print(mean(lst["S"][[1]]))
  print(sd(lst["D"][[1]]))
  print(sd(lst["T"][[1]]))
  print(sd(lst["S"][[1]]))
}
