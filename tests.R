
source("return_calculations.R")
errCount <- 0

# test ccinterest
y1 <- ccinterest(0,123)
if (y1 != 1.1) {
  print("Test ccinterest not passed")
  errCount <- errCount + 1
}

y2 <- ccinterest(0.1,5)
y2_true <- exp(0.1*5)
if ( realequality(y2,y2_true) != TRUE ) {
  print("Test ccinterest not passed")
  errCount <- errCount + 1
}

sprintf('Number of errors %3i',errCount)
