

fix(White_wines)
RegModel.1 <- 
  lm(quality~alcohol+density+residual.sugar+volatile.acidity, 
  data=White_wines)
summary(RegModel.1)

influencePlot(RegModel.1, id.n=3)



