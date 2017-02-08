
data(Prestige, package="car")
summary(Prestige)
# Table for income:
with(Prestige, tapply(income, list(type), mean, na.rm=TRUE))
# Table for prestige:
with(Prestige, tapply(prestige, list(type), mean, na.rm=TRUE))
scatterplotMatrix(~census+education+income+prestige+women, reg.line=FALSE, 
  smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), 
  id.n=0, diagonal = 'histogram', data=Prestige)
scatterplotMatrix(~census+education+income+prestige+women, reg.line=FALSE, 
  smooth=TRUE, spread=FALSE, span=0.7, ellipse=FALSE, levels=c(.5, .9), 
  id.n=0, diagonal = 'histogram', data=Prestige)
Prestige$log2income <- with(Prestige, log2(income))
RegModel.2 <- lm(prestige~education+log2income+women, data=Prestige)
summary(RegModel.2)
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(RegModel.2)
par(oldpar)
qqPlot(RegModel.2, simulate=TRUE, id.method="y", id.n=2)
vif(RegModel.2)
RegModel.3 <- lm(prestige~education+log2income, data=Prestige)
summary(RegModel.3)
compareCoefs(RegModel.2, RegModel.3)

