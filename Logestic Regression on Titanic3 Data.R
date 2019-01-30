#Data
require(PASWR)
titanic3

head(titanic3)

#here we are interested in servived variables.

qplot(age,survived, data = titanic3) + geom_smooth(se = FALSE)

require(mgcv)
options(na.action = "na.exclude")

gmod <- gam(survived ~ s(age), data = titanic3, family = "binomial")
summary(gmod)

#predicated value of y(here probabilities)

fitted(gmod)
predict(gmod, type = "response")

#prediction graph
qplot(age,survived, data = titanic3) + 
    geom_smooth(se = FALSE)+
    geom_line(aes(y = fitted(gmod)), color = "red")
