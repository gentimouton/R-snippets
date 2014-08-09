# data
temp = c(88.6,71.6,93.3,84.3,80.6,75.2,69.7,82.0,69.4,83.3,79.6,82.5,80.6)
chirps = c(20.0,16.0,19.8,18.4,17.1,15.5,14.7,17.1,15.4,16.2,15.0,17.2,16.0)
# build lm
temp.lm <- lm(temp ~ chirps)
summary(temp.lm)
# predict how many chirps for 18 temp?
df=data.frame(chirps=18)
predict(temp.lm, newdata=df)