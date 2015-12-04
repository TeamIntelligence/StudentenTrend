library(forecast)
HBOWOsub <- aggregate(studenten_ingeschrevenen$aantal, by=list(iscedNaam=studenten_ingeschrevenen$iscedCode.iscedNaam,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)

mydata <- tapply(HBOWOsub$x, HBOWOsub$iscedNaam, ts, start=1990, 2014, frequency=1)


plot(mydata$Architectuur)

fit <- ets(mydata$Architectuur)
plot(fit)
plot(forecast(fit, 5))


fit <-  HoltWinters(mydata$Architectuur)
plot(fit)
plot(forecast(fit, 10))

