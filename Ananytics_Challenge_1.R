PSID = read.csv("PSID.csv", header = TRUE)
summary(PSID)
plot(PSID)
head(PSID)
age_step=ecdf(PSID$age)
plot(age_step, cex.main=0.75)


mean(PSID$earnings)

median(PSID$earnings)


mean(PSID$hours)

median(PSID$hours)

head(PSID)

kc=kmeans(PSID[,6:7],3)
kc

par(mfrow=c(1,2))
plot(PSID[,2:3], col=kc$cluster)
plot(PSID, cex.main = 0.75)


plot(PSID[,4:5], cex.main = 0.75)
plot(PSID[,8:9], cex.main = 0.75)

kc = kmeans (PSID[4:9], 5)
plot(PSID[,4:9], col=kc$cluster , cex.main = 0.75)

#Meaningful
plot(PSID[,6:7], col=kc$cluster , cex.main = 0.75)

plot(PSID[,4:5], col=kc$cluster , cex.main = 0.75)
plot(PSID[,5:6], col=kc$cluster , cex.main = 0.75)



plot(PSID[,6:7], col=kc$cluster , cex.main = 0.75)
points(kc$centers[, 6:7], col=6:7, pch=8, cex=2)
