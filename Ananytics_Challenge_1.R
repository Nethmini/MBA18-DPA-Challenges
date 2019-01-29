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
kc$cluster

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

plot(density(PSID))
remove(
  
  
table(PSID$married)


#2019-01-28
PSID = read.csv("PSID.csv", header = TRUE)
View(PSID)
barplot(table(PSID$married))
levels(PSID$married)
table(PSID$married)
barplot(xtabs(~PSID$married), space = F, col = rainbow(7), ylab = "Frequency")
barplot(xtabs(~PSID$married), space = F, col = rainbow(7),legend.text = T, main = "RECORDS BY MARRIED" , ylab = "Frequency")
barplot(xtabs(~PSID$married), space = F, col = rainbow(length(levels(PSID$married))), main = "RECORDS BY MARRIED" , ylab = "Frequency")
PSID_temp <- read.csv("PSID.csv")
head(PSID_temp)


married = subset(PSID, married %in% "married")
View(married)
str(PSID)
pie(table(PSID$married) , col = rainbow(7))
pie(table(PSID$married)/length(PSID$married) , col = rainbow(7))
show(table(PSID$married)/length(PSID$married))
str(PSID)

View(summary(PSID))
show(summary(PSID$age))
show(table(PSID$married)/length(PSID$married))
pie(table(PSID$married)/length(PSID$married) , col = rainbow(7))

#WORK
psid_non_working = subset(PSID, hours == 0)
nrow(subset(PSID, hours > 1811))
nrow(subset(PSID, hours > 4000))
show(psid_non_working)

head(psid_non_working)

psid_non_working[, c("age", "kids", "married")]
nrow(psid_non_working)
nrow(PSID)     
nrow(psid_non_working)/nrow(PSID)

#KIDS
barplot(xtabs(~PSID$kids), space = F, col = rainbow(12), main = "Frequency of No of KIDS" , ylab = "Frequency", xlab = "No of Kids")
show(table(PSID$kids))


psid_kids = subset(PSID, kids < 11)
summary(psid_kids)
barplot(xtabs(~psid_kids$kids), space = F, col = rainbow(12), main = "Frequency of No of KIDS" , ylab = "Frequency", xlab = "No of Kids")
show(table(psid_kids$kids))


#EARNING

hist(PSID$earnings)
boxplot(PSID$earnings)
summary(PSID$earnings)

#AGE
hist(PSID$age)
boxplot(PSID$age, horizontal = T)

#KMEANS
View(PSID)
PSID.features = PSID
PSID.features$married <- NULL
PSID.features$Seq.No <- NULL
View(PSID.features)
head(PSID.features)

kmeans_results = kmeans(na.omit(PSID.features), 3)
kmeans_results
kmeans_results$cluster
kmeans_results$size

table(PSID$married, kmeans_results$cluster)

plot(PSID)
plot(PSID[c("earnings", "hours")], col = kmeans_results$cluster)
plot(PSID[c("age", "kids")], col = kmeans_results$cluster)
plot(PSID[c("educatn", "earnings")], col = kmeans_results$cluster)
plot(PSID[c("hours", "earnings")], col = PSID$married)
