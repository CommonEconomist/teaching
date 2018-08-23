# R code test 1

str(X)

# 1
mean(X$temp.anomaly)
sd(X$temp.anomaly)
                                                                      
# 2
boxplot(X$temp.anomaly)

# 3
plot(X$temp.anomaly,type="l")

# 4
sum(X$elnino)
sum(X$elnino)/nrow(X)

# 5
sum(X$elnino[X$elnino.strength=="strong" | X$elnino.strength=="very strong" ])

# 6
table(X$elnino.strength)
sum(X$elnino[X$elnino.strength=="very strong"])/sum(X$elnino)

# 7
d<-table(X$elnino,X$elnino.l)
prop.table(d,2)

# 8
boxplot(X$ond~X$elnino.strength)

# 9
d<-table(X$ond.above,X$elnino)
prop.table(d,1)

# 10
d<-table(X$ond.abov,X$elnino.strength)
prop.table(d,1)
