# Code for R test 2

str(d)

# 1:
hist(d$yield81to01,breaks=10)

# 2: 
mean(d$yield81to01[d$world.d==1]);sd(d$yield81to01[d$world.d==1])
mean(d$yield81to01[d$world.d==0]);sd(d$yield81to01[d$world.d==0])

# 3: 
plot(d$yield61to80,d$yield81to01)

# 4: 
points(d$yield61to80[d$world.d==1],d$yield81to01[d$world.d==1],pch=19)

# 5: 
boxplot(d$yield81to01~d$world)

# 6: 
d$reduction<-d$yield81to01-d$yield61to80
table(d$reduction<0)

# 7: 
t.test(d$yield61to80[d$world.d==0],
       d$yield81to01[d$world.d==0],paired=TRUE)

# 8: 
t.test(d$yield81to01[d$world.d==1],
       d$yield61to80[d$world.d==1],paired=TRUE,
       alternative="greater")

# 9: 
m1<-lm(yield81to01~yield61to80+world.d,d)
summary(m1)
