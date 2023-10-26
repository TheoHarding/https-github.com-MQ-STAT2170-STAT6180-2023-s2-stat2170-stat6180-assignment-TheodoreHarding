
cake <- read.csv("cake.csv")


#Part A
table(cake$Temp, cake$Recipe)

#temp and recipe do have an impact on the angle the cake broke but the  interaction variable is insignificant 

#It is balanced - there is the same number of subjects in each cohort

#Part B
boxplot(cake$Angle~cake$Temp)

boxplot(cake$Angle~cake$Recipe)

#Part C
av = aov(formula = Angle~Recipe*Temp, data = cake)

#Part D
summary(av)
plot(av)  



#Part E
me <- aov(formula = Angle~Recipe+Temp, data = cake)
summary(me)
plot(me)

