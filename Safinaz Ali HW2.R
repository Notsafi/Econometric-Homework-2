# Safnaz Ali 
# Homework 2
# 09/08/2022

#Group 1: John Robison & Suguru Iwashiro

#Hypothesis: If i roll the dice 100 times the dice will be not fair
#Null Hypothesis: If i roll the dice 1000 times the dice will be fair

#PP3- 100 DICE ROLLS
how_many_rolls <- 100
sim_rolls <- sample(1:6, how_many_rolls, replace = TRUE)
if_come_up_6 <- as.numeric(lots_of_sim_rolls == 6)
mean(if_come_up_6)
sd(sim_rolls,na.rm = TRUE)
np <- how_many_rolls*0.167 #1/6 = 0.167 
(sum(if_come_up_6)-np)/sqrt(np*(1-0.167))
t.test(sim_rolls,var.equal = TRUE) 


#Based on this stimulation we can judge the dice can be fair or not by 
#evaluating the z score and seeing if the data is within the standard of error which is
#taking the standard deviation and dividing it by the square root of the sample size.
#Based on the z-score i fail to reject my null hypothesis because I received 6 less then 16% of the rolls 

#EPn- 1,000 DICE ROLLS  
how_many_rolls <- 1000
sim_rolls <- sample(1:6, how_many_rolls, replace = TRUE)
if_come_up_6 <- as.numeric(lots_of_sim_rolls == 6)
mean(if_come_up_6)
sd(sim_rolls,na.rm = TRUE)
np <- how_many_rolls*0.167 #1/6 = 0.167 
(sum(if_come_up_6)-np)/sqrt(np*(1-0.167))
t.test(sim_rolls,var.equal = TRUE) 

#To also see if the dice is fair or not to determine if our hypothesis is right 
#i ran the t test score for the 100 rolls and 1000 rolls to distinguish if the the p-value is less then 0.5 
#in which we fail to reject the null hypothesis since the t value is less then -2 and greater then +2 which makes it acceptable
#because the higher the t-value the greater the confidence we have.   



