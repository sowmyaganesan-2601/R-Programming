"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM1001 Module 5 Applied Activity
****************************************************************
"
###########################################################################################							   	                  
"Question #1 - Use functions in R and the prob package to answer the following questions regarding 
rolling a 9‐sided die.	"		
##########################################################################################

##########################################################################################
"
Question 1a) Create a vector that contains the outcomes for rolling a 9‐sided die once. Use the 
sample function to simulate 1000 rolls of the die. Create a plot that shows the 
probability of rolling a 6 after each roll and add a line that shows the theoretical 
probability of rolling a 6. HINT: follow the code completed in class for rolling a die 1000 
times and obtaining 3s. 
"
##########################################################################################

#vector for all the possible outcomes for rolling a 9-sided die once
outcomes <- c(1,2,3,4,5,6,7,8,9)

#Sample function to simulate 1000 rolls of the die
sample1000<-sample(outcomes,1000,TRUE)
sample1000

six<-ifelse(sample1000==6,1,0)
six

#Plot that shows the probability of rolling a 6 for each roll
plot(1:1000,  # x axis values from 1 to 1000 rolls
     cumsum(six)/(1:1000), # percentage of 3s rolled after each roll
     xlab ="Number of Rolls",
     ylab = "Percent of 6s Rolled",
     main ="Probability of 6s in 1000 Rolls",type ="l") #Line Chart
#add a line that shows the theoretical that shows theoretical probability of rolling a 6.
abline(h=1/9,col="blue")  

##########################################################################################
"
Question 1b) - Repeat part a. but simulate rolling the die 10 000 times. What do you notice about the 
relationship between the empirical and theoretical probability when you roll the die 
more times? "
##########################################################################################
#Repeat the same process a 1a) but simulate rolling the die for 10000 times
sample10000<-sample(outcomes,10000,TRUE)
three <-ifelse(sample10000==6,1,0)
plot(1:10000,cumsum(three)/(1:10000),xlab="Number of Rolls",ylab="Percent of 6s Rolled",
     main="Probability of 6s in 10000 Rolls",type="l")
abline(h=1/9,col="Blue")
"
Relationship between empirical and theretical probability is :-
If the rolls of the die are less then the percentage of 3s are all over the place ,when we roll
the die more the empirical probability of rolling a 6 (represented by the blue line) tends to converge towards the theoretical probability of rolling a 6 (represented by the red dashed line). 
This convergence is a result of the law of large numbers, which states that the empirical probability of an event approaches its theoretical probability as 
the number of trials increases.
"
##########################################################################################
"Question 1c) - Using the prob package, use the rolldie() function to create a probability space for 
rolling a 9‐sided die two times"
##########################################################################################
#use roll die function to create probability space for rolling a 9-side die two times
die<-rolldie(2,9,makespace =TRUE)
die 

##########################################################################################
"Question 1d) - Find the probability that the sum of two rolls of the 9‐sided die is greater than 6."
##########################################################################################
Prob(die,X1+X2 >6)

#Probability for sum of two rolls of the 9-sided die is greater than 6 is 0.8148148

##########################################################################################
"Question 1e) -Find the probability that the sum of two rolls of 9‐sided die is less than 15."
##########################################################################################
Prob(die,X1+X2 < 15)

#Probability for sum of two rolls of 9-sided die is less than 15 is 0.8765432

##########################################################################################
"Question 1f) - Find the probability that the sum of the rolls of two 9‐sided die is less than 15 AND 
greater than 6. "
##########################################################################################
Prob(die,X1 +X2 <15 & X1+X2 >6 )
#  The  probability that the sum of the rolls of two 9‐sided die is less than 15 AND 
# greater than 6 is  0.691358

##########################################################################################
"Question 1g) - Find the probability that the sum of the rolls of two 9‐sided die is greater than 15 AND 
less than 6. Explain this result."
##########################################################################################
Prob(die,X1 +X2 >15 & X1+X2 <6 )
# the probability that the sum of the rolls of two 9‐sided die is greater than 15 AND 
# less than 6 is 0

" Explaination for the result :-
Probability that the sum of the rolls of two 9‐sided die is greater than 15 AND less than 6is 0 because 
When rolling two 9-sided dice, the minimum possible sum is 2 (when both dice show a 1), 
and the maximum possible sum is 18 (when both dice show a 9). 
Therefore, it's not possible for the sum of two dice rolls to be less than 6 and greater than 15 at the same time."

##########################################################################################
"Question 1h) - Find the probability that the sum of the rolls of two 9‐sided die is less than 15 OR 
greater than 6. Explain this result. "
##########################################################################################
Prob(die,X1 +X2 < 15 | X1+X2 >6)

# The probability that the sum of the rolls of two 9‐sided die is less than 15 OR 
# greater than 6 is 1 

#Since the events sum of rolls greater than 6 and sum of rolls less than 15 cover all possible outcomes, 
#they are mutually exclusive. Therefore, the probability of their intersection is 0. 
# Hence, the probability of the event sum of rolls greater than 6 or less than 15 (exclusive) is simply the sum of the probabilities of the individual events.

##########################################################################################
# Question 2a) - Create a contingency table for the NEWS variable and 
# ONE of your personalized categorical variables (variable 7 or variable 8). Choose whichever 
# variable you think may have a relationship with watching the news. Create a question that you 
# are interested in answering about watching the news that uses ONE of the probability rules: 
# complement (NOT), addition (OR), conditional (given), or multiplication (AND). For example, in 
# my personalized data set I could find the probability of watching the news given I score my 
# productivity level a 5 (productivity level is my categorical variable of choice) which would be 
# denoted P(News = YES | Productivity = 5) .  
# Note: on Case Study 2 you will have to make a question for each of the probability rules." 
##########################################################################################
library(rpivotTable)
personalized <- read.csv('personal.csv',header=TRUE)
head(personalized)
rpivotTable(personalized) 
"
#Probability Rule 1 - Conditional (GIVEN)

#1) What is the probability of listening to music GIVEN you spent day happily
=P(listening.music| Happy) = 15/19 = 78.95%

Explanation for the above rule result - The rule above used is Conditional (Given)
where we found the probability of listening to music given that the day spent happily
, the probability of this to happen is 78.95 % which means the listening music will make the 
better.
"
#In R:-
prop.table(table(personalized$Listening.music, personalized$Day), 2)[2,3] 
"
#Probability Rule 2 - Addition (OR)

#2)What is the probability of not listening to music or having busy day
P(not.listening.music or Busy) =P(not.listening.music) +P(Busy) - P(not.listening.music and busy)
                              = 20/64 +13/64 -8/64
                              = 39.06%
"
#  In R :-
prop.table(table(personalized$Listening.music))[1]+prop.table(table(personalized$Day))[2]-prop.table(table(personalized$Listening.music, personalized$Day))[1, 2]
#Explanation for the above result is that the probability of not listening to music may be because of busy day that has 39.06% 

" Probability Rule 3 - Multiplication (AND)
#3) What is the probability of listening music and having a happy day ?
P(listening.music AND happy) = P(listening.music) x P(listening.music|happy)
                             = 44/64 x 15/19
                             = 23.43%  "
#In R -
prop.table(table(personalized$Listening.music,personalized$Day))[2, 3]  
#Explanation of the above result is that probability of listening music and having a happy day is of 23.43%

#4) Are listening music and Happy spent disjoint events?
#In R -
  prop.table(table(personalized$Listening.music, personalized$Day ))[2, 3]
 #0.234 = NOT DISJOINT
  #P(listening.music and Happy) = 15/64=0.234375
  #These two events are not disjoint as they are not 0
 