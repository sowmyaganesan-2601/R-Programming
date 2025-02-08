"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM1001 Module 9 Applied Activity
****************************************************************
"

###########################################################################################		
# Question 1) A group of 350 employees is randomly selected at a large company. They are 
# asked if they spend greater than 4 hours a day looking at a computer screen. 288 
# responded yes. 
##########################################################################################

##########################################################################################
#Question 1a) - Find a 95% confidence interval for the population proportion of employees that 
# spend greater than 4 hours a day looking at a computer screen.
##########################################################################################
n <- 350          # Sample Size

p_hat <- 288/n      #Sample Proportion
p_hat
q_hat <- 1- p_hat
q_hat
SE <- sqrt(p_hat * q_hat/n)       # Standard Error
z.crit <- qnorm(0.95/2+0.5, 0, 1, lower.tail=TRUE)
p_hat-z.crit*SE
p_hat+z.crit*SE

# (0.78,0.86)

##########################################################################################
#Question 1b) - Assume that the sample proportion stays the same but the number of 
# employees selected is increased to 1400. Find an 85% confidence interval for the 
# population proportion of employees that spend greater than 4 hours a day 
# looking at a computer screen.
##########################################################################################
n<- 1400               # Sample Size is increased to 1400
p_hat <- 0.8228571
q_hat <- 1-p_hat

SE <- sqrt(p_hat * q_hat/n)
z.crit <- qnorm(0.85/2+0.5, 0, 1, lower.tail=TRUE)
p_hat-z.crit*SE
p_hat+z.crit*SE
# (0.81,0.84)

##########################################################################################
#Question 1C) - Is the confidence interval in part b wider or more narrow than the interval in 
# part a? Explain the result in relation to the sample size and level of confidence
##########################################################################################
"
Sample Size: In part b, the sample size is increased to 1400 employees compared to 350 employees in part a. As the sample size increases, the standard error decreases.
This reduction in standard error leads to a narrower confidence interval. A larger sample size provides more precise estimates of the population parameter, 
resulting in a tighter range around the sample proportion.

Level of Confidence: Part a has a 95% confidence level, while part b has an 85% confidence level. 
When the confidence level decreases, the corresponding z-score decreases as well. 
A lower z-score results in a smaller margin of error and, consequently, a narrower confidence interval. 
Therefore, the decrease in the level of confidence in part b contributes to the narrower interval.

In summary, the narrower confidence interval in part b compared to part a is a result of both the larger sample size and the lower level of confidence.

"
"
##########################################################################################
#Question 2) - Write a function that takes as input the sample proportion, the margin of 
error, and the confidence level and outputs the required sample size. Using your 
function, answer the following question: It is believed that 20% of adults over the age of 
50 never graduated from high school. We wish to see if this percentage is the same 
among the 25 to 30 age group. How many of this younger age group must be surveyed 
in order to estimate the proportion of non‐graduates to within 5% with 95% 
confidence?  
##########################################################################################
"
#p_hat = 0.20
#ME = 0.05 
#cl = 0.95
# Define a function that takes as input the sample proportion, the margin of error, and the confidence level
CI<-function(p.hat, ME, cl)          
  {
  #p.hat = sample proportion, enter as decimal
  #n = sample size
  #ME = Marginal Error
  #cl = confidence level, enter as decimal
  
  q.hat<-1-p.hat #find probability of failure
   z.crit<-qnorm(cl/2+0.5, 0, 1, lower.tail=TRUE) #find critical value based on level of confidence
  n <- ceiling((z.crit/ME)^2*p.hat*q.hat)
  return (n)  #Return the confidence interval as output
}

CI(0.20,0.05,0.95)

# Sample Size is 246 i.e. 246 youngsters need to be surveyed in order to estimate the proportion of non‐graduates to within 5% with 95% 
# confidence

"
##########################################################################################
#Question 3) - Find the sample proportion of the number of days that 
you watched the news. Create and INTERPRET a 95% confidence interval for the proportion of 
days that you watch the news. 
##########################################################################################
"
# I have taken variable Listening_Music instead of Watching news 

n= 92    # Sample Size
days_listened = 54     # No. of days I Listened Music
p_hat = days_listened/n
q_hat <- 1- p_hat
q_hat
SE <- sqrt(p_hat * q_hat/n)       # Standard Error
z.crit <- qnorm(0.95/2+0.5, 0, 1, lower.tail=TRUE)
p_hat-z.crit*SE
p_hat+z.crit*SE
