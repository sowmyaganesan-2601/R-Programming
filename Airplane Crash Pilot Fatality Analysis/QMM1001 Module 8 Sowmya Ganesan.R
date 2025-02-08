"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM1001 Module 8 Applied Activity
****************************************************************
"

###########################################################################################	
# Question 1 ) Researchers studied airplane crashes and determined that 5.2% of the time 
# the pilot died.  A sample of 8400 airplane crashes was taken
##########################################################################################

##########################################################################################
#Question 1a) - Check that np and nq are both greater than 10
##########################################################################################

# Given data
p <- 0.052  # Probability of pilot dying in a crash
n <- 8400   # Sample size
q <- 1-p
# Calculate np and nq
np <- n * p
nq <- n * (q)
np
nq
# np is 436.8
# nq is 7963.2
# np and nq both are greater than 10
##########################################################################################
#Question 1b) - Find the standard deviation of the sampling distribution of the proportion.
##########################################################################################
sd<-sqrt(p*q/n)
sd
# Standard Deviation of the sampling distribution of the proportion is 0.002422513

##########################################################################################
#Question 1c) - What is the probability that 425 or fewer pilots will die?
##########################################################################################

probability <- pbinom(425,   n, p,)
probability
 # Probability that 425 or fewer pilots will die is 0

##########################################################################################
#Question 1d) -  What is the probability that more than 500 pilots die?
##########################################################################################
probability_more_than_500 <- 1- pbinom(500,   n, p)
probability_more_than_500
# Probability that more than 500 pilots die is 0

##########################################################################################
#Question 1e) -  What is the probability that between 400 and 465 pilots will die? 
##########################################################################################
prob <- pbinom(465,n,p,lower.tail=TRUE)-pbinom(400,n, p, lower.tail=TRUE)
prob
#Probability between 400 and 465 pilots will die is 0

##########################################################################################
#Question 1f) -  Determine the 10th percentile for the proportion of pilot deaths. Report your 
# answer as a NUMBER of pilots.
##########################################################################################
qbinom(0.1,n,p)
# Number of pilots are 44

##########################################################################################
#Question 2a) -  Case Study 2 Check Point Statista reports that Canadians access the news 59% = 
# 0.59 of the time. You have recorded personalized data throughout the semester. Let the sample 
# size n be the number of days that you collected data. Find the sample proportion of the number 
# of days that you watched the news. What is the probability of getting a value less than or equal 
# to your sample proportion given that the population proportion is 0.59? 
  
##########################################################################################
# As  I have taken variable Listening_music instead of watching_news so I am continuing this question by taking
# the variable listening_music
  
# sample proportion of the number of days that I Listened to music
  days_listen = 51   # number of days I listened to music is 51 
  n = 85  # sample size 
  sample_proportion = days_listen / sample_size 
  sample_proportion
# sample proportion of the number of days I listened to music is 0.6
  p= 0.59
  q= 1-p
  p_hat = 0.6
  SD = sqrt(p*q/n)  # Standard Deviation 
  Z = (p_hat - p)/ SD
  
  pnorm(0.6,p,SD,lower.tail = TRUE)
  # 1 is the probability of getting a value less than or equal to your sample proportion given that the population proportion is 0.59
    