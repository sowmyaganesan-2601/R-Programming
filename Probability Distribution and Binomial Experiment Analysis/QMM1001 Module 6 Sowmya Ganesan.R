"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM1001 Module 6 Applied Activity
****************************************************************
"
###########################################################################################							   	                  
"Question #1 -Use functions in R to create a probability distribution.   	"		
##########################################################################################

##########################################################################################
"
Question 1a) Create a probability distribution in R for the possible 
earnings shown on the spinner. Note that GAME OVER 
means $0 in earnings and that all pieces of the spinner are 
the same size. You must create two vectors, one for the 
outcomes for the random variable X = earnings and one for 
the probabilities P(X) of landing on each option, and bind 
them together to create a probability distribution
"
##########################################################################################
# Create a vector for the Random Variable X which is variable X 
X= c(0,10,5,0,50)
#Create a vector for probabilities 
probs <- c(1/5,1/5,1/5,1/5,1/5)
spinner_bind <-cbind(X, probs)
spinner_bind
"
X probs
[1,]  0   0.2
[2,] 10   0.2
[3,]  5   0.2
[4,]  0   0.2
[5,] 50   0.2
"
##########################################################################################
"
Question 1b) - . Sample from this distribution 5 times, 500 times, and 50 000 
times. In each case, create a frequency table and bar plot for 
the frequency table"
##########################################################################################
#Sample for distribution 5 
sample5<-sample(X,5,prob=probs,TRUE)
sample5
 
# Frequency table for distribution 5 
freq_sample5 <-table(sample5)
freq_sample5

barplot(freq_sample5, main = "Frequency Table : 5", xlab = "Outcomes", ylab = "Frequency",col="black")

#Sample for distribution 500 
sample500<-sample(X,500,prob=probs,TRUE)
sample500

# Frequency table for distribution 500
freq_sample500 <-table(sample500)
freq_sample500

barplot(freq_sample500, main = "Frequency Table : 500", xlab = "Outcomes", ylab = "Frequency",col="black")

#Sample for distribution 50000 

sample50000<-sample(X,50000,prob=probs,TRUE)
sample50000

# Frequency table for distribution 50000

freq_sample50000 <-table(sample50000)
freq_sample50000

barplot(freq_sample50000, main = "Frequency Table : 50000", xlab = "Outcomes", ylab = "Frequency",col="black")

##########################################################################################
"
Question 1c) -For each of the three simulations in part b) calculate the experimental/empirical 
expected value and variance
"
##########################################################################################
#Emperical expected value of the 5 times distribution 
 emp.exp5 <- mean(sample5)
emp.exp5

#Variance of the 5 times distribution
var.sample5 <- var(sample5)
var.sample5
# Variance of the 5 times distribution is 417.5

#Emperical expected value of the 500 times distribution 
emp.exp500 <- mean(sample500)
emp.exp500
# Emperical expected value of the 500 times distribution is 13.49

#Variance of the 500 times distribution
var.sample500 <- var(sample500)
var.sample500
#Variance of the 500 times distribution is 377.6252

#Emperical expected value of the 50000 times distribution 
emp.exp50000 <- mean(sample50000)
emp.exp50000
#Emperical expected value of the 50000 times distribution is 377.6252

#Variance of the 50000 times distribution
var.sample50000 <- var(sample50000)
var.sample50000
#Variance of the 50000 times distribution is 357.1812

##########################################################################################
"
Question 1d) - Calculate the theoretical expected value and variance for the probability 
distribution
"
##########################################################################################
# Theoretical expected value for the probability distribution 
theoretical_exp_value <- sum(X * probs)
theoretical_exp_value
# Theoretical expected value for the probability distribution  is 13
# variance for the probability distribution
variance <- sum((X-theoretical_exp_value ) ^ 2 * probs)
variance
# variance for the probability distribution  356

##########################################################################################
"
Question 1e) - If this was a carnival game and you had to pay $7 to spin the wheel once for a 
chance to win $50, would you play? Explain using the findings from b – d
"
##########################################################################################
"
Whether to play the carnival game depends on comparing the expected value and variance with the cost and potential payout. 
If the expected value is positive and the variance is manageable, it might be worth playing. However, 
if the variance is high or the expected value is negative, it might not be advisable to play.
These findings from parts b to d help inform the decision about whether to play the carnival game. 
If the expected value outweighs the cost and the risk, it might be worth playing; otherwise, it might be better to avoid playing.
"
###########################################################################################							   	                  
"Question #2 -In the spinner game above, you do not want to spin Game Over. You spin the 
wheel 7 times and count how many Game Overs you receive.  
  	"		
##########################################################################################

##########################################################################################
"
Question 2a) - . Is this a binomial experiment? Check each of the 4 conditions and explain your 
reasoning"
##########################################################################################
"
To determine whether the scenario described constitutes a binomial experiment, we need to check four conditions:
  
1) Fixed number of trials: The number of spins is fixed at 7= n.
2) Each trial is independent: The outcome of one spin does not affect the outcome of another spin.
3) Two possible outcomes: The outcomes are "Game Over" or "Not Game Over."
4) Constant probability of success: The probability of getting 'Game Over' remains the same for each spin = p.

Given that all four conditions are met, we can conclude that this scenario constitutes a binomial experiment
"

##########################################################################################
"
Question 2b) - Create a probability distribution for the number of Game Overs spun in 7 spins"
##########################################################################################
#by Binomial Model
choose(7,0) * (3/5)^0*(2/5)^(7-0)
#Answer is = 0.0016384

dbinom(0,  7, 2/5)
#0.0016384
##########################################################################################
"
Question 2c) - Find the expected value and variance for the distribution "

##########################################################################################
 # Expected value for the distribution 
# n*p
7*(2/5)
# Expected Value is 2.8

#Variance for the distribution
# n*p*q
7*(2/5)*(3/5)
# Variance Value is 1.68

##########################################################################################
"
Question 2d) - Find the probability of spinning more than 3 Game Overs " 

##########################################################################################
sum(dbinom(4:7, 7, 2/5))
# Probability of spinning more than 3 Game Overs is 0.289792
##########################################################################################
"
Question 2e) Find the probability of spinning less than 2 Game Overs  " 

##########################################################################################
sum(dbinom(0:1, 7, 2/5))
# Probability  of spinning less than 2 Game Overs is 0.1586304
##########################################################################################
"
Question 2f)  Find the probability of spinning at least 1 Game Over  " 

##########################################################################################
sum(dbinom(1:7, 7, 2/5))
# Probability of spinning at least 1 Game Over is 0.9720064
##########################################################################################
"
Question 2g) Find the probability of spinning more than 1 but less than 6 Game Overs
" 
##########################################################################################
sum(dbinom(2:5, 7, 2/5))
# Probability of spinning more than 1 but less than 6 Game Overs is 0.822528
