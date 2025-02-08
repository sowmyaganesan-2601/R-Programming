"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM1001 Module 10 Applied Activity
****************************************************************
"

###########################################################################################		
# Question 1)  In a survey, it was found that 20% of all Canadians aged 15 or older had a 
# university degree. You interview a random sample of Canadians this year to see whether 
# that proportion has changed. The random sample of 250 people results in a sample 
# proportion of 0.25.
##########################################################################################

##########################################################################################
#Question 1a) -State the null and alternative hypotheses 
##########################################################################################
Null Hypothesis H_0 : p = 0.20
# Null hypothesis represents no change from the known population proportion.

Alternate Hypothesis H_A : p != 20
# Alternate hypothesis represents a change in the proportion, either an increase or a decrease.
  
##########################################################################################
#Question 1b) Check the four conditions required to perform a hypothesis test and explain why 
# they are met or not
############################################################################################
# 1) Independence Assumption - Random sample selected are independent
# 2) Randomization Condition - sample selected are random
# 3) 10% Condition - 250 people is less than 10% of the total population
# 4) Success/Failure condition - 
n = 250
p = 0.20
np <- n* p
np    # np is 50 >10 
nq <- n*(1-p)
nq    # nq is 200 >10 


##########################################################################################
#Question 1c) Perform the hypothesis test at the 0.05 level of significance and report the p‐
# value.
############################################################################################

p_hat <- 0.25
n <- 250
p0 <- 0.20
sd <- sqrt(p0 *(1-p0)/n)
# sd is 0.02529822  
z<- (p_hat -p0) / sd
# z   is 1.976424   

pnorm(z,0,1,lower.tail = FALSE )

p.value <- 2* pnorm(z,0,1,lower.tail = FALSE )
p.value

# p.value is 0.04810683

##########################################################################################
#Question 1d)Clearly state if you reject or fail to reject the null hypothesis and interpret the 
# decision in the context of the problem
############################################################################################
# Since p.value is 0.04810683 which is less than level of significance , reject the null hypothesis and 
# conclude that there is evidence to suggest the proportion of 
# Canadians aged 15 or older who have a university degree is different from 20%.
# The hypothesis test indicates a significant change in the proportion of Canadians aged 15 or older who have a 
# university degree compared to the previously known proportion of 20%.

############################################################################################
#Question 1e) Change the level of significance to 0.10. How does that change your decision?
############################################################################################
"
since the p-value (0.0481) is still less than the new level of significance (0.10), your decision remains the same: reject the null hypothesis.

Changing the level of significance from 0.05 to 0.10 does not change your decision in this particular case. 
You continue to reject the null hypothesis and conclude that there is evidence to suggest that the proportion of Canadians aged 15 or
older who have a university degree is different from 20%.
"
############################################################################################
#Question 2) Find the sample proportion of the number of days that 
# you watched the news. The general population of Canadians watches the news 59% of the time. 
# Depending on your sample proportion test the following:
############################################################################################

############################################################################################
#Question 2a)  If your sample proportion is more than 0.59 test the hypothesis that you watch the 
# news more than the general population of Canadians. 
############################################################################################
"
when the sample proportion is greater than 0.59 :
  
Null Hypothesis H_0 : p <= 0.20
Alternate Hypothesis H_A : p > 20
"
# I have taken variable Listening_Music instead of Watching news 

n= 99    # Sample Size
days_listened = 59     # No. of days I Listened Music
p_hat = days_listened/n
p_hat          # p_hat is 0.5959596
p0 <- 0.59
sd <- sqrt(p0 *(1-p0)/n)
sd             # sd is 0.04943111
z<- (p_hat -p0) / sd
z             # z is  0.1205637

pnorm(z,0,1,lower.tail = FALSE )

p.value <- pnorm(z,0,1,lower.tail = FALSE )
p.value     # p.value is 0.4520183

#  0.05 is the level of significance 
# p.value > 0.05
# p value is greater than level of significance so we failed to reject the null hypothesis.

############################################################################################
#Question 2b) If your sample proportion is less than 0.59 test the hypothesis that you watch the news 
# less than the general population of Canadians. 
############################################################################################
 "
 when the sample proportion is less than 0.59:
Null Hypothesis H_0 : p <= 0.20
Alternate Hypothesis H_A : p > 20
"
# I have taken variable Listening_Music instead of Watching news 

n= 99    # Sample Size
days_listened = 59     # No. of days I Listened Music
p_hat = days_listened/n
p_hat          # p_hat is 0.5959596
p0 <- 0.59
sd <- sqrt(p0 *(1-p0)/n)
sd            # sd is 0.04943111
z<- (p_hat -p0) / sd
z             # z value is 0.1205637

pnorm(z,0,1,lower.tail = TRUE)

p.value <- pnorm(z,0,1,lower.tail = TRUE )
p.value

# p.value is 0.5479817 
#  0.05 is the level of significance 
# p.value > 0.05 , As p-value is greater than level of significance we have failed to reject the null hypothesis.

 