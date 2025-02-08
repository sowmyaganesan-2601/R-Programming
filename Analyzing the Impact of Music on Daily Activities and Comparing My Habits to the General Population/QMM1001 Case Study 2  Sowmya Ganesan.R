"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM 1001 Case Study 2 [20%]
****************************************************************
"
#####################################################################################	
# Part 1 -: Does Listening music  affect how you spend your day?  
#####################################################################################

#####################################################################################	
# contingency table for the NEWS variable and one of your personalized categorical 
# variables (variable 7 or variable 8).
#####################################################################################
# I dont have NEWS variable instead i have taken Listening music variable 
# categorical variable is Happiness 

#Create a contingency table for the Listening music  and Happiness scale 1 to 5 in a day
personal<- read.csv('personal.csv')
  rpivotTable(personalized)  #using rpivotTable function to build a contingency table
  
 #####################################################################################	
 "
  Using the contingency table, you will formulate four questions to capture how watching the 
 news relates to your chosen personalized categorical variable. Each question must be answered 
 using a different type of probability rule: 
 a. Question 1 must be answered using the rule of complement (NOT) 
 b. Question 2 must be answered using an addition rule for probability (OR) 
 c. Question 3 must be answered using a conditional probability (GIVEN) 
 d. Question 4 must be answered using a multiplication rule for probability (AND)
 "
 #####################################################################################
 # Q1) Did Not Listen to music ?
 not_listen_music <- prop.table(table(personalized$Listening.music))[1]  # Row index 1 is "Did Not Watch News"
not_listen_music
 
#Q2) What is the probability of not listening to music or having busy day
prop.table(table(personalized$Listening.music))[1]+prop.table(table(personalized$Day))[2]-prop.table(table(personalized$Listening.music, personalized$Day))[1, 2]

#Q3 ) What is the probability of listening to music GIVEN you spent day happily
prop.table(table(personalized$Listening.music, personalized$Day), 2)[2,3] 

#Q4 ) What is the probability of listening music and having a happy day ?
prop.table(table(personalized$Listening.music,personalized$Day))[2, 3]

#####################################################################################	
# 3) Are any of the events in the contingency table disjoint? Explain how you know and how these 
# findings relate to your daily activities.  
#####################################################################################
prop.table(table(personalized$Listening.music, personalized$Day ))[2, 3]

#These two events are not disjoint as they are not 0

#####################################################################################	
# 4) Check for independence between two events of your choice in the contingency table. 
#####################################################################################
# Are Listening Music and Overall Day Spent Happily independent events?
prop.table(table(personalized$Listening.music, personalized$Day))[2, 3]

prop.table(table(personalized$Listening.music))[2]*prop.table(table(personalized$Day))[3]
# these are not equal so dependent events 

#####################################################################################	
# Part 2 -:   Do you watch the news more than other people? 
#####################################################################################

#####################################################################################	
"
1) Statista reports that 59% = 0.59 of Canadians access the news daily. This will be used as the 
population proportion. Find and report the proportion of days that you watch the news AND the 
proportion of days that you do not watch the news. The proportion of days that you watch the 
news will be used as the sample proportion. Compare your sample proportion and the 
population proportion by stating if you watch the news more than, less than, or about the same 
amount as the general Canadian population and comment on why this is the case. 
"
#####################################################################################

# I have taken listening to music variable instead of watching news
n= 101    # Sample Size
days_listened = 59     # No. of days I Listened Music

p_hat = days_listened/n
# p_hat is the sample proportion 
p_hat          # p_hat is 0.5959596

# sample proportion is 0.5841584

############################################################################################
# 2) What would the mean and standard deviation of the normal model be? Report these values. 
############################################################################################
mean <- mean(personalized$Listening.music)
mean
# mean is 0.5841584

sd <- sd(personalized$Listening.music)
sd
# standard deviation is  0.4953247

############################################################################################
# 3)  Find the probability of getting a value LESS THAN OR EQUAL to your sample proportion using 
# 0.59 as the population proportion. Interpret what this value means.  
############################################################################################
n= 101    # Sample Size
days_listened = 59     # No. of days I Listened Music

p_hat = days_listened/n
# p_hat is the sample proportion 
p_hat          # p_hat is 0.5959596

# sample proportion is 0.5841584
p0 <- 0.59
sd <- sqrt(p0 *(1-p0)/n)
sd             # sd is 0.04943111
z<- (p_hat -p0) / sd
z             # z is  0.1205637

pnorm(z,0,1,lower.tail = TRUE)
#  probability of getting a value LESS THAN OR EQUAL to your sample proportion using 
# 0.59 as the population proportion is 0.4524935

############################################################################################
# 4) Create a 95% confidence interval for the proportion of days that you watch the news. In your 
# report, include the confidence interval using the correct notation and interpret what it means. 
# Does it seem that you watch the news more than 50% of the time? Explain how you know.   
############################################################################################
n= 101   # Sample Size
days_listened = 59    # No. of days I Listened Music
p_hat = days_listened/n
p_hat
q_hat <- 1- p_hat
q_hat
SE <- sqrt(p_hat * q_hat/n)       # Standard Error
z.crit <- qnorm(0.95/2+0.5, 0, 1, lower.tail=TRUE)
p_hat-z.crit*SE
p_hat+z.crit*SE

# (0.4880378,0.6802791)

############################################################################################
# 5)  Test the hypothesis that you watch the news more or less than the general population of 
# Canadians (that watch the news 59% of the time). Make a choice for the alternative hypothesis 
# (more or less) based on how much you watch the news. In your report, write out the hypotheses 
# using the proper notation, state the p‐value, make a final decision (reject or do not reject) and 
# interpret that decision in context (do you watch the news more or less than a general 
#                                    Canadian?).
############################################################################################
# # I have taken variable listening to music instead of watching news

"

""
Null Hypothesis H_0 : p = p0
Alternate Hypothesis H_A : p < 20
Since your sample proportion is less than the general population's proportion, this is a one-tailed test.
""
 when the sample proportion is less than 0.59:

"
# I have taken variable Listening_Music instead of Watching news 

n= 101   # Sample Size
days_listened = 59     # No. of days I Listened Music
p_hat = days_listened/n
p_hat          # p_hat is 0.5841584
p0 <- 0.59
sd <- sqrt(p0 *(1-p0)/n)
sd            # sd is 0.04893924
z<- (p_hat -p0) / sd
z             # z value is  -0.119364

pnorm(z,0,1,lower.tail = TRUE)

p.value <- pnorm(z,0,1,lower.tail = TRUE )
p.value

# p.value is 0.4524935

# p.value > 0.05
# p value is greater than level of significance so we failed to reject the null hypothesis.
