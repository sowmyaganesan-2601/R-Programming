"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM1001 Module 3 Applied Activity
****************************************************************
"

########################################################################################							   	                  
#Question #1 -The data set “House.Price.csv” shows house price and the amount of living 
#space of the house in square feet. Read this data set into R and use it to answer the 
#following questions.  			
#######################################################################################

##########################################################################################
#Question 1a) - Find the mean of the house price variable. 
##########################################################################################
# Load the dataset
house.data <- read.csv("House.Price.csv")

# Calculate the mean of the house price variable
mean.house.price <- mean(house.data$House.Price)
mean.house.price

#mean of the house variable is 220287.9

##########################################################################################
#Question 1b) - Find the standard deviation of the house price variable. 
##########################################################################################
 # calculate the standard deviation of the house price variable
sd.house.price <- sd(house.data$House.Price)
sd.house.price

#Standard Deviation of house variable is 94463.06

##########################################################################################
#Question 1c) -Find the summary (five number summary + mean) of the house price variable. 
##########################################################################################
# Calculate the summary of the house price variable
summary(house.data$`House.Price`)

# Summary of house price variable
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#52500  149975  199950  220288  273300  590000 

#Mean of the house variable 
(mean.house.data<-mean(house.data$House.Price))

#Mean of the house variable is 220287.9


##########################################################################################
#Question 1d) - Find the z‐scores for the house price variable. How many outliers are there?
 ##########################################################################################
# Calculating z-scores for the house price variable
house.data$z.score<-(house.data$House.Price - mean.house.data)/sd.house.price
head(house.data)

# z-scores for the house price variable
#House.Price Living.Area     z.score
#1      218000        1580 -0.02422007
#2      158900        1100 -0.64986146
#3      229000        2560  0.09222757
#4       94000        1064 -1.33690251
#5      233000        2080  0.13457216
#6      176000        1718 -0.46883832

# Calculating outliers for the housing variable
(outliers.housing<-subset(house.data,z.score > 3 | z.score < -3))

# There are 12 outliers for the housing price variable 
#10       505000        1738 3.014005
#39       550500        3152 3.495674
#249      569000        2972 3.691518
#290      530000        4859 3.278658
#357      523500        3982 3.209848
#683      575000        3080 3.755035
#748      539000        3068 3.373934
#850      567500        4208 3.675639
#867      535000        3596 3.331589
#918      542500        3047 3.410985
#986      590000        3690 3.913827
#990      538000        4137 3.363348

##########################################################################################
#Question 1e) - Create a boxplot for the house price variable. Create an appropriate title and 
#colour the box in any colour (other than white). How many outliers are there?
##########################################################################################
# Creating a box plot for the house price variable
bp.housing<-boxplot(house.data$House.Price,col="Blue",outcol="Red",main="Housing Price",ylab="Housing Price")
bp.housing$out

# Outliers are 
# [1] 505000 550500 490000 490000 569000 530000 460000 523500 496000 479074 486000 501000 575000 465000 539000 495000 567500 535000
# [19] 542500 500000 590000 538000

##########################################################################################
#Question 1f) - What do you notice about the house prices that are outliers? 
##########################################################################################

# Outliers may indicate anomalies or irregularities in the housing market.
#Outliers represent house prices that are significantly higher or lower than the majority of the dataset. 
#They are typically located far from the main body of data points on the boxplot.

##########################################################################################
# Question 1g) - Create a histogram of the house price variable. Add an appropriate title and x 
# and y axis labels. Change the color of the bars to any color other than white and 
# the color of the borders to any color other than black. Describe the shape of this 
# distribution (symmetric, left skewed, right skewed).
##########################################################################################
# Creating histogram for the house price variable

hist(house.data$House.Price, col = "lightgreen", border = "darkblue",
     main = "Histogram of House Prices",
     xlab = "House Price", ylab = "Frequency")
#Right Skewwed distribution

##########################################################################################
# Question 1h) -Transform the house price variable using a log transformation. Create a 
# histogram for the log of house prices. What do you notice about the shape of the 
# distribution? 
##########################################################################################

#Transforming the house price variable using log transformation 
loghouse.price<-log10(house.data$House.Price)

#Creating a histogram for the log of housing prices 
hist(loghouse.price)

#Shape of the distribution is Symmetric 

##########################################################################################
# Question 2) -  The data set “Workforce.csv” shows the year, annual average workforce 
#participation (the percentage of the population 16 years and older that is employed or 
#unemployed), male workforce participation (the annual average for males only), and 
#female workforce participation (the annual average for females only). Read this data 
#into R and use it to answer the following questions.  
##########################################################################################

##########################################################################################
# Question 2a) Create a time series object for the annual average workforce participation, male 
# workforce participation, and female workforce participation using the ts() 
# function. This time series object should start in year 1948 and end in year 2015 
##########################################################################################

# Read the dataset workforce.csv into the variable workforce 
workforce<-read.csv("Workforce.csv",header=TRUE)

#Create a time series object for 
workforce.ts<-ts(subset(workforce,select=c("Annual.Average","Male","Female")),start=1948,end=2015)
workforce.ts

#Time Series object is 
'Time Series:
Start = 1948 
End = 2015 
Frequency = 1 
Annual.Average Male Female
1948       58.84167 86.6   32.7
1949       59.03333 86.6   33.2
1950       59.16667 86.3   33.8
1951       59.28333 86.5   34.6
1952       59.05833 86.3   34.7
1953       58.88333 86.0   34.5
1954       58.78333 85.6   34.6
'
# New Column names are :-
colnames(workforce.ts)<-c("Annual Average","Male","Female")
workforce.ts

##########################################################################################
# Question 2b) Plot the time series data. Add an appropriate title.
##########################################################################################
#Plotting the time series data 
plot.ts(workforce.ts,main="Time series plot for housing prices",plot.type ="multiple")

##########################################################################################
# Question 2c)  Briefly describe the trends in average workforce participation, male workforce 
# participation, and female workforce participation over time.
##########################################################################################
# The trend in average workforce participation over time  exhibit fluctuations reflecting economic conditions initially.then 
# gradually keeps on increasing over time.

#For male workforce participation,during initial years there was high participation from them 
#later on decreased continously

#For female workfoce participation,during initial years their participation was very low as years passed by 
# it increased significantly

##########################################################################################
# Question 3) Case Study 1 Check Point Choose a quantitative variable from your 
# personalized data set. Create a histogram with an appropriate title and x and y axis 
# labels. Is the distribution skewed? If it is symmetric, calculate the mean and standard 
# deviation. If it is asymmetric, calculate the median and IQR. You must also upload your 
# personalized data set to the assignment drop box.
##########################################################################################
# Read the data set and store to variable personalized
personalized<-read.csv('personalized.csv',header=TRUE)

#Creating histogram for my personalized dataset 

hist(personalized$Water.Bottle,col = "lightgreen", border = "darkblue",
       main = "Histogram of Personalized Data",
       xlab = "Water Bottle", ylab = "Frequency")

#No the distribution is not skewed
#The distribution is symmetric 
# Calculating the mean of the distribution
(mean.personalized<- mean(personalized$Water.Bottle))

# The mean is 4.277778

# Calculating the standard deviation of the distribution 

#The standard deviation is 1.766532