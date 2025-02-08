"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM1001 Module 4 Applied Activity
****************************************************************
"

########################################################################################							   	                  
#Question #1 -The data set “Penguins.csv” shows the duration of underwater dives 
# (minutes) and the heart rates (beats per minute) of emperor penguins – some of the 
# best divers among birds! Read this data set into R and answer the following questions. 			
#######################################################################################

##########################################################################################
#Question 1a)- Create a scatterplot of duration vs. heart rate. Add appropriate axis titles, a main 
# title, and change the marker symbol to anything other than the default open 
# circle.
##########################################################################################
# Load the dataset in the variable penguins
penguins <- read.csv('Penguin.csv')

# Create a scatterplot
plot(penguins$Duration, penguins$Dive.Heart.Rate, 
     xlab = "Duration (minutes)", ylab = "Heart Rate (beats per minute)",
     main = "Emperor Penguin Dives",
     pch = 20)  # Change marker symbol to solid circle (pch = 20)

##########################################################################################
#Question 1b)- Find the correlation coefficient between the two variables. State the direction 
# and strength of the relationship. 
##########################################################################################
# Calculate the correlation coefficient
cor(penguins)

# Duration.min. Dive.Heart.Rate
# Duration.min.       1.0000000      -0.8457572
# Dive.Heart.Rate    -0.8457572       1.0000000

##########################################################################################
#Question 1c)- Find the equation of the regression line to predict a penguin’s heart rate based 
# on the duration of the dive. 
##########################################################################################
penguins.line <-lm(Dive.Heart.Rate ~Duration.min., penguins)

#Retrieve slope and intercept
penguins.line$coefficients
#(Intercept) Duration.min. 
# 96.901980     -5.468023 
##########################################################################################
#Question 1d)- State and interpret the slope and y‐intercept in the context of the problem.
##########################################################################################
# Slope: The slope of the regression line represents the average change in the response variable (heart rate) 
# for each one-unit increase in the predictor variable (duration of the dive)

# y-intercept : The y-intercept is the value of the response variable (heart rate) 
# when the predictor variable (duration of the dive) is zero.

# Interpreting the slope and y-intercept in the context of the problem 
# As the slope is negative it would suggest that longer dive durations are associated with lower predicted heart rates.
##########################################################################################
#Question 1e)- State and interpret the slope and y‐intercept in the context of the problem.
##########################################################################################
#Plot the regression line and the equation of the line (in text) on your scatterplot. 
# Change the colour of the line and text to any colour other than black. 

plot(penguins$Duration.min., penguins$Dive.Heart.Rate, 
     xlab = "Duration (minutes)", ylab = "Heart Rate (beats per minute)",
     main = "Emperor Penguin Dives",
     pch = 20)  # Change marker symbol to solid circle (pch = 20)

#Plot the Equation of the line (in text) on the scatter plot 
abline(penguins.line,col='green')
text(15,125,cex=0.8,"Dive.Heart.Rate = -5.468023 x Duration.min. + 96.901980")

##########################################################################################
#Question 1f) - Using your regression line, what is the predicted heart rate of a penguin that 
# dives for 14 minutes? Show the calculation using R
##########################################################################################
# Creating dataframe
heartrate<-data.frame(Duration.min. = c(14))
heartrate
#Predicted heart rate of a penguin dives for 14 min
predict.lm(penguins.line,heartrate)
#      1 
# 20.34966 
##########################################################################################
# Question 2 - Case Study 1 Check Point Find the correlation coefficients for all pairs of 
# quantitative variables in your personalized data set. To do this, you will need to create a 
# subset of data that only contains quantitative variables using the following command:

#  subset(name of data, select = c(“quantitative variable 1”, “quantitative variable 2”, …)) 

# Identify which TWO variables have the highest correlation and state the direction and 
# strength of that relationship. You must also upload your personalized data set to the 
# assignment drop box. 
##########################################################################################
# Load the dataset to the variables
quantitative.variables
#create a subset of data that only contains quantitative variables
subset.data <- subset(quantitative.variables, select = c("Zoom", "Study", "Sleep", "Water.Bottle", "Listening.music", "Happiness","Steps"))
View(subset.data)

#Finding the highest correlation coefficient of two variables
cor(subset.data)

# Water.Bottle and Zoom are the pair of variable having highest correlational coefficient
# 0.43559207

#Plot the two variables having highest correlation coefficient 
plot(x=subset.data$Zoom , y=subset.data$Water.Bottle , main = "Filling the no. of Water Bottles Depends on Hours I Slept on Zoom", xlab = "Water Bottles", ylab = "Hours Spent on Zoom", pch=7, col="darkblue")


