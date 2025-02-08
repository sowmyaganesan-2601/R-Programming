"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM1001 Module 2 Applied Activity
****************************************************************
"

###########################################################################################							   	                  
#Question #1 - The data set “Criminal_Iowa.csv” tracks former criminals from Iowa over the 
#3‐year period after their release from prison to determine whether they were convicted 
#of a new crime during that time and returned to prison (called recidivism).			
##########################################################################################

##########################################################################################
#Question 1a) - Read this dataset into R and store the data in a variable with the name 
# of your choosing.
##########################################################################################

# Store the dataset in variable criminals
criminals <- read.csv('Criminal_Iowa.csv',header=TRUE)

##########################################################################################
#Question 1b) - Create a frequency distribution for the variable 
# “Convicting.Offense.Type” using the table() function..
##########################################################################################
# Create a convicting_offense_freq variable to save the frequency distribution table using table()

convicting_offense_freq <- table(criminals$Convicting.Offense.Type)
convicting_offense_freq

###########################################################################################
#Question 1c) - Create a relative frequency distribution for the variable 
# “Convicting.Offense.Type” using the prop.table() function.
##########################################################################################
# Create a relative frequency distribution variable to save the relative frequency distribution table using prop.table()  
relative_freq_dist <- prop.table(convicting_offense_freq)


###########################################################################################
#Question 1d) - Create a properly labelled bar chart for the “Convicting.Offense.Type” 
# variable. Be sure to include a title, x‐axis label, and y‐axis label. Change the y‐axis 
# so that it has a lower limit of 0 and an upper limit of 8000. Change the bar 
# colours so that each offense type is a different colour.
#(HINT: use c() to create a vector of colours. Colour options can be found on Moodle in the Help for R 
#section).
##########################################################################################
# Plot a Bar chart for the Convicting.offense.Type variable 
barplot(convicting_offense_freq,main = "Convicting Offense Type Distribution",xlab="Convicting Offense Type",ylab="Frequency", ylim = c(0, 8000)
        ,col=c("blue", "green", "red", "purple", "orange"))



###########################################################################################
#Question 1e) -  Create a pie chart for the “Convicting.Offense.Type” variable. Be sure 
#to include a title for the plot. 
##########################################################################################
# Plot a Pie chart for the Convicting.offense.Type variable 
pie(convicting_offense_freq,main="Convicting Offense Type Distribution")

###########################################################################################
#Question 2) The data set “Titanic_Survival_Data.csv” contains data on the survival of 
# passengers of different classes, genders, and ages. Read this data set into R and use the 
# rpivotTable function to build a contingency table or tables to answer the following 
# questions. 
##########################################################################################

###########################################################################################
#Question 2a) - Do the crew or third‐class passengers have a better chance of survival? 
# Report the percentages for each group 
##########################################################################################
titanic <-read.csv(file="Titanic_Survival_Data.csv",header = TRUE)
head(titanic)   #show first 6 rows of the dataset
table(titanic$Survival,titanic$Male.Female)
rpivotTable(titanic)  #using rpivotTable function to build a contingency table

# Survival chances of crew - 40.2%
# Survival chances of third class passengers - 32.1%

# Survival chance of crew higher than third class passengers
###########################################################################################
#Question 2b) - For males do the crew or third class passengers have a better chance 
# of survival? Report the percentages for each group. 
##########################################################################################
# For Male :-
# Survival chances of crew -  39.2%%
# Survival chances of third class passengers - 23.2%
# Survival chances of crew is higher than third class passengers for males
###########################################################################################
#Question 2c) - For females do the crew or third class passengers have a better chance 
# of survival? Report the percentages for each group. 
##########################################################################################
# For Female :-
# Survival chances of crew -   1.0%
# Survival chances of third class passengers - 8.9%
# Survival chances of third class passengers is higher than survival chances of  crew for females
###########################################################################################
#Question 2d) -  What do you think is happening?  
# Women and Children have better chances of survival than male from the above calculations
##########################################################################################


###########################################################################################
# Question 3) - Case Study 1 Check Point Choose a categorical variable from your 
# personalized data set. Create ONE visualization (a bar chart or pie chart) with 
# appropriate labels that you think is most appropriate for your chosen variable. Explain 
# your choice of visualization. You must also upload your personalized data set to the 
# assignment drop box.
##########################################################################################

# Categorical variable from the personalized dataset is Happiness
personalized_data<- read.csv(file='Ganesan, Sowmya Personalized Data.csv',header = TRUE)
head(personalized_data)

# Frequency Distribution table for the variable Day using the table() function.
Day_freq<- table(personalized_data$Day)
Day_freq

# Creating one visualization - Bar Chart
barplot(Day_freq,main = "Categorical Variable Day Distribution",xlab="Day",ylab="Frequency", ylim = c(0,10)
        ,col=c("blue", "green", "red", "purple", "orange","pink"))

# Explaination for choice of visualization - Bar Chart
# 1) I chose a bar chart because it is suitable for displaying the distribution of a categorical variable (in this case, Day variable).
# 2) The x-axis represents the overall day , and the y-axis represents the count or frequency .
# 3) I have used vector for color 






