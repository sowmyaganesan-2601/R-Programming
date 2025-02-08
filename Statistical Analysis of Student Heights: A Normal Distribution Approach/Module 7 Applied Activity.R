"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM1001 Module 7 Applied Activity
****************************************************************
"
###########################################################################################							   	                  
#Question #1 -  Read in the “ClassHeights.csv” data set that contains the heights in 
#centimetres of the students in the class. Assume that heights can be modelled with the 
# normal distribution. 			
##########################################################################################

##########################################################################################
#Question 1a) - Create a histogram of the student heights. Do you think the normal distribution 
# assumption is reasonable? Explain. 
##########################################################################################
# Store the dataset in variable criminals
heights <- read.csv('ClassHeights.csv')

# Create a histogram using R
hist(heights$Height..cm., col = "green", main = "Histogram of Student Heights", xlab = "Height", ylab = "Frequency")

# Right - Skewed Distribution
# The normal distribution assumption is reasonable because the histogram follows a bell-shaped curve
# resembling a normal distribution

##########################################################################################
#Question 1b) - Calculate the mean and standard deviation of the heights.
##########################################################################################
numeric_height <- as.numeric(heights$Height)
numeric_height
any(is.na(heights$Height))

#Calculate the mean of the height 
mean_height <- mean(heights$Height)
mean_height
# Mean of the heights are 166.1875

# calculate the standard deviation of the height
sd_height <- sd(heights$Height)
 sd_height
 #Standard deviation of the heights are 9.801045
 
##########################################################################################
 #Question 1c) - What heights do 95% of students fall between?
 ##########################################################################################

 lower_bound <- mean_height - 2 * sd_height
 upper_bound<- mean_height + 2 * sd_height
# lower_bound is 146.5854
# upper_bound is  185.7896
#Heights of about 95% of students fall between approximately 146.5854 cm and 185.7896 cm.
 
##########################################################################################
 #Question 1d) - Calculate the z‐scores for the shortest and tallest students in the class ‐ are they 
 # considered outliers? 
#########################################################################################
 height_shortest <- min(heights)
 height_tallest <- max(heights)
 z_score_shortest <- (height_shortest - mean_height) / sd_height
 z_score_shortest
 # Z-score for the shortest students in the class is -1.855669
 
 z_score_tallest <- (height_tallest - mean_height) / sd_height
z_score_tallest
# z-score for the tallest students in the class is 2.480603

# Based on the z-scores calculated, they are not exceeding 3 or -3, the corresponding student is not considered an outlier.

##########################################################################################
#Question 1e) - What is the probability that a student is less than 160cm?
##########################################################################################
 pnorm(160,166.1875,9.801045,lower.tail = TRUE)
 # probability that a student is less than 160cm is 0.2639188

 ##########################################################################################
 #Question 1f) -  What is the probability that a student is taller than 180cm?
 ##########################################################################################
 pnorm(180,166.1875,9.801045,lower.tail = FALSE)
 # probability that a student is taller than 180cm is 0.07937494

 ##########################################################################################
 #Question 1g) -   What is the probability that a student is between 165cm and 175cm?
 ##########################################################################################
 pnorm(165,166.1875,9.801045,lower.tail = TRUE) - pnorm(175,166.1875,9.801045,lower.tail = TRUE)
 # probability that a student is between 165cm and 175cm is -0.3639287

 ##########################################################################################
 #Question 1h) -   What height in cm corresponds to the 90th percentile? 
 ##########################################################################################
qnorm(0.90,166.1875,9.801045,lower.tail = TRUE)
 # 178.748 cm is the height in cm corresponds to the 90th percentile

 ##########################################################################################
 #Question 1i) -   What height in cm corresponds to the bottom 15% of heights?
 ##########################################################################################
 qnorm(0.15,166.1875,9.801045,lower.tail= TRUE)
 qnorm(0.85,166.1875,9.801045,lower.tail=FALSE)
 # 156.0294 cm is the height in cm corresponds to the bottom 15% of heights
 
 ##########################################################################################
 #Question 1j) -What TWO heights in cm do the MIDDLE (LAST TWO DIGITS OF STUDENT 
 # NUMBER)% of the data lie between?
 ##########################################################################################
# Student ID = A00295227
#Last two digits are 27
 
 lower_bound <- mean_height - 1.5 * sd_height
 lower_bound 
 # Lower bound is 151.4859 for middle of 27% 
 
 upper_bound<- mean_height + 1.5 * sd_height
 upper_bound
 # Upper bound is 180.8891 for middle of 27% 
 
 
 
 
 