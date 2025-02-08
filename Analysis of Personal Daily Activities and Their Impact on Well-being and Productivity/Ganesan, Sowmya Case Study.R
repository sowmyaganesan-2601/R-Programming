"
****************************************************************
Name: Sowmya Ganesan
Student Number: A00295227

QMM 1001 Case Study 1 [20%]
****************************************************************
"

#########################################################		
# Categorical Variables
#########################################################
# Nominal Categorical variable - Overall Day 
# Store the dataset in a variable personalized

personalized <-read.csv('personal.csv')
personalized

#Frequency Table for the nominal categorical variable (Day)
#Create a  overall_day variable to save the frequency distribution table using table()

overall_day_1 <-table(personalized$Day)
overall_day_1

# Create a relative frequency distribution variable to save the relative frequency distribution table using prop.table()  
overall_day_prop <-prop.table(overall_day_1)
overall_day_prop

 # frequency table with percentages
percentage <- overall_day_prop * 100
percentage

#Using paste0() function which concatenates the input values in a single character string
overall_day<- paste0(round(percentage, 2), "%") 
overall_day

# Plot a Bar chart for the frequency distribution table
barplot(overall_day_1,main = "Overall Day Spent",xlab="Overall Day",ylab="Frequency", ylim = c(0, 40)
        ,col=c("blue", "green", "red", "purple")) 


# Ordinal Categorical variable - Happiness 

# Frequency Table for the ordinal categorical variable (Happiness)
# Create a happiness variable to save the frequency distribution table using table()

happiness <- table(personalized$Happiness)
happiness

# Create a relative frequency distribution variable to save the relative frequency distribution table using prop.table()  
happiness_prop <-prop.table(happiness)
happiness_prop 

# frequency table with percentages
percentage_happiness <-  happiness_prop * 100
percentage_happiness

#Using paste0() function which concatenates the input values in a single character string
percentage_happiness <- paste0(round(percentage_happiness, 2), "%") 
percentage_happiness

# Plot a Bar chart for the frequency distribution table
barplot(happiness,main = "Happiness Scale 1-5 ",xlab="Happiness",ylab="Frequency", ylim = c(0, 40)
        ,col=c("blue", "green", "red", "purple","orange")) 


freq_data <- table(personalized$Happiness,personalized$Day)
rpivotTable(freq_data)



#########################################################		
# Quantitative Variables
#########################################################

#########################################################		
# Quantitative Variable 1 - Zoom
#########################################################

# Quantitative variable - Hours spent on Zoom
# Plotting histogram for hours spent on Zoom
hist(personalized$Zoom,col = "lightgreen", border = "darkblue",
     main = "Histogram of Hours spent on Zoom",
     xlab = "Zoom", ylab = "Frequency")

# Calculate mean for the quantitative variable zoom
mean_zoom <- mean(personalized$Zoom)
mean_zoom

# Summary  for the quantitative variable Zoom
summary(personalized$Zoom)

# Calculate standard deviation for the quantitative variable Zoom
sd_zoom <- sd(personalized$Zoom)
sd_zoom

# Calculate IQR of the variable Zoom
IQR(personalized$Zoom)

# Plotting the box plot 
bp.zoom<-boxplot(personalized$Zoom,col="Blue",outcol="Red",main="Hours spent on Zoom",ylab="Zoom")
bp.zoom$out

# Calculating z-scores for the zoom variable
z.score<-(personalized$zoom - mean_zoom/sd_zoom)
z.score

#create a subset of data that only contains quantitative variables

subset.data <- subset(personalized, select = c("Zoom", "Study", "Sleep", "Water.Bottle", "Listening.music", "Happiness","Steps"))
View(subset.data)

#Finding the highest correlation coefficient of two variables
cor(subset.data)


#########################################################		
# Quantitative Variable 2 - Study
#########################################################

# Quantitative variable - Hours spent Studying

# Plotting histogram for hours spent Studying
hist(personalized$Study,col = "lightgreen", border = "darkblue",
     main = "Histogram of Hours Studying",
     xlab = "Study", ylab = "Frequency")

# Calculate mean for the quantitative variable Study
mean_study <- mean(personalized$Study)
mean_study

# Summary  for the quantitative variable Study
summary(personalized$Study)

# Calculate standard deviation for the quantitative variable Study
sd_study <- sd(personalized$Study)
sd_study

# Calculate IQR of the variable Study 
IQR(personalized$Study)

#Plotting the box plot for study variable
bp.study<-boxplot(personalized$Study,col="Blue",outcol="Red",main="Hours spent Studying ",ylab="Study")
bp.study$out

# Calculating z-scores for the study variable
z.score<-(personalized$Study - mean_study/sd_study)
z.score

#create a subset of data that only contains quantitative variables
subset.data <- subset(personalized, select = c("Zoom", "Study", "Sleep", "Water.Bottle", "Listening.music", "Happiness","Steps"))
View(subset.data)

#Finding the highest correlation coefficient of two variables

cor(subset.data)


#########################################################		
# Quantitative Variable 3 -  Sleep
#########################################################

# Quantitative variable - Hours spent Sleeping

# Plotting histogram for hours spent sleeping
hist(personalized$Sleep,col = "lightgreen", border = "darkblue",
     main = "Histogram of Hours spent Sleeping",
     xlab = "Sleep", ylab = "Frequency")

# Calculate mean for the quantitative variable Sleep
mean_sleep <- mean(personalized$Sleep)
mean_sleep


# Summary  for the quantitative variable Sleep
summary(personalized$Sleep)

# Calculate standard deviation for the quantitative variable sleep
sd_sleep <- sd(personalized$Sleep)
sd_sleep

# Calculate IQR of the variable Sleep
IQR(personalized$Sleep)

#Plotting the box plot for sleep variable
bp.sleep<-boxplot(personalized$Sleep,col="Blue",outcol="Red",main="Hours spent Sleeping ",ylab="Sleep")
bp.sleep$out

# Calculating z-scores for the sleep variable
z.score<-(personalized$Sleep - mean_sleep/sd_sleep)
z.score

#create a subset of data that only contains quantitative variables

subset.data <- subset(personalized, select = c("Zoom", "Study", "Sleep", "Water.Bottle", "Listening.music", "Happiness","Steps"))
View(subset.data)

#Finding the highest correlation coefficient of two variables
cor(subset.data)

#########################################################		
# Quantitative Variable 4 - Water Bottle
#########################################################

# Quantitative variable -Water Bottle

# Plotting histogram for No. of water bottles filled
hist(personalized$Water.Bottle,col = "lightgreen", border = "darkblue",
     main = "Histogram of No. of water bottles filled",
     xlab = "Sleep", ylab = "Frequency")

# Calculate mean for the quantitative variable Water Bottle
mean_waterbottle <- mean(personalized$Water.Bottle)
mean_waterbottle

# Summary  for the quantitative variable Water Bottle
summary(personalized$Water.Bottle)

# Calculate standard deviation for the quantitative variable Water Bottle
sd_waterbottle <- sd(personalized$Water.Bottle)
sd_waterbottle

# Calculate IQR of the variable Water Bottle 

IQR(personalized$Water.Bottle)

#Plotting the box plot forWater Bottle variable
bp.waterbottle<-boxplot(personalized$Water.Bottle,col="Blue",outcol="Red",main="No. of times Water Bottle filled ",ylab="Water Bottle")
bp.waterbottle$out

# Calculating z-scores for the Water Bottle variable
z.score<-(personalized$Water.Bottle - mean_waterbottle/sd_waterbottle)
z.score

#create a subset of data that only contains quantitative variables

subset.data <- subset(personalized, select = c("Zoom", "Study", "Sleep", "Water.Bottle", "Listening.music", "Happiness","Steps"))
View(subset.data) 

#Finding the highest correlation coefficient of two variables
cor(subset.data)

#########################################################		
# Quantitative Variable 4 - Listening Music
#########################################################

# Quantitative variable - Listening Music

# Plotting histogram for Whether Listen to music or not
hist(personalized$Listening.music,col = "lightgreen", border = "darkblue",
     main = "Histogram of Hours spent Sleeping",
     xlab = "Sleep", ylab = "Frequency")


# Summary  for the quantitative variable Listening music
summary(personalized$Listening.music)

# Calculate mean for the quantitative variable  Listening Music
mean_music <- mean(personalized$Listening.music)
mean_music


# Calculate standard deviation for the quantitative variable Listening music
sd_music <- sd(personalized$Listening.music)
sd_music

# Calculate IQR of the variable Listening music 
IQR(personalized$Listening.music)

#Plotting the box plot for Listening music variable
bp.listenmusic<-boxplot(personalized$Listening.music,col="Blue",outcol="Red",main="Whether I listened to music or not  ",ylab="Listening music")
bp.listenmusic$out

# Calculating z-scores for the Listening music variable
z.score<-(personalized$Listening.music - mean_music/sd_music)
z.score

#create a subset of data that only contains quantitative variables
subset.data <- subset(personalized, select = c("Zoom", "Study", "Sleep", "Water.Bottle", "Listening.music", "Happiness","Steps"))
View(subset.data)

#Finding the highest correlation coefficient of two variables
cor(subset.data)

#########################################################		
# Quantitative Variable 4 - Steps
#########################################################

# Quantitative variable - Steps

# Plotting histogram for No. of steps in a day 
hist(personalized$Steps,col = "lightgreen", border = "darkblue",
     main = "Histogram of No. of Steps in a Day",
     xlab = "Steps", ylab = "Frequency")


# Summary  for the quantitative variable Steps
summary(personalized$Steps)

# Calculate mean for the quantitative variable Steps
mean_steps <- mean(personalized$Steps)
mean_steps

# Calculate standard deviation for the quantitative variable Steps
sd_steps <- sd(personalized$Steps)
sd_steps

# Calculate IQR of the variable Steps 

IQR(personalized$Steps)

#Plotting the box plot for study variable

bp.steps<-boxplot(personalized$Steps,col="Blue",outcol="Red",main=" No. of Steps in a day  ",ylab="Steps")
bp.steps$out

# Calculating z-scores for the Steps Variable
z.score<-(personalized$Steps - mean_steps/sd_steps)
z.score

#create a subset of data that only contains quantitative variables

subset.data <- subset(personalized, select = c("Zoom", "Study", "Sleep", "Water.Bottle", "Listening.music", "Happiness","Steps"))
View(subset.data)

#Finding the highest correlation coefficient of two variables

cor(subset.data)
 

#Highest correlation Variables
#Study and sleep has the highest correlation coefficient 
personalized.line <-lm(Study ~Sleep, personalized)

#Retrieve slope and intercept
personalized.line$coefficients
 # (Intercept)       Sleep 
# -0.1033668   0.3490845 

# Create a scatterplot
plot(personalized$Study, personalized$Sleep, 
     xlab = "Study(Hours)", ylab = "Sleep (Hours)",
     main = "Highest Correlation Coefficient",
     pch = 20)  # Change marker symbol to solid circle (pch = 20)

#Plot the regression line and the equation of the line (in text) on your scatterplot. 

#Plot the Equation of the line (in text) on the scatter plot 
abline(personalized.line,col='green')
text(15,125,cex=0.8,"Study = - -0.1033668 x Sleep + 0.3490845  ")
