install.packages("tidyverse")
install.packages("jsonlite")
library(jsonlite)
x <- 3
numlist <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
demo_table <- read.csv(file='demo.csv',check.names=F,stringsAsFactors = F)
demo_table2 <- fromJSON(txt='demo.json')
filter_table <- demo_table2[demo_table2$price > 10000,]
filter_table2 <- subset(demo_table2, price > 10000 & drive == "4wd" & "clean" %in% title_status) #filter by price and drivetrain
filter_table3 <- demo_table2[("clean" %in% demo_table2$title_status) & (demo_table2$price > 10000) & (demo_table2$drive == "4wd"),]

#Sample function selects 3 random values from the vector that is used to select the rows from the demo_table
#Create a numerical vector that is the same length as the number of rows in the data frame using the colon (:) operator.
num_rows <- 1:nrow(demo_table)
#Use the sample() function to sample a list of indices from our first vector.
sample_rows <- sample(num_rows, 3)
#Use bracket notation to retrieve data frame rows from sample list.
demo_table[sample(1:nrow(demo_table), 3),]

#To transform a data frame and include new calculated data columns, we'll use the mutate() function.
demo_table <- demo_table %>% mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE) #add columns to original data frame

#Summarizing data with group_by() and summarise()
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer), .groups = 'keep') #create summary table
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer),Maximum_Price=max(price),Num_Vehicles=n(), .groups = 'keep') #create summary table with multiple columns

#Demo 2 - gather() (wide to long) and spread() (long to wide)
demo_table3 <- read.csv('demo2.csv',check.names = F,stringsAsFactors = F)
long_table <- gather(demo_table3,key="Metric",value="Score",buying_price:popularity)
#long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)
wide_table <- long_table %>% spread(key="Metric",value="Score")

#using ggplot2
plt <- ggplot(mpg,aes(x=class)) #import dataset into ggplot2
plt + geom_bar() #plot a bar plot

#geom_col() expects two variables x and y for size while geom_bar() expects one
mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n(), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count)) #import dataset into ggplot2
plt + geom_col() #plot a bar plot
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") #plot bar plot with labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") + #plot a boxplot with labels
  theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees

#line plots
mpg_summary <- subset(mpg,manufacturer=="toyota") %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=cyl,y=Mean_Hwy)) #import dataset into ggplot2
plt + geom_line()
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30)) #add line plot with labels and markers on axes


#scatter plots
plt <- ggplot(mpg,aes(x=displ,y=cty)) #import dataset into ggplot2
plt + geom_point() + xlab("Engine Size (L)") + ylab("City Fuel-Efficiency (MPG)") #add scatter plot with labels

#scatter plots with quantitative variables categorized
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class") #add scatter plot with labels

#additional categorization for scattered plots
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive") #add scatter plot with multiple aesthetics

#box plot
plt <- ggplot(mpg,aes(y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() #add boxplot

#box plot with multiple manufacturers
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot and rotate x-axis labels 45 degrees

#heatmap of the average highway fuel efficiency across the type of vehicle class from 1999 to 2008
mpg_summary <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table

plt <- ggplot(mpg_summary, aes(x=class,y=factor(year),fill=Mean_Hwy))

plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle Year",fill="Mean Highway (MPG)") #create heatmap with labels

#heatmap of difference in average highway fuel efficiency across each vehicle model from 1999 to 2008
## heatmaps used for large trends in data (single numerical with multiple categorical variables)
mpg_summary <- mpg %>% group_by(model,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary, aes(x=model,y=factor(year),fill=Mean_Hwy)) #import dataset into ggplot2
plt + geom_tile() + labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") + #add heatmap with labels
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) #rotate x-axis labels 90 degrees

#boxplot with scattered plot overlay
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
 plt + geom_boxplot() + #add boxplot
   theme(axis.text.x=element_text(angle=45,hjust=1)) + #rotate x-axis labels 45 degrees
   geom_point() #overlay scatter plot on top
 
#scattered plot with overlay of complementary data
mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") #add scatter plot with lavels
geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine)) #overlay with error bars
  
#converting dataset to long
mpg_long <- mpg %>% gather(key="MPG_Type",value="Rating",c(cty,hwy)) #convert to long format
head(mpg_long)

#boxplot faceting by MPG type (city or hwy)
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + facet_wrap(vars(MPG_Type)) + #create multiple boxplots, one for each MPG type
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels

#testing for normality with geom_density()
ggplot(mtcars,aes(x=wt)) + geom_density() #visualize distribution using density plot

#Shapiro Wilk Test for Normality, normal if p-value >=.05
shapiro.test(mtcars$wt)

#testing if sample is representative of population
population_table <- read.csv('used_car_data.csv',check.names = F,stringsAsFactors = F) #import used car dataset
plt <- ggplot(population_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot
 
#sample size of 50 and log transformation to make more normal
sample_table <- population_table %>% sample_n(50) #randomly sample 50 data points
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

#t-test for determining if pop and sample have similar means
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven))) #compare sample versus population means

#two sample t-test
sample_table <- population_table %>% sample_n(50) #generate 50 randomly sampled data points
sample_table2 <- population_table %>% sample_n(50) #generate another 50 randomly sampled data points
t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven)) #compare means of two samples

#Pair t-test for two samples from two diff populations, means are calculated as diff between each paired observation
mpg_data <- read.csv('mpg_modified.csv') #import dataset
mpg_1999 <- mpg_data %>% filter(year==1999) #select only data points where the year is 1999
mpg_2008 <- mpg_data %>% filter(year==2008) #select only data points where the year is 2008
t.test(mpg_1999$hwy,mpg_2008$hwy,paired = T) #compare the mean difference between two samples

#ANOVA test for comparing means of continuous dependent variable across a number of groups (independent variables)
## Groups must be categorical
## Answering "Is there any statistical difference in the horsepower of a vehicle based on its engine type?"
mtcars_filt <- mtcars[,c("hp","cyl")] #filter columns from mtcars dataset
mtcars_filt$cyl <- factor(mtcars_filt$cyl) #convert numeric column to factor
aov(hp ~ cyl,data=mtcars_filt) #compare means across multiple levels
summary(aov(hp ~ cyl,data=mtcars_filt))

#Pearson correlation coefficient, between horsepower and qtr-mile time
head(mtcars)
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() #create scatter plot
cor(mtcars$hp,mtcars$qsec) #calculate correlation coefficient

#correlation between miles driven and selling price
used_cars <- read.csv('used_car_data.csv',stringsAsFactors = F) #read in dataset
head(used_cars)
plt <- ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price)) #import dataset into ggplot2
plt + geom_point() #create a scatter plot
cor(used_cars$Miles_Driven,used_cars$Selling_Price) #calculate correlation coefficient

#Correlation Matrix
used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")]) #convert data frame into numeric matrix
cor(used_matrix)

#creating and fitting a linear model of qtr mile (y) from horsepower (x)
lm(qsec ~ hp,mtcars) #create linear model
summary(lm(qsec~hp,mtcars)) #summarize linear model
model <- lm(qsec ~ hp,mtcars) #create linear model
yvals <- model$coefficients['hp']*mtcars$hp +
  model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2 
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model

#multiple linear regression; using more indep variables to predict qtr mile (y)
lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars) #generate multiple linear regression model
summary(lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars)) #generate summary statistics

#testing categorical data if difference in frequency of distribution
## passing a contingency table into a chi-squared test
table(mpg$class,mpg$year) #generate contingency table
tbl <- table(mpg$class,mpg$year) #generate contingency table
chisq.test(tbl) #compare categorical distributions


















