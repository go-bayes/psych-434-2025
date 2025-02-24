# laboratory 1: introduction to R
# date: 
# author: 
# permenant email: 


# lab 1: install r and rstudio

# website: https://go-bayes.github.io/psyc-434-2024/

# why learn r?

# you'll need it for your final report.
# supports your psychology coursework.
# enhances your coding skills, makes you smarter


# installing r ------------------------------------------------------------


# installing r

# visit the comprehensive r archive network (cran) at https://cran.r-project.org/.
# select the version of r suitable for your operating system (windows, mac, or linux).
# download and install it by following the on-screen instructions.

# installing rstudio
# after downloading r...

# intstalling r-studio ----------------------------------------------------

# step 1: install r-studio

# if you have already downloaded and installed r from the comprehensive r archive network (cran):
# go to the rstudio download page at https://www.rstudio.com/products/rstudio/download/
# choose the free version of rstudio desktop, and download it for your operating system.
# download and install rstudio desktop.
# open rstudio to begin setting up your project environment.

# step 2: create a new project

# in rstudio, go to file > new project.
# choose directory
# for a new project, select new project, then provide a directory name.
# specify the location where the project folder will be created.
# click create project.

# order your r-studio/r workflow

# step 3: give project structure

# organising files and folders:
# within your project, create folders to organise your scripts and data.
# common folder names include r/ for r scripts, data/ for datasets, and doc/ for documentation.
# you can create these folders using rstudio's files pane or through your operating system's file explorer.
# **use clear labels that anyone could understand.**@!!!!


# creating and managing r scripts:
# to create a new r script, go to file > new file > r script.
# save the script in your project directory's r/ folder to keep your work organised.
# **use meaningful file names that describe the script's purpose.**!!

# step 4: working with r-scripts

# write your r code in the script editor.
# execute code by selecting lines and pressing ctrl + enter (windows/linux) or cmd + enter (mac).
# use comments (preceded by #) to document your code for clarity and future reference.

# saving and organising scripts:
# regularly save your scripts (ctrl + s or cmd + s).
# organise scripts into folders within your project for different analyses or data processing tasks.

# step 5: when you exit r-studio

# before concluding your work, save your workspace or
# clear it to start fresh in the next session (session > restart r).

# again, use clearly defined script names
# annotate your code
# save your scripts often (ctrl + s or cmd + s).


# exercise 1 --------------------------------------------------------------
# exercise 1: install the tidyverse package

# open rstudio: launch rstudio on your computer.
# access package installation:
# navigate to the menu at the top of rstudio and click on tools > install packages.... 
# this opens the install packages dialogue box.

# install tidyverse:
# in the install packages dialogue box > click in this field and type 'tidyverse'

# begin installation:
# click on the install button 

# the installation might take a few minutes. 

# load tidyverse: after successful installation, you can load the tidyverse package into your r session by typing 

library(tidyverse) 

# if that failed, do run this code:

if (!require(tidyverse)) install.packages("tidyverse")

library(tidyverse)


# basic R commands --------------------------------------------------------


# basic r commands

# how to copy the code on this page

# r script > new file > r
# name your new r script and save it in a folder.
# hover your cursor over the top right of the code panel, and click the copy tab.
# copy the text into your script.
# save: ctrl + s or cmd + s.


# important: executing code macro -----------------------------------------
# executing code

# ctrl + enter (windows/linux) or cmd + enter (mac).

# assignment --------------------------------------------------------------
# assignment (<-)

# assignment in r is done using the '<-' operator, which on my machine renders <-.
# this operator assigns values to variables:

x <- 10 # assigns the value 10 to x
y <- 5 # assigns the value 5 to y


# this does the same
x = 10
y = 5

# note what happens when we do this
# 10 = 5 # not run

# but we can do this
10 == 5 # considered below

# rstudio assignment operator shortcut

# for macos: option + - (minus key) inserts <-.
# for windows and linux: alt + - (minus key) inserts <-.


# keyboard shortcuts ------------------------------------------------------
# keyboard shortcuts help (tools -> keyboard shortcuts help)


# concatenation -----------------------------------------------------------
# concatenation (c())

# the c() function combines multiple elements into a vector.

numbers <- c(1, 2, 3, 4, 5) # a vector of numbers
print(numbers)


# addition subtraction ----------------------------------------------------
# operations (+, -)

# basic arithmetic operations include addition (+) and subtraction (-).

# this does the same
x <-  10
y <-  5

sum <- x + y # adds x and y


print(sum)

difference <- x - y # subtracts y from x

# note we did not need to use the `print()` function
difference

# multiplication ----------------------------------------------------------

### multiplication (`*`) and division (`/`)

# multiplication and division in r are performed using the `*` and `/` operators, respectively.
# these operations can be applied both to scalar values and to vectors, element-wise.

# scalar multiplication and division
x <- 10
y <- 5

product <- x * y # multiplies x by y
quotient <- x / y # divides x by y

product
quotient


# vector multiplication ---------------------------------------------------

# vector multiplication and division
vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)

# show
vector1
vector2

vector_product <- vector1 * vector2 # element-wise multiplication
vector_division <- vector1 / vector2 # element-wise division

vector_product
vector_division

# be cautious with division by zero as it results in `inf` or `nan`.

# example of division by zero
result <- 10 / 0 # results in inf
result

zero_division <- 0 / 0 # results in nan
zero_division

# integer division and modulo operation
integer_division <- 10 %/% 3 # results in integer division
integer_division

remainder <- 10 %% 3 # results in modulo operation
remainder

# `rm()` remove object ----------------------------------------------------

devil_number  <- 666 # create object
# view
devil_number

# remove the object
rm(devil_number)

# check
devil_number

# logical operators -------------------------------------------------------
### logic (`!`, `!=`, `==`)

# logical operations include not (`!`), not equal (`!=`), and equal (`==`).
x <- 10
y <- 5

x_not_y <- x != y # checks if x is not equal to y
x_not_y

x_equal_10 <- x == 10 # checks if x is equal to 10
x_equal_10


### or (`|` and `||`)

# `|` for element-wise logical or, `||` for single logical or operation.

vector_or <- c(x, y) x| c(x, y) # element-wise or
vector_or

single_or <- x || y # single or
single_or

### and (`&` and `&&`)

# `&` for element-wise logical and, `&&` for single logical and operation.
vector_and <- c(x, y) & c(x, y) # element-wise and
vector_and

single_and <- x && y # single and
single_and



# data types in r ---------------------------------------------------------


# integers ----------------------------------------------------------------
# whole numbers without decimal points, defined with an `L` suffix
x <- 42L
str(x) # check type

# note `as.numeric` 
y <- as.numeric(x)
str(y)


# characters --------------------------------------------------------------
# text strings enclosed in quotes
name <- "alice"

name
# factors -----------------------------------------------------------------
# represent categorical data with limited values
colors <- factor(c("red", "blue", "green"))
colors

# ordered factors ---------------------------------------------------------
# factors ordinary
education_levels <- c("high school", "bachelor", "master", "ph.d.")
education_factor_no_order <- factor(education_levels, ordered = FALSE)
str(education_factor_no_order)

# factors with inherent order
education_factor <- factor(education_levels, ordered = TRUE)
education_factor

# another way to do the same
education_ordered_explicit <- factor(education_levels, levels = education_levels, ordered = TRUE)

# operations with ordered factors
edu1 <- ordered("bachelor", levels = education_levels)
edu2 <- ordered("master", levels = education_levels)
edu2 > edu1 # logical comparison

# modifying ordered factors
new_levels <- c("primary school", "high school", "bachelor", "master", "ph.d.")
education_updated <- factor(education_levels, levels = new_levels, ordered = TRUE)

education_updated

str(education_updated)
table(education_updated)

# strings -----------------------------------------------------------------
# sequences of characters
you <- 'world!'
greeting <- paste("hello,", you)

# hello world
greeting


## vectors
# fundamental data structure in r
numeric_vector <- c(1, 2, 3, 4, 5)
character_vector <- c("apple", "banana", "cherry")
logical_vector <- c(TRUE, FALSE, TRUE, FALSE)
logical_vector


# manipulating vectors
vector_sum <- numeric_vector + 10
vector_sum

vector_multiplication <- numeric_vector * 2
vector_multiplication

vector_greater_than_three <- numeric_vector > 3
vector_greater_than_three


# table() -----------------------------------------------------------------
table(vector_greater_than_three)

# dataframes --------------------------------------------------------------
# clear any previous `df` object 
rm(df)
df <- data.frame(
  name = c("alice", "bob", "charlie"),
  age  = c(25, 30, 35),
  gender = c("female", "male", "male")
)
# check structure
head(df)
str(df)


table(df$gender)
table(df$age)
table(df$name)

# access data frame elements ----------------------------------------------
# by column name
names <- df$name  # extracts the `name` column
names

# by row and column
df[ , "age"]
df$age

second_person <- df[2, ] # extracts the second row
age_column <- df[, "age"] # extracts the `age` column
second_person
age_column

# using `subset()` function
very_old_people <- subset(df, age > 25)  # extracts rows where `age` is greater than 18

very_old_people
summary(very_old_people$age)
mean(very_old_people$age)
min(very_old_people$age)
max(very_old_people$age)

### explore your data frames

head(df)  # first six rows
tail(df)  # last six rows
str(df)   # structure of the data frame

### manipulating data frames

# adding columns
df$employed <- c(TRUE, TRUE, FALSE)  # adds a new column "employed"
head(df)

# adding rows
new_person <- data.frame(name = "diana", 
                         age = 28, 
                         gender = "female", 
                         employed = TRUE)
df <- rbind(df, new_person)
head(df)

# modifying values
df[4, "age"] <- 26  # changes diana's age to 26
df[4, ]
df
# removing columns or rows
# df$employed <- NULL # removes the employed column
# df <- df[-4, ]  # removes the fourth row
# df

### add rows with `rbind()`

### adding columns with `cbind()`

new_person <- data.frame(name = "eve", age = 32, gender = "female", employed = TRUE)
df <- rbind(df, new_person)
# Add columns with `cbind()`
occupation_vector <- c("engineer", "doctor", "artist", "teacher", "doctor")
df <- cbind(df, occupation_vector)
df <- cbind(df, occupation_vector)
head(df)

### considerations for `rbind()` and `cbind()`

# when using `rbind()` and `cbind()`, ensure columns or rows match in name and order. be mindful of factor levels.

### view data structure (`summary()`, `str()`, `head()`, `tail()`)

str(iris) # displays structure of scores_df
summary(iris) # summary statistics
head(iris) # first few rows
tail(iris) # last few rows

### mean(), sd(), min(), max(), and table()

set.seed(12345)
vector <- rnorm(n = 40, mean = 0, sd = 1)
hist(vector)

mean(vector)  # calculates mean
sd(vector)  # computes standard deviation
min(vector)  # finds minimum value
max(vector)  # finds maximum value

# generates a frequency table for categorical data
table(df$gender, df$occupation_vector)  # cross-tabulation

# graphs - ggplot2 --------------------------------------------------------
# ggplot2 for creating data visualisations based on the grammar of graphics.
# first data visualisation with `ggplot2`

# install and load ggplot2
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# simulate student data
# seed for data reproducibility
set.seed(12345)
student_data <- data.frame(
  name = c("alice", "bob", "charlie", "diana", "ethan", "fiona", "george", "hannah"),
  score = sample(80:100, 8, replace = TRUE)
)

student_data


# determine pass/fail
student_data$passed <- ifelse(student_data$score >= 90, "passed", "failed")
# convert 'passed' to factor
student_data$passed <- factor(student_data$passed, levels = c("failed", "passed"))

# simulate study hours
student_data$study_hours <- sample(5:15, 8, replace = TRUE)

# basic components of a ggplot2 plot:
# data, aesthetics (aes), and geometries (geom_ functions)


# ggplot2 barplot ---------------------------------------------------------

# bar plot showing score for each name
ggplot(student_data, aes(x = name, y = score)) +
  geom_bar(stat = "identity")

# enhanced bar plot with titles, axis labels, and modified colours
ggplot(student_data, aes(x = name, y = score, fill = passed)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
  labs(title = "student scores", x = "student name", y = "score") +
  theme_minimal()

# ggplot2 scatterplot -----------------------------------------------------
# compare student scores against study hours
p1 <-ggplot(student_data, aes(x = study_hours, y = score, color = passed)) +
  geom_point(size = 4) +
  labs(title = "student scores vs. study hours", x = "study hours", y = "score") +
  theme_minimal() +
  scale_color_manual(values = c("failed" = "red", "passed" = "blue"))


# ggplot2 boxplot ---------------------------------------------------------
# visualising the distribution of scores by pass/fail status
p2 <- ggplot(student_data, aes(x = passed, y = score, fill = passed)) +
  geom_boxplot() +
  labs(title = "score distribution by pass/fail status", x = "status", y = "score") +
  theme_minimal() +
  scale_fill_manual(values = c("failed" = "red", "passed" = "blue"))
library(patchwork)
p1 / p2 +plot_annotation(tag_levels = "A")

# median (Q2/50th percentile): divides the dataset into two halves.
# first quartile (Q1/25th percentile): lower edge/ 25% of the data falls below this value.
# third quartile (Q3/75th percentile): upper edge/ third quartile/ 75% of the data are below this value.
# interquartile range (IQR): height of the box is IQR: distance between the 1 and 3 quartiles (Q3 - Q1) / middle 50% of the data.
# whiskers: lines extending from the top and bottom of the box (the "whiskers") indicate the range of the data
# outliers: points oustide whiskers

# ggplot2 histogram -------------------------------------------------------
# understanding the distribution of scores
# experiment with binwidth
student_data

ggplot(student_data, aes(x = score, fill = passed)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  labs(title = "histogram of scores", x = "score", y = "count") +
  theme_minimal() +
  scale_fill_manual(values = c("failed" = "red", "passed" = "blue"))

# ggplot2 lineplot --------------------------------------------------------
# monthly study hours over a semester
months <- factor(month.abb[1:8], levels = month.abb[1:8])
study_hours <- c(0, 3, 15, 30, 35, 120, 18, 15)

study_data <- data.frame(month = months, study_hours = study_hours)
str(study_data)
# line plot for monthly study hours
ggplot(study_data, aes(x = month, y = study_hours, group = 1)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(color = "red", size = 1) +
  labs(title = "monthly study hours", x = "month", y = "study hours") +
  theme_minimal()
study_data

# base r graphs -----------------------------------------------------------
# basic plotting functions

# `plot()`: creates scatter plots and line graphs.
# `hist()`: generates histograms for continuous variable distribution.
# `boxplot()`: compares distributions across groups.
# `barplot()`: visualises categorical data with bar graphs.


# base r scatter plot -----------------------------------------------------
# scatter plot of scores vs. study hours
plot(student_data$study_hours, student_data$score,
     main = "scatter plot of scores vs. study hours",
     xlab = "study hours", ylab = "score",
     pch = 19, col = ifelse(student_data$passed == "passed", "blue", "red"))


# base r histogram --------------------------------------------------------
# histogram to visualise distribution of student scores
hist(student_data$score,
     breaks = 5,
     col = "skyblue",
     main = "histogram of student scores",
     xlab = "scores",
     border = "white")

# base r boxplots ---------------------------------------------------------
# box plots for score distribution by pass/fail status
boxplot(score ~ passed, data = student_data,
        main = "score distribution by pass/fail status",
        xlab = "status", ylab = "scores",
        col = c("red", "blue"))

# base r linegraph --------------------------------------------------------
# clears previous graph
dev.off()
# must be numeric
months_num <- 1:length(study_data$month) # Simple numeric sequence

# plot points with suppressed x-axis
plot(months_num, study_data$study_hours, 
     type = "p", # Points=
     pch = 19,   # Type of point
     col = "red", 
     xlab = "Month", 
     ylab = "Study Hours", 
     main = "Monthly Study Hours",
     xaxt = "n") # Suppress the x-axis

# add lines between points
lines(months_num, study_data$study_hours, 
      col = "blue", 
      lwd = 1) # Line width

# add custom month labels to the x-axis at appropriate positions
axis(1, at = months_num, labels = study_data$month, las=2) # `las=2` makes labels perpendicular to axis

# Optional: adding a box around the plot for a minimalistic look
box()

# do your exercises here --------------------------------------------------
# find them here: https://go-bayes.github.io/psyc-434-2024/content/01-content.html#appendix-a-at-home-exercises


# exercise helpers --------------------------------------------------------


# begin exercise 1: install the tidyverse package
# open rstudio and navigate to tools > install packages...
# type tidyverse in the dialogue box and ensure install dependencies is checked
# click install and wait for the process to complete
# load tidyverse with library(tidyverse)
# end exercise 1

# begin exercise 2: install the parameters and report packages
# open rstudio and navigate to tools > install packages...
# type parameters, report in the dialogue box, ensuring install dependencies is checked
# click install and wait for completion
# end exercise 2

# begin exercise 3: basic operations and data structure manipulation
vector_a <- c(2, 4, 6, 8)
vector_b <- c(1, 3, 5, 7)
vector_a_b_sum <- vector_a + vector_b
vector_a_b_diff <- vector_a - vector_b
vector_a_doubled <- vector_a * 2
vector_b_halved <- vector_b / 2
mean_a <- mean(vector_a)
sd_a <- sd(vector_a)
mean_b <- mean(vector_b)
sd_b <- sd(vector_b)
# end exercise 3

# begin exercise 4: working with data frames
student_data <- data.frame(id = 1:4, name = c("alice", "bob", "charlie", "diana"), score = c(88, 92, 85, 95), stringsasfactors = FALSE)
student_data$passed <- student_data$score >= 90
passed_students <- student_data[student_data$passed, c("name", "score")]
summary(student_data)
head(student_data)
str(student_data)
# end exercise 4

# begin exercise 5: logical operations and subsetting
mean_score <- mean(student_data$score)
above_mean <- student_data[student_data$score > mean_score, ]
attendance <- c("present", "absent", "present", "present")
student_data$attendance <- attendance
present_students <- student_data[student_data$attendance == "present", ]
# end exercise 5

# begin exercise 6: cross-tabulation and analysis
fruit <- factor(c("apple", "banana", "apple", "orange", "banana"))
color <- factor(c("red", "yellow", "green", "orange", "green"))
fruit_data <- data.frame(fruit, color)
fruit_colour_table <- table(fruit_data$fruit, fruit_data$color)
fruit_colour_table
# which fruit has the most colour variety?
# note try this shortcut
rowSums(fruit_color_table)
# what do we get with colSums? colSums(fruit_color_table)
# end exercise 6

# begin exercise 7: visualization with ggplot2
# install and load ggplot2 if not already done
# create a bar plot showing scores of students
ggplot(student_data, aes(x = name, y = score, fill = as.factor(passed))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue")) +
  labs(title = "student scores", x = "name", y = "score") +
  theme_minimal()
# end exercise 7



