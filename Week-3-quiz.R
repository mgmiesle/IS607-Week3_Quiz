# Matthew Miesle
# CUNY SPS
# IS 607 SECT 01
# Week 3 Quiz

#### Prob 1 ####
# 1. Write a function that takes a numeric vector and calculates the mean of
# the observations in the vector.
func1 <- function(x)
{
    mean(x)
}

#### Prob 2 ####
# 2. Modify your function in the previous question so that it can handle a
# numeric vector with missing values.
func2 <- function(x)
{
    mean(x, na.rm = TRUE)
}

#### Prob 3 ####
# 3. Write a function that takes two numeric input values and calculates 
# the greatest common divisor of the two numbers.
func3 <- function(x, y)
{
    i <- max(c(x, y))
    while(i > 0)
    {
        if((x %% i == 0) & (y %% i == 0))
        {
            return(i)
        }
        i <- i - 1
    }
}
func3(462, 1071)
func3(1071, 462)
# GCD of this pair is 21

# seems like there should be an way to implement this with apply
# at least replacing the if statement


#### Prob 4 ####
# 4. Write a function that implements Euclid’s algorithm (you may need 
# to do a bit of research to find this algorithm) for finding the greatest
# common divisor of two numeric inputs.
func4 <- function(x, y)
{
    a <- abs(x)
    b <- abs(y)
    v <- c(a, b)
    r <- 1
    while(r != 0)
    {
        if(v[1] < v[2])
        {
            v <- c(v[2], v[1])
        }
        r <- v[1] %% v[2]
        if(r == 0)
        {
            return(v[2])
        }
        else
        {
            v <- c(v[2], r)
        }
    }
}
## referenced: http://www.math.rutgers.edu/~greenfie/gs2004/euclid.html
func4(462, 1071)
func4(1071, 462)
# GCD of this pair is 21
# pair found here: http://en.wikipedia.org/wiki/Euclidean_algorithm

#### Prob 5 ####
# 5. Write a function that takes two numeric inputs x and y and calculates
# x ^ 2 * y - 2 * x * y - x * y ^2
func5 <- function(x, y)
{
    x ^ 2 * y - 2 * x * y - x * y ^ 2
}
func5(5, 7)

#### Prob 6 ####
# 6. Read in the week-3-price-data.csv and week-3-make-model-data.csv 
# files as data frames and then merge them by the ModelNumber key. 
# Leave the “all” parameters as their defaults. How many observations 
# end up in the result? Is this what you would have expected?

price <- read.table(file = "C:/Users/MattM/Downloads/week-3-price-data.csv", header = TRUE, sep = ",")
model <- read.table(file = "C:/Users/MattM/Downloads/week-3-make-model-data.csv", header = TRUE, sep = ",")
# IMPORTANT: needed to include header = TRUE for merge to work properly
# class(price)
# class(model)
pm6 <- merge(x = price, y = model, by.x = "ModelNumber", by.y = "ModelNumber")
# would have expected 28 (if all overlapped) or up to 36 (if no overlaps)
# but we see in the next problem that we would need to set all = TRUE for this
# only returned the rows that overlapped (union?)
# I see that ID 12 is missing - it seems to have a typo with an extra 0
# on the end of it's ModelNumber
# 

#### Prob 7 ####
# 7. Use the data sets from the previous question, but this time 
# merge them so that the rows from the price-data table all appear, 
# even if there is no match in the make-model table.
pm7 <- merge(x = price, y = model, by.x = "ModelNumber", by.y = "ModelNumber", all = TRUE)
# 28 observations in this data


#### Prob 8 ####
# 8. Take your result from question 7 and subset it so that only the 
# 2010 vehicles are included.
pm8 <- subset(pm7, pm7$Year == 2010)
# http://stackoverflow.com/questions/1686569/filtering-a-data-frame-in-r

#### Prob 9 ####
# 9. Take your result from question 7 and subset it so that only the 
# red cars that cost more than $10,000 are included.
pm9 <- subset(pm7, pm7$Color == "Red" & pm7$Price > 10000)

#### Prob 10 ####
# 10. Take your result from question 9 and subset it so that the ModelNumber
# and Color columns are removed.
pm10 <- subset(pm9, select = -c(ModelNumber, Color))
# http://stackoverflow.com/questions/9845929/removing-a-list-of-columns-from-a-data-frame-using-subset

#### Prob 11 ####
# 11. Write a function that takes as input a character vector and returns a
# numeric vector with the numbers of characters in each of the elements in the original vector.
func11 <- function(v11)
{
    nchar(v11)
}
func11(c("abc", "aaaaaa", "b", "1234"))

#### Prob 12 ####
# 12. Write a function that takes two character vectors of equal length 
# and concatenates them element by element with a space as the separator. 
# Have the function die gracefully if the vectors are the same length.
func12 <- function(v121, v122)
{
    str_c(v121, v122, sep = " ")
}
func12(c("abc", "aaaaaa", "b", "1234", "bbb"), c("abc", "aaaaaa", "b", "1234"))
# character manipulation section in Ch.2 of "Data Manipulation with R"
#    paste() and str_c() introduced here
# Ch 13.1 in "R for Everone" explains paste()
# https://stat.ethz.ch/R-manual/R-patched/library/base/html/paste.html
# Unsure what the last line of instruction is.  should it be "if the vectors are NOT
# the same length"?
# Using either function, if the vectors are not the same length then the shorter
# vector is recycled starting from the beginning of the vector

#### Prob 13 ####
# 13. Write a function that takes a character vector and returns the 
# substring of three characters that begins with the first vowel in the string.
# Have the function handle gracefully substrings where this isn’t possible.
func13 <- function(v13)
{
#    require(stringr) 
#    I just attached stringr by checking the box in Rstudio instead
    v13temp <- v13[str_detect(string = v13, pattern = "A|a|E|e|I|i|O|o|U|u")]
#     str_extract(string = v13temp, "[A|a|E|e|I|i|O|o|U|u][][]")
}
func13(c("abc", "aaaaaa", "b", "1234", "bbb"))
# can return the strings that have a vowel in them
# but need to extract the 1st vowel + 2 more characters for each string

#### Prob 14 ####
# 14. Suppose you have a data frame where one column gives the month 
# (in numeric format), the next gives the day, and the third column gives
# the year. Use R to create such a data frame (by hand is fine) and then 
# add a fourth column with the date in date format.
month = c(1, 3, 5, 12)
day = c(4,22, 11, 31)
year = c(2000, 1999, 1980, 2012)
df14 <- data.frame(month, day, year)
df14[, "date"] <- as.Date(paste(year, month, day, sep = "-"))
# Date Processing in Ch.2 of "Data Manipulation with R"


#### Prob 15 ####
# 15. Illustrate the code necessary to take a string of MM-DD-YYYY format
# and convert it to a date.
as.Date("09-11-2014", format = "%m-%d-%Y")
# Date Processing in Ch.2 of "Data Manipulation with R"

#### Prob 16 ####
# 16. Illustrate the code necessary to take a date and extract the month of the date.
as.numeric(format(as.Date("09-11-2014", format = "%m-%d-%Y"), "%m"))
# alternative is to use the month() function in the lubridate package:
# month(as.Date("09-11-2014", format = "%m-%d-%Y"))
# Date Processing in Ch.2 of "Data Manipulation with R"

#### Prob 17 ####
# 17. Create a sequence of all of the dates from January 1, 2005, to December 31, 2014.
# dates17 <- as.Date(c(as.Date("2005-01-01"):as.Date("2014-12-31")), origin = "2005-01-01")
dates17 <- as.Date(c(1:(10 * 365 + 2 - 1)), origin = "2005-01-01")
# added 2 days for 2 leap days that would occur within 10 years and subtracted 1 to take off
# the 1st day of 2015
# seems like there's a more elegant way to implement this