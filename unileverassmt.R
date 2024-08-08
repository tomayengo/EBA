#Loading Packages to use
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("skimr")
library(skimr)
install.packages("ggplot2")
library(ggrepel)
library(ggplot2)
library(reshape2)
install.packages("tidyr")
library(tidyr)
install.packages("writexl")
library(writexl)
# Loading the excel file
data<- read_excel("unilever.xlsx")

#print the loaded data set
print(data)

#summary of the data set
summary(data)
skim(data)

#check for missing values
missingvalues <- is.na(data)
print (missingvalues)

#summary of missing values
missingsummary <- colSums(missingvalues)
print(missingsummary)

#Replacing missing values with 0
data_fill <- data
data_fill[is.na(data_fill)] <-0
print(data_fill)

#Updating the dataset
data <- data_fill
print (data)

#Identifying and removing Duplicate Values
findduplicate <- function(df) {
  duplicatecolumns <- duplicated(as.list(df)) 
  return(duplicatecolumns)
}
duplicatecolumns <-findduplicate(data)
print(duplicatecolumns)
#removing Duplicate columns
data_unique <-data[, !duplicatecolumns]
#updating dataset after removing duplicate column
data <- data_unique
print(data)

#Identifying and correcting errors like typos 
# Identify columns with non-numerical values
non_numerical_cols <- sapply(data, function(x) !is.numeric(x))
print(names(data)[non_numerical_cols])

#Replacing all non-numeral to zero
data_cleaned <- data %>%
  mutate(across(-1, ~ ifelse(is.na(as.numeric(as.character(.))), 0, 
                             as.numeric(as.character(.)))))
data <-data_cleaned
print(data)

#Standardizing data to be of the same data type
numerical_cols <- sapply(data, is.numeric)
#setting all data to zero decimal places
data[numerical_cols] <- lapply(data[numerical_cols], function(x) {
  round(x, 0)
})

print(data)

#importing additional data
data1 <- read_excel("malawicongo.xlsx")
print(data1)

#decimal places to zero
numerical_cols1 <-sapply(data1, is.numeric)
data1[numerical_cols1] <-lapply(data1[numerical_cols1], function(x){
  round(x,0)
})
print(data1)

#Full Outer join of two datasets
data3<-merge(data, data1, by = "Product", all = TRUE)
print(data3)

#dimension of the data
head(data3)
str(data3)
missing_values <-colSums(is.na(data))
print(missing_values)
glimpse(data3)
View(data3)
summary(data3)
print(data3)
data3 %>% count(UG)
#summary of Data
data_with_sums <- data3 %>% mutate(Sum = rowSums(select(., -1), na.rm = TRUE))
print(data_with_sums)
#visualize the data with a histogram
ggplot(data3, aes(x=Product, y=KY)) + geom_point(color = "red")+ 
  ggtitle("Sales in Kenya") +
  xlab("Products") +
  ylab("Sales in Kenya")
theme_minimal()

reference_col <-"Product"
col1 <- "UG"
col2 <- "KY"

data$Difference1 <-data3[[col2]] - data3[[col1]]
print(data$Difference1)
#correlation between Kenya and Uganda

#Outputing the dataset data3 to an excel file
output_file <-"data3.xlsx"
write_xlsx(data3,output_file)

