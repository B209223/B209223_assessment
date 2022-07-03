# Title: Loading NHSRdatasets
# Author details: Author: B209223, contact details: s2272326@ed.ac.uk
# Script and data information:
#This script will load the Stranded Patient dataset from the NHSRdatasets package.
#It will explore and tabulate data on stranded patient in NHS services and save it to "RawData" folder.
#A subset of variables will be selected.
#It will then be splited into training and test data and save it to "Data" folder, which will be available for furhter analysis.
#Data consists of character data, numeric data and date from NHSRdatasets.
#These datasets have been synthetically generated. (Chris Mainey, 2021)

## Load packages and data
# To load the packages
library(NHSRdatasets) #data is from NHSRdatasets
library(tidyverse) #for data read-in and manipulation
library(ggplot2) #for data visualisation
library(here) #for data workflows
library(lubridate) #to deal with dates
library(caret) #to split data into training and testing dataset
library(knitr) #for dynamic report generation
library(gtsummary) #for descriptive statistics



# To load the Stranded Patient dataset from NHSRdataset
data(stranded_data)
stranded <- stranded_data

# To check for its class
class(stranded)
#Class of stranded is "tbl_df", which means it's a tibble.

#To get an overview of the stranded patient dataset.
stranded

#The dataset has 768 rows and 9 columns, including three character variables (stranded.label, admit_date and frailty_index), 
#as well as 6 numeric variables(age, care.home.referral, medicallysage, hcop, menatal_health_care and periods_of_previous_care).
#Description of selected variables is available on Data Dictionary.

## To view the stranded patient data.
#A glimpse() function is used to view variables in this dataframe.
glimpse(stranded)
#To look at top and bottom rows.
head(stranded)
tail(stranded)

## To check if there's missing data
stranded %>% 
  map(is.na) %>%
  map(sum)
#There's 69 data missing on periods_of_previous_care.
#We will leave them here because this variable is irrelevant to this study and will not be included in further analysis.

## To add an index column to standed patient data.
#Index column is set to link raw data with partitioned datasets.The rowid_to_column() can set index.
stranded <- rowid_to_column(stranded, "index")

## To convert variable admit_date into a date variable
#The admit_date is a character variable but we would like it to be a date and in date format, so a conversion is done here with a as.Date() function
stranded$admit_date <- as.Date(stranded$admit_date, format = "%d/%m/%Y")

## To tabulate the raw data
kable(stranded)

## To save the raw stranded patient data to "RawData" folder
write_csv(stranded, here("RawData", "stranded.csv"))

## To visualise the relationship between the outcome variable (stranded label) and predictors (age, mental_health_care and admit_date).
#Relationship between stranded label and age
stranded %>% select(stranded.label,age) %>%
  ggplot(aes(x = stranded.label, y = age)) +
  geom_boxplot() +
  ggtitle("Relationship between stranded label and age of patients") +
  xlab("Stranded label") +
  ylab("Age of patients") +
  theme_classic()
ggsave(filename = here("Figures","age_stranded.bmp"))
#The plot shows there are differences on ages between two groups.

#Relationship between stranded label and mental health care service needed
mental_health <- stranded %>% 
  select(stranded.label, mental_health_care) %>%
  mutate(mental_health_care_needed = ifelse(mental_health_care == 0, "No", "Yes"))
count <- table(mental_health$stranded.label,mental_health$mental_health_care_needed)
mosaicplot(count, main = "Relationship between stranded label and needs of mental health care",
           xlab = "Stranded label", ylab = "Whether patients need mental health care")
ggsave(filename = here("Figures","mental_care_stranded.bmp"))
#The mosaic plot indicates on differences between two groups.

#Relationship between stranded label and admit date
cdplot(factor(stranded$stranded.label) ~ stranded$admit_date,
       main = "Relationship between stranded label and admit date", 
       xlab = "Admit date",
       ylab = "Stranded label")
ggsave(filename = here("Figures","dates.bmp"))
#The mosaic plot indicates on differences between two groups.

## To select variables for the data capture tool
#The project intends to explore relationship between one outcome (stranded vs non-stranded) and 3 predictors, namely age, mental_health_care and admit_date.
#So only 4 variables will be selected, namely stranded.label, age, mental_health_care and admit_date along with index.
stranded_4var <- stranded %>% select(index, stranded.label, age, mental_health_care, admit_date)

## To tabulate and overview the subsetted data
stranded_4var %>% head(5) %>% kable()
glimpse(stranded_4var)

## To save the subsetted data to the "RawData" folder
write_csv(stranded_4var, here("RawData", "stranded_4var.csv"))

## To split the stranded_4var data into training and testing datasets
nrow(stranded_4var)
#The dataset has 768 rows of data.

## To have a descriptive statistics on subsetted data
stranded_4var$mental_health_care <- factor(stranded_4var$mental_health_care) 
tbl_summary(stranded_4var) %>% 
  bold_labels() %>%
  modify_caption("**Summary Statistics**")
ggsave(filename = here("Tables","summary.bmp"))

#A testing dataset of 10-15 records is sufficient to evaluate the data capture tool. So the proportion of the raw data assigning to the training dataset is:
prop<-(1-(15/nrow(stranded_4var))) #0.9804688, so 2% to testing dataset and 98% to training dataset.
print(prop)

## To split the stranded_4var data into testing and training dataset
# A random seed is set to ensure outputs are reproducible.
set.seed(42)

# To set up the training dataset. The function createDataPartition() helps to split datasets randomly with indexing.
trainIndex <- createDataPartition(stranded_4var$index, p = prop, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
stranded_train <- stranded_4var[ trainIndex,]
nrow(stranded_train)
# There are 756 rows in the training dataset.

# To tabulate the training dataset and save to "Data" folder.
kable(stranded_train)
write_csv(stranded_train, here("Data", "stranded_train.csv"))

# To set up the testing dataset by removing the training dataset from the original dataset.
stranded_test <- stranded_4var[-trainIndex,]
nrow(stranded_test)
# There are 12 rows in the testing dataset, sufficient to evaluate the data capture tool.

# To set aside one record for test markers, tabulate and save to "Data" folder.
stranded_TestMarker <- stranded_test[1, ]
kable(stranded_TestMarker)
write_csv(stranded_TestMarker, here("Data", "stranded_TestMarker.csv"))

# To tabulate the testing dataset and save to "Data" folder.
write_csv(stranded_test, here("Data", "stranded_test.csv"))

#End