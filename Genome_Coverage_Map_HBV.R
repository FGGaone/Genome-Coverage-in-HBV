#setting working directory
setwd("/Users/florencegaongalelwe/Downloads/florence_work/bcpp_bomolemo_datasets")
#check working directory

getwd()
#read the data
library(readxl)

data <- read_excel("final_work_book.xlsx")

#load excel
library(readxl)

#import the excel file
data <- read_excel("final_work_book.xlsx")

#check the loaded data
head(data)

#view column names
names(data)

#calculate the median and IQR of hiv_vl
median(data$hiv_vl, na.rm = TRUE)
IQR(data$hiv_vl, na.rm = TRUE)

#calculate the median and IQR for hbv_vl
median(data$hbv_vl, na.rm = TRUE)
IQR(data$hbv_vl, na.rm = TRUE)

#percentiles
quantile(data$hbv_vl, probs = c(0.25, 0.75), na.rm = TRUE)

#calulate the 25th and 75th percentiles
quantile(data$hiv_vl, probs = c(0.25, 0.75), na.rm = TRUE)

#calculating the total number of people on ART (+ or -)
sum(data$anti_hbc_status)

#counting
nrow(data[data$art_status == 1 & data$anti_hbc_status == 1, ])

#those on ART versus those who are not on ART
sum(data$art_status == 1, na.rm = TRUE)   # number of people on ART
sum(data$art_status == 0, na.rm = TRUE)   # number of people not on ART

#calculating the total number of anti-HBc positives in ART
sum(data$art_status == 1 & data$anti_hbc_status == 1, na.rm = TRUE)

# checkin anti-hbc by status
table(data$anti_hbc_status)

#proportions
prop.table(table(data$anti_hbc_status)) * 100


#checking if the differnce is statistically significant (binomial test; small sample size)
binom.test(sum(data$anti_hbc_status == 1), 
           sum(data$anti_hbc_status %in% c(1,2)),
           p = 0.5)

#calculating the total number of anti-HBc positives with VL coun
sum(data$art_status == 1 & data$hiv_vl == 1, na.rm = TRUE)

#calculating the total number of anti-HBc negatives with vl 
sum(data$art_status == 1 & data$hiv_vl == 1, na.rm = TRUE)
