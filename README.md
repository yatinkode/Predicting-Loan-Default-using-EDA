# Predicting-Loan-Default-using-EDA
Predict whether a customer will default loan using EDA in R

You work for a consumer finance company which specialises in lending various types of loans to urban customers. When the company receives a loan application, the company has to make a decision for loan approval based on the applicant’s profile. Two types of risks are associated with the bank’s decision:

    If the applicant is likely to repay the loan, then not approving the loan results in a loss of business to the company

    If the applicant is not likely to repay the loan, i.e. he/she is likely to default, then approving the loan may lead to a financial loss for the company

 

The data given below contains the information about past loan applicants and whether they ‘defaulted’ or not. The aim is to identify patterns which indicate if a person is likely to default, which may be used for taking actions such as denying the loan, reducing the amount of loan, lending (to risky applicants) at a higher interest rate, etc.

Lets start the code

### Loading Libraruies and theme for ggplot
```R
# Loading libraries
load.libraries <- c('tidyr','dplyr','lubridate','ggplot2','stringr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

# pre-set the theme.
theme_loan <- function () { 
  theme_bw(base_size=9, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_rect(fill="gray80", colour=NA),
      plot.background = element_rect(fill="gray96", colour=NA), 
      legend.background = element_rect(fill="white", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      legend.position="bottom"
    )
}   
```
### Loading Data
```R
# Loading data
loan_records_master <- read.csv("loan.csv", stringsAsFactors = FALSE)

# Structure of the dataframe
str(loan_records_master)
# There are 39,717 rows(observations) in 111 columns(variables)
```
### Data Cleaning
```R
# In data cleaning, we will be looking to fix rows and columns, fix missing values, 
# standardise values, fix invalid values, etc. 

#--------------1. Fixing rows -- Removing duplicate rows and rows not required -------- 
# checking for duplicate rows
sum(duplicated(loan_records_master))
# There is no duplicate rows in the dataset
#Since we cannot determine anything based on current applicants we can filter them out
loan_records_master <- subset(loan_records_master, loan_records_master$loan_status %in% c("Charged Off", "Fully Paid"))

#--------------2. Fixing columns - Removing All NA columns--------
# Counting NA in all the columns (also the names of all the columns)
NAcols<-as.numeric(which(colSums(is.na(loan_records_master))==nrow(loan_records_master)))
#creating vector a containing column indexes of all columns  from  loan_records_master
colindex<-as.numeric(c(1:ncol(loan_records_master)))
#Removing columns with all NA values
loan_records_master<-loan_records_master[,setdiff(colindex,NAcols)]
# We removed 54 columns from the dataframe because they have all the entries as NA

#--------------3. Fixing columns - Removing Single Value columns ----
# We also observed that there are few columns with only single value
#getting the column indexes where  there is only single value in the whole column
SingleValuecols <- as.numeric(which(sapply(loan_records_master, function(x) length(unique(x)))==1))
#creating vector  containing column indexes of all columns  from  loan_records_master-redefined after removing NA values previously
colindex<-as.numeric(c(1:ncol(loan_records_master)))
#Removing columns with only 1 value per column 
loan_records_master <- loan_records_master[,setdiff(colindex,SingleValuecols)]

#--------------4. Fixing columns - Removing unnecessary columns ------
unique(loan_records_master$purpose) # We will keep this column as it can provide information on what type of loans default
unique(loan_records_master$title) # Since this is manually entered and may not be categorized
# We will addr_state to analyze if any particular area is defaulting
unique(loan_records_master$delinq_2yrs) # We will keep this column to check if this can provide some informaiton on which borrower is going to default
unique(loan_records_master$inq_last_6mths) # Let's keep this column for now
unique(loan_records_master$open_acc) # Does more credit line means less default, we will check that
unique(loan_records_master$pub_rec) # Should we invest more in collection public records
unique(loan_records_master$out_prncp) # Remove the column
unique(loan_records_master$out_prncp_inv) # Remove the column
unique(loan_records_master$next_pymnt_d) # Only empty value hence can be removed

# We will also remove some other unnecessary columns or rows that may not add value to analysis
loan_records_master <- select(loan_records_master, -c(member_id,url,desc,emp_title,mths_since_last_record,mths_since_last_delinq))
# We are removing columns that are post-loan information about loans and will not add value to our analysis
loan_records_master <- select(loan_records_master, -c(title, sub_grade,total_pymnt,total_pymnt_inv,total_rec_prncp, total_rec_int, total_rec_late_fee, recoveries, 
                                                      collection_recovery_fee, last_pymnt_amnt, collections_12_mths_ex_med,chargeoff_within_12_mths, zip_code,last_pymnt_d))

# Dataframe for our analysis after fixing columns
str(loan_records_master)
# We are now left with 38,577 rows(observations) of 28 columns(variables or features)

#---------------4. Standardize columns - Removing unnecessary symbols and texts after numbers ------------------
#Removing '%' sign after interest rate
loan_records_master$int_rate = as.numeric(gsub("\\%", "", loan_records_master$int_rate))
#Removing '%' sign after revol_util
loan_records_master$revol_util = as.numeric(gsub("\\%", "", loan_records_master$revol_util))

#---------------5. Standardize columns - Standardising date formats ---------------------------------------------
# We have four columns(issue_d,earliest_cr_line, last_payment_d & last_credit_pull_d)with dates. We will convert them to date format                              

loan_records_master$issue_d <- parse_date_time(loan_records_master$issue_d , orders = 'b-y')
loan_records_master$issue_d <- format(loan_records_master$issue_d,format = "%B-%Y")
loan_records_master$earliest_cr_line <- parse_date_time(loan_records_master$earliest_cr_line , orders = 'b-y')
loan_records_master$earliest_cr_line<- format(loan_records_master$earliest_cr_line,"%b-%Y")
loan_records_master$last_credit_pull_d <- parse_date_time(loan_records_master$last_credit_pull_d , orders = 'b-y')
loan_records_master$last_credit_pull_d<- format(loan_records_master$last_credit_pull_d,"%b-%Y")
str(loan_records_master)
# uber_request$Request.timestamp <- as.POSIXct(uber_request$Request.timestamp, format = "%d-%m-%Y %H:%M")

#---------------6. Fix missing values  ------------------------------------------
sapply(loan_records_master, function(x) sum(is.na(x)))

# Now we have only three columns with missing values pub_rec_bankruptcies,tax_liens, revol_util
# Those columns have missing values because value is not applicable 

#---------------7. Standardize columns - Coverting relevant columns to factors ---
# Coverting relevant columns to factors
loan_records_master <- mutate_if(loan_records_master, is.character, as.factor)
str(loan_records_master)
```

### Univariate and Bivariate Analysis
```R
# "Correlation does not mean causation", so we will stay from drawing conclusions. 

# Plotting the records of loan status
ggplot(loan_records_master, aes(x = loan_status )) + # Identify the extent of default
  geom_bar(fill = "steelblue4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Loan Status",
       y = "Count",
       title = "Fig 1: Loan Request Status")
```
![alt text](https://github.com/yatinkode/Predicting-Loan-Default-using-EDA/blob/main/images/1.png)
```R
prop.table(table(loan_records_master$loan_status))*100
# Approximately 15% loans have defaulted 

# Range of loan amount, funded amount and funded amount committed by investors
sum(loan_records_master$loan_amnt) #426,161,100
sum(loan_records_master$funded_amnt) #416,016,625
sum(loan_records_master$funded_amnt_inv) #394,352,654
```

