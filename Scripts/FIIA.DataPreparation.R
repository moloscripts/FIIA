# Data Prep libraries ####
library(easypackages)
libraries("tidyverse","inspectdf","dlookr","lubridate")

# Datasets {Global} ####
raw.training.FIIA <- read.csv("Data/Train_v2.csv", stringsAsFactors = T)
testing.FIIA <- read.csv("Data/Test_v2.csv")

# Structure
dim(raw.training.FIIA)
str(raw.training.FIIA)
head(raw.training.FIIA)
tail(raw.training.FIIA)

# Rearrange columns in the DF
training.FIIA <- raw.training.FIIA[,c(3,1,2,5,8,6,9,7,10,11,12,13,4)]

# Create a new response Column with values 0 and 1
training.FIIA <- training.FIIA %>%
  mutate(BankAccount = ifelse(bank_account=="No", 0,1))

# Data Health ####

# Using inspectdf package
inspect_types(raw.training.FIIA) %>%
  show_plot()
inspect_na(raw.training.FIIA) %>%
  show_plot()
inspect_imb(raw.training.FIIA)

# diagnose using dlookr
training.FIIA %>%
  select(-year) %>%
  diagnose_numeric()

diagnose_numeric(training.FIIA)

training.FIIA %>%
  select(-uniqueid) %>%
  diagnose_category()
diagnose_category(training.FIIA)

# Descriptive Statistics ####

## Univariate Analysis for Numerical Data

### Age of Respondent ###

# Measures of Location
range(training.FIIA$age_of_respondent)
mean(training.FIIA$age_of_respondent)
median(training.FIIA$age_of_respondent)

# Quartiles
quantile(training.FIIA$age_of_respondent, 0.25)
quantile(training.FIIA$age_of_respondent, 0.5)
quantile(training.FIIA$age_of_respondent, 0.75)

sd(training.FIIA$age_of_respondent)

summary(training.FIIA$age_of_respondent)

by(training.FIIA, training.FIIA$age_of_respondent, summary)


# Histogram of Age
ggplot(training.FIIA, aes(x=age_of_respondent, fill=gender_of_respondent)) +
  geom_histogram(aes(y = ..density..), position="identity", alpha=0.6) +
  stat_function(fun=dnorm, color="red",
                args = list(mean=mean(training.FIIA$age_of_respondent),
                            sd=sd(training.FIIA$age_of_respondent))) +
  theme_minimal()


# Outlier Detection in both Age and Household  Column
ggplot(training.FIIA, aes(x=age_of_respondent)) +
  geom_boxplot() + 
  theme_minimal()

boxplot.stats(training.FIIA$age_of_respondent)$out # Outliers start from the age of  95

# Household Size
summary(training.FIIA$household_size)

ggplot(training.FIIA, aes(household_size)) +
  geom_histogram(aes(y=..density..)) + 
  stat_function(fun=dnorm, color="red",
                args=list(mean=mean(training.FIIA$household_size),
                          sd=sd(training.FIIA$household_size))) +
  theme_minimal()

ggplot(training.FIIA, aes(x=household_size)) +
  geom_boxplot()

boxplot.stats(training.FIIA$household_size)$out

# Curve out a final DF without outliers
training.set <- training.FIIA %>%
  filter(age_of_respondent < 95 & household_size< 10)
dim(training.set)  
dim(training.FIIA)

training.set <- training.set %>%
  rename(`Location Type` =location_type,
         Age = age_of_respondent,
         Gender= gender_of_respondent,
         `Household Size` =household_size,
         `Relationship With Head` = relationship_with_head,
         `Marital Status` = marital_status,
         `Education Level` = education_level,
         `Job Type` = job_type,
         `Cellphone Access` =cellphone_access)


