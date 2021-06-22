# Libraries ####
library(easypackages)
libraries("tidyverse","dlookr","pastecs","ggthemes")

# Descriptive Statistics  ####
numeric.descriptives.training.set <- training.set %>%
  select(Age, `Household Size`) %>%
  describe() %>%
  select(variable,mean,sd,se_mean,IQR,skewness,kurtosis,p25,p50,p75,p100) %>%
  rename(SEM = se_mean) %>%
  mutate(across(is.numeric, ~ round(.,2)))

### Assumption of Normality
par(mfrow=c(2,2))
qqnorm(training.set$Age, main = "Normal Q-Q Plot for Age") 
qqline(training.set$Age, col = "red")
qqnorm(training.set$`Household Size`, main = "Normal Q-Q Plot for Household Size") 
qqline(training.set$`Household Size`, col = "red")
par(mfrow=c(2,2))

# Show relationship between target and numeric variables

### Distribution of the countries
training.set %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count) * 100, 1)) %>%
  ggplot(aes(reorder(country, -percent), percent)) +
  geom_text(aes(label=percent), vjust=-0.5, size = 5) +
  labs(x="") +
  geom_col(fill="#126e82") + 
  theme_tufte() +
  theme(aspect.ratio = 0.96)

# Exploratory Data Analysis ####

## Data set groups  ####
Demographics <- training.set %>% 
  select(Age, Gender, `Household Size`, country, `Marital Status`, `Relationship With Head`, `Education Level`, `Job Type`, bank_account) %>%
  rename(`Do you have a bank account` = bank_account)

Environment <- training.set %>%
  select(`Location Type`, bank_account) %>%
  rename(`Do you have a bank account` = bank_account)

TimeFrame <- training.set %>%
  select(year, Gender, bank_account) %>%
  rename(`Do you have a bank account` = bank_account)

  



