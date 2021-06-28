# Libraries ####
library(easypackages)
libraries("tidyverse","dlookr","pastecs","ggbeeswarm","ggthemes","plotly")
theme_set(theme_minimal())

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

# Exploratory Data Analysis ####

## Data set groups  and colors ####
Demographics <- training.set %>% 
  select(Age, Gender, `Household Size`, country, `Marital Status`, `Relationship With Head`, `Education Level`, `Job Type`, bank_account) %>%
  rename(`Do you have a bank account` = bank_account)

Environment <- training.set %>%
  select(`Location Type`, bank_account) %>%
  rename(`Do you have a bank account` = bank_account)

TimeFrame <- training.set %>%
  select(year, Gender, bank_account, country) %>%
  rename(`Do you have a bank account` = bank_account)

YearLevels <- c("2016", "2017", "2018")
TimeFrame$year <- factor(TimeFrame$year, ordered = T, levels = YearLevels)
PrimaryColors <- c("#FFC107","#3C8DAD")
NoYesColors <- c("#FF8474","#5AA897")


## Average age of individuals who have a bank account ####
MeanAgeBankAccountPlot <- Demographics %>%
  select(`Do you have a bank account`, Age) %>%
  group_by(`Do you have a bank account`) %>%
  summarise(`Mean Age` = round(mean(Age),1)) %>%
  ggplot(aes(`Do you have a bank account`, `Mean Age`, fill=`Do you have a bank account`)) + 
  geom_col() + 
  labs(title = "Fig I: Mean age of those who own and don't own a bank account") +
  theme(legend.position ="none", 
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12, face = "italic"),
        plot.title = element_text(color = "#343A40", size=14, face = "bold")) + 
  geom_text(aes(label = `Mean Age`, fontface="bold"), vjust = 2, size=5, color="#393E46") +
  scale_fill_manual("legend", values = NoYesColors)
MeanAgeBankAccountPlot


## Percentage of males and females who have a bank account ####
GenderBA <- Demographics %>%
  select(Gender, `Do you have a bank account`) %>%
  group_by(Gender, `Do you have a bank account`) %>%
  summarise(count = n()) %>%
  mutate(`Percentage (%)` = round(count/sum(count)*100, 1)) %>%
  ggplot(aes(`Do you have a bank account`, `Percentage (%)`, fill=Gender)) +
  geom_col(position = position_dodge2()) +
  labs(title = "Fig II :Bank account holders per gender") +
  theme_minimal() + 
  theme(legend.position = "top", 
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12, face = "italic"),
        legend.text = element_text(size = 11, face = "italic"),
        plot.title = element_text(color = "#343A40", size=14, face = "bold"), 
        legend.title = element_blank()
        ) + 
  geom_text(aes(label=`Percentage (%)`, fontface="bold"), position = position_dodge(0.9), vjust=2, size=5, color="#393E46") +
  scale_fill_manual("legend", values = PrimaryColors)
GenderBA


## Relationship of the Household head to the respondent ####
# 72% of all male headed household possess a bank account
GenderHHBA <- Demographics %>%
  select(`Relationship With Head`, Gender, `Do you have a bank account`) %>%
  filter(`Do you have a bank account` =="Yes") %>%
  group_by(`Relationship With Head`, Gender) %>%
  summarise(count = n()) %>%
  mutate(`Percentage (%)` = round(count/sum(count)*100, 1)) %>%
  ggplot(aes(`Relationship With Head`, `Percentage (%)`, fill=Gender)) + 
  labs(title = "Fig III: Relationship of the respondent with respect to the household head", 
       subtitle = "This chart is for all respondents who have a bank account") +
  geom_col()+
  theme(legend.position = "top", 
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"), 
        axis.text = element_text(size = 11, face = "italic"),
        legend.text = element_text(size = 11, face = "italic"),
        plot.title = element_text(color = "#343A40", size=13, face = "bold"), 
        plot.subtitle = element_text(color = "#343A40", size=12, face = "bold"), 
        legend.title = element_blank()
  ) + 
  geom_text(aes(label=`Percentage (%)`, fontface="bold"), position = position_stack(vjust=0.5), size=4, color="#393E46") + 
  scale_fill_manual("legend", values = PrimaryColors)
GenderHHBA


## What is the relationship of the Household head and the respondent -II #### 
GenderHHNoBA <- Demographics %>%
  select(`Relationship With Head`, Gender, `Do you have a bank account`) %>%
  filter(`Do you have a bank account` =="No") %>%
  group_by(`Relationship With Head`, Gender) %>%
  summarise(count = n()) %>%
  mutate(`Percentage (%)` = round(count/sum(count)*100, 1)) %>%
  ggplot(aes(`Relationship With Head`, `Percentage (%)`, fill=Gender)) + 
  labs(title = "Fig IV: Relationship of the respondent with respect to the household head", 
       subtitle = "This chart is for all respondents who do not have a bank account") +
  geom_col()+
  theme(legend.position = "top", 
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"), 
        axis.text = element_text(size = 11, face = "italic"),
        legend.text = element_text(size = 11, face = "italic"),
        plot.title = element_text(color = "#343A40", size=13, face = "bold"), 
        plot.subtitle = element_text(color = "#343A40", size=12, face = "bold"), 
        legend.title = element_blank()
  ) + 
  geom_text(aes(label=`Percentage (%)`, fontface="bold"), position = position_stack(vjust=0.5), size=4, color="#393E46") + 
  scale_fill_manual("legend", values = PrimaryColors)
GenderHHNoBA


## Education levels of respondents who have have a bank account ####
EducationGenderBA <- Demographics %>%
  select(`Education Level`, Gender, `Do you have a bank account`) %>%
  filter(`Do you have a bank account` == "Yes") %>%
  group_by(`Education Level`, Gender) %>%
  summarise(count = n()) %>%
  mutate(`Percentage (%)` = round(count/sum(count)*100,1)) %>%
  ggplot(aes(`Education Level`, `Percentage (%)`, fill=Gender)) +
  labs(title = "Fig V: Education levels of respondents who own a bank account") +
  geom_col(position = position_dodge()) + 
  theme(legend.position = "top", 
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"), 
        axis.text = element_text(size = 11, face = "italic"),
        legend.text = element_text(size = 11, face = "italic"),
        plot.title = element_text(color = "#343A40", size=13, face = "bold"), 
        plot.subtitle = element_text(color = "#343A40", size=12, face = "bold"), 
        legend.title = element_blank()
  ) + 
  geom_text(aes(label=`Percentage (%)`, fontface="bold"), position = position_dodge(0.9), vjust=2, size=4, color="#393E46") +
  scale_fill_manual("legend", values = PrimaryColors)
EducationGenderBA


## Education levels of respondents who don't have a bank account - II ####
EducationGenderNoBA <- Demographics %>%
  select(`Education Level`, Gender, `Do you have a bank account`) %>%
  filter(`Do you have a bank account` == "No") %>%
  group_by(`Education Level`, Gender) %>%
  summarise(count = n()) %>%
  mutate(`Percentage (%)` = round(count/sum(count)*100,1)) %>%
  ggplot(aes(`Education Level`, `Percentage (%)`, fill=Gender)) +
  labs(title = "Fig VI: Education levels of respondents who don't own a bank account") +
  geom_col(position = position_dodge()) + 
  theme(legend.position = "top", 
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"), 
        axis.text = element_text(size = 11, face = "italic"),
        legend.text = element_text(size = 11, face = "italic"),
        plot.title = element_text(color = "#343A40", size=13, face = "bold"), 
        plot.subtitle = element_text(color = "#343A40", size=12, face = "bold"), 
        legend.title = element_blank()
  ) + 
  geom_text(aes(label=`Percentage (%)`, fontface="bold"), position = position_dodge(0.9), vjust=2, size=4, color="#393E46") +
  scale_fill_manual("legend", values = PrimaryColors)
EducationGenderNoBA








