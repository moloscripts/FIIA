# Res
# http://www.feat.engineering/classes-of-feature-selection-methodologies.html. Tree based models don't need feature selection as they're intrinsic

# Libraries ####
library(easypackages)
libraries("dlookr","pastecs","randomForest","data.table","tidyverse","pdp")
theme_set(theme_minimal())

# Data Prep ####

# Munge the training data 
set.seed(123)
training <- training.set %>%
  select(-c("uniqueid","bank_account")) %>%
  rename(Location = `Location Type`, 
         cellphone = `Cellphone Access`,
         household_size = `Household Size`,
         relationship_with_head=`Relationship With Head`,
         marital_status=`Marital Status`,
         education_level=`Education Level`,
         job_type=`Job Type`)
training$BankAccount <- as.factor(training$BankAccount)

# Munge the testing data
testing <- testing.FIIA %>%
  select(-uniqueid) %>%
  rename(Location = location_type, 
         cellphone = cellphone_access,
         Age = age_of_respondent, 
         Gender = gender_of_respondent)


# Fitting the Random Forest ####
# model parameters
# ntrees - Number of trees 
# mtry is the number of variables split at each node. in a regression problem mtry is gotten by p/3. In a classification problem, mtry is gotten by the square root of p. 
# where p is the predictor variables.

model <- randomForest(formula = BankAccount ~ ., data = training)
model

## Variable Importance Plot  ###
VariableImportance <- model$importance
VariableImportance <- as.data.frame(VariableImportance)
setDT(VariableImportance, keep.rownames = TRUE)[]
VariableImportance <- VariableImportance %>%
  rename(Variable = rn) 
VariableImportance

# Plot
varImportancePlot <- ggplot(VariableImportance, aes(reorder(Variable, MeanDecreaseGini), MeanDecreaseGini, fill=MeanDecreaseGini)) +
  geom_col() + 
  labs(title = "Order of variable importance", 
       x = "Variable", 
       y = "Mean Decrease Gini") + 
  scale_fill_gradient(low="blue", high="red") +
  theme(legend.position = "none") +
  coord_flip() +
  theme_minimal()
varImportancePlot

## Partial Dependence Plots ####

# Job Type
jobTypePDP <- model %>%  # the %>% operator is read as "and then"
  partial(pred.var = "job_type") %>%
  autoplot(smooth = F, ylab = expression(f(job_type)), color="#FFA900", size=4) +
  theme_light() + 
  coord_flip()
jobTypePDP

# Education Level
EducationLevelPDP <- model %>%  # the %>% operator is read as "and then"
  partial(pred.var = "education_level") %>%
  autoplot(smooth = F, ylab = expression(f(education_level)), color="#FFA900", size=4) +
  theme_light() + 
  coord_flip()
EducationLevelPDP

# Age
AgePDP <- model %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Age") %>%
  autoplot(smooth = T, ylab = expression(f(Age)), color="#FFA900") +
  theme_light() 
AgePDP

# Hyper Parameter testing ####
# tuning the mtry
tune_mtry <- tuneRF(training[,1:11], 
                    training$BankAccount, 
                    ntreeTry = 500, 
                    stepFactor = 2, 
                    plot = T, 
                    trace = T, 
                    # doBest = T,
                    improve = 0.01)
# print(tune_mtry)



