# Res
# http://www.feat.engineering/classes-of-feature-selection-methodologies.html. Tree based models don't need feature selection as they're intrinsic

library(easypackages)
library(tidyverse)

libraries("dlookr","pastecs","randomForest","data.table")
theme_set(theme_minimal())

# Data Prep ####
set.seed(123)

# Munge the training data 
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
model$confusion

# Variable Importance 
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
