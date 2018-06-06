# Clear all variables
rm(list=ls(all=TRUE)) 
# Close all figures
graphics.off()


# Importing Data
dataset=read.csv("Social_Net_Ad.csv")
dataset=dataset[,3:5]

# Splitting the dataset into Training Set and Test Set
install.packages("caTools")
library(caTools)
set.seed(123)
#split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

# Feature Scaling
#Apply to training set and test set
# Apply to Independent Variables
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])

#Logistic Regression
classifier=glm(formula=Purchased~.,family=binomial,data=training_set)

# Predicting Logistic Regression
# type="response" will give probabilities listed in a single vector
prob_pred=predict(classifier,type="response",newdata=test_set[-3])
# to predict 0 and 1s
y_pred=ifelse(prob_pred>0.5,1,0)

####### CONFUSION MATRIX #######
# Confusion Matrix to evaluate the model
# here you put index of dependent variable
cm=table(test_set[,3],y_pred)
# test_set[,3] is vector of real values
# y_pred is vector of predictions

# Visualization the training_set results
#install.packages("ElemStatLearn")
#library(ElemStatLearn)
set=training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


