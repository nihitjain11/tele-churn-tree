# Churn Analysis of Telecom Customers using Decision Trees

# Importing the dataset
dataset = read.csv('Churn.txt')

# count of missing values in dataset
sum(is.na(dataset))

# 5-point summary
summary(dataset)

# factorizing categorical
dataset$X = factor(dataset$x,
                   levels = levels(dataset$x),
                   labels = c(1:length(levels(dataset$x))))
dataset$X = factor(dataset$x,
                   levels = levels(dataset$x),
                   labels = c(1:length(levels(dataset$x))))
dataset$X = factor(dataset$x,
                   levels = levels(dataset$x),
                   labels = c(1:length(levels(dataset$x))))
dataset$X = factor(dataset$x,
                   levels = levels(dataset$x),
                   labels = c(1:length(levels(dataset$x))))
dataset$X = factor(dataset$x,
                   levels = levels(dataset$x),
                   labels = c(1:length(levels(dataset$x))))



library(rpart)

# grow tree 
fit <- rpart(dataset$Churn. ~ dataset$Int.l.Plan + dataset$VMail.Plan + dataset$VMail.Message + 
               dataset$Day.Mins + dataset$Day.Calls + dataset$Day.Charge + 
               dataset$Eve.Mins + dataset$Eve.Calls  + dataset$Eve.Charge + 
               dataset$Night.Mins + dataset$Night.Calls + dataset$Night.Charge +
               dataset$Intl.Mins + dataset$Intl.Calls + dataset$Intl.Charge +
               dataset$CustServ.Calls,
             method="class", data=dataset)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for CHURN")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for CHURN")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)




# Fitting the Regression Model to the dataset
# install.packages('rpart')
library(rpart)
regressor = rpart(formula = Churn. ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))


# Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the Regression Model results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Churn.),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('Level') +
  ylab('Salary')

# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)



# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Churn.),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('Level') +
  ylab('Salary')



# Classification template

# Importing the dataset
dataset = read.csv('Churn.txt')
#dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Lace = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Churn., SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
-# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting classifier to the Training set
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = Churn. ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Classifier (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Classifier (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

plot(classifier)
text(classifier)
