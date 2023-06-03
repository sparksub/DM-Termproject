# 1. install packages 
install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

install.packages('rattle')
library('rattle')

install.packages("party")
library(party)


# 2. Get & Check dataset
depression = read.csv("data/preprocessing_hn18.csv", header=TRUE) 
depression <- subset(depression, select=-c(ID))
names(depression)

# 3. Devide training set & test set
set.seed(1234)
train <- sample(nrow(depression), 0.7*nrow(depression))  # stratified sampling
depression.train <- depression[train,]
depression.test <- depression[-train,]
table(depression.train$depression_scale)
table(depression.test$depression_scale)

# 4. Triaining
depression.dtree <- rpart(formula=depression_scale ~ ., data=depression.train, parms =list(split="gini"))

rpart.plot(depression.dtree)

depression.dtree.pred <- predict(depression.dtree, newdata=depression.test, type ="prob")
head(depression.dtree.pred)

depression.dtree.pred <- predict(depression.dtree, newdata=depression.test, type ="class")
head(depression.dtree.pred)

table(depression.test$depression_scale, depression.dtree.pred, dnn=c("Actual", "Predicted"))
table(depression.dtree.pred, depression.test$depression_scale, dnn=c("Predicted","Actual"))

mean(depression.dtree.pred == depression.test$depression_scale)

depression.dtree$cptable
printcp(depression.dtree)

plotcp(depression.dtree)

depression.dtree_pruned <- rpart(formula= depression_scale ~ ., data=depression.train, method="class", cp=0.020115, parms =list(split="gini"))
depression.dtree_pruned2 <- prune(depression.dtree, cp=0.020115)
printcp(depression.dtree)

rpart.plot(depression.dtree_pruned, box.palette="Blues")


depression.dtree.pred_prunded <- predict(depression.dtree_pruned, newdata=depression.test, type ="class")
mean(depression.dtree.pred_prunded == depression.test$depression_scale)
