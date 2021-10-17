
# Libraries ---------------------------------------------------------------

library(data.table)
library(caret)
library(glmnet)
library(plotmo)


# Load train and test -----------------------------------------------------

train <- fread("./project/volume/data/interim/train.csv")
test <- fread("./project/volume/data/interim/test.csv")

### remove duplicates
train <- train[!duplicated(train),]

### keep future_price as train_y
train_y <- train$future_price


# Drop id -----------------------------------------------------------------

drops <- c('id')
train <- train[, !drops, with = FALSE]
test <- test[, !drops, with = FALSE]


# fit a linear model ------------------------------------------------------

model0 <- lm(future_price~., data=train)
summary(model0)
pred <- predict(model0, newdata=test)

sample_sub <- fread("./project/volume/data/raw/sample_sub.csv")
sample_sub$future_price <- pred
fwrite(sample_sub, "./project/volume/data/processed/submit_lm.csv")


# make a submission file with merge ---------------------------------------

### instead of simple replacing future_price with pred,
### merging is a safer approach

train <- fread("./project/volume/data/interim/train.csv")
test <- fread("./project/volume/data/interim/test.csv")
train <- train[!duplicated(train),]
train_y <- train$future_price

### we will keep id in train and test
### but do lm and prediction without id
drops <- c('id')
model0 <- lm(future_price~., data=train[, !drops, with=F])
pred <- predict(model0, newdata=test[, !drops, with=F])
result <- data.table(id=test$id, future_price=pred)

### load the sample submission file and remove future_price
sample_sub <- fread("./project/volume/data/raw/sample_sub.csv")
sample_sub$future_price <- NULL

### merge sample_sub with result while keeping the order
setkey(sample_sub, id)
setkey(result, id)
sample_sub <- merge(sample_sub, result, sort=F)
fwrite(sample_sub, "./project/volume/data/processed/submit_lm.csv")


# fit a lasso model -------------------------------------------------------

drops <- c('id')
train <- train[, !drops, with = FALSE]
test <- test[, !drops, with = FALSE]


# Make dummy variables ----------------------------------------------------

### make fake future_price for dummyVars
test$future_price <- 0

### work with dummies
dummies <- dummyVars(future_price ~ ., data=train)
train <- predict(dummies, newdata=train)
test <- predict(dummies, newdata=test)

### convert them back to data.table
train <- data.table(train)
test <- data.table(test)


# Fit a model using cross-validation --------------------------------------

### glmnet needs matrix
train <- as.matrix(train)

### fit a model
model1 <- cv.glmnet(train, train_y, alpha=1, family="gaussian")

### choose the best lambda
best_lambda <- model1$lambda.min

### see what predictors chosen from the lambda
predict(model1, s=best_lambda, newx=test, type="coefficients")


# Fit a full model --------------------------------------------------------

model2 <- glmnet(train, train_y, alpha=1, family="gaussian")

plot_glmnet(model2)

### save the model
saveRDS(model2,"./project/volume/models/model.model")


# Predict by the model ----------------------------------------------------

### glmnet needs matrix
test <- as.matrix(test)

pred <- predict(model2, s=best_lambda, newx=test)


# Make a submission file --------------------------------------------------

sample_sub <- fread("./project/volume/data/raw/sample_sub.csv")

sample_sub$future_price <- pred

fwrite(sample_sub, "./project/volume/data/processed/submit_lasso.csv")
