
# Libraries ---------------------------------------------------------------

library(data.table)
library(caret)


# Load datasets -----------------------------------------------------------

train <- fread("./project/volume/data/raw/start_train.csv")
test <- fread("./project/volume/data/raw/start_test.csv",skip=1)
test <- test[,.(V1)]
setnames(test,'V1','id')
card_tab <- fread("./project/volume/data/raw/card_tab.csv")

### remove duplicates
train <- train[!duplicated(train),]
card_tab <- card_tab[!duplicated(card_tab),]


# Make master -------------------------------------------------------------

### First make train and test the same dimension,
### then bind into one table so you can do the same thing to both datasets

### make a future price column for test, even though it is unknown
### We will not use this, which is only to make them two tables the same size
test$future_price <- 0

### add a column that lets you easily differentiate
### between train and test rows once they are together
train$train <- 1
test$train <- 0

### now bind them together
master <- rbind(train, test)


# Combine features with master --------------------------------------------

### we will use [rarity], [types], and [color]

### make a table for type
unique(card_tab$types)
types_tab <- as.data.table(tstrsplit(card_tab$types," "))
types_tab$id <- card_tab$id

### make a dummy variable for type
types_tab_m <- melt(types_tab, id.vars = "id")
types_tab_m <- types_tab_m[!is.na(types_tab_m$value)]
types_tab <- dcast(types_tab_m, id ~ value, length)

### make a table for color
unique(card_tab$colors)
colors_tab <- as.data.table(tstrsplit(card_tab$colors," "))
colors_tab$id <- card_tab$id

### make a dummy variable for color
colors_tab_m <- melt(colors_tab, id.vars = "id")
colors_tab_m <- colors_tab_m[!is.na(colors_tab_m$value)]
colors_tab <- dcast(colors_tab_m, id ~ value, length)
colors_tab[colors_tab == 2] <- 1

### merge with master
setkey(master,id)
setkey(card_tab,id)
master <- merge(master, card_tab[,.(id, rarity)], all.x=T)
setkey(types_tab,id)
master <- merge(master, types_tab, all.x=T)
setkey(colors_tab,id)
master <- merge(master, colors_tab, all.x=T)

### set color na's in master to 0
master[is.na(master)] <- 0

# Split back to train and test --------------------------------------------

train <- master[train==1]
test <- master[train==0]

### clean up columns
train$train <- NULL
test$train <- NULL
test$future_price <- NULL


# Save train and test -----------------------------------------------------

fwrite(train, "./project/volume/data/interim/train.csv")
fwrite(test, "./project/volume/data/interim/test.csv")
