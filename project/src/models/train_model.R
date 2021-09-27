
# Libraries 

library(data.table)

# Loading 'train' and 'test'

train <- fread('./project/volume/data/interim/train.csv')
test <- fread('./project/volume/data/interim/test.csv')

# Save model to a separate file

saveRDS(modelx,"./project/volume/models/modelx.model") # replace with intended model

# Making submission file

test$result <- test$modelx # replace with intended model

fwrite(test[, .(id, result)], './project/volume/data/processed/submit.csv')
