# Week3 assignment
getwd()
setwd("/Users/nezihyalabik/Desktop/School book 2/Pazarlama analizi/R scripts/week3")
ds <- read.csv("adult_income.csv", header =T)


# Shuffling Splitting data into train and test sets
row_count <- nrow(ds)
shuffled_rows <- sample(row_count)
train <- ds[head(shuffled_rows,floor(row_count*0.75)),]
test <- ds[tail(shuffled_rows,floor(row_count*0.25)),]

# We will continue to work with train data from now on
nrow(train)

summary(train)
plot(train)

# Omitting unique attirbute "ID", Wor
train2 <- train[c(-1,-3,-9,-10)]
plot(train2)
cor(train2)
train2

model <- glm(income_high ~ ., family = "binomial", data=train2)
summary(model)


attributes(model)
model$coefficients


# Now we get the individual risk factors (or odds ratios).
# The 2 at the end is the number of decimals we want.
round(exp(coef(model)), 2) 

x_test <- subset(test, select = -c(income_high) )
y_test <- subset(test, select = c(income_high) )
predictions <- predict(model, x_test)
# summarize results
head(predictions)

for(i in 1:length(predictions)){
  if(predictions[i] >= 0.5){
    
    predictions[i] = 1
    
  } else {
    
    predictions[i] = 0
    
  }
  
}

confusionMatrix(predictions$class, y_test)


#Find the probability of admitting chance
probofadmit<-round((exp(admitance660and3)/(exp(admitance660and3)+1)), 2) 
probofadmit


train2["capital_gain"<1000,]

