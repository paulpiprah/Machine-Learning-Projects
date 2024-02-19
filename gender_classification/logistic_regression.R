#load library
library(e1071)
library(ggplot2)


#import dataset
df <- read.csv("gender_classification/data/gender_classification_v7.csv")
df

# data preprocessing
str(df)
summary(df)

# check for unique values and comments
unique(df$long_hair)
unique(df$nose_wide)
unique(df$nose_long)
unique(df$lips_thin)
unique(df$distance_nose_to_lip_long)

# variable conversion
df$long_hair <- as.factor(df$long_hair)
df$nose_wide <- as.factor(df$nose_wide)
df$nose_long <- as.factor(df$nose_wide)
df$lips_thin <- as.factor(df$lips_thin)
df$distance_nose_to_lip_long <- as.factor(df$distance_nose_to_lip_long)

# convert male/female to binary variable
df$gender <- ifelse(df$gender == "Male" , 1, 0)
df$gender <- as.factor(df$gender)

# checking for missing values
missing_values <- colSums(is.na(df))
missing_values

# visualize variable
ggplot(df, aes(x = forehead_width_cm))+
  geom_histogram(col= "black", fill = "red")+
  labs(title = "Distribution of Forehead Widths",
       x="Forehead Width (cm)",
       y="Frequency")+
  theme_minimal()

ggplot(df,aes(x=forehead_height_cm))+
  geom_histogram(bins = 15,col ="black", fill= "dodgerblue")+
  labs(title = "Distribution of Forehead Heights",
       x="Forehead Height (cm)",
       y="Wave")+
  theme_minimal()

ggplot(df, aes(x=gender))+
  geom_bar(col="black", fill="steelblue")+
  labs(title = "Bar Chart of Gender",
       x="Gender",
       y="count")+
  theme_minimal()

# split data into testing and training
index <- sample(1:nrow(df), nrow(df) * 0.7)
train <- df [index, ]
test <- df [-index, ]

# Fit in logistic regression model
baseline_model <- glm(gender ~ ., data= train, family= "binomial")
summary(baseline_model)

# Make Predictions with the baseline model
predictions <- predict(baseline_model, newdata = test, type = "response")

# Threshold predictions to obtain predicted classes
predicted_gender <- ifelse(predictions > 0.3, 1,0)

# Generate confusion model for baseline model
cm <- table(predicted_gender, test$gender)
cm

# Calculate accuracy of the baseline model
accuracy <- sum(diag(cm)/ sum(cm))
round(accuracy * 100, 2)

