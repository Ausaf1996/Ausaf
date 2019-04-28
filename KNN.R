


setwd("D:\\Data Science - Course\\KNN")

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

prop.table(table(wbcd$diagnosis))


str(wbcd)

prop.table(table(wbcd$diagnosis))

# drop the id feature
wbcd <- wbcd[,-1]

target = wbcd[,1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

## table of proportions
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}




a=c(20,30,40,50)
a=c(200,600, 900, 1200)
normalize(a)
head(wbcd)
x=colnames(wbcd)



# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))


head(wbcd_n)

# confirm that normalization worked
summary(wbcd_n$area_mean)



# create training and test data

set.seed(111)

index = sample(nrow(wbcd_n), nrow(wbcd_n)*0.8)

wbcd_train = wbcd_n[index,]

wbcd_test = wbcd_n[-index,]

head(wbcd_train)
# create labels for training and test data

wbcd_train_labels <- wbcd[index, 1]
wbcd_test_labels <- wbcd[-index, 1]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)


wbcd_test_pred = knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 3)

### confusion matrix
table(wbcd_test_labels, wbcd_test_pred)

 summary(wbcd_test_pred)
 
 rec = 35/39
 
 rec
 
 pre = 35/36

 pre
library(gmodels)
CrossTable(wbcd_test_labels,wbcd_test_pred )

2*0.97*0.89/(0.97+0.89)

35/36

35/39


2*0.97*0.9/( 0.97+0.9)


## Step 4: Evaluating model performance ----

# load the "gmodels" library

library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)



# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

### Try different values of K 

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
