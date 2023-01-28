###Set env
getwd()
path <- readline()    ## insert your directory:
C:\DirveD\Data-Scinece\8-ML in R\Session1\knn_project
setwd(path)

dir()

### Load Data set
df <- read.csv("KnnDataSet.csv")


head(df)
str(df)
summary(df)

#### Drop ID column
df <- df[-1]
str(df)

###Preprocessing =>  recode Target column as Factor
### we can use table function
##step1
table(df$diagnosis)

#or we can use 
#library(plyr)
#df %>% 
 # count(diagnosis)

prop.table(table(df$diagnosis))
df$diagnosis <- factor(x = df$diagnosis , levels = c("B", "M") , labels = c("Khosh" , "Bad"))
str(df)
round(prop.table(table(df$diagnosis))*100 ,digits = 1)

###step2
#definiition a function for normalize data
normalize <- function(x){
   return((x- min(x))/(max(x)-min(x)))
}
#normalizing data set
df_n <- as.data.frame(lapply(df[2:31], normalize ))  #notice: df_n must be a dataframe again
summary(df_n)


#step3
#spiliting dataset
df_train <- df_n[1:469,]
df_test <- df_n[470:569,]

df_train_lable <- df$diagnosis[1:469]
df_test_lable  <- df$diagnosis[470:569]

#modeling
library("class")
df_test_predict <- knn(train =df_train,test = df_test,cl = df_train_lable,k = 3 )

# evaluation
library(gmodels)
CrossTable(x = df_test_lable, y = df_test_predict)

library('caret')
cm <- confusionMatrix(df_test_predict, df_test_lable)


#determining of perfomance K
error_rate= 0
for (i in 1:50) {
  df_test_predict <- knn(train =df_train,test = df_test,cl = df_train_lable,k = i )
  error_rate[i] <- mean(df_test_predict != df_test_lable)
}


library(ggplot2)

k.values <-  1:50
error_df <- data.frame(error_rate ,k.values)


error_df

ggplot(data = error_df ,mapping = aes( x= k.values , y = error_rate ))+
  geom_point() + geom_line(color="red")

