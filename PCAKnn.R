############################################################################
#KNN - comaprison of accuracy with principal components
#Created By: Anand
#Date      : 10-12-2020
############################################################################
rm(list=ls())

set.seed(2021)

library("caret")
library("class")
library("e1071")
library("corrplot")
library("RColorBrewer")

data(iris)
dim(iris)

names(iris)
head(iris)

iris<-na.omit(iris)
dim(iris)
train_index <- sample(1:nrow(iris), (3/4) * nrow(iris))
test_index <- setdiff(1:nrow(iris), train_index)

iris.train<-iris[train_index,]
iris.test<-iris[test_index,]

dim(iris)
dim(iris.train)
dim(iris.test)

unique(iris$Species)
unique(iris.train$Species)
unique(iris.test$Species)

#####################################################################
kntrainerror=c()
kntesterror=c()

applyknn<-function(k,tdata,testdata){
  knn(train=tdata[,-5],
      test=testdata[,-5],
      cl=as.factor(tdata$Species),
      k=k)
} 

for(i in c(1,2,3,4,5,6,7,8,9,10)){
  predict.knn.train<-applyknn(i,iris.train,iris.train)
  predict.knn.test<-applyknn(i,iris.train,iris.test)
  
  classif<-(predict.knn.train!=iris.train$Species)
  kntrainerror=append(kntrainerror,length(classif[classif==TRUE])/length(iris.train$Species))
  
  classif<-(predict.knn.test!=iris.test$Species)
  kntesterror=append(kntesterror,length(classif[classif==TRUE])/length(iris.test$Species))
}


##########################Plotting Errors########################
x11()
comperror<-data.frame("K"=c(1,2,3,4,5,6,7,8,9,10),
                      "knntrainerror"=kntrainerror,
                      "knntesterror"=kntesterror)

colors <- c("k-NN Train Error" = "turquoise","k-NN Test Error"="springgreen")
ggplot()+
  geom_line(data=comperror,aes(K, knntrainerror,color="k-NN Train Error"),size = 0.8)+
  geom_line(data=comperror,aes(K, knntesterror,color="k-NN Test Error"),size = 0.8)+
  labs(x = "K",
       y = "Classification Error",
       color = "Legend") +
  scale_color_manual(values = colors)+ggtitle("Classification Error for k-NN")

################################################################
##Optimal k = 3 for which the error is less
################################################################

predict.knn.test<-applyknn(3,iris.train,iris.test)

testconfmat<-confusionMatrix(predict.knn.test, iris.test$Species)

testconfmat

################################################################
#################Principal Components KNN#######################

cor.iris <-cor(iris[,-5])
x11()
corrplot(cor.iris, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#From this we know Sepal length and petal width covers most of the dataset


pcomp$rotation
var<-pcomp$sdev^2

pervar<-var/sum(var)
pervar

################################################################
pca<-princomp(iris[,-5])
pcomp<-pca$scores
pca.comp1<- -1*pcomp[,1]
pca.comp2<- -1*pcomp[,2]

iris.pcomp<-cbind(pca.comp1,pca.comp2)
head(iris.pcomp)
dim(iris.pcomp)

iris.train<-iris.pcomp[train_index,]
iris.test<-iris.pcomp[test_index,]
train.label<-iris[train_index,5]
test.label<-iris[test_index,5]
dim(iris.train)
dim(iris.test)
length(train.label)
length(test.label)
#####################################################################
kntrainerror=c()
kntesterror=c()

applyknn<-function(k,tdata,testdata,responsevar){
  knn(train=tdata,
      test=testdata,
      cl=as.factor(responsevar),
      k=k)
} 

for(i in c(1,2,3,4,5,6,7,8,9,10)){
  predict.knn.train<-applyknn(i,iris.train,iris.train,train.label)
  predict.knn.test<-applyknn(i,iris.train,iris.test,train.label)
  
  classif<-(predict.knn.train!=train.label)
  kntrainerror=append(kntrainerror,length(classif[classif==TRUE])/length(train.label))
  
  classif<-(predict.knn.test!=test.label)
  kntesterror=append(kntesterror,length(classif[classif==TRUE])/length(test.label))
}


##########################Plotting Errors####################################################
x11()
comperror<-data.frame("K"=c(1,2,3,4,5,6,7,8,9,10),
                      "knntrainerror"=kntrainerror,
                      "knntesterror"=kntesterror)

colors <- c("k-NN Train Error" = "turquoise","k-NN Test Error"="springgreen")
ggplot()+
  geom_line(data=comperror,aes(K, knntrainerror,color="k-NN Train Error"),size = 0.8)+
  geom_line(data=comperror,aes(K, knntesterror,color="k-NN Test Error"),size = 0.8)+
  labs(x = "K",
       y = "Classification Error",
       color = "Legend") +
  scale_color_manual(values = colors)+ggtitle("Classification Error for k-NN")


#############################################################################################

predict.knn.train<-applyknn(3,iris.train,iris.train,train.label)
predict.knn.test<-applyknn(3,iris.train,iris.test,train.label)


conftrain<-confusionMatrix(predict.knn.train,train.label)
conftest<-confusionMatrix(predict.knn.test,test.label)
conftrain
conftest

#############################################################################################
x11()
plot(pca.comp1,pca.comp2,col=c("brown1","dodgerblue1","limegreen")[as.integer(iris$Species)],
     pch=c(1,2,3)[as.integer(iris$Species)])
legend(x="topleft",
       legend=c("setosa","versicolor","virginica"),
       col=c("brown1","dodgerblue1","limegreen"),pch=c(1,2,3))

#############################################################################################