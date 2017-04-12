library(stringr)
library(e1071)
library(pacman)
library(caret)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(text2vec, dplyr, qlcMatrix, kernlab, knitr)

AKumar <- data.frame(scan("../data/nameset/AKumar.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set

AKumar$AuthorID <- sub("_.*","",AKumar$Coauthor)
AKumar$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", AKumar$Coauthor)
AKumar$Coauthor <- gsub("<","",sub("^.*?\\s","", AKumar$Coauthor))
AKumar$Paper <- gsub("<","",AKumar$Paper)
AKumar$PaperID <- rownames(AKumar)
AKumar$Journal <- gsub("<","",AKumar$Journal)

it_train1 <- itoken(AKumar$Journal, 
                   preprocessor = tolower, 
                   tokenizer = word_tokenizer,
                   ids = AKumar$PaperID,
                   # turn off progressbar because it won't look nice in rmd
                   progressbar = FALSE)
vocab1 <- create_vocabulary(it_train1, stopwords = c("a", "an", "the", "in", "on",
                                                   "at", "of", "above", "under"))

vectorizer1 <- vocab_vectorizer(vocab1)
dtm_train1 <- create_dtm(it_train1, vectorizer1)

tfidf <- TfIdf$new()
dtm_train_tfidf1 <- fit_transform(dtm_train1, tfidf)

dim(dtm_train_tfidf1)

AKumar$PaperID <- as.numeric(AKumar$PaperID)
AKumar$AuthorID <- as.numeric(AKumar$AuthorID)
# AKumar$Journal <- as.numeric(AKumar$Journal)


per_train <- 0.8 # percentage of training data
smp_size <- floor(per_train * nrow(AKumar)) # size of the sample
index <- sample(seq_len(nrow(AKumar)), size = smp_size)

# # numbers of AuthorIDs
# authorid<- length(table(AKumar$AuthorID))
# #training size of each AuthorID, take around 50%
# samplesize<-ceiling(table(AKumar$AuthorID)/2)
# #index for the training
# index<-NULL
# for (i in 1:authorid){
#   index<-c(index,sample(AKumar$PaperID[AKumar$AuthorID == i], size = samplesize[i]))
# }


AKumar$AuthorID <- factor(AKumar$AuthorID)

x.train1<-dtm_train_tfidf1[index,]
x.test1<-dtm_train_tfidf1[-index,]
y.train<-AKumar[index,]$AuthorID
y.test<-AKumar[-index,]$AuthorID



# model<- svm(x = x.train, y = y.train,cost = 222,gamma = 0.1,cross = 5)
# pred<-predict(model,x.test)
# mean(pred == y.test)



#Cross Validation
# cost.list<- c(0.1,1,10,100,1000)
# gamma.list<- seq(0.1,1,0.1)
# error<-matrix(NA,nrow = length(gamma.list),ncol = length(cost.list))
# test.sd<-matrix(NA,nrow = length(gamma.list),ncol = length(cost.list))
# for (i in 1:length(cost.list)) {
#   for (j in 1:length(gamma.list) ) {
#     model<- svm(x = x.train, y = y.train,cost = i,gamma = j)
#     pred<-predict(model,x.test)
#     mean(pred == y.test)
# 
#     error[j,i]<- as.numeric(crossvalid$evaluation_log[crossvalid$best_iteration,4])
#   }
# }

cost.list<-c(0.1,1,10,20,30,40,50,60,70,80,90,100,200,300,1000)
gamma.list<- c(seq(0.1,1,0.1),2,5,9,20)

svm_tune1 <- tune(svm, train.x=x.train1, train.y=y.train, kernel="radial",
                 ranges=list(cost =c(0.1,1,10,20,30,40,50,60,70,80,90,100,200,300,1000),
                             gamma=c(seq(0.1,1,0.1),2,5,9,20)))


per<-svm_tune1$performance
best_mar<-per$cost[which.min(per$error)] 
best_gam<-per$gamma[which.min(per$error)] 
best_mar;best_gam

pre<-predict(svm_tune1$best.model,x.test1)
confusionMatrix(pre,y.test)





