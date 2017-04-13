library(stringr)
library(e1071)
library(caret)
#set working directory

setwd("~/Documents/Columbia University/GR5243-Applied Data Science/Spr2017-proj4-team1-master/doc")
source('../lib/evaluation_measures.R')
#data cleaning
data.lib="../data/nameset"
data.files=list.files(path=data.lib, "*.txt")

data.files

## remove "*.txt"
query.list=substring(data.files, 
                     1, nchar(data.files)-4)

query.list
## add a space
query.list=paste(substring(query.list, 1, 1), 
                 " ", 
                 substring(query.list, 
                           2, nchar(query.list)),
                 sep=""
)
query.list

f.line.proc=function(lin, nam.query="."){
  
  # remove unwanted characters
  char_notallowed <- "\\@#$%^&?" # characters to be removed
  lin.str=str_replace(lin, char_notallowed, "")
  
  # get author id
  lin.str=strsplit(lin.str, "_")[[1]]
  author_id=as.numeric(lin.str[1])
  
  # get paper id
  lin.str=lin.str[2]
  paper_id=strsplit(lin.str, " ")[[1]][1]
  lin.str=substring(lin.str, nchar(paper_id)+1, nchar(lin.str))
  paper_id=as.numeric(paper_id)
  
  # get coauthor list
  lin.str=strsplit(lin.str, "<>")[[1]]
  coauthor_list=strsplit(lin.str[1], ";")[[1]]
  
  #print(lin.str)
  for(j in 1:length(coauthor_list)){
    if(nchar(coauthor_list[j])>0){
      nam = strsplit(coauthor_list[j], " ")[[1]]
      if(nchar(nam[1])>0){
        first.ini=substring(nam[1], 1, 1)
      }else{
        first.ini=substring(nam[2], 1, 1)
      }
    }
    last.name=nam[length(nam)]
    nam.str = paste(first.ini, last.name)
    coauthor_list[j]=nam.str
  }
  
  match_ind = charmatch(nam.query, coauthor_list, nomatch=-1)
  
  #print(nam.query)
  #print(coauthor_list)
  #print(match_ind)
  
  if(match_ind>0){
    
    coauthor_list=coauthor_list[-match_ind]
  }
  
  paper_title=lin.str[2]
  journal_name=lin.str[3]
  
  list(author_id=author_id, 
       paper_id=paper_id, 
       coauthor_list=coauthor_list, 
       paper_title=paper_title, 
       journal_name=journal_name)
}

data_list=list(1:length(data.files))

for(i in 1:length(data.files)){
  
  ## Step 0 scan in one line at a time.
  
  dat=as.list(readLines(paste(data.lib, data.files[i], sep="/")))
  data_list[[i]]=lapply(dat, f.line.proc, nam.query=query.list[i])
  
  
}
####


#add an extra list for coauthor
for (k in 1: length(query.list)){
  for (j in 1:length(data_list[[k]])){
    data_list[[k]][[j]]$coauthor<-paste(data_list[[k]][[j]][[3]], collapse = ' ')
  }
}


#paper title
it_train_list1 <- list(1:length(data.files))
#journal name
it_train_list2 <- list(1:length(data.files))
#coauthor
it_train_list3 <- list(1:length(data.files))
PaperID_list<- list(1:length(data.files))
AuthorID_list<- list(1:length(data.files))


vocab1 <- list(1:length(data.files))
vocab2 <- list(1:length(data.files))
vocab3 <- list(1:length(data.files))
for (j in 1:length(data.files)) {
  data_unlist <- unlist(data_list[[j]])
  paper_title<- as.vector(data_unlist[which(names(data_unlist)=="paper_title")])
  journal_name<- as.vector(data_unlist[which(names(data_unlist)=="journal_name")])
  coauthor_name<- as.vector(data_unlist[which(names(data_unlist)=="coauthor")])
  #paper_id<- as.vector(data_unlist[which(names(data_unlist)=="paper_id")])
  PaperID_list[[j]]<- 1:length(data_list[[j]])
  AuthorID_list[[j]]<- as.numeric(as.vector(data_unlist[which(names(data_unlist)=="author_id")]))
  #for paper title
  it_train_list1[[j]] <- itoken(paper_title, 
                               preprocessor = tolower, 
                               tokenizer = word_tokenizer,
                               #ids =paper_id,
                               ids =PaperID_list[[j]],
                               progressbar = FALSE)
  #for journal name
  it_train_list2[[j]] <- itoken(journal_name, 
                               preprocessor = tolower, 
                               tokenizer = word_tokenizer,
                               #ids =paper_id,
                               ids =PaperID_list[[j]],
                               progressbar = FALSE)
  #for coauthor name
  it_train_list3[[j]] <- itoken(coauthor_name, 
                               preprocessor = tolower, 
                               tokenizer = word_tokenizer,
                               #ids =paper_id,
                               ids =PaperID_list[[j]],
                               progressbar = FALSE)
  
  vocab1[[j]] <- create_vocabulary(it_train_list1[[j]], stopwords = c("a", "an", "the", "in", "on",
                                                               "at", "of", "above", "under"))
  
  vocab2[[j]] <- create_vocabulary(it_train_list2[[j]], stopwords = c("a", "an", "the", "in", "on",
                                                                     "at", "of", "above", "under"))
  
  vocab3[[j]] <- create_vocabulary(it_train_list3[[j]], stopwords = c("a", "an", "the", "in", "on",
                                                                     "at", "of", "above", "under"))

}

#### deal with the issue with author8 [J Smith]
AuthorID_list[[8]][AuthorID_list[[8]]==1] <- 2
AuthorID_list[[8]] <- AuthorID_list[[8]]-1
###

vectorizer<-list()
dtm_train<-list()
for ( i in 1:3){
vectorizer[[i]]<-list(1:length(data.files))
dtm_train[[i]] <- list(1:length(data.files))
}


  for (i in 1:length(data.files)){
    vectorizer[[1]][[i]] <- vocab_vectorizer(vocab1[[i]])
    dtm_train[[1]][[i]] <- create_dtm(it_train_list1[[i]], vectorizer[[1]][[i]])
    vectorizer[[2]][[i]] <- vocab_vectorizer(vocab2[[i]])
    dtm_train[[2]][[i]] <- create_dtm(it_train_list2[[i]], vectorizer[[2]][[i]])
    vectorizer[[3]][[i]] <- vocab_vectorizer(vocab3[[i]])
    dtm_train[[3]][[i]] <- create_dtm(it_train_list3[[i]], vectorizer[[3]][[i]])
  }


dtm_train_tfidf<-list()
for (i in 1:3){
  dtm_train_tfidf[[i]]  <- list(1:length(data.files))
}

for (j in 1:3){
for(i in 1:length(data.files)){
  tfidf <- TfIdf$new()
  dtm_train_tfidf[[j]][[i]] <- fit_transform(dtm_train[[j]][[i]], tfidf)
 }
}

#Perfrom Hybrid I, cbind dtm_train_tfidf[[1~3]] ----> dtm_train_tfidf[[4]]
dtm_train_tfidf[[4]]<-list()
for (j in 1:14){
  dtm_train_tfidf[[4]][[j]]<- cbind(dtm_train_tfidf[[1]][[j]],dtm_train_tfidf[[2]][[j]],dtm_train_tfidf[[3]][[j]])
}


authorid<-list()
samplesize<-list()
index_list<-list()
for (i in 1:length(data.files)){
  # numbers of AuthorIDs
  authorid[[i]]<- length(table(AuthorID_list[[i]]))
  #training size of each AuthorID, take around 50%
  samplesize[[i]]<-ceiling(table(AuthorID_list[[i]])/2)
  #index for the training
  index<-NULL
  for (j in 1:authorid[[i]]){
    index<-c(index,sample(PaperID_list[[i]][AuthorID_list[[i]] == j], size = samplesize[[i]][j]))
  }
  index_list[[i]]<-index
}

#factor y variable
for (i in 1:length(AuthorID_list)){
  AuthorID_list[[i]]<-factor(AuthorID_list[[i]])
}


#get train and test data
#Note: tm_train_tfidf[[i]][[j]] : 
#i= 1: paper title. i = 2: journal name. i = 3 : coauthor. i = 4 : Hybrid I  j: 1~14 authors

x.train<-list();x.test<-list();y.train<-list();y.test<-list()
for (i in 1:4){
  x.train[[i]]  <- list(1:length(data.files))
  x.test[[i]]  <- list(1:length(data.files))
  y.train[[i]]  <- list(1:length(data.files))
  y.test[[i]]  <- list(1:length(data.files))
  
}
for ( i in 1:4){
  for (j in 1:length(data_list)){
    x.train[[i]][[j]]<-dtm_train_tfidf[[i]][[j]][index_list[[j]],]
    x.test[[i]][[j]]<-dtm_train_tfidf[[i]][[j]][-index_list[[j]],]
    y.train[[i]][[j]]<-AuthorID_list[[j]][index_list[[j]]]
    y.test[[i]][[j]]<-AuthorID_list[[j]][-index_list[[j]]]
  }
}


#Choose the best parameter
svm_tune<-list()
for (i in 1:4){
  svm_tune[[i]]  <- list(1:length(data.files))
}
############# Warning, this step takes forever###############


a<-Sys.time()
for (i in 1:4){
  for (j in 1:length(data_list)){
svm_tune[[i]][[j]] <- tune(svm, train.x=x.train[[i]][[j]], train.y=y.train[[i]][[j]], kernel="radial",
                 ranges=list(cost =c(10,20,30,40,60,80,100,150,200),
                             gamma=c(0,0.01,0.05,seq(0.1,1,0.2),2)))
  }
}

Sys.time()-a
############# Warning, this step takes forever###############



best_mar<-matrix(NA,nrow = 14, ncol = 4)
best_gam<-matrix(NA,nrow = 14, ncol = 4)
for (i in 1:4){
  for (j in 1:14){
    best_mar[j,i]<-svm_tune[[i]][[j]]$performance$cost[which.min(svm_tune[[i]][[j]]$performance$error)] 
    best_gam[j,i]<-svm_tune[[i]][[j]]$performance$gamma[which.min(svm_tune[[i]][[j]]$performance$error)]
  }
}


#predict y value
pred<-list()
for (i in 1:4){
  pred[[i]]  <- list(1:length(data.files))
}

for (i in 1:4){
  for (j in 1:14){
    pred[[i]][[j]]<-predict(svm_tune[[i]][[j]]$best.model,x.test[[i]][[j]])
  }
}


# accuracy matrix
accuracy<-matrix(NA,nrow = 14, ncol = 4)
for (i in 1:4){
  for (j in 1:14){
    accuracy[j,i]<-mean(pred[[i]][[j]]==y.test[[i]][[j]])
  }
}



colnames(accuracy)<-c("Paper Title","Journal Title","Coauthor","Hybrid")
accuracy<-rbind(accuracy,apply(accuracy,2,mean),apply(accuracy,2,sd))
rownames(accuracy)<-c(query.list,"Mean","StdDev")

#shows accuracy
accuracy

confusionMatrix(pred[[1]][[1]], y.test[[1]][[1]])




