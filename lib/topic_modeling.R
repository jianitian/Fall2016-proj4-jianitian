##### topic modeling method

####install libraries
devtools::install_github("cpsievert/LDAvisData")
install.packages("LDAvis")
install.packages("lda")
library(LDAvisData)
data(reviews, package = "LDAvisData")
library(NLP)
library(tm)
library(lda)
library(LDAvis)

train_lyr <- lyr[index_train,]
test_lyr <- lyr[-index_train,]

documents<-list()
word_index<-list()
for (i in 1:dim(train_lyr)[1]) {
  this.row <- train_lyr[i,]
  word_index[[i]] <- which(this.row!=0)
  this.n<-length(word_index[[i]])
  this.mat <- matrix(nrow = 2,ncol = this.n-1)
  for (j in 1:this.n-1){
    this.mat[1,j] <- as.integer((word_index[[i]][2:this.n]-1)[j]-1)
    this.mat[2,j] <- as.integer((as.numeric(this.row[word_index[[i]]])[2:this.n])[j])
  }
  documents[[i]] <- as.matrix(this.mat)
}

save(documents,file = "document.RData")
vocab <- colnames(train_lyr)[2:dim(train_lyr)[2]]

# MCMC and model tuning parameters:
# I use validation try to find a better tune, the larger G, the more exactly the result will be 
# the larger alpha and eta, the more even the topic will be

K <- 18
G <- 5000
alpha <- 0.1
eta <- 0.1


# Fit the model:

set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # time is 6.67 min in my laptop

save(fit,file = "fit.RData")
topics<-fit$topics
topic1<-which(topics[1,]!=0,arr.ind = T, useNames = T)
topic_rank_result <- matrix(nrow = 18,ncol = 4973)
for (i in 1:18) {
  topic_rank_result[i,] <- rank(-topics[i,],ties.method ="min")
}
save(topic_rank_result,file = "topic_rank_result.RData")
unique(fit$assignment[[2]])

topic_mat <- matrix(nrow = 2000,ncol = 18)
percentages_mat <- matrix(nrow = 2000, ncol=18)
colnames(topic_mat) <- 0:17
colnames(percentages_mat) <- 0:17
for (i in 1:2000) {
  for (j in 1:18) {
    thismat <- t(as.matrix(table(fit$assignment[i])))
    these_var<-as.numeric(colnames(thismat))+1
    topic_mat[i,these_var]<-as.numeric(thismat)
    n<-18-length(these_var)
    topic_mat[i,-these_var]<-rep(0,n)
    percentages_mat[i,] <- topic_mat[i,]/rowSums(topic_mat)[i]
  }
}

# Another lad method to do clusting, the effect is not good enough
# fit1 <- lda.cvb0(documents, K=K, vocab = vocab, num.iterations = G, alpha = alpha, eta = eta, trace = 0L)


### Generalized linear regression to do the multinomial regression
X=solve(t(newfeature.total)%*%newfeature.total,t(newfeature.total)%*%percentages_mat)
save(X,file = "corelationmt.RData")

### find only one topic for each song
songs.topic <- vector()
for (i in 1:2000) {
  thismat <- t(as.matrix(table(fit$assignment[i])))
  ind <- which.max(thismat)
  songs.topic[i] <- colnames(thismat)[ind]
}
length(songs.topic)
table(songs.topic)
songs_topic <- as.factor(songs.topic)

### Now I want to use regression to find the relationship between features and songs' topics
library(foreign)
library(nnet)
feature.topic <- as.data.frame(cbind(newfeature.total,songs_topic))
colnames(feature.topic)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14")
test <- multinom(songs_topic ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14, data = feature.topic)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors



predict(test, newdata = dses)




