### Generalized linear regression to do the multinomial regression
X=solve(t(newfeature.total)%*%newfeature.total,t(newfeature.total)%*%percentages_mat)
save(X,file = "corelationmt.RData")

# Another lad method to do clusting, the effect is not good enough
fit1 <- lda.cvb0(documents, K=K, vocab = vocab, num.iterations = G, alpha = alpha, eta = eta, trace = 0L)

### Now I want to use regression to find the relationship between features and songs' topics
library(foreign)
library(nnet)
feature.topic <- as.data.frame(cbind(newfeature.total,songs_topic))
colnames(feature.topic)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14")
test <- multinom(songs_topic ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14, data = feature.topic)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
