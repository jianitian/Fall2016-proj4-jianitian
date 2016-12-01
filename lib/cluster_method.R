library(cluster)

### Naive method to do clustering, comparing their performance about clustering results

medians2<-apply(newfeature.total,2,med)
mads2 <- apply(newfeature.total,2,mad)
scaled.feature.total <- scale(newfeature.total,center = medians2,scale = mads2)
featuremat.dist <- dist(scaled.feature.total)
feature.hclust <- hclust(featuremat.dist)
plot(feature.hclust,k=20,border="red",main='clustering of features')
groups.20 = cutree(feature.hclust,20)
clust.centroid = function(i, dat, groups.20) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}

sapply(unique(groups.20), clust.centroid, scaled.feature.total, groups.20)
table(groups.20)
s<-sapply(unique(groups.20),function(g)featuredf$songnames[groups.20 == g])
feature.pam <- pam(featuremat.dist,20)
names(feature.pam)
table(groups.20,feature.pam$clustering)
plot(feature.pam)

for (i in 1:20) {
  
  A<-gsub(".*/", "", s[[i]])
  selected_var<-vector()
  for (j in 1:length(A)) {
    selected_var[j]<-strsplit(A,".",fixed = TRUE)[[j]][1]
  }
  select_mat <- train_lyr[train_lyr$`dat2$track_id`==selected_var,]
}

rect.hclust(feature.hclust,k=5,border = 2)
identify(feature.hclust,N=20,MAXCLUSTER = 20)
### kmeans methods ####
k.means.fit <- kmeans(scaled_featuremat, 20)
attributes(k.means.fit)
k.means.fit$cluster



