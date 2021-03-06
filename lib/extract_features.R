setwd("/Users/jianitian/Desktop/Project4")

##read data
load("lyr.RData")
### delete number 2 3 and 6:30 column
lyr <- lyr[,-c(2,3,6:30)]

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)

###########################cluster according to the analysis features##########################

########## loop to get all the songs features#######
feature.list <- list()

barstart_avgtime <- list()
barstart_sdtime <- list()
barstart_length <- list()
barstart_time <- list()
bar_confidence_mean <- list()

beatstart_avgtime <- list()
beatstart_sdtime <- list()
beatstart_length <- list()
beatstart_time <- list()
beat_confidence_mean <-list()

sectioncenter <- list()
sectionmean <- list()
sectionsd <- list()
section_length <- list()
section_confidence_mean <-list()

segment_confidence_mean <-list()
segment_length <- list()
segment_avgleng <- list()

loudness_quantile <- list()
loudness_range <- list()
loudness_mean <- list()
loudness_sd <- list()
loudness_trend <- list()
loudness_loud_timeline <- list()
loudness_timeline <- list()

pitches <- list()
pitch_mean <- list()
pitch_quantile <- list()

timber <- list()
timbre_mean <- list()
timbre_quantile <-list()

tatum_confidence_mean <- list()
tatum_avgtime<-list()
tatum_sdtime <- list()

filenames<-list.files(path = "/Users/jianitian/Desktop/Project4/data",pattern = ".h5",all.files = TRUE,
           full.names = TRUE,recursive = TRUE, include.dirs = TRUE)

### firstly I choose 350 songs as test data, the rest 2000 songs as train data
set.seed(100)
test_files_names <- sample(filenames,350,replace = FALSE,prob = NULL)
test_files_names <- list.files(path = "/Users/jianitian/Desktop/Project4/TestSongFile100",pattern = ".h5",all.files = TRUE,
                               full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
train_filenames <- filenames[!filenames %in% test_files_names]

### get features 
for (i in 1:length(train_filenames)){
  feature.list [[i]]<- h5read(train_filenames[[i]],"/analysis")
  thisfile <- feature.list[[i]]
  
  ### The difference between each bar and beats are almost the same
  ### we can take average time between each bar and beats 
  ### as features to represent the rhythm of a song;
  ### additionally, the length of the bar and beats can represent the length of a song 
  ### that is also a feature of a song
  
  ### Also, if I want to put all the features together, I neet to give them a weight
  ### Let their related confidence average be their weight

  barstart_length[[i]] <- length(thisfile$bars_start)
  barstart_avgtime[[i]]<-mean(diff(thisfile$bars_start))
  barstart_sdtime[[i]] <- sd(diff(thisfile$bars_start))
  barstart_time[[i]] <- thisfile$bars_start[1]
  bar_confidence_mean[[i]]<-mean(thisfile$bars_confidence)
  
  beatstart_length[[i]] <- length(thisfile$beats_start)
  beatstart_avgtime[[i]] <- mean(diff(thisfile$beats_start))
  beatstart_sdtime[[i]] <- sd(diff(thisfile$beats_start))
  beatstart_time[[i]] <- thisfile$beats_start[1]
  beat_confidence_mean[[i]]<-mean(thisfile$beats_confidence)
  
  ### After analyzing the section part, I decided to divided each songs into three part using kmeans method
  ### To use the cluster means and clustering vector as the features for section part
  section_avglength <- diff(thisfile$sections_start)
  if(length(section_avglength)>3){
  k<-kmeans(section_avglength,3)
  sectioncenter[[i]] <- k$centers
  #section_cluster[[i]] <- k$cluster
  }
  else
  {sectioncenter[[i]] <- section_avglength}
  section_length[[i]] <- length(section_avglength)
  sectionmean[[i]] <- mean(section_avglength)
  sectionsd[[i]]<-sd(section_avglength)
  section_confidence_mean[[i]]<-mean(thisfile$sections_confidence)
  
  segment_confidence_mean[[i]] <- mean(thisfile$segments_confidence)
  segment_length[[i]] <- length(thisfile$segments_start)
  segment_avgleng[[i]] <- mean(diff(thisfile$segments_start))
  
  loudness_quantile[[i]] <- quantile(thisfile$segments_loudness_max)
  loudness_range[[i]] <- max(thisfile$segments_loudness_max)-min(thisfile$segments_loudness_max)
  loudness_mean[[i]] <- mean(thisfile$segments_loudness_max)
  loudness_sd[[i]] <- sd(thisfile$segments_loudness_max)
  
  index_time <-as.integer(seq(1,length(thisfile$segments_loudness_max), length.out = 20))
  loudness_trend[[i]] <-thisfile$segments_loudness_max[index_time]
  loudness_loud_timeline[[i]] <- thisfile$segments_loudness_start[index_time]
  loudness_timeline[[i]] <- thisfile$segments_start[index_time]
  
  pitches[[i]] <- thisfile$segments_pitches[,index_time]
  pitch_mean[[i]] <- rowMeans(thisfile$segments_pitches)
  pitch_quantile[[i]] <- apply(thisfile$segments_pitches, 1, quantile, probs = c(0.05, 0.9),  na.rm = TRUE)
  
  timber[[i]] <- thisfile$segments_timbre[,index_time]
  timbre_mean[[i]] <- rowMeans(thisfile$segments_timbre)
  timbre_quantile[[i]] <- apply(thisfile$segments_timbre, 1, quantile, probs = c(0.05, 0.9),  na.rm = TRUE)
  
  tatum_confidence_mean[[i]] <- mean(thisfile$tatums_confidence)
  tatum_avgtime[[i]] <- mean(diff(thisfile$tatums_start))
  tatum_sdtime[[i]] <- sd(diff(thisfile$tatums_start))
}

index_train <- match(train_filenames,filenames)
file.names<-list.files(path = "/Users/jianitian/Desktop/Project4/data",pattern = ".h5",all.files = TRUE,
                      full.names = FALSE,recursive = TRUE, include.dirs = FALSE)
file.names.train <- file.names[index_train]
file.names.train.s <- gsub(".*/", "", file.names.train)
file.names.train.short <- strsplit(file.names.train.s,".",fixed = TRUE)[[1]][1]

file.names.train.s <-gsub(".*", "", file.names.train.s,fixed = TRUE)
file.names.test <- file.names[-index_train]


sec_center_mat <- matrix(nrow = 2000,ncol = 3)
for(i in 1:2000){
    for (j in 1:3) {
      sec_center_mat[i,j] <- sectioncenter[[i]][j]
    }
}
sec_center_mat[is.na(sec_center_mat)] <- 0

quantilemat_loudness<-matrix(nrow = 2000,ncol = 5)
for (i in 1:2000) {
  for (j in 1:5) {
    quantilemat_loudness[i,j] <- loudness_quantile[[i]][j]
  }
}

featuremat_loudness_trend_mat <- matrix(nrow = 2000,ncol = 20)
featuremat_loudness_timeline <- matrix(nrow = 2000,ncol = 20)
featuremat_timeline <- matrix(nrow = 2000,ncol = 20)
for (i in 1:2000) {
  for (j in 1:20) {
    featuremat_loudness_trend_mat[i,j]<-loudness_trend[[i]][j]
    featuremat_loudness_timeline[i,j] <- loudness_loud_timeline[[i]][j]
    featuremat_timeline[i,j] <- loudness_timeline[[i]][j]
  }
}

pitches_mat <- matrix(nrow = 2000,ncol = 240)
for (i in 1:2000) {
  mat <- pitches[[i]]
  thismat <- matrix(t(mat),nrow = 1,byrow = TRUE)
  for (j in 1:240) {
    pitches_mat[i,j] <- as.numeric(thismat[j])
  }
}

quantilemat_pitch<-matrix(nrow = 2000,ncol = 24)
for (i in 1:2000) {
  thismat<-pitch_quantile[[i]]
  matr <- matrix(t(thismat),nrow = 1,byrow = TRUE)
  for (j in 1:24) {
    quantilemat_pitch[i,j] <- matr[j]
  }
}

pitch_mean_mat <- matrix(nrow = 2000,ncol = 12)
for (i in 1:2000) {
  for (j in 1:12) {
    pitch_mean_mat[i,j] <- pitch_mean[[i]][j]
  }
}


timbre_mat <- matrix(nrow = 2000,ncol = 240)
for (i in 1:2000) {
  mat <- timber[[i]]
  thismat <- matrix(t(mat),nrow = 1,byrow = TRUE)
  for (j in 1:240) {
    timbre_mat[i,j] <- as.numeric(thismat[j])
  }
}

timbre_mean_mat <- matrix(nrow = 2000,ncol = 12)
for (i in 1:2000) {
  for (j in 1:12) {
    timbre_mean_mat[i,j] <- timbre_mean[[i]][j]
  }
}

quantilemat_timbre<-matrix(nrow = 2000,ncol = 24)
for (i in 1:2000) {
  thismat<-timbre_quantile[[i]]
  matr <- matrix(t(thismat),nrow = 1,byrow = TRUE)
  for (j in 1:24) {
    quantilemat_timbre[i,j] <- matr[j]
  }
}

### The features are as follows
featuremat_bar<-cbind(as.numeric(barstart_length),as.numeric(barstart_avgtime),as.numeric(barstart_sdtime),as.numeric(barstart_time))
featuremat_beat <-cbind(as.numeric(beatstart_length),as.numeric(beatstart_avgtime),as.numeric(beatstart_sdtime),as.numeric(beatstart_time))
featuremat_section <- cbind(sec_center_mat,as.numeric(sectionmean),as.numeric(sectionsd),as.numeric(section_length))
featuremat_segment <- cbind(as.numeric(segment_length),as.numeric(segment_avgleng))
featuremat_loudness_statis <- cbind(quantilemat_loudness,as.numeric(loudness_range),as.numeric(loudness_mean),as.numeric(loudness_sd))
feature_timbre_statistics <-cbind(timbre_mean_mat,quantilemat_timbre)
feature_tatum <-cbind(as.numeric(tatum_avgtime),as.numeric(tatum_sdtime))
pitch_feature_statitcs <- cbind(quantilemat_pitch,pitch_mean_mat)

###And another five matrix
# The dimention of featuremat_loudness_trend_mat is 2000*20
# The dimention of featuremat_loudness_timeline is 2000*20
# The dimention of featuremat_timeline is 2000*20
# The dimention of pitches_mat is 2000*240
# The dimention of timbre_mat is 2000*240

### Using whole features and do the pca

### I use their mean confidence as their relative weight

feature.total <- cbind(featuremat_bar,featuremat_beat,featuremat_section,featuremat_segment,featuremat_loudness_statis,
                       feature_timbre_statistics,pitch_feature_statitcs,feature_tatum)

for (i in 1:dim(feature.total)[2]) {
  index<-which(is.na(feature.total[,i]))
  feature.total[index,i]<-mean(na.omit(feature.total)[,i])
}

featuredf<-as.data.frame(feature.total)
featuredf$songnames <- file.names.train


### The dim of featuredf is 2000*99

confidence <- cbind(unlist(bar_confidence_mean),unlist(beat_confidence_mean),unlist(section_confidence_mean),
                    unlist(segment_confidence_mean),unlist(tatum_confidence_mean))
for (i in 1:5) {
  confidence[,i][which(is.na(confidence[,i]))] <- means[i]
}

new_matrix <-featuredf
for(i in 1:4){
  new_matrix[,i] <- featuredf[,i]*confidence[,1]
}
for (i in 5:8) {
  new_matrix[,i] <- featuredf[,i]*confidence[,2]
}
for (i in 9:14) {
  new_matrix[,i] <- featuredf[,i]*confidence[,3]
}
for (i in 15:96) {
  new_matrix[,i] <- featuredf[,i]*confidence[,4]
}
for (i in 97:98) {
  new_matrix[,i] <- featuredf[,i]*confidence[,5]
}

#new_matrix <- new_matrix[,-41]

### get statistics from features
mea <- function(x){mean(x,na.rm=TRUE)}
med <- function(x){median(x,na.rm = TRUE)}
mads <- apply(new_matrix[,1:98],2,mea)
medians <- apply(new_matrix[,1:98],2,med)

### replace NA with their means if numeric

scaled_featuremat <- scale(new_matrix[,1:98],center=medians,scale=mads)
pca1 <- prcomp(scaled_featuremat)

### 12 vecotors can explain 80.1% cumulative proportion of information of statistic features
newfeature_mat <- as.matrix(new_matrix[,1:98]) %*% pca1$rotation[,1:12]

### time series features
### delete number 21 and 41 column as their related variance are 0
time_series_feature <- cbind(featuremat_loudness_trend_mat,featuremat_loudness_timeline,featuremat_timeline,pitches_mat,timbre_mat)
which(is.na(time_series_feature))
time_series_feature <- time_series_feature[,-c(21,41)]
medians1 <- apply(time_series_feature,2,med)
mads1 <- apply(time_series_feature,2,mad)

scaled_time_feature <- scale(time_series_feature,center = medians1, scale=mads1)
pca2 <- prcomp(scaled_time_feature)
summary(pca2)
### nine variables can explain 99.34% information
plot(pca2)
newfeature_time_mat <- time_series_feature %*% pca2$rotation[,1:2]
newfeature.total <- cbind(newfeature_mat,newfeature_time_mat)


