#Downloading packages
if (!require("ggplot2")) {
  install.packages("ggplot2", repos="http://cran.rstudio.com/") 
  library("ggplot2")
}
if (!require("dplyr")) {
  install.packages("dplyr", repos="http://cran.rstudio.com/") 
  library("dplyr")
}
if (!require("gridExtra")) {
  install.packages("gridExtra", repos="http://cran.rstudio.com/") 
  library("gridExtra")
}
if (!require("ggthemes")) {
  install.packages("ggthemes", repos="http://cran.rstudio.com/") 
  library("ggthemes")
}
if (!require("fmsb")) {
  install.packages("fmsb", repos="http://cran.rstudio.com/") 
  library("fmsb")
}
if (!require("caret")) {
  install.packages("caret", repos="http://cran.rstudio.com/") 
  library("caret")
}
if (!require("ggdendro")) {
  install.packages("ggdendro", repos="http://cran.rstudio.com/") 
  library("ggdendro")
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", repos="http://cran.rstudio.com/") 
  library("RColorBrewer")
}

#Loading dependencies
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(fmsb)
library(caret)
library(ggdendro)
library(RColorBrewer)
#Loading The data
d<-read.csv("/Users/grv/Desktop/Spring 18/513/Project/PUBG-Game-statistical-analysis/PUBG_Player_Statistics.csv",sep=',',stringsAsFactors=F)

#Removing NA fields
d <- na.omit(d)

#Normalizing the data
d <- scale(d, center = TRUE, scale = TRUE)

#Defining the features in group
performances_features<-c('KillDeathRatio','WinRatio','TimeSurvived','RoundsPlayed','Wins','WinTop10Ratio','Top10s','Top10Ratio','Losses','WinPoints')
ratings_features<-c('Rating','BestRating')
support_features<-c('Heals','Revives','Boosts','DamageDealt','DBNOs')
survival_features<-c('Days','LongestTimeSurvived','MostSurvivalTime','AvgSurvivalTime')
distance_features<-c('WalkDistance','RideDistance','MoveDistance','AvgWalkDistance','AvgRideDistance','LongestKill')
combat_features<-c('Kills','Assists','Suicides','TeamKills','HeadshotKills','HeadshotKillRatio','VehicleDestroys','RoadKills','DailyKills','WeeklyKills','RoundMostKills','MaxKillStreaks','WeaponAcquired')

#Define base features name
#Append the group category to this vector
#Loop over these features and save the histograms(currentGroup, currentSection) into a list ; having an array of feature names makes the histogramming code better

makeSummary<-function(myfeatures){
  features_solo<-as.vector(sapply(myfeatures, function(x) paste0('solo_',x)))
  features_duo<-as.vector(sapply(myfeatures, function(x) paste0('duo_',x)))
  features_squad<-as.vector(sapply(myfeatures, function(x) paste0('squad_',x)))
  
  #subset data 'd' by groups
  d_solo<-d %>% select(.dots = features_solo)
  colnames(d_solo)<-myfeatures
  d_solo$cat<-'solo'
  
  d_duo<-d %>% select(.dots = features_duo)
  colnames(d_duo)<-myfeatures
  d_duo$cat<-'duo'
  
  d_squad<-d %>% select(.dots = features_squad)
  colnames(d_squad)<-myfeatures
  d_squad$cat<-'squad'
  
  features_d<-rbind(d_solo,d_duo,d_squad)
  features_d$cat<-as.factor(features_d$cat)
  return(features_d)
}

#Performances Summary
perf_d<-makeSummary(performances_features)
#time in minutes
perf_d$TimeSurvived<-perf_d$TimeSurvived/3600
perfPlots<-list()

#Histograms
for(i in 1:length(performances_features)){
  x = performances_features[i]
  
  perfPlots[[i]]<- perf_d %>% dplyr::select(.dots=x,cat) %>%
    ggplot(aes(x=.dots,fill=cat)) + geom_histogram(bins=100,alpha=.75) +
    theme_fivethirtyeight() + ggtitle(x) +
    theme(legend.position='none',plot.title = element_text(size =12)) + scale_y_log10() +
    scale_fill_manual(name="",values=c("Blue","Red","Yellow")) + facet_grid(~cat)
}

do.call(grid.arrange, c(perfPlots, ncol=2))

#Grouping of Performance features
length_features<-length(support_features)
res <- perf_d %>% group_by(cat) %>% select_if(is.numeric) %>% dplyr::summarise_all(funs(mean))
maxVal<- ceiling(apply(res[,2:(length_features+1)], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,length_features)
res$color<-c("#46ACC8","#42c600","#ff7b00")

par(mfrow=c(2,2),bg="#F0F0F0",mar=c(1,1,1,1))
for(i in 1:nrow(res)){
  curCol<-(col2rgb(as.character(res$color[i]))%>% as.integer())/255
  radarchart(rbind(maxVal,min,res[i,2:(length_features+1)]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", 
             caxislabels=seq(0,2000,5), 
             cglwd=0.8, vlcex=.8, palcex=1.2,
             title=as.character(res$cat[i]))
}

#Skills/Ratings
ratings_d<-makeSummary(ratings_features)

#Plotting the Skill features radar chart
ggplot(data=ratings_d,aes(x=Rating)) +
  geom_histogram(aes(fill=cat),position="dodge",alpha=.75) +
  theme_fivethirtyeight() +
  theme(legend.position='none',plot.title = element_text(size =12)) + 
  scale_fill_manual(name="",values=c("#46ACC8","#42c600","#ff7b00")) + facet_wrap(~cat) +
  ggtitle('Ratings per category')

#Support/heals
support_d<-makeSummary(support_features)
support_d$cat<-as.factor(support_d$cat)

#Grouping Support Features
length_features<-length(support_features)
res <- support_d %>% group_by(cat) %>% select_if(is.numeric) %>% dplyr::summarise_all(funs(mean))
maxVal<- ceiling(apply(res[,2:(length_features+1)], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,length_features)
res$color<-c("#46ACC8","#42c600","#ff7b00")

#Plotting the support features radar chart
par(mfrow=c(2,2),bg="#F0F0F0",mar=c(1,1,1,1))
for(i in 1:nrow(res)){
  curCol<-(col2rgb(as.character(res$color[i]))%>% as.integer())/255
  radarchart(rbind(maxVal,min,res[i,2:(length_features+1)]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", 
             caxislabels=seq(0,2000,5), 
             cglwd=0.8, vlcex=.8, palcex=1.2,
             title=as.character(res$cat[i]))
}

#Distance
distance_d<-makeSummary(distance_features)
distance_d$cat<-as.factor(distance_d$cat)

#Grouping distance features
length_features<-length(distance_features)
res <- distance_d %>% group_by(cat) %>% select_if(is.numeric) %>% dplyr::summarise_all(funs(mean))
maxVal<- ceiling(apply(res[,2:(length_features+1)], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,length_features)
res$color<-c("#46ACC8","#42c600","#ff7b00")

#Plotting the Distance features radar chart
par(mfrow=c(2,2),bg="#F0F0F0",mar=c(1,1,1,1))
for(i in 1:nrow(res)){
  curCol<-(col2rgb(as.character(res$color[i]))%>% as.integer())/255
  radarchart(rbind(maxVal,min,res[i,2:(length_features+1)]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", 
             caxislabels=seq(0,2000,5), 
             cglwd=0.8, vlcex=.8, palcex=1.2,
             title=as.character(res$cat[i]))
}

#Survival
survival_d<-makeSummary(survival_features)
survival_d$cat<-as.factor(survival_d$cat)

#Grouping Survival features
length_features<-length(survival_features)
res <- survival_d %>% group_by(cat) %>% select_if(is.numeric) %>% dplyr::summarise_all(funs(mean))
maxVal<- ceiling(apply(res[,2:(length_features+1)], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,length_features)
res$color<-c("#46ACC8","#42c600","#ff7b00")

#Plotting Survival Features Radar Chart
par(mfrow=c(2,2),bg="#F0F0F0",mar=c(1,1,1,1))
for(i in 1:nrow(res)){
  curCol<-(col2rgb(as.character(res$color[i]))%>% as.integer())/255
  radarchart(rbind(maxVal,min,res[i,2:(length_features+1)]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", 
             caxislabels=seq(0,2000,5), 
             cglwd=0.8, vlcex=.8, palcex=1.2,
             title=as.character(res$cat[i]))
}

#Combat
combat_d<-makeSummary(combat_features)
combat_d$cat<-as.factor(combat_d$cat)

#Grouping combat features
length_features<-length(combat_features)
res <- combat_d %>% group_by(cat) %>% select_if(is.numeric) %>% dplyr::summarise_all(funs(mean))
maxVal<- ceiling(apply(res[,2:(length_features+1)], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,length_features)
res$color<-c("#46ACC8","#42c600","#ff7b00")

#Plotting combat features radar chart
par(mfrow=c(2,2),bg="#F0F0F0",mar=c(1,1,1,1))
for(i in 1:nrow(res)){
  curCol<-(col2rgb(as.character(res$color[i]))%>% as.integer())/255
  radarchart(rbind(maxVal,min,res[i,2:(length_features+1)]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", 
             caxislabels=seq(0,2000,5), 
             cglwd=0.8, vlcex=.8, palcex=1.2,
             title=as.character(res$cat[i]))
}

#Data Preparation for Classification
all_features<-c(performances_features,ratings_features,support_features,survival_features,distance_features,combat_features)
length(all_features)
#skip WeaponAcquired feature
all_features<-all_features[-40]
all_d<-makeSummary(all_features)
str(all_d)

#Train/Test for solo players
solo_d<-all_d %>% filter(cat=='solo')
solo_d$cat<-factor(solo_d$cat, levels = c('solo'))
set.seed(619)
split <- createDataPartition(y=solo_d$cat, p = 0.75, list = FALSE)
train_solo <- solo_d[split,]
test_solo <- solo_d[-split,]

#Train/Test for duo players
duo_d<-all_d %>% filter(cat=='duo')
duo_d$cat<-factor(duo_d$cat, levels = c('duo'))
set.seed(619)
split <- createDataPartition(y=duo_d$cat, p = 0.75, list = FALSE)
train_duo <- duo_d[split,]
test_duo<- duo_d[-split,]

#Train/Test for squad players
squad_d<-all_d %>% filter(cat=='squad')
squad_d$cat<-factor(squad_d$cat, levels = c('squad'))
set.seed(619)
split <- createDataPartition(y=squad_d$cat, p = 0.75, list = FALSE)
train_squad <- squad_d[split,]
test_squad<- squad_d[-split,]

#Creating samples with 2000 entries to reduce computing overhead.
train_solo_sample<-sample_n(train_solo, 2e3)
train_duo_sample<-sample_n(train_duo, 2e3)
train_squad_sample<-sample_n(train_squad, 2e3)
all_train<-rbind(train_solo_sample,train_duo_sample,train_squad_sample)

#Creating samples with 500 entries to reduce computing overhead.
train_solo_sample1<-sample_n(train_solo, 500)
train_duo_sample1<-sample_n(train_duo, 500)
train_squad_sample1<-sample_n(train_squad, 500)
all_train1<-rbind(train_solo_sample1,train_duo_sample1,train_squad_sample1)

#Classifiers Training
classifiers<-c('knn','nb','rf','svmLinear','nnet')
trControl <- trainControl(method = "cv",number = 5)
timing<-c()
res<-list()
cnt<-0
for(r in classifiers){
  cnt<-cnt+1
  start.time <- Sys.time()
  res[[cnt]]<-train(cat ~ ., data = all_train,method=r,trControl = trControl,preProcess=c('center', 'scale'),metric='Accuracy')
  end.time<-Sys.time()
  timing<-c(timing,as.numeric(difftime(end.time,start.time,units="sec")))
}

#Results of the classifiers
results<-resamples(list('KNN'=res[[1]],"Naiveb"=res[[2]],"Randomf"=res[[3]],"SVM"=res[[4]],"NeuralN"=res[[5]]))
bwplot(results,scales = list(relation = "free"),xlim = list(c(0.5,1), c(0.5,1)))

#Time spent by Classifiers 
names<-c()
for(i in 1:length(res)){
  names[i]<-res[[i]]$method
}
timingData<-data.frame('classifier'=names,'val' = timing)
ggplot(data=timingData,aes(x=reorder(classifier,val),y=val)) + 
  geom_bar(stat='identity') + theme_fivethirtyeight() +
  coord_flip() + 
  xlab('') + ylab('Time [sec]') + ggtitle('Time[sec] spent by classifier')

#confusion matrix of Neural Network
caret::confusionMatrix(res[[5]])

#Results for Neural Network classifier
test_solo_sample<-sample_n(test_solo, 2e3)
test_duo_sample<-sample_n(test_duo, 2e3)
test_squad_sample<-sample_n(test_squad, 2e3)
all_test<-rbind(test_solo_sample,test_duo_sample,test_squad_sample)
test_pred <- predict(res[[5]], newdata = all_test)
confusionMatrix(test_pred, all_test$cat)

#Plotting most important features
#plot(varImp(object = res[[5]]))

?theme_
#Heirarchial Cluster analysis
makeDendogram <- function(curDf, curCat){
  dd <- dist(scale(curDf %>% select_if(is.numeric)), method = "euclidean")
  dd.dendro <- as.dendrogram(hclust(dd, method = "ward.D2"))
  dendro_data <- dendro_data(dd.dendro)
  ggplot(segment(dendro_data)) + 
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + theme_wsj() + 
     ggtitle(paste0('Dendrogram of ',curCat,' stats'))
}

makeDendogram(train_solo_sample,'Solo')
makeDendogram(train_duo_sample,'Duo')
makeDendogram(train_squad_sample,'Squad')

#Hierarchical cluster for solo (Heatmap)
heatmap(as.matrix(train_solo_sample[,1:39]), col = brewer.pal(9,'Reds'),main = "Solo category")

#Hierarchical cluster for duo (Heatmap)
heatmap(as.matrix(train_duo_sample[,1:39]), col =brewer.pal(9,'Blues'),main = "Duo category")

#Hierarchical cluster for squad (Heatmap)
heatmap(as.matrix(train_squad_sample[,1:39]), col =brewer.pal(9,'Greens'),main = "Squad category")

#hclust
#train_solo_sample.d <-  dist(scale(train_solo_sample %>% select_if(is.numeric)), method = "euclidean")
#train_solo_sample.d.h <- hclust(train_solo_sample.d, method = "average")
#ddd <- as.dendrogram(train_solo_sample.d.h)
#plot(ddd, axes = FALSE)
#plot(train_solo_sample.d.h, main = "Hierarchical Clustering dendogram")
#rect.hclust(train_solo_sample.d.h, k=2, border = "red")

#K-Means Clustering
all_train[is.na(all_train)] <- 0
all_train_s <- scale(all_train[-40])

#All train kmeans cluster
km <- kmeans(all_train_s, 3)
km$cluster

#Choosing optimum number of clusters.
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(all_train_s, i)
}

bet_totss <- list()
for (i in 1:10) {
  bet_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

plot(1:10, bet_totss,type = "b", ylab = "Between SS/Total SS", xlab = "Cluster", col = "Blue", main = "Optimum number of Clusters")
?plot
palette()
km1 <- km$cluster
table(all_train$cat,km$cluster)

#plot of winratio 
plot(all_train$WinRatio, col = km$cluster, xlab = "Index", ylab = "Win Ratio", main = "Win Ratios of Solo, Duo and Squad")
legend("topright", legend = c("Solo", "Duo", "Squad"), pch = c(1,1,1) ,col = c("Black","red","green3"))

#plot of distance
plot(all_train$WalkDistance, col = km$cluster, xlab = "Index", ylab = "Walk Distance", main = "Walk Distance of Solo, Duo and Squad")
legend("topright", legend = c("Solo", "Duo", "Squad"), pch = c(1,1,1) ,col = c("Black","red","green3"))

#plot of time survived
plot(all_train$RideDistance, col = km$cluster, xlab = "Index", ylab = "Ride Distance", main = "Ride Distance of Solo, Duo and Squad")
legend("topright", legend = c("Solo", "Duo", "Squad"), pch = c(1,1,1) ,col = c("Black","red","green3"))

#plot of losses
plot(all_train$Losses, col = km$cluster, xlab = "Index", ylab = "Losses", main = "Losses of Solo, Duo and Squad")
legend("topright", legend = c("Solo", "Duo", "Squad"), pch = c(1,1,1) ,col = c("Black","red","green3"))

#clearly 3 is the optimum number of clusters we can make from the given observations.
