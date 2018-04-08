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

#Summary of Performance features
length_features<-length(support_features)
res <- perf_d %>% group_by(cat) %>% select_if(is.numeric) %>% dplyr::summarise_all(funs(mean))
maxVal<- ceiling(apply(res[,2:(length_features+1)], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,length_features)
res$color<-c("#46ACC8","#F21A00","#EBCC2A")

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

#Data Preparation
all_features<-c(performances_features,ratings_features,support_features,survival_features,distance_features,combat_features)
#skip WeaponAcquired feature
all_features<-all_features[-length(all_features)]
all_d<-makeSummary(all_features)

solo_d<-all_d %>% filter(cat=='solo')
solo_d$cat<-factor(solo_d$cat, levels = c('solo'))
set.seed(619)
split <- createDataPartition(y=solo_d$cat, p = 0.75, list = FALSE)
train_solo <- solo_d[split,]
test_solo <- solo_d[-split,]

duo_d<-all_d %>% filter(cat=='duo')
duo_d$cat<-factor(duo_d$cat, levels = c('duo'))
set.seed(2345)
split <- createDataPartition(y=duo_d$cat, p = 0.75, list = FALSE)
train_duo <- duo_d[split,]
test_duo<- duo_d[-split,]

squad_d<-all_d %>% filter(cat=='squad')
squad_d$cat<-factor(squad_d$cat, levels = c('squad'))
set.seed(3456)
split <- createDataPartition(y=squad_d$cat, p = 0.75, list = FALSE)
train_squad <- squad_d[split,]
test_squad<- squad_d[-split,]

train_solo_sample<-sample_n(train_solo, 2e3)
train_duo_sample<-sample_n(train_duo, 2e3)
train_squad_sample<-sample_n(train_squad, 2e3)
all_train<-rbind(train_solo_sample,train_duo_sample,train_squad_sample)

#Classifiers Training

regressors<-c('knn','svmLinear','rf','nb','gbm','lda')
trControl <- trainControl(method = "cv",number = 5, repeats=3)
timing<-c()
res<-list()
cnt<-0
for(r in regressors){
  cnt<-cnt+1
  start.time <- Sys.time()
  res[[cnt]]<-train(cat ~ ., data = all_train,method=r,trControl = trControl,preProcess=c('center', 'scale'),metric='Accuracy')
  end.time<-Sys.time()
  timing<-c(timing,as.numeric(difftime(end.time,start.time,units="sec")))
}

#Results

results<-resamples(list('knn'=res[[1]],"svmLinear"=res[[2]],"rf"=res[[3]],"nb"=res[[4]],"gbm"=res[[5]],"LDA"=res[[6]]))
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

#confusion matrix
caret::confusionMatrix(res[[2]])

#Results for svm classifier
test_solo_sample<-sample_n(test_solo, 2e3)
test_duo_sample<-sample_n(test_duo, 2e3)
test_squad_sample<-sample_n(test_squad, 2e3)
all_test<-rbind(test_solo_sample,test_duo_sample,test_squad_sample)
test_pred <- predict(res[[2]], newdata = all_test)
confusionMatrix(test_pred, all_test$cat)

#Heirarchial Cluster analysis
makeDendogram <- function(curDf, curCat){
  dd <- dist(scale(curDf %>% select_if(is.numeric)), method = "euclidean")
  dd.dendro <- as.dendrogram(hclust(dd, method = "ward.D2"))
  dendro_data <- dendro_data(dd.dendro)
  ggplot(segment(dendro_data)) + 
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
    coord_flip() + theme_fivethirtyeight() + 
    theme(axis.text=element_blank()) + ggtitle(paste0('Dendrogram of ',curCat,' stats'))
}

#For solo
h <- as.numeric(as.matrix(train_solo_sample[,1:39]))
heatmap(h, col = brewer.pal(9,'Reds'),main = "Solo category")
