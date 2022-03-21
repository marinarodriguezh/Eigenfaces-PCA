# Package required ------------

if(!require(OpenImageR))
  install.packages('OpenImageR')
library(OpenImageR)

if(!require(class))
  install.packages('class')
library(class)

if(!require(tictoc))
  install.packages('tictoc')
library(tictoc)


# A Practise --------------------------

# This directory has to be the one where you have your photos for the data base

setwd("C:/Users/ander/Desktop/UNIVERSIDAD/2 AÑO/1 CUATRIMESTRE/Statistical Learning/ASSIGNAMENT 1/Training")
dire<-getwd()
setwd(dire)

list.files(path = dire,recursive = F)
b<-as.vector(list.files())

data=NULL
for (i in 1:length(b)){
  Im = readImage(b[i])
  red=Im[,,1]
  green=Im[,,2]
  blue=Im[,,3]
  
  concatenation = c(red,green,blue)
  
  data=cbind(data,as.vector(concatenation))

}

data = t(data)
labels = c(rep(10:19, each = 6), rep(1, each = 6), rep(20:25, each = 6), rep(2:9, each = 6))
# This directory is the one where you have stored our folder
setwd("C:/Users/ander/Desktop/UNIVERSIDAD/2 AÑO/1 CUATRIMESTRE/Statistical Learning/ASSIGNAMENT 1/Assignament 1")

# B Practise -----------------
load('data.RData')
set.seed(1)

nfolds = 6
folds <- cut(seq(1,nrow(data)),breaks=nfolds,labels=FALSE)
folds = sample(folds, length(folds), replace = FALSE)

k= c(2:7)
distances = c('euclidean','manhattan', 'maximum', 'canberra', 'minkowski')
var_PC = c(0.9, 0.95, 0.97)
params = expand.grid( k=k, dist = distances, var_PC = var_PC)
result = data.frame(params,acc=0)


dim(data)

tic() # it takes us 600 sec to calculate 

for(i in 1:nrow(result)){
  
  accuracy_list = c(rep(0,6))

  for (k in 1:nfolds){
    
    cum.var = cumsum(D) / sum(D)
    var_per = min(which(cum.var > result[i,3]))
    
    data.new = as.data.frame(data%*%(P[,1:var_per]))
    data.new = cbind(data.new,labels)
    
    datatst = data.new[which(folds == k),-ncol(data.new)] 
    datatr = data.new[which(folds != k), -ncol(data.new)]
    
    label_datatst = data.new[which(folds == k),ncol(data.new)]
    label_datatr = data.new[which(folds != k), ncol(data.new)]
    
    # training part:

    label_test = c(rep(0,nrow(datatst)))

    for (obs in 1:nrow(datatst)){
      
      observation = datatst[obs,]
      
      dmatrix=dist(rbind(observation,datatr), method = result[i,2], diag = TRUE, upper = TRUE)
      dmatrix=as.matrix(dmatrix)
      
      dmatrix=dmatrix[1,2:(nrow(datatr)+1)]
      ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE)
     
      knn=result[i,1]
      labels_sel=label_datatr[ordenados$ix[1:knn]]
      
      uniqv = unique(labels_sel)
      label_obs = uniqv[which.max(tabulate(match(labels_sel, uniqv)))]
      
      label_test[obs] = label_obs
   
    }
      
    accuracy_obs = sum(label_test == label_datatst)/nrow(datatst)
    
    accuracy_list[k] = accuracy_obs
    
  }
  
  final_accuracy = mean(accuracy_list)
  
  result$acc[i] = final_accuracy
}
toc()

best_accuracy=which.max(result$acc)
result[best_accuracy,]

K = result[best_accuracy,1]
dist = as.character(result[best_accuracy,2])
var_PC = result[best_accuracy,3]

save(K,dist,var_PC, file = 'paramsB.RData')


# D Practise -----------------
# Read data 

load('data.RData')

# Choose the possibles paramiters
set.seed(1)

nfolds = 6
folds <- cut(seq(1,nrow(data)),breaks=nfolds,labels=FALSE)
folds = sample(folds, length(folds), replace = FALSE)

k= c(2:7)
distances = c('euclidean','manhattan', 'maximum', 'canberra', 'minkowski')
params = expand.grid(k=k, dist = distances)
result = data.frame(params,acc=0)

labels = c(rep(10:19, each = 6), rep(1, each = 6), rep(20:25, each = 6), rep(2:9, each = 6))
data = cbind(data,labels)

dim(data)

tic()

for(i in 1:nrow(result)){
  
  accuracy_list = c(rep(0,6))
  
  for (k in 1:nfolds){
    
    datatst = data[which(folds == k),-ncol(data)] 
    datatr = data[which(folds != k), -ncol(data)]
    
    label_datatst = data[which(folds == k),ncol(data)]
    label_datatr = data[which(folds != k), ncol(data)]
    
    # training part:
    
    label_test = c(rep(0,nrow(datatst)))
    
    for (obs in 1:nrow(datatst)){
      
      observation = datatst[obs,]
      
      dmatrix=dist(rbind(observation,datatr), method = result[i,2], diag = TRUE, upper = TRUE)
      dmatrix=as.matrix(dmatrix)
      
      dmatrix=dmatrix[1,2:(nrow(datatr)+1)]
      ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE)
      
      knn=result[i,1]
      labels_sel=label_datatr[ordenados$ix[1:knn]]
      
      uniqv = unique(labels_sel)
      label_obs = uniqv[which.max(tabulate(match(labels_sel, uniqv)))]
      
      label_test[obs] = label_obs
      
    }
    
    accuracy_obs = sum(label_test == label_datatst)/nrow(datatst)
    
    accuracy_list[k] = accuracy_obs
    
  }
  
  final_accuracy = mean(accuracy_list)
  
  result$acc[i] = final_accuracy
}
toc()

best_accuracy = which(result$acc==1)
result[best_accuracy,]

K = final_params[,1]
dist = as.character(final_params[,2])
var_PC = final_params[,3]

save(K,dist,var_PC, file = 'paramsD.RData')


