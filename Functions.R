# This directory has to be the one where you have strore our folder
setwd("C:/Users/ander/Desktop/UNIVERSIDAD/2 AÑO/1 CUATRIMESTRE/Statistical Learning/ASSIGNAMENT 1/Assignament 1")

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

# A FUNCION ---------------

partA = function(data){
  
  if (ncol(data)>1000){ #Some people have noted that the code for Kendall's tau is slow for very large datasets (many more than 1000 cases). 
    
    Mean = apply(data,2,mean)
    
    data = scale(data, center = T, scale = F)
    
    G = data
    
    Sigma = G%*%t(G)/(nrow(data)-1)
    Eigen = eigen(Sigma)
    
    D = Eigen$values
    
    P = t(G)%*%(Eigen$vector) 
    
    save(Mean,P,D,data,labels, file = 'data.RData')
    
    list=list(Mean,D,P)
    
    return(list)
    
  } else {
    
    data.scaled = scale(data, center = T, scale = F)
    
    Mean = apply(data,2,mean)
    
    Sigma = cov(data.scaled)
    round(Sigma,2)
    
    Eigen = eigen(Sigma)
    
    D = Eigen$values
    
    P = Eigen$vectors
    
    list=list(Mean,D,P)
    
    save(Mean,P,D,data,labels, file = 'data.RData')
    return(list)
  }
}
X = partA(data)


# B FUNCION -----------
load('paramsB.RData')
objectB = c(K,dist,var_PC)

partB = function(imagen,objectB){
  
  load('data.RData')

  Im = readImage(imagen)
  red=Im[,,1]
  green=Im[,,2]
  blue=Im[,,3]
  
  im = c(red,green,blue)
  
  image = im - Mean
  
  cum.var = cumsum(D) / sum(D)
  var_per = min(which(cum.var > objectB[3]))
  
  dataB = data%*%P[,1:var_per]
  
  image = image%*%P[,1:var_per]
  
  dmatrix=dist(rbind(image,dataB), method = objectB[2], diag = TRUE, upper = TRUE)
  dmatrix=as.matrix(dmatrix)
  
  dmatrix=dmatrix[1,2:(nrow(dataB)+1)]
  ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE)
  
  if (ordenados$x[1] < 5000){
    
    labels_sel=labels[ordenados$ix[1:objectB[1]]] 
    uniqv <- unique(labels_sel)
    label_img=uniqv[which.max(tabulate(match(labels_sel, uniqv)))]
    
  } else{
    
    label_img=0
    
  }
  return(label_img)
  
}

# D FUNCION -----------
load('paramsD.RData')
objectD = c(K,dist)

partD = function(imagen,objectD){
  
  load('data.RData')

  Im = readImage(imagen)
  red=Im[,,1]
  green=Im[,,2]
  blue=Im[,,3]
  
  im = c(red,green,blue)
  
  image = im - Mean
  
  dmatrix=dist(rbind(image,data), method = objectD[2], diag = TRUE, upper = TRUE)
  dmatrix=as.matrix(dmatrix)
  
  dmatrix=dmatrix[1,2:(nrow(data)+1)]
  ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE)
  
  if (ordenados$x[1] < 5001){
    
    labels_sel=labels[ordenados$ix[1:objectD[1]]] 
    uniqv <- unique(labels_sel)
    label_img=uniqv[which.max(tabulate(match(labels_sel, uniqv)))]
    
  } else{
    
    label_img=0
    
  }
  return(label_img)
  
}

