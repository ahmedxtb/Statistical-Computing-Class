

set.seed(95616)

digit = read.csv("digitsTrain.csv", header = TRUE)
ranRow = sample(1:5000, 5000, replace = FALSE) #randomize the order of rows
digit = digit[ranRow,]  #we can get the random full dataset now. 
rownames(digit) = 1:5000 #rerowname the new randomized dataset
#use Euclidean method to get the distance
distance = as.matrix(dist(digit[,-1], method = "euclidean"))  

#Get random 5 blocks of the ranRow.
group = cut(1:5000, breaks = 5, labels = c("g1", "g2", "g3",  "g4", "g5"))
b1 = digit[which(group == "g1"),]  
b2 = digit[which(group == "g2"),]  
b3 = digit[which(group == "g3"),]  
b4 = digit[which(group == "g4"),]  
b5 = digit[which(group == "g5"),]  

knn = function(test, k){
  test = as.numeric(rownames(test))
  #Order the distance matrix
  od = apply(distance[test,-test], 1, order)
  #get the matrix which contain all rownames of the original dataset
  rowK = matrix(as.numeric(colnames(distance[test,-test])[od[1:k,]]), nrow = k)
  #get predicted matrix containging k labels for each data from test fold
  pred = apply(rowK, 2 , function(x) { digit$label[x]}) 
  predta = apply(pred, 2, table) #get the predicted label table of each element  
  #use this function to get the maximum showing times of the label. 
  #If the showing times are the same, the closest one will be chosen 
  chose = lapply(predta, which.max) 
  #get the labels we chose
   sum(as.numeric(unlist(lapply(chose, names))) != digit$label[test])/1000  # misclassification rate 
  }



#I searched in google and as a rule of thumb, setting K to the square root of the number of training patterns/samples can lead to better results. Then I chose k from 3 to 63. (since sqrt(4000) equals 63.24555)
knnE = sapply(3:63, function(k) {
  mean(c(knn(b1, k), knn(b2, k), knn(b3, k), knn(b4, k), knn(b5, k))) })
knnE

plot(3:63, knnE , xlab = "k value", ylab = "average of  misclassification rate", main = "Euclidean plot")

#The output contains all the average misclassification rate value of five test datasets with k from 3 to 63. And we can see the value is increasing with k increases. 


#Use Manhattan method 
distance1 = as.matrix(dist(digit[,-1], method = "manhattan"))  
knn1 = function(test, k){
  test = as.numeric(rownames(test))
  #Order the distance matrix
  od = apply(distance1[test,-test], 1, order)
  #get the matrix which contain all rownames of the original dataset
  rowK = matrix(as.numeric(colnames(distance1[test,-test])[od[1:k,]]), nrow = k)
  #get predicted matrix containging k labels for each data from test fold
  pred = apply(rowK, 2 , function(x) { digit$label[x]}) 
  predta = apply(pred, 2, table) #get the predicted label table of each element  
  #use this function to get the maximum showing times of the label. 
  #If the showing times are the same, the closest one will be chosen 
  chose = lapply(predta, which.max) 
  #get the labels we chose
  sum(as.numeric(unlist(lapply(chose, names))) != digit$label[test])/1000  # misclassification rate 
}

knnM = sapply(3:63, function(k) {
  mean(c(knn1(b1, k), knn1(b2, k), knn1(b3, k), knn1(b4, k), knn1(b5, k))) })
knnM

plot(3:63, knnE, col = "black", xlab = "k value", ylab = "average of  misclassification rate", main = "comparing plot")
points(3:63, knnM , col = "grey")

#confusion matrix:
 
predict = function(test, k){
  test = as.numeric(rownames(test))
  #Order the distance matrix
  od = apply(distance[test,-test], 1, order)
  #get the matrix which contain all rownames of the original dataset
  rowK = matrix(as.numeric(colnames(distance[test,-test])[od[1:k,]]), nrow = k)
  #get predicted matrix containging k labels for each data from test fold
  pred = apply(rowK, 2 , function(x) { digit$label[x]}) 
  predta = apply(pred, 2, table) #get the predicted label table of each element  
  #use this function to get the maximum showing times of the label. 
  #If the showing times are the same, the closest one will be chosen 
  as.numeric(names(sapply(predta, which.max)))
  #get the numeric labels we chose 
}

#Get 5 groups (full dataset) prediction value together
allPredict = c(predict(b1, 3), predict(b2,3),predict(b3,3), predict(b4,3), predict(b5,3))
#Get the confusion matrix
table(allPredict,digit$label) 

conf_matrix = table(allPredict,digit$label) 
#get the truely predicted rate of every label
sapply(1:10, function(i) conf_matrix[,i][i]/sum(conf_matrix[,i]))

#6.
getImage =
  function(vals)
  {
    matrix(as.integer(vals), 28, 28, byrow = TRUE)
  }

draw = function(vals, colors = rgb((255:0)/255, (255:0)/255, (255:0)/255), ...)
{
  if(!is.matrix(vals))
    vals = getImage(vals)
  
  m = t(vals)  # transpose the image
  m = m[,nrow(m):1]  # turn up-side-down
  # rotate
  image(m, col = colors, ..., xaxt = "n", yaxt = "n")
}
digits = digit #prevent from changing the original dataset
#add a new column which are predicted values of the whole dataset
digits$predict = allPredict
#extract the "predict" and "label" columns from the whole data
compare = as.data.frame(cbind(digits$label,digits$predict))
names(compare) = c("label", "predict") #name the columns
head(compare)
#get the misclassified labels
misclass = compare[compare$label != compare$predict,]
head(misclass)
#get misclassified data when label = 8
row8 = rownames(misclass[misclass$label == 8, ])

par(mfrow = c(5, 5), mar = rep(0, 4))
invisible(sapply(1:25, function(i) draw(digit[row8,][i, -1])))

head(misclass[misclass$label == 8, ],25)$predict

#get misclassified data when label = 2
row2 = rownames(misclass[misclass$label == 2, ])

par(mfrow = c(5, 5), mar = rep(0, 4))
invisible(sapply(1:25, function(i) draw(digit[row2,][i, -1])))

head(misclass[misclass$label == 2, ],25)$predict

#get misclassified data when label = 5
row5 = rownames(misclass[misclass$label == 5, ])

par(mfrow = c(5, 5), mar = rep(0, 4))
invisible(sapply(1:25, function(i) draw(digit[row5,][i, -1])))

head(misclass[misclass$label == 5, ],25)$predict

#get some plots of both misclassified labels and their 3 neighbors (k = 3) 
plotMis= function(data, k){
  test = as.numeric(rownames(data))
  #Order the distance matrix
  od = apply(distance[test,-test], 1, order)
  #get the matrix which contain all rownames of the original dataset
  rowK = matrix(as.numeric(colnames(distance[test,-test])[od[1:k,]]), nrow = k)
  #get predicted matrix containging k labels for each data from test fold
  pred = apply(rowK, 2 , function(x) { digit$label[x]}) 
  predta = apply(pred, 2, table) #get the predicted label table of each element  
  #use this function to get the maximum showing times of the label. 
  #If the showing times are the same, the closest one will be chosen 
  chose = lapply(predta, which.max) 
  #get the labels we chose
  #get misclassificated elements
  c = which(as.numeric(unlist(lapply(chose, names))) != digit$label[test])
  rowM = rowK[,c] #get the neighbors of misclassificated elements
  #get the original element rownames
  original = which(as.data.frame(rowK) %in% as.data.frame(rowM))
 #then we can get the original element rowname followed by three neighbors rowname
  sapply(1:10, function(i) c(original[i],rowM[,i]))
}

#use the first dataset's misclassified value to draw
par(mfrow = c(7, 7), mar = rep(0, 4))
invisible(sapply(1:40, function(i) draw(digit[as.vector(plotMis(b1,3)),][i, -1])))





#Distance to average and CV:
dta = function(test, metric){
  testN = as.numeric(rownames(test))
  train = digit[-testN,]  #get train dataset
  #get the average value of each column (each pixel) for each label(0-9) in train dataset
  seperate = split(train, train$label) #split as label first
  avg = sapply(2:785,function(j) lapply(1:10, function (i) mean(seperate[[i]][,j])))
  avg = as.data.frame(avg)
  #add label column into average data frame
  avg$label = 0:9
  avg = avg[,c(785, 1:784)]  #move label column to the first column
  #make rownames of avg consistent with the test data row names
  rownames(avg) = (testN[1000] + 1 ) : (testN[1000] + 10) 
  #make average data frame column names equal the test dataset column names
  names(avg) = names(test)  
  together = rbind(test, avg) #gather test dataset and average data. 
  #get the distance between average and the test dataset
  dista = as.matrix(dist(together[,-1], method = metric))  
  #Order the distance matrix
  od = apply(dista[1:1000,-(1:1000)], 1, order)
  #get the matrix which contain all rownames from the together(rbind average and test data) dataset
  rowK = matrix(as.numeric(colnames(dista[1:1000,-(1:1000)])[od[1,]]), nrow = 1)
  rowK = rowK - (testN[1]-1)  #to be consistent with the row number of the "together" data frame
  #get predicted label for each data from test fold
  pred = apply(rowK, 2 , function(x) { together$label[x]}) 
  #get the misclassification rate 
  sum(pred != together$label[1:1000])/1000 
}

#Get the mean value of using Euclidean with looping over all test datasets
meanE = sapply("euclidean", function(m) {
  mean(c(dta(b1, m), dta(b2, m), dta(b3, m), dta(b4, m), dta(b5, m))) })
meanE


#Get the mean value of using Manhattan with looping over all test datasets
meanM = sapply("manhattan", function(m) {
  mean(c(dta(b1, m), dta(b2, m), dta(b3, m), dta(b4, m), dta(b5, m))) })
meanM




