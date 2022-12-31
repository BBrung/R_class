#2022.12.13
#Multivarite_continued2


#Families
#Hard and Soft (may overlap) -> only have one choice

#cluster 之後希望cor() value is close to 1

#Fusion Level Values: How many groups we have

#silhouette(): get how amny groups and correlation

#At some point the marginal gain will drop, giving an angle in the graph. 
#The number of clusters is chosen at this point, hence the “elbow criterion” (wss) .
#elbow: 在做出圖的斜率變平處可能就是cluster的極限分群數

#Mantel test
#Mantel-optimal number of clusters - UPGMA當中最高點就是最高分群數


## How to visualize the cluster
#avoid one sigle individual to be a group


#Non-Hierarchical Clustering----
#they reflect well what I really see

#最常用的是k mean
#先決定要分幾群(可以靠前面檢定決定，像Mantel test，但分出的結果不滿意，
#可能一群裡面數量太少，就縮減分群數量，其他方法找最佳分群數:elbow, silhouette)
#cascadeKM可以直接看幾個群最好

#K mean 重要三步驟
#initialization 
#Assignment step
#Update step 
#定初始值
#分類
#確定平均值，再次分類
#=>最後分完平均值有變，就再次定新的平均值，接著再分一次，直到平均值沒變
#=>最後分完平均值沒變，就代表stable，就確定是這個group

#得到的結果沒有tree，就是知道分到哪群
#用ordination去呈現，like PCA
#fviz_cluster

#soft:幾%在哪 and hard: 不能切幾%
#Fuzzy clustering: soft
#很好應用在gradient數據

#Probabilistic 
#Decision trees
#find the smallest value 
#不能做linear model的
#R function: tree
#e.g., tree1<-tree(Species~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
#summary(tree1 )
#can also use: tree(Species~., data = iris)

#fancier option to draw decision plot with the package rpart


#random forest: in begining, less tree cause the huge error

#partialPlot(iris.rf,iris,Petal.Width,"setosa")
#意思是越接近小數字的petal.with越像setosa(probability越高)

#Multivariate regression trees 
#如:結合生物跟環境資料，畫decision tree



































