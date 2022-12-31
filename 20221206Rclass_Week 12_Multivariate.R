#2022.12.06
#Multivariate

#We don't see the one we don't share
#有時為了讓某群很像，造成其他其實也很像的因為沒有被比到像的特徵而被認為不像
#=> need some transform

#explanatory variables自變數，可控變量 eg.Latitude, Depth
#response variable反應變量=> 反應值 eg.Temperature, light, calcification rate


#Cluster analysis => usually use in microbiology

#先hypothsis -> 了解要做甚麼分析 -> 再做hypothesis testing

#通常資料格式:
#列名:time, site
#欄名:Species, temperature


#有些資料比對時，比到兩個都是0的資料，卻以為兩個很相似，相似在兩個都是0的地方=> No No
#=> remove 0值的影響
#=> need transform data
#***我們不比叫我們同時沒有的東西
#**But有些0值是有意義的

library(vegan)
data (varespec)
?decostand()
#要做log => for 那些分布差異很大的，為了讓他distribution集中的
decostand(varespec, 'pa') #有值呈現1，沒值(0)呈現0


#Bray-Curtis (D14)
#Double 0 will not be considered

# Bray-Curtis *dissimilarity matrix* on raw data
vegdist(varespec)
spe <- varespec
# Bray-Curtis dissimilarity matrix on log-transformed data
spe.dbln <- vegdist(log1p(spe)) # log(x+1)

# Chord distance matrix
spe.norm<-decostand(spe,'nor') #normalize
spe.dc <- vegdist(spe.norm)

# Hellinger distance matrix
spe.hel<-decostand(spe,'hel')
spe.dh <- vegdist(spe.hel)


#=> all is to transform data first and then make dissimilarity matrix


data(varechem)
#eg. Ca 的值超高，要讓每個值標準都一樣
#在這裡0 value is meaningful，因為是化學劑量，相較於species是0時就沒意義
env <- varechem
env.st<-decostand(env,'stan') # standardized [or scale(env)]
env.de<-vegdist(env.st,method='euc') # then compute D1


# binary data
# Jaccard dissimilarity matrix using vegdist()
spe.dj1 <- vegdist(spe,'jac',binary=T)# binary p/a 

# Jaccard dissimilarity matrix using dist()
spe.dj2 <- dist(spe,'binary') 

# Sorensen dissimilarity matrix using vegdist()
spe.ds<-vegdist(spe,binary=T)

# Ochiai dissimilarity matrix using dist.binary() (ade4)
spe.och<-dist.binary(spe, method=7)


##library(vegan)有很多方法

#Euclidean distance is the most common method

# binary data
# Jaccard dissimilarity matrix using vegdist()
spe.dj1 <- vegdist(spe,'jac',binary=T)# binary p/a 

# Jaccard dissimilarity matrix using dist()
spe.dj2 <- dist(spe,'binary') 

# Sorensen dissimilarity matrix using vegdist()
spe.ds<-vegdist(spe,binary=T)

# Ochiai dissimilarity matrix using dist.binary() (ade4)
spe.och<-dist.binary(spe, method=7)


#There are many way to visualize the result


qgraph(1-spe.db, layout='spring', vsize=4)


#Clustering----
#to make a tree
#it is depend on the dissimilarity maxtrix***

#Hierarchical Clustering=> good for gradients
#This is the ‘closest’ friend’ procedure.
#先抓一個人，再根據dissimilarity matrix去看誰跟他最近就綁在一起
#partitions difficult to interpret, but gradients quite clear

#Complete linkage agglomerative clustering (furthest neighbor sorting)
#not common in ecology

#Average agglomerative clustering
#it is the method the most common in ecology (species data)
#UPGMA method is most common in ecology for partitions


#Ward’s Minimum Variance clustering
#base on linear model with sum of square


#Clustering quality----
#cophenetic correlation => higher value mean better cluster

#shepard-like diagram: compare cluster correlation and 原始資料的distance 
#higher correlation => better 

















