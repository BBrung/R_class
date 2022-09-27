#R class Week 3_dplyr & tidyr
#Date:2022-09-27

#dplyr -> from 'Tidyverse' is for data analysis----
library(dplyr)
?dplyr

#Mean.Width設定計算出的名字
summarised <- summarise(iris, Mean.Width = mean(Sepal.Width))

dim(iris)#可看iris的規格，150列，5行(變數)


#select data using the name of a column by select()
iris$Sepal.Length #the same with select() but only can choose one column
select(iris, Sepal.Length, Sepal.Width, Petal.Length) #function in dplyr

# ::means不一定要下載那個package就可以用那個套件裡的function
selected1 <- dplyr::select(iris, Sepal.Length, Sepal.Width, Petal.Length)
head(selected1) 

selected2 <- dplyr::select(iris, Sepal.Length:Petal.Length)
head(selected2, 1) #後面數字代表秀出前幾行

#取第幾行到第幾行in iris
selected3 <- dplyr::select(iris,c(2:5))
head(selected3)
#取第幾行
dplyr::select(iris,c(1,3,5))

# - 代表不要的
selected4 <- dplyr::select(iris, -Sepal.Length, -Sepal.Width)

# Select setosa species
filter(iris, Species == "setosa" ) #同於 iris[ iris$Species == 'setosa', ]
#iris$Species == 'setosa'只會邏輯判斷，要秀出資料要再篩選[]

filtered2 <- filter(iris, Species == "versicolor", Sepal.Width > 3) #可以同時篩選兩個以上條件
tail(filtered2) #看最後幾列

#以下公式也可以解上面
iris$yn <- ifelse(iris$Species == "versicolor" & iris$Sepal.Width > 3,'yes','no')
which(iris$yn == 'yes')


#create new columns (variables) while preserving existing columns 上週是用第42行的方法
#To create a column “Greater.Half” which stores a logical vector
mutated1 <- mutate(iris, Greater.Half = Sepal.Width > 0.5 * Sepal.Length)
tail(mutated1)
#check how many flowers fall in 上面的條件 -> 可以算那column裡各種分類有幾個
table(mutated1$Greater.Half)
table(iris$yn)

#used to sort rows by variables  arrange() 預設是小排到大
arrange(iris, Sepal.Width)
#以下是上次的做法
sort <- order(iris$Sepal.Width, decreasing = F)
iris[sort,]

arrange(iris, desc(Sepal.Width)) #大排到小的方法
#arrange跟order不同的地方在:arrange直接把列名重排，order會保持原本列名
arrange(iris, Sepal.Width, Sepal.Length) #也可以排兩個參數，寫在前面的先排


#groups observations within a data set by one or more variables.
gp <- group_by(iris, Species)
gp.mean <- summarise(gp,Mean.Sepal = mean(Sepal.Width)) #這樣可以根據group (species group)來個別平均
summarise(iris,Mean.Sepal = mean(Sepal.Width)) #這樣只會將所有的資料作平均
#ungroup removes grouping

#感覺很好用來做dataframe的工具，下次可試試
?tibble::tibble


#'>%>'Pipe operator use of the previous functions straightforward, allows to wrap multiple functions together
iris %>% group_by(Species) %>% summarise(Mean.Length = mean(Sepal.Length)) #把上面做的都合成一句




#tidyr -> can see tidy data 像蜂巢狀整理資料----
library(tidyr)

#Pivoting which converts between long and wide forms. use pivot_longer() and pivot_wider()
#Rectangling, which turns deeply nested lists (as from JSON [JavaScript Object Notation]) into tidy tibbles. use unnest_longer(), unnest_wider(), hoist()
#Nesting converts grouped data to a form where each group becomes a single row containing a nested data frame. use nest()
#Splitting and combining character columns. use separate() and extract()
#Make implicit missing values explicit with complete()


#Call the data file -> remember that the file should be in your working directory
TW_corals<-read.table('Data/tw_corals.txt', header=T, sep='\t', dec='.') #dec表示小數點分開方式
TW_corals

#pivot_longer把資料變長，pivot_longer(哪些資料, names_to = 那些資料想取的行名, values_to = 那些資料值的行名)
TW_corals_long <- TW_corals %>% pivot_longer(Southern_TW:Northern_Is, names_to = "Region", values_to = "Richness")

# 這句可以用出跟上面一樣的結果 TW_corals_long <-TW_corals %>% pivot_longer(cols = everything(), names_to = "Region", values_to = "Richness") 
TW_corals_long 
#之前的公式是用gather(
TW_corals %>% gather("Region", "Richness",Southern_TW:Northern_Is)


#TW_corals_wide <- pivot_wider(哪個資料來源檔案名, names_from = 欲取資料的名稱的行名, values_from = 欲取資料的數值那行行名) 
TW_corals_wide <- pivot_wider(TW_corals_long, names_from = Region, values_from = Richness) 
TW_corals_wide


income<-read.table('Data/metoo.txt',header=T, sep="\t", dec=".", na.strings = "n/a")
income

#State那行因為沒有數值所以不能放入不然無法排，pivot_longer會根據原本資料一行行去做
income_long <- income %>%  pivot_longer(cols = -state, 
                                        names_to = c("gender","work"), 
                                        names_sep = "_", 
                                        values_to = "income")
#因為在取的時候直接不用取State,就不用再減去state了
income_wide <- income_long %>% pivot_wider(names_from = c(gender, work), 
                                           values_from = income,
                                           names_sep = ".") #預設是用'_'分隔名字
#資料整理的時候名字最好有東西可以切割，而且盡量用_或.或#


#Splitting: offers other functions not directly tied to pivoting.分割東西separate()
income_long_var <- income %>%  pivot_longer(cols = -1, 
                                            names_to = "var1", 
                                            values_to = "income")
income_long_var

# Split var1 column into two new columns 切開的值會各自有一行
income_sep <- income_long_var %>%  separate(col = var1, 
                                            sep = "_", 
                                            into = c("gender", "work"))
income_sep

#切開的值會各自有一列
income_long_var %>% separate_rows(var1, sep = "_")

#Lauriane補充 用其他公式去做
df <- tibble(x = c('Dark', 'Brunette', 'Blond'), n = c(10, 3, 1))
uncount(df, n)
uncount(df, n, .id = 'id')


# Practice 2.2 ----
rairuoho <- read.table(url('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt'),
                       header = T)

#Replace nutrient with enriched in the data set.
rairuoho$treatment <- ifelse(rairuoho$treatment == 'nutrient', 'enriched', 'water')

#Reformat the table in order to have the day as a single variable (factor) 
#containing 6 levels (day3, day4, day5, day6, day7, day8).
library('dplyr')
library('tidyr')
rairuoho_day <- rairuoho %>% pivot_longer(cols = 1:6, 
                                          names_to = 'day', 
                                          values_to = 'length')



#Merge variables Spatial1 and Spatial2 
library('tidyverse')
rairuoho_spatial <- rairuoho_day %>%  
  mutate(spacial.coordinate = str_c(spatial1, spatial2, sep = "_")) #glue two characters together


#Remove variables row and column
rairuoho_goal <- rairuoho_spatial %>% select(-spatial1, -spatial2, -row, -column) 

