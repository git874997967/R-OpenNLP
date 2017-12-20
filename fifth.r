# 协同过滤推荐系统的R实现 collarborative filtering recommendation
# # 场景  图书管借阅喜欢的书籍
#  1 在熟悉的栏目 类似书籍附近寻找
#  2 通过熟人推荐
#  3 找到图书馆目录从不同目录中寻找
#  4 从搜索引擎输入相关信息寻找 解决信息过载的方法
# 如何评价推荐系统
#  离线实验
# 用户调查
# 在线实验
# top N推荐和评分推荐
library("recommenderlab")
set.seed(1234)
#generate the matrix the prob 6 the same 66.7 with one 60
#next time do not save abbr in complexed method
m = matrix(
  sample(
    x = c(as.numeric(0:5), NA),
    size = 50,
    replace = T,
    prob = c(rep(.4 / 6, 6), .6)
  ),
  ncol = 10,
  dimnames = list(
    user = paste("u", 1:5, seq = ''),
    item = paste("i", 1:10, seq = '')
  )
)
# 5 x 10 rating matrix of class 'realRatingMatrix' with 19 ratings.
# which will filter the NA automaticly
r = as(m, 'realRatingMatrix')
as(r, 'data.frame')
image(r, main = "main means the title")
image(normalize(r), main = 'this is the normalized ')
# 二元分类转换
r_b = binarize(r, minRating = 4)
b = as(r_b, "matrix")
b
#replace(b,FALSE,'0')
b1 = b
for (j in 1:nrow(b)) {
  for (i in 1:ncol(b)) {
    if (b[j, i] == FALSE) {
      b1[j, i] = 0
    }
  }
}
# visualize of the data
data(MovieLense)
movieData = sample(x = MovieLense, size = 943, replace = T)
image(movieData)
# break means the break between two cells
hist(x = getRatings(normalize(MovieLense)), breaks = 100)
hist(x = rowCounts(movieData), breaks = 100)
# diff algos realized by recommondLab  fifteen in total
recommenderRegistry$get_entry_names()
r_recom = Recommender(movieData, method = "IBCF")
r_popul = Recommender(movieData, method = "POPULAR")
names(getModel(r_recom))
# top N predict
movieData

pred = predict(r_popul, movieData[940:943], n = 5)
predTop3 = bestN(pred, n = 3)
# for four diff users recommond 3 movies for each  12 in total
as(predTop3, 'list')
# eval the predict model
rate = predict(r_popul, movieData[940:943], type = 'ratings')
as(rate, 'matrix')[, 1:5]
# method   load the data chose the correct method to build model and make a predcition
# 建立推荐系统模型后，非常关心的是对预测模型的评价。
# 可通过evaluationScheme()将数据按一定规则分为训练集和测试集
# (参数method = "split",)，或进行k-fold交叉验证(如method = "cross",k=4)
# ，given参数表示用来进行模型评价的items的数量。分别运用UBCF及IBCF算法，
# 进行预测评价。


# 模型评价部分没有使用其他参数  rmse rse 去评估  而是使用自带函数
a = as(movieData, 'matrix')
View(a)
e =
  evaluationScheme(
    movieData[1:800],
    method = "split",
    train = 0.9,
    given = 15,
    goodRating = 5
  )
# # getData(e,"train")表示获取训练集数据，predict(r1,getData(e,"known"),
# type = "ratings")表示对“已知”训练集数据进行预测。
# 计算预测模型的准确度可通过calcPredictionAccuracy()函数实现，
# 参数“unknown”表示对“未知”test集进行比较。
# 结果发现两种方法的均方根误差(RMSE)基本一致。

#p1 p2 对已知训练集数据进行预测
# r1 r2  获取训练数据
r1 = Recommender(getData(e, 'train'), 'UBCF')
p1 = predict(r1, getData(e, 'known'), type = 'ratings')
r2 = Recommender(getData(e, 'train'), 'IBCF')
p2 = predict(r2, getData(e, 'known'), type = 'ratings')
##计算预测模型的准确度
c1 = calcPredictionAccuracy(p1, getData(e, "unknown"))
c2 = calcPredictionAccuracy(p2, getData(e, "unknown"))
error = rbind(c1, c2)
rownames(error) = c("UBCF", "IBCF")
error
# all evaluate should keep as small as possible


# evaluate top N method
# 让我们来评价TOP-1,TOP-3,TOP-5,TOP-10推荐准确性。
# 通过4-fold交叉验证方法分割数据集，运用evaluate()进行TOP-N预测模型评价。
# 评价结果可通过ROC曲线及准确率-召回率曲线展示。
#
tops = evaluationScheme(
  movieData[1:800],
  method = 'cross',
  k = 4,
  given = 3,
  goodRating = 5
)
tops
results = evaluate(tops,
                   method = 'POPULAR',
                   type = 'topNList',
                   n = c(1, 3, 5, 10))
#获得混淆矩阵
# what is the meanning of the confusionMatrix
getConfusionMatrix(results)[[1]]
# how to understand the line
#ROC曲线
plot(results, annotate = T)
#准确率-召回率曲线
plot(results, "prec/rec", annotate = TRUE)

## compare diff algos
# 除了对预测模型进行评价，还可以对不同推荐算法进行比较。
# 可首先构建一个推荐算法列表，通过ROC曲线、
# 准确率-召回率曲线或RMSE直方图进行比较。

#compare the diff algos instead of the models.

set.seed(2016)
scheme =
  evaluationScheme(
    movieData,
    method = "split",
    train = 0.9,
    k = 1,
    given = 10,
    goodRating = 5
  )
#构建推荐算法列表
algorithms = list(
  "random items" = list(name = "RANDOM", param = NULL),
  "popular items" = list(name = "POPULAR", param = list(normalize = "Z-score")),
  "user-based CF" = list(
    name = "UBCF",
    param = list(
      normalize = "Z-score",
      method = "Cosine",
      nn = 25,
      goodRating = 5
    )
  ),
  "item-based CF" = list(name = "IBCF", param = list(k = 50)),
  "SVD approximation" = list(name = "SVD", param = list(approxRank = 50))
)
#构建不同算法模型
results = evaluate(scheme, algorithms, n = c(1, 3, 5, 10, 15, 20))
#模型比较#ROC曲线
plot(results, annotate = c(1, 3), legend = "bottomright")


plot(results,
     'prec/rec',
     annotate = c(2, 3, 4),
     legend = 'topleft')
#compare diff rating algo

results2 = evaluate(scheme, algorithms, type = 'ratings')
plot(results2, ylim = c(0, 20))
# 当然在recommenderlab包说明文档中也有自定义新的推荐算法的举例，
# 通过编辑推荐算法的函数实现符合实际的推荐算法。
# 但要说明的是运用R进行协同过滤推荐并不是推荐系统应用的主流，
# 主要原因在于算法调整不灵活，并且R主要依靠内存的单线程计算，
# 并不太适用于过大规模的推荐应用。
# 但是有了parel包  应该能够得到改善
# 
# http://blog.jobbole.com/86959/