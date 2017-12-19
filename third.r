library(rmmseg4j)
library(Rwordseg)
install.packages("Rwordseg", repos = "http://R-Forge.R-project.org")
result = mmseg4j('李彦宏会对马云造成威胁吗')
summary(result)

#movie recommendation  system
movies = read.csv(file.choose(),stringsAsFactors = F)
ratings = read.csv(file.choose(),stringsAsFactors = F)
genres = as.data.frame(movies$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 = as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) = c(1:7)
View(genres2)
genre_list = c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
genreLength=length(genre_list)

genre_matrix = matrix(0,nrow(genres2)+1,genreLength) #empty matrix
genre_matrix[1,] = genre_list #set first row to genre list
colnames(genre_matrix) = genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] = 1
  }
}
View(genre_matrix)
#convert into dataframe
genre_matrix2 = as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] = as.integer(genre_matrix2[,c])
} #convert from characters to integers

binaryratings = ratings
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] = 1
  }
  else{
    binaryratings[i,3] = -1
  }
}

View(binaryratings2)
binaryratings2 = dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] = 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds
ncol(binaryratings2)
#Remove rows that are not rated from movies dataset
movieIds = length(unique(movies$movieId)) #9125
ratingmovieIds = length(unique(ratings$movieId)) #9066
 
movies2 = movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) = NULL
#Remove rows that are not rated from genre_matrix2
genre_matrix3 = genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) = NULL
#Calculate dot product for User Profiles
result = matrix(0,genreLength,ncol(binaryratings2))
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] = sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}

#Convert to Binary scale
for (i in 1:nrow(result)){
  if (result[i] < 0){
    result[i] = 0
  }
  else {
    result[i] = 1
  }
}
result2 = result[1,] #First user's profile
sim_mat = rbind.data.frame(result2, genre_matrix3)
sim_mat = data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer

#Calculate Jaccard distance between user profile and all movies
library(proxy)
sim_results = dist(sim_mat, method = "Jaccard")
sim_results = as.data.frame(as.matrix(sim_results[1:ratingmovieIds]))
rows = which(sim_results == min(sim_results))
#Recommended movies
movies[rows,2]
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat = dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat = as.matrix(ratingmat[,-1]) #remove userIds


library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat = as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm = normalize(ratingmat)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model = Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom = predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list = as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result = matrix(0,10)
 ### such a stupid error evey the dataset should set stringAsFactor otherwise it will be mircle.
for (i in c(1:10)){
  print(movies[as.integer(recom_list[[1]][i]),2])
  name[i]=  movies[as.integer(recom_list[[1]][i]),2]
}
name 
evaluation_scheme = evaluationScheme(ratingmat, method="cross-validation", k=5, given=3, goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results = evaluate(evaluation_scheme, method="UBCF", n=c(1,3,5,10,15,20))
eval_results = getConfusionMatrix(evaluation_results)[[1]]
 
