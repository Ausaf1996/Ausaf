install.packages('recommenderlab')
library('recommenderlab')
data('MovieLense')
movie=MovieLense
View(movie)
head(as(movie[1,],'list')[[1]])
class(movie)
#
rowCounts(movie)#for each row there are number of non missing values
hist(rowCounts(movie))

#
colCounts(movie)
length(colCounts(movie))  
hist(colCounts(movie))

#getting users with 100 ratings
movie_100=movie[rowCounts(movie)>100]

#converting to matrix
movie_m=as(movie,'matrix')
View(movie_m)

#training
train=movie_100[1:50]#50 users
rec=Recommender(train,method='UBCF')#user based collaborative filtering
rec

#predicitng
pred=predict(rec,movie_100[101:102],n=10)#top 10 ratings for user 101,102, n=10
pred

as(pred,'list')
