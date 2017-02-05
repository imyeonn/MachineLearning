academy <- read.csv("academy.csv" , stringsAsFactors = F , header = T)
academy <- academy[-1]
dist_academy <- dist(academy , method = "euclidean")

# 계층적 군집기법
mat_academy <- as.matrix(dist_academy)
temp_mat <- mat_academy[1:5,1:5]
rownames(temp_mat) <- LETTERS[1:5]
colnames(temp_mat) <- LETTERS[1:5]
diag(temp_mat) <- rep(9999,5) 
temp_mat

# 최소값인 b-d 군집 만들고 각각 b, d 삭제
pos <- which(temp_mat == min(temp_mat))[1]
rown <- pos %/% 5 + 1
coln <- pos %% 5 ; if(coln==0) rown = rown - 1;coln = 5
temp_mat2 <- temp_mat[c(-2,-4) , c(-2,-4)]
temp_mat2 <- rbind(temp_mat2 , BD=rep(0,3))
temp_mat2 <- cbind(temp_mat2 , BD=rep(0,3))
temp_mat2

# bd, a, c, e와의 각각 거리 구하기. 최단연결법으로 거리값을 채움.
new_distance <- c()
for(i in setdiff(c(1:5) , c(2,4))) { #1,3,5
    new_distance <- c(new_distance , min(temp_mat[i,2] , temp_mat[i,4]))
}
new_distance <- c(new_distance , 9999) 
temp_mat2[dim(temp_mat2)[1],] <- new_distance
temp_mat2[,dim(temp_mat2)[1]] <- new_distance
temp_mat2 

# 처음 했던 일 반복(가장 가까운 항목 고르고 합치기, 삭제)
pos <- which(temp_mat2 == min(temp_mat2))[1]
rown <- pos %/% 4 + 1
coln <- pos %% 4 ; if(coln==0) rown = rown - 1;coln = 4
temp_mat3 <- temp_mat2[c(-3,-4) , c(-3,-4)]
temp_mat3 <- rbind(temp_mat3 , BDE=rep(0,3))
temp_mat3 <- cbind(temp_mat3 , BDE=rep(0,3))
temp_mat3

#bde, a, c와의 거리 구하기 
new_distance <- c()
for(i in setdiff(c(1:4) , c(3,4))) {
  new_distance <- c(new_distance , min(temp_mat2[i,3] , temp_mat2[i,4]))
}
new_distance <- c(new_distance , 9999) 
temp_mat3[dim(temp_mat3)[1],] <- new_distance
temp_mat3[,dim(temp_mat3)[1]] <- new_distance
temp_mat3 

# 반복
pos <- which(temp_mat3 == min(temp_mat3))[1]
rown <- pos %/% 3 + 1
coln <- pos %% 3 ; if(coln==0) rown = rown - 1;coln = 3
temp_mat4 <- temp_mat3[c(-1,-3) , c(-1,-3)];temp_mat4 <- as.matrix(temp_mat4)
rownames(temp_mat4) <- c("C");colnames(temp_mat4) <- c("C")
temp_mat4 <- rbind(temp_mat4 , BDEA=rep(0,2))
temp_mat4 <- cbind(temp_mat4 , BDEA=rep(0,2))
temp_mat4

# bdea와 c 최단연결법으로 거리 구하기
new_distance <- c()
for(i in setdiff(c(1:3) , c(1,3))) {
  new_distance <- c(new_distance , min(temp_mat2[i,1] , temp_mat2[i,3]))
}
new_distance <- c(new_distance , 9999) 
temp_mat4[dim(temp_mat4)[1],] <- new_distance
temp_mat4[,dim(temp_mat4)[1]] <- new_distance
temp_mat4 

# 이렇게 반복작업 안하고 hclust 함수로 편하게 이용 가능

# 거리함수 정의(유클리드 제곱거리)
dis <- function(x,y) {
  return ((x[1]-x[2])^2 + (y[1]-y[2])^2)
}

#2-means 시뮬레이션
x <- c(rnorm(20,3,1) , rnorm(20,7,1))
y <- c(rnorm(20,4,1) , rnorm(20,8,1))
plot(x , y , cex=.5 , xlim=c(0,10) , ylim=c(0,10)) 

c <- locator(2) ; print(c)
points(c$x , c$y , pch=20 , col="red")

# 첫번째 군집중심과 점들간의 거리
distance1 <- c()
for(i in 1:length(x)) {
  lines(c(x[i] , c$x[1]) , c(y[i], c$y[1]) , lty=2)
  # 거리계산 
  distance1 <- c(distance1 , dis(c(x[i] , c$x[1]) , c(y[i], c$y[1])))
}
distance1

# 두번째 군집중심과 점들간의 거리
plot(x , y , cex=.5 , xlim=c(0,10) , ylim=c(0,10)) 
points(c$x , c$y , pch=20 , col="red")
distance2 <- c()
for(i in 1:length(x)) {
  lines(c(x[i] , c$x[2]) , c(y[i], c$y[2]) , lty=2)
  # 거리계산
  distance2 <- c(distance2 , dis(c(x[i] , c$x[2]) , c(y[i], c$y[2])))
}
distance2

# 각각 점들마다 2개의 군집 중 가까운 군집을 해당군집으로 정한다.
clusters <- c()
f <- factor(distance1 > distance2)
levels(f) <- c("1" , "2")
f

# 새로운 군집이 정해졌으면 각각 군집마다 평균(중심점)을 구한다.
x1_var <- mean(x[f == "1"])
x2_var <- mean(x[f == "2"])
y1_var <- mean(y[f == "1"])
y2_var <- mean(y[f == "2"])
x1_var;x2_var;y1_var;y2_var

# 중심점의 변화
c$x - c(x1_var , x2_var) 
c$y - c(y1_var , y2_var) 

# 다시 그림
plot(x , y , cex=.5 , xlim=c(0,10) , ylim=c(0,10)) 
points(c(x1_var , x2_var) , c(y1_var , y2_var) , pch=20 , col="red")

# 다시 계산
# 첫번째 군집중심과 점들간의 거리
distance1 <- c()
for(i in 1:length(x)) {
  lines(c(x[i] , x1_var) , c(y[i], y1_var) , lty=2)
  # 거리계산
  distance1 <- c(distance1 , dis(c(x[i] , x1_var) , c(y[i], y1_var)))
}
distance1
  
# 두번째 군집중심과 점들간의 거리
plot(x , y , cex=.5 , xlim=c(0,10) , ylim=c(0,10)) 
points(c$x , c$y , pch=20 , col="red")
distance2 <- c()
for(i in 1:length(x)) {
  lines(c(x[i] , x2_var) , c(y[i], y2_var) , lty=2)
  # 거리계산
  distance2 <- c(distance2 , dis(c(x[i] , x2_var) , c(y[i], y2_var)))
}
distance2

# 반복(해당 소속군집 결정)
clusters <- c()
f <- factor(distance1 > distance2)
levels(f) <- c("1" , "2")
f

# 다시 평균점 구함
x1_var2 <- mean(x[f == "1"])
x2_var2 <- mean(x[f == "2"])
y1_var2 <- mean(y[f == "1"])
y2_var2 <- mean(y[f == "2"])
x1_var2;x2_var2;y1_var2;y2_var2

# 다시 중심점의 변화 구함. 변화 거의 없어서 이제 중단
c(x1_var2 , x2_var2) - c(x1_var , x2_var) 
c(y1_var2 , y2_var2) - c(y1_var , y2_var) 

# DBSCAN
academy 

#?????????? 
library(cluster)
hcl <- hclust(dist(academy)^2 , method = "single")
plot(hcl , hang = -1 , xlab = "strudent" , ylab = "distance")

#k-means???? 
library(graphics)
kms <- kmeans(academy , 5);kms
plot(academy , col = kms$cluster)

#Elbow point???ϱ? 
wss <- 0

#k?? 1~10???? ??ȭ??Ű?鼭 ?? withinss ??�� wss?? ?????Ѵ?.
for(i in 1:10) wss[i] <- sum(kmeans(academy,centers = i)$withinss)

#withinss??�� ?׷??��? ?׸???, ?????Ⱑ ?ް??ϰ? ?ϸ??????? ?κ??? ???? elbow point.
plot(1:10, wss, type="b",xlab = "Number of Clusters", ylab = "Within group sum of squares")


