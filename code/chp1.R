# 비슷한 학생끼리 묶는 예제
academy <- read.csv("academy.csv", stringsAsFactors = F, header = T)
academy <- academy[-1]
head(academy)

# dist 함수로 학생들 간 거리 구하기
dist_academy <- dist(academy, method = "euclidean")
dist_academy

# cmdscale 함수로 거리 시각화
two_coord <- cmdscale(dist_academy)
plot(two_coord, type = "n")
text(two_coord, as.character(1:52))

# 선호하는 음식들끼리 묶기
food <- read.csv("food.csv", stringsAsFactors = F, header = T)
food <- food[-1]
head(food)

# 상관관계 나타내기
food.mult <- t(as.matrix(food)) %*% as.matrix(food)
food.mult

# 거리 구하기
dist.food <- dist(food.mult)
dist.food

# 시각화
two_coord2 <- cmdscale(dist.food)
plot(two_coord2 , type = "n", family = "AppleGothic")
text(two_coord2 , rownames(food.mult))
