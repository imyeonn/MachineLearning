# 의사결정나무
skin <- read.csv("skin.csv", header = T)
skin <- skin[-1]
head(skin)
# skin에 대한 정보 표시
str(skin)

# 정보분류: 엔트로피, 정보이득
x <- c("red", "blue", "blue", "red", "red")
# 정보 엔트로피를 구하는 함수
info_entropy <- function(x) {
  factor_x <- factor(x)
  entropy <- 0
  for(str in levels(factor_x)) {
    pro <- sum(x ==str) / length(x)
    entropy <- entropy - pro * log2(pro)
  }
  return (entropy)
}
info_entropy(x)

# 엔트로피 함수 데이터셋에 적용
# 맨 처음의 쿠폰반응여부에 대한 엔프로피
first_entropy <- info_entropy(skin[,"쿠폰반응여부"])

for (str in colnames(skin)[1:5]) {
  # str: 조건변수, factors: 조건값집합
  factors <- levels(skin[[str]])
  # 조건변수를 각각의 가능한 속성값으로 분류했을 때 쿠폰반응여부에 대한 엔트로피의 합계
  sum_entropy <- 0
  for (str2 in factors) {
    test_x <- skin[skin[[str]] == str2,][6]
    sum_entropy <- sum_entropy + info_entropy(test_x[,1])
  }
  cat(str , '--->' , sum_entropy, '\n')
}

# 트리 만들기
# minsplit: 최소가지치기 데이터 갯수. 크게 할수록 트리 간단해짐.
# compress, uniform: 세로폭, 가로폭 좁히기
library(rpart)
tree1 <- rpart(쿠폰반응여부 ~ ., data = skin , control=rpart.control(minsplit = 2)) 
plot(tree1 , compress = T , uniform = T , margin=0.1) 
text(tree1 , use.n = T , col = "blue")

# 트리 속성별 갯수와 비율 알고 싶을 때
tree1

# prunning(가지치기): 의미 없는 분할 스탑

# 정보분류: 카이제곱
xtabs(~ 결혼여부 + 쿠폰반응여부 , data = skin)
#카이제곱 독립성검정
chisq.test(xtabs(~ 결혼여부 + 쿠폰반응여부 , data = skin))

# 정보분류: 지니계수
# 지니계수 구하는 함수
info_gini <- function(x) {
  factor_x <- factor(x)
  gini_sum <- 0
  for (str in levels(factor_x)) {
    pro <- sum(x == str) / length(x)
    gini_sum <- gini_sum + pro^2
  }
  return (1 - gini_sum)
}

# 지니계수 데이터셋에 적용
first_gini <- info_gini(skin[,"쿠폰반응여부"])

for(str in colnames(skin)[1:5]) { 
  
  # str: 조건변수, factors: 조건값집합
  factors <- levels(skin[[str]])
  
  # 조건변수를 각각 가능한 속성값으로 분류했을 때 쿠폰반응여부에 대한 엔트로피 합계
  sum_gini <- 0 
  for(str2 in factors) { 
    test_x <- skin[skin[[str]] == str2,][6]
    sum_gini <- sum_gini + info_gini(test_x[,1])
  }
  cat(str , '--->' , sum_gini,'\n')
  
}
