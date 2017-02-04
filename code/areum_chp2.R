# 의사결정나무
skin <- read.csv("skin.csv", header = T)
skin <- skin[-1]
head(skin)
# skin에 대한 정보 표시
str(skin)

# 엔트로피, 정보이득
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