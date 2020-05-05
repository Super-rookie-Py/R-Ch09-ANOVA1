###  Ch09.분산분석1

###  2020/05/05 keonwoo park

## 04.반복측정 분산분석실습 ###################

# 호흡과 뇌파와의 관계를 연구한 자료이다.
# 호흡을 3번(흡호=1:1, 7:3, 3:7)측정하였다.
# 호흡에 따라 뇌파간 차이가 있는가?



# 01. 데이터 불러오기.
bre <-read.csv("Ch0904.호흡과 뇌파.csv",
               header = T,
               na.strings = '.')
bre$breath <- factor(bre$breath,
                   levels = c(1:3),
                   labels = c("1:1","7:3","3:7"))

str(bre)


## 02.기본 통계량 확인: describe(psych패키지 이용)
library(psych)
describeBy(bre$ch2al, bre$breath, mat=F) #두 그룹일때 보는거.


# 03.그래프그리기(박스그래프, 히스토그램)
boxplot(ch2al ~ breath,
        data=bre,
        ylab='ch2al',
        xlab='breath')

## 04.통계분석
# 다변량 분석을 위한 구형성(sphericity)검정 : Mauchly's test

library(car)
bre.matrix <- cbind(bre$ch2al[bre$breath=='1:1'],
                    bre$ch2al[bre$breath=='7:3'],
                    bre$ch2al[bre$breath=='3:7'])

head(bre.matrix)

bre.model.lm <- lm(bre.matrix ~ 1) # ~1 : 데이터를 하나로 묶기
time.f <- factor(c('1:1','7:3','3:7'))
options(contrasts = c('contr.sum','contr.poly'))
bre.result.mt <-Anova(bre.model.lm,
                      idata = data.frame(time.f),
                      idesign=~time.f,
                      type = "III")


summary(bre.result.mt, multivariate = F)
# 구형성 0.25729 -> 구형성 있다.



# 다중비교 (Multicamparison test) - t-value 포함
# install.packages('multcomp')

library(multcomp)
result.lm <- lm(ch2al ~ breath, data=bre)
tukey.result <- glht(result.lm, linfct=mcp(breath='Tukey'))
summary(tukey.result)
plot(tukey.result)


