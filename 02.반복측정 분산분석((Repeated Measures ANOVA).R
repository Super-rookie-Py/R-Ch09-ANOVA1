###  Ch09.분산분석1

###  2020/05/05 keonwoo park

## 02.반복측정 분산분석 ###################

# k대학에서 운영하는 어학프로그램이있다.
# 참여전, 3개월후, 6개월 후에 영어 실력을 테스트하면
# 프로그램 효과가 있는가.
# 귀무가설  H0: 시점마다 차이가 없다.
# 변량분해  




# 01. 데이터 불러오기.
rma<- read.csv("Ch0902.RMA.csv",
               header = T,
               na.strings = '.')
rma$time <- factor(rma$time,
                    levels = c(1:3),
                    labels = c("사전","3개월","6개월"))

str(rma)


## 02.기본 통계량 확인: describe(psych패키지 이용)
library(psych)
describeBy(rma$score, rma$time, mat=F) #두 그룹일때 보는거.


# 03.그래프그리기(박스그래프, 히스토그램)
boxplot(score ~ time,
        data=rma,
        ylab='score',
        xlab='time')

## 04.통계분석
# 다변량 분석을 위한 구형성(sphericity)검정 : Mauchly's test

library(car)
rma.matrix <- cbind(rma$score[rma$time=='사전'],
                    rma$score[rma$time=='3개월'],
                    rma$score[rma$time=='6개월'])

head(rma.matrix)

rma.model.lm <- lm(rma.matrix ~ 1) # ~1 : 데이터를 하나로 묶기
time.f <- factor(c('사전','3개월','6개월'))
options(contrasts = c('contr.sum','contr.poly'))
rma.result.mt <-Anova(rma.model.lm,
                      idata = data.frame(time.f),
                      idesign=~time.f,
                      type = "III")


summary(rma.result.mt, multivariate = F)

# 일변량 ANOVA 검정
rma.result <- aov(score ~ time+Error(id/time),
                  data= rma)
summary(rma.result)

# 다중비교 (Multicamparison test) - t-value 포함
# install.packages('multcomp')

library(multcomp)
result.lm <- lm(score ~ time, data=rma)
tukey.result <- glht(result.lm, linfct=mcp(time='Tukey'))
summary(tukey.result)
plot(tukey.result)


# 부록: 사후검정(기본함수사용): Tukey HSD
tukey.result <- aov(rma$score ~ rma$time,
                    data= rma)
TukeyHSD(tukey.result)
plot(TukeyHSD(tukey.result))
