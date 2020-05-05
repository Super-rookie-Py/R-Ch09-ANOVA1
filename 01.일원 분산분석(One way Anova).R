###  Ch09.분산분석1

###  2020/05/05 keonwoo park

## 01.일원 분산분석 ###################




# 01. 데이터 불러오기.
owa<- read.csv("Ch0901.OWA.csv",
               header = T,
               na.strings = '.')
owa$group <- factor(owa$group,
                      levels = c(1:4),
                      labels = c("강남","강서","강동","강북"))

str(owa)


## 02.기본 통계량 확인: describe(psych패키지 이용)
library(psych)
describeBy(owa$score, owa$group, mat=F) #두 그룹일때 보는거.


# 03.그래프그리기(박스그래프, 히스토그램)
#install.packages('ggplot2')
library(ggplot2)
ggplot(owa,
       aes( x= group, y= score)) +
  geom_boxplot(outlier.colour = 'red') +
  ggtitle("매장별 만족도") +
  theme_classic() + # ggplot2 테마
  theme(title = element_text(color='darkblue', size= 20))

# facet_grid(): 그룹으로 구분~ 범주형 변수
# facet_grid(.~): 수직
# facet_grid(~.): 가로

ggplot(owa, aes(x=score)) +
  geom_histogram(binwidth = 10) +
  facet_grid(. ~ group) +
  ggtitle('매장별 만족도')+
  theme_classic()


## 04.통계분석
# 등분산 검정(같은 분산을 따를 때)
# 이분산일때는 하단의 부록 참조
bartlett.test(score ~ group,
              data=owa)
# bartlett.test는 정규분포에 민감하기 때문에 leveneTest많이사용
# install.packages('car')
library(car)
leveneTest(score ~ group,
           data= owa)

# p-value > 0.05 이므로 귀무가설 참.(등분산이다.)
# 등분산 일때
owa.result <- aov(score ~ group,
                  data= owa)
summary(owa.result)

# 부록: 이분산일때 welch's ANOVA test
oneway.test(owa$score ~ owa$group,
            data= owa,
            var.equal = FALSE)


# 사후검정###########
# Fisher LSD  (잘 안쓰임)
pairwise.t.test(owa$score,
                owa$group,
                data= owa,
                p.adj='non')

# Bonferroni, Tukey HSD, Duncan LSR
pairwise.t.test(owa$score,
                owa$group,
                data= owa,
                p.adj='bonf')

# Tukey HSD, Duncan LSR
TukeyHSD(owa.result)

# group으로 표현
#install.packages('agricolae')
library(agricolae)


# console= TRUE: 결과를 화면에 표시
# group= TRUE: 그룹으로 묶어서 표시, FALSE: 1:1 비교

LSD.test(owa.result,
         'group',
         console=T,
         p.adj = 'bonf')

duncan.test(owa.result,
            'group',
            group=T,
            console = T)
scheffe.test(owa.result,
             'group',
             group=F,
             console = T)

# 05.통계결과 그래프
tukeyPlot <- TukeyHSD(owa.result)
plot(tukeyPlot)


# 정규분포로 표시(강남)

x = 88.87
se=1.34 # 표본이므로 표준편차sd 대신 표준오차 se사용
data <- rnorm(1000, x, se)
data <- sort(data) 
plot(data,
     dnorm(data, x, se),
     col = 'blue',
     type = 'l',
     main = '매장별 고객만족도',
     xlim=c(75,95),
     ylim=c(0,0.3))
abline(v=x, col='blue', lty=3) #수직라인 초록색평균

# 그래프를 곂쳐서 표현하기
par(new=T) #x값, y값 경계를 같게 지정해야한다. xlim, ylim 두그래프 같게




# 정규분포로 표시(강서)

x = 88.19
se=1.33 # 표본이므로 표준편차sd 대신 표준오차 se사용
data <- rnorm(1000, x, se)
data <- sort(data) 
plot(data,
     dnorm(data, x, se),
     col = 'red',
     type = 'l',
     main = '매장별 고객만족도',
     xlim=c(75,95),
     ylim=c(0,0.3))
abline(v=x, col='red', lty=3) #수직라인 초록색평균


# 정규분포로 표시(강북)
par(new=T)
x = 86.05
se=1.58 # 표본이므로 표준편차sd 대신 표준오차 se사용
data <- rnorm(1000, x, se)
data <- sort(data) 
plot(data,
     dnorm(data, x, se),
     col = 'black',
     type = 'l',
     main = '매장별 고객만족도',
     xlim=c(75,95),
     ylim=c(0,0.3))
abline(v=x, col='black', lty=3) #수직라인 초록색평균


par(new=T)
# 정규분포로 표시(강동)

x = 82
se=2.05 # 표본이므로 표준편차sd 대신 표준오차 se사용
data <- rnorm(1000, x, se)
data <- sort(data) 
plot(data,
     dnorm(data, x, se),
     col = 'green',
     type = 'l',
     main = '매장별 고객만족도',
     xlim=c(75,95),
     ylim=c(0,0.3))
abline(v=x, col='green', lty=3) #수직라인 초록색평균
