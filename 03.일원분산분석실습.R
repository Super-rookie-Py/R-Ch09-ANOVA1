###  Ch09.분산분석1

###  2020/05/05 keonwoo park

## 03.일원분산분석실습 ###################

# K대학에서는 재학생을 대상으로 교육과정에 대한 현황조사를 실시함   
# 학부(1=신학, 2=사회복지, 3=아동상담, 4=경영)과
# 학년(1,2,3,4학년)에 따라
# 교양강의운영, 전공강의운영, 비교과운영, 종합점수가 차이가 있는가?
# 학부와 종합점수 비교



# 01. 데이터 불러오기.
edu <- read.csv("Ch0903.교육역량분석.csv",
               header = T,
               na.strings = '.')
edu$학부 <- factor(edu$학부,
                    levels = c(1:4),
                    labels = c("신학","사회복지","아동상담","경영"))
edu$학년 <- factor(edu$학년,
                 levels = c(1:4),
                 labels = c('1학년','2학년','3학년','4학년'))
head(edu)


## 02.기본 통계량 확인: describe(psych패키지 이용)
library(psych)
describeBy(edu$종합점수, edu$학부, mat=F) #두 그룹일때 보는거.


# 03.그래프그리기(박스그래프, 히스토그램)
#install.packages('ggplot2')
library(ggplot2)
ggplot(edu,
       aes( x= 학부, y= 종합점수)) +
  geom_boxplot(outlier.colour = 'red') +
  ggtitle("학부별 종합점수") +
  theme_classic() + # ggplot2 테마
  theme(title = element_text(color='darkblue', size= 20))

# facet_grid(): 그룹으로 구분~ 범주형 변수
# facet_grid(.~): 수직
# facet_grid(~.): 가로

ggplot(edu, aes(x=종합점수)) +
  geom_histogram(binwidth = 10) +
  facet_grid(. ~ 학부) +
  ggtitle('학부별 종합점수')+
  theme_classic()


## 04.통계분석
# 등분산 검정(같은 분산을 따를 때)
# 이분산일때는 하단의 부록 참조
bartlett.test(종합점수 ~ 학부,
              data=edu)
# bartlett.test는 정규분포에 민감하기 때문에 leveneTest많이사용
# install.packages('car')
library(car)
leveneTest(종합점수 ~ 학부,
           data= edu)

# p-value > 0.05 이므로 귀무가설 참.(등분산이다.)
# 등분산 일때
edu.result <- aov(종합점수 ~ 학부,
                  data= edu)
summary(edu.result)

############ 아노바 분석에서 0.855
# 학부간의 점수차이가 없다고 85프로나나옴
# 사후검정 할 필요가 없다.
