# 생각해보니깐 lasso-> logistic regression(0,1), ppt에서도 생존분석과 엮어서 했음 
# 회귀와 가까우므로 stepwise 사용

df <- read.csv("C:/Users/kimch/Desktop/의생명통계/기말프로젝트/gee_glmer_df")

# <범주형 변수> -> 수치형 말고 yes/no 식으로 바꾸기
table(df$gender)
df[df$gender==1, "gender"] = "M"
df[df$gender==2, "gender"] = "F"
table(df$gender)

table(df$educ)
df[df$educ==4, "educ"] = "BS"
df[df$educ==5, "educ"] = "MSPHD"
table(df$educ)

table(df$marital)  # 그냥 재혼이든 이혼이든 결혼상태인지 아닌지로 두 범주로 나누는 것이 나을 듯 함
df[df$marital==2, "marital"] = "married"
df[(df$marital==1)|(df$marital==3)|(df$marital==4)|(df$marital==5), "marital"] = "notmarried"
table(df$marital)

table(df$child)
df[df$child==1, "child"]="Y"
df[df$child==2, "child"]="N"
table(df$child)

table(df$employ)
df[df$employ==1, "employ"] ="fulltime"
df[df$employ==2, "employ"] ="temporary"
table(df$employ)

df[df$year==2016, "year"] = 1
df[df$year==2017, "year"] = 2
df[df$year==2018, "year"] = 3
df[df$year==2019, "year"] = 4
df[df$year==2020, "year"] = 5
summary(df$year)

table(df$field)  # 99 -> 미분류
df$field <- ifelse(df$field==1, "ResearchTech", "else")
table(df$selfconfident) # 수치형으로 해도 될 듯 (순서형 범주)

# 취업여부 변수인 job은 이 데이터셋에서 다 5(취업)
table(df$job)

# 데이터 타입 확인 및 변경
str(df)
df$gender = as.factor(df$gender)
df$educ = as.factor(df$educ)
df$marital = as.factor(df$marital)
df$employ = as.factor(df$employ)
df$child = as.factor(df$child)
df$field = as.factor(df$field)
str(df)

# <수치형 변수> : income, birthyr, wrkhr, leastincome, selfconfident
# 변수들의 분포 확인해보기 
par(mfrow=c(2,2))
hist(df$income, main="income", xlab="income")
hist(df$wrkhr, main="wrkhr", xlab="wrkhr")
hist(df$leastincome, main="least income", xlab="leastincome")
hist(df$selfconfident, main="self confident", xlab="selfconfident")

# income -> 꼬리 길어서 로그 변환 (봉오리가 2개)
# birthyr -> 2016년 기준 나이로 변환 -> 안하기로
# df$birthyr = 2016-df$birthyr+1
# leastincome -> 꼬리 길어서 로그 변환

write.csv(df, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/final_lmer_gee_df.csv", row.names=FALSE)

# ----------------------------------------------------------------------------------------
library(lme4)
library(gee)
library(geepack)

df <- read.csv("C:/Users/kimch/Desktop/의생명통계/기말프로젝트/final_lmer_gee_df.csv")
options(scipen=10)
# < 1) lmer 모형 > 

# fit1: lmer 랜덤절편
fit1 <- lmer(log(income)~year+birthyr+gender+educ+marital+child+employ+field+wrkhr+selfconfident+(year|id), data=df)
# fit2: lmer 랜덤절편 + 랜덤기울기
# fit2 <- lmer(log(income)~birthyr+gender+educ+marital+child+employ+factor(field)+wrkhr+log(leastincome)+selfconfident+(year|id), data=df) 
# anova(fit1, fit2)  

# stepwise
# install.packages("lmerTest")
library(lmerTest)
step(fit1)

# 선택된 모형 : 랜덤절편만, year, employ, field, wrkhr, selfconfident 
lmer.fit1 <- lmer(log(income)~year+employ+field+wrkhr+selfconfident+(year|id), data=df)
summary(lmer.fit1)
AIC(lmer.fit1)

# 성별과 시간의 교호작용
lmer.fit2 <- lmer(log(income)~year+employ+field+wrkhr+selfconfident+year*gender+(year|id), data=df)
summary(lmer.fit2)


# 모델 적합성
AIC(lmer.fit1, lmer.fit2)


# < 2 ) gee 모형 > : lmer 최종 모형으로 gee 모형도 돌려봄

# gee/geeglm 모형

mf <-formula(log(income)~year+employ+field+wrkhr+selfconfident)
geeglm1<-glmgee(mf, id=id, family=gaussian, data=df, corstr="independence", scale.fix=TRUE)
geeglm2<-glmgee(mf, id=id, family=gaussian, data=df, corstr="exchangeable", scale.fix=TRUE)
geeglm3<-glmgee(mf, id=id, family=gaussian, data=df, corstr="AR-M-dependent(m)", scale.fix=TRUE)
geeglm4<-glmgee(mf, id=id, family=gaussian, data=df, corstr="unstructured", scale.fix=TRUE)
QIC(geeglm1, geeglm2, geeglm3, geeglm4)

# AR1 모형 쓰는 것이 가장 QIC 작음 
# gee1: 선택된 모형만 
gee1 <- glmgee(log(income)~year+employ+field+wrkhr+selfconfident, id=id, family=gaussian, data=df, corstr="AR-M-dependent(m)", scale.fix=TRUE)
summary(gee1)

# gee2: 성별과 결혼의 교호작용 추가
# year이 gederM에 비해 단위가 커서 그런 것 같음 year 2016 -> 1로 코딩하는 것은 어떤지 
gee2 <- glmgee(log(income)~year+employ+field+wrkhr+selfconfident+year*gender, id=id, family=gaussian, data=df, corstr="AR-M-dependent(m)", scale.fix=TRUE)
summary(gee2)


QIC(gee1, gee2)


# 최종모형: lmer -> lmer.fit1, gee -> gee.fit1

# EDA
library(ggplot2)

# 1. 남녀별 임금 분포도
p1 <- ggplot(df, aes(x=income, fill=gender, color=gender))+
  geom_histogram(alpha=0.5, position="dodge")+
  labs(title = "성별에 따른 연봉 히스토그램")+
  theme_classic()
p1

# 2. 결혼과 미혼의 임금 분포도
p2 <- ggplot(df, aes(x=income, fill=marital, color=marital))+
  geom_histogram(alpha=0.5, position="dodge")+
  labs(title = "결혼여부에 따른 연봉 히스토그램")+
  theme_classic()
p2

# 3. 자녀 유무에 따른 임금 분포도 
p3 <- ggplot(df, aes(x=income, fill=child, color=child))+
  geom_histogram(alpha=0.5, position="dodge")+
  labs(title = "자녀여부에 따른 연봉 히스토그램")+
  theme_classic()
p3


# + 추가: 정규직 비정규직 임금 분포도 
p4 <- ggplot(df, aes(x=income, fill=employ, color=employ))+
  geom_histogram(alpha=0.5, position="dodge")+
  labs(title = "정규직여부에 따른 연봉 히스토그램")+
  theme_classic()
p4
summary(df$income)
table(df$income)

temporary <- df[df$employ=="temporary", "income"]# temporary에 이상치 있는 듯
par(mfrow=c(1,1))
temp <- boxplot(temporary)
temp$out
summary(temporary)
fulltime <- df[df$employ=="fulltime", "income"]
summary(fulltime)
full <- boxplot(fulltime)
full$out

# 남녀별 income 스파게티 플랏
df$year<-df$year+2015
xyplot(df[df$gender=="M", 2]~df[df$gender=="M", 14], groups=df[df$gender=="M", 1], type="l", xlab="year", ylab="income", main="Male", ylim=c(50,37000))
xyplot(df[df$gender=="F", 2]~df[df$gender=="F", 14], groups=df[df$gender=="F", 1], type="l", xlab="year", ylab="income", xlim=c(2016,2020), ylim=c(50,37000), main="Female")

summary(df$income)
help(xyplot)

library(lattice)
library(faraway)
psid <-data.frame(psid)
