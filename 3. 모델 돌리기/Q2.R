# 생각해보니깐 lasso-> logistic regression(0,1), ppt에서도 생존분석과 엮어서 했음 
# 회귀와 가까우므로 stepwise 사용

library(lme4)
library(gee)
library(geepack)
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
hist(df$income)
hist(df$wrkhr)
hist(df$leastincome)
hist(df$selfconfident)

# income -> 꼬리 길어서 로그 변환 (봉오리가 2개)
# birthyr -> 2016년 기준 나이로 변환
df$birthyr = 2016-df$birthyr+1
# leastincome -> 꼬리 길어서 로그 변환

write.csv(df, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/final_lmer_gee_df.csv", row.names=FALSE)

# ----------------------------------------------------------------------------------------

df <- read.csv("C:/Users/kimch/Desktop/의생명통계/기말프로젝트/final_lmer_gee_df.csv")

# fit1: glmer 모형
# fit1 <- glmer(log(income)~birthyr+gender+educ+marital+child+employ+field+wrkhr+log(leastincome)+selfconfident+(year|id), data=df, family="gaussian")
fit1 <- lmer(log(income)~birthyr+gender+educ+marital+child+employ+factor(field)+wrkhr+log(leastincome)+selfconfident+(year|id), data=df) 
summary(fit1)


# stepwise
install.packages("cAIC4")
library(cAIC4)
stepcAIC(fit1)  # ERROR

# fit2: gee/geeglm 모형
fit2_1 <- gee(log(income)~birthyr+gender+educ+marital+child+employ+factor(field)+wrkhr+log(leastincome)+selfconfident, id=id, family=gaussian, data=df, corstr="independence", scale.fix=TRUE)
summary(fit2_1)
fit2_2 <- gee(log(income)~birthyr+gender+educ+marital+child+employ+factor(field)+wrkhr+log(leastincome)+selfconfident, id=id, family=gaussian, data=df, corstr="exchangeable", scale.fix=TRUE)
summary(fit2_2)
fit2_3 <- geeglm(log(income)~birthyr+gender+educ+marital+child+employ+factor(field)+wrkhr+log(leastincome)+selfconfident, id=id, family=gaussian, data=df, corstr="ar1", scale.fix=TRUE)
summary(fit2_3)
fit2_4 <- gee(log(income)~birthyr+gender+educ+marital+child+employ+factor(field)+wrkhr+log(leastincome)+selfconfident, id=id, family=gaussian, data=df, corstr="unstructured", scale.fix=TRUE)
summary(fit2_4)


mf <-formula(log(income)~birthyr+gender+educ+marital+child+employ+factor(field)+wrkhr+log(leastincome)+selfconfident)
geeglm1<-geeglm(mf, id=id, family=gaussian, data=df, corstr="independence", scale.fix=TRUE)
geeglm2<-geeglm(mf, id=id, family=gaussian, data=df, corstr="exchangeable", scale.fix=TRUE)
geeglm3<-geeglm(mf, id=id, family=gaussian, data=df, corstr="ar1", scale.fix=TRUE)
geeglm4<-geeglm(mf, id=id, family=gaussian, data=df, corstr="unstructured", scale.fix=TRUE)
QIC(geeglm1, geeglm2, geeglm3, geeglm4)


# stepwise -> QIC 젤 작은 constr="ar1" 모델로
# 참고: https://search.r-project.org/CRAN/refmans/glmtoolbox/html/stepCriterion.glmgee.html
install.packages("glmtoolbox")
library(glmtoolbox)
stepCriterion(fit2_3, criterion="qic")

