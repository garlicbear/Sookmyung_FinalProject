df <- read.csv("C:/Users/kimch/Desktop/의생명통계/기말프로젝트/final_lmer_gee_df.csv")

# fit1: glmer 모형
# fit1 <- glmer(log(income)~birthyr+gender+educ+marital+child+employ+field+wrkhr+log(leastincome)+selfconfident+(year|id), data=df, family="gaussian")
fit1 <- lmer(log(income)~birthyr+gender+educ+marital+child+employ+factor(field)+wrkhr+log(leastincome)+selfconfident+(year|id), data=df) 
summary(fit1)


memory.size(max = TRUE)    # OS에서 얻은 최대 메모리 크기 = OS로부터 R이 사용 가능한 메모리
memory.size(max = FALSE)   # 현재 사용중인 메모리 크기
memory.limit(size = NA) 

# stepwise
install.packages("cAIC4")
library(cAIC4)
stepcAIC(fit1)

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


# stepwise -> QIC 젤 작은 constr="independence" 모델로
install.packages("glmtoolbox")
library(glmtoolbox)
stepCriterion(fit2_1, criterion="p-value")

