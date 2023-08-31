# 변수 뽑아내기
library(readxl)
df16 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w10.xlsx")
table(df16$w10edu)
# 우리의 표본 id : 2016년 기준 대졸자+석사 이상
df16 <- df16[(df16$w10edu==4)|(df16$w10edu==5),]
#  y10g719: 학생 대상 유보임금 -> 제거, y10g718과 y10g720 한 컬럼으로 합치기
col <- c("y10b224","y10b225","yob","gender","w10edu","sampid","y10a034","y10g101", "y10a065","y10d401","y10d402", "y10b222", "y10b223",
         "y10a080","y10b308","y10b156z","y10b161", "y10b220", "y10b221", "w10type", "y10g718", "y10g720","y10f310", "y10g500")
df16 <- df16[,col]

# 결측값 확인 -> 우리의 y 변수인 취업여부가 비어 있는 사람은 빼야할 것 같음
colSums(is.na(df16))
df16 <- df16[!is.na(df16$w10type),]

# 다시 결측치 확인 
# 거의 모든 응답자가 결측치: y10a034(학과), y10a065(학점), y10d402(자격증 저번 조사 후 취득한 자격증 개수), y10a080(복수전공/부전공)
# 취업자만 답 가능: y10b224, y10b225(임금), y10b156z(산업군 대분류), y10b161(직위), y10b220, y10b221(근로시간), y10g718(취업자 대상 유보임금)
colSums(is.na(df16))
# 우리의 표본 id
id <- unique(df16$sampid) 

## 1. 자격증 변수 -> 2인 변수가 대부분이고 y10d402 변수가 질문이 저번 조사 후 추가로 취득한 자격증 개수는? 이어서 정확한 자격증 개수를 찾기 어려워보임
df16[,c("y10d401", "y10d402")]
table(df16$y10d401)
### 자격증 새로 딴 사람들 거의 없고 따도 새로 딴 자격증 개수 1,2개가 대부분
table(df16[df16$y10d401==1,c("y10d401", "y10d402")]$y10d402)
### 자격증 변수 제거
df16 <- subset(df16, select=-c(y10d401, y10d402))

## 2. 근로시간: 정규 + 추가 합치기 : 0 -> 결측값
### sum 쓰면 이상하게 연산함
df16$y10b220[is.na(df16$y10b220)] <- 0
df16$y10b221[is.na(df16$y10b221)] <- 0
df16$y10b22 <- df16$y10b220 + df16$y10b221
df16$y10b22
df16 <- subset(df16, select=-c(y10b220, y10b221))

## 3. 유보임금: y10g718, y10g720 한줄로 합치기 :  0 -> 결측값
### 역시나 sum쓰면 연산 이상하게 됨
df16$y10g718[is.na(df16$y10g718)] <- 0
df16$y10g720[is.na(df16$y10g720)] <- 0
df16$y10g718
df16$y10g7 <- df16$y10g718 + df16$y10g720
df16$y10g7
df16 <- subset(df16, select=-c(y10g718, y10g720))

## 2,3 -> 0 결측치로 되어있어 처리 필요, 3에서 9090909도 처리 필요
df16$y10b22 <- replace(df16$y10b22, df16$y10b22==0, NA)
df16$y10g7 <- replace(df16$y10g7, df16$y10g7==0, NA)
df16$y10b22
df16$y10g7

## 거의 모든 값이 결측값인 변수 제거 
df16 <- subset(df16, select=-c(y10a034, y10a065, y10a080))


# 2017년
df17 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w11.xlsx")
col <- c("y11b224","y11b225","yob", "gender", "w11edu","sampid","y11g101","y11a034", "y11a065","y11a080", "y11b222", "y11b223",
         "y11b308","y11b156z","y11b161", "y11b220", "y11b221", "w11type", "y11g718", "y11g720","y11f310", "y11g500")
df17 <- subset(df17[id,], select=col)

## 결측치 확인 
### 결측치 많은 변수들 역시나 결측치 많음
colSums(is.na(df17))

## 2. 근로시간: 정규 + 추가 합치기
df17$y11b220[is.na(df17$y11b220)] <- 0
df17$y11b221[is.na(df16$y11b221)] <- 0
df17$y11b22 <- df17$y11b220 + df17$y11b221
df17 <- subset(df17, select=-c(y11b220, y11b221))

## 3. 유보임금: y10g718, y10g720 한줄로 합치기 
df17$y11g718[is.na(df17$y11g718)] <- 0
df17$y11g720[is.na(df17$y11g720)] <- 0
df17$y11g718
df17$y11g7 <- df17$y11g718 + df17$y11g720
df17 <- subset(df17, select=-c(y11g718, y11g720))

## 2,3 -> 0 결측치로 되어있어 처리 필요, 3에서 9090909도 처리 필요
df17$y11b22 <- replace(df17$y11b22, df17$y11b22==0, NA)
df17$y11g7 <- replace(df17$y11g7, df17$y11g7==0, NA)

## 거의 모든 값이 결측값인 변수 제거 
df17 <- subset(df17, select=-c(y11a034, y11a065, y11a080))

# 2018년
df18 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w12.xlsx")
col <- c("y12b224","y12b225", "yob", "gender", "w12edu","sampid","y12g101","y12a034", "y12a065","y12a080", "y12b222", "y12b223",
         "y12b308","y12b156z","y12b161", "y12b220", "y12b221", "w12type", "y12g718", "y12g720","y12f310", "y12g500")
df18 <- subset(df18[id,], select=col)

## 결측치 확인 
colSums(is.na(df18))

## 2. 근로시간: 정규 + 추가 합치기
df18$y12b220[is.na(df18$y12b220)] <- 0
df18$y12b221[is.na(df18$y12b221)] <- 0
df18$y12b22 <- df18$y12b220 + df18$y12b221
df18 <- subset(df18, select=-c(y12b220, y12b221))

## 3. 유보임금: y10g718, y10g720 한줄로 합치기 
df18$y12g718[is.na(df18$y12g718)] <- 0
df18$y12g720[is.na(df18$y12g720)] <- 0
df18$y12g718
df18$y12g7 <- df18$y12g718 + df18$y12g720
df18 <- subset(df18, select=-c(y12g718, y12g720))

## 2,3 -> 0 결측치로 되어있어 처리 필요, 3에서 9090909도 처리 필요
df18$y12b22 <- replace(df18$y12b22, df18$y12b22==0, NA)
df18$y12g7 <- replace(df18$y12g7, df18$y12g7==0, NA)

## 거의 모든 값이 결측값인 변수 제거 
df18 <- subset(df18, select=-c(y12a034, y12a065, y12a080))


# 2019년
df19 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w13.xlsx")
col <- c("y13b224","y13b225", "yob", "gender" ,"w13edu","sampid","y13g101","y13a034", "y13a065","y13a080", "y13b222", "y13b223", 
         "y13b308","y13b156z","y13b161", "y13b220", "y13b221", "w13type", "y13g718", "y13g720","y13f310", "y13g500")
df19 <- subset(df19[id,], select=col)

## 결측치 확인 
### 결측치 많은 변수들 역시나 결측치 많음
colSums(is.na(df19))

## 2. 근로시간: 정규 + 추가 합치기
df19$y13b220[is.na(df19$y13b220)] <- 0
df19$y13b221[is.na(df19$y13b221)] <- 0
df19$y13b22 <- df19$y13b220 + df19$y13b221
df19 <- subset(df19, select=-c(y13b220, y13b221))

## 3. 유보임금: y10g718, y10g720 한줄로 합치기 
df19$y13g718[is.na(df19$y13g718)] <- 0
df19$y13g720[is.na(df19$y13g720)] <- 0
df19$y13g718
df19$y13g7 <- df19$y13g718 + df19$y13g720
df19 <- subset(df19, select=-c(y13g718, y13g720))

## 2,3 -> 0 결측치로 되어있어 처리 필요, 3에서 9090909도 처리 필요
df19$y13b22 <- replace(df19$y13b22, df19$y13b22==0, NA)
df19$y13g7 <- replace(df19$y13g7, df19$y13g7==0, NA)

## 거의 모든 값이 결측값인 변수 제거 
df19 <- subset(df19, select=-c(y13a034, y13a065, y13a080))


# 2020년
df20 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w14.xlsx")

## 시간가변 공변량 + sampid+학과,학점,복수/부전공여부(결측치 많은 공변량)
col <- c("y14b224","y14b225","yob", "gender", "w14edu","sampid","y14g101","y14a034", "y14a065","y14a080", "y14b222", "y14b223",
         "y14b308","y14b156z","y14b161", "y14b220", "y14b221", "w14type", "y14g718", "y14g720","y14f310", "y14g500")
df20 <- subset(df20[id,], select=col)

## 결측치 확인 
### 결측치 많은 변수들 역시나 결측치 많음
colSums(is.na(df20))

## 2. 근로시간: 정규 + 추가 합치기
df20$y14b220[is.na(df20$y14b220)] <- 0
df20$y14b221[is.na(df20$y14b221)] <- 0
df20$y14b22 <- df20$y14b220 + df20$y14b221
df20 <- subset(df20, select=-c(y14b220, y14b221))

## 3. 유보임금: y10g718, y10g720 한줄로 합치기 
df20$y14g718[is.na(df20$y14g718)] <- 0
df20$y14g720[is.na(df20$y14g720)] <- 0
df20$y14g7 <- df20$y14g718 + df20$y14g720
df20 <- subset(df20, select=-c(y14g718, y14g720))

## 2,3 -> 0 결측치로 되어있어 처리 필요, 3에서 9090909도 처리 필요
df20$y14b22 <- replace(df20$y14b22, df20$y14b22==0, NA)
df20$y14g7 <- replace(df20$y14g7, df20$y14g7==0, NA)

## 거의 모든 값이 결측값인 변수 제거 
df20 <- subset(df20, select=-c(y14a034, y14a065, y14a080))


# 결측치, 모름, 응답거절 처리
## 결측치 처리
df16 = na.omit(df16)
df17 = na.omit(df17)
df18 = na.omit(df18)
df19 = na.omit(df19)
df20 = na.omit(df20)

## 모름, 응답거절 처리
library(dplyr)
names(df16)
df16<-df16%>%filter_at(vars(c(y10b224, y10b225, yob, gender, w10edu, sampid, y10g101, y10b222, y10b223, y10g500, y10b308, y10b156z, y10b161, w10type, y10f310, y10b22, y10g7)),all_vars(. <9090908))
df17<-df17%>%filter_at(vars(c(y11b224, y11b225, yob, gender, w11edu, sampid, y11g101, y11b222, y11b223, y11g500, y11b308, y11b156z, y11b161, w11type, y11f310, y11b22, y11g7)),all_vars(. <9090908))
df18<-df18%>%filter_at(vars(c(y12b224, y12b225, yob, gender, w12edu, sampid, y12g101, y12b222, y12b223, y12g500, y12b308, y12b156z, y12b161, w12type, y12f310, y12b22, y12g7)),all_vars(. <9090908))
df19<-df19%>%filter_at(vars(c(y13b224, y13b225, yob, gender, w13edu, sampid, y13g101, y13b222, y13b223, y13g500, y13b308, y13b156z, y13b161, w13type, y13f310, y13b22, y13g7)),all_vars(. <9090908))
df20<-df20%>%filter_at(vars(c(y14b224, y14b225, yob, gender, w14edu, sampid, y14g101, y14b222, y14b223, y14g500, y14b308, y14b156z, y14b161, w14type, y14f310, y14b22, y14g7)),all_vars(. <9090908))


# 생성한 데이터프레임 저장
write.csv(df16, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df16.csv", row.names=FALSE)
write.csv(df17, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df17.csv", row.names=FALSE)
write.csv(df18, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df18.csv", row.names=FALSE)
write.csv(df19, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df19.csv", row.names=FALSE)
write.csv(df20, file="C:/Users/kimch/Desktop/의생명통계/기말프로젝트/df20.csv", row.names=FALSE)

