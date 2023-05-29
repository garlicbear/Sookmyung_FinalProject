# 변수 뽑아내기
library(readxl)
df16 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w10.xlsx")
table(df16$w10edu)
# 우리의 표본 id : 2016년 기준 대졸자+석사 이상
df16 <- df16[(df16$w10edu==4)|(df16$w10edu==5),]
#  y10g719: 학생 대상 유보임금 -> 제거, y10g718과 y10g720 한 컬럼으로 합치기
col <- c("y10b224","y10b225","yob","gender","w10edu","sampid","y10a034","y10g101", "y10a065","y10d401","y10d402",
         "y10a080","y10b308","y10b156z","y10b161", "y10b220", "y10b221", "w10type", "y10g718", "y10g720","y10f310")
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

# 1. 자격증 변수 -> 2인 변수가 대부분이고 y10d402 변수가 질문이 저번 조사 후 추가로 취득한 자격증 개수는? 이어서 정확한 자격증 개수를 찾기 어려워보임
df16[,c("y10d401", "y10d402")]
table(df16$y10d401)
## 자격증 새로 딴 사람들 거의 없고 따도 새로 딴 자격증 개수 1,2개가 대부분
table(df16[df16$y10d401==1,c("y10d401", "y10d402")]$y10d402)
# 자격증 변수 제거
df16 <- subset(df16, select=-c(y10d401, y10d402))

# 2. 근로시간: 정규 + 추가 합치기
df16$y10b22 <- sum(df16$y10b220, df16$y10b221, na.rm=T)
df16 <- subset(df16, select=-c(y10b220, y10b221))

# 3. 유보임금: y10g718, y10g720 한줄로 합치기 
df16$y10g7 <- sum(df16$y10g718, df16$y10g720, na.rm=T)
df16 <- subset(df16, select=-c(y10g718, y10g720))

# 4. 임금변수 월평균로 통일
table(df16$y10b224) # 9090908, 9090909: 응답거절, 모름 -> 어떻게 처리할 것인지? 
for (i in range(1,length(df16))){
  if (is.na(df16$y10b224[i])){
    next()
  }
  else if (df16$y10b224[i]==1){
    df16$y10b225[i] <- df16$y10b225[i]/12
  }
  else if (df16$y10b224[i]==3){
    df16$y10b225[i] <- df16$y10b225[i]*4
  }
  else if (df16$y10b224[i]==4){
    df16$y10b225[i] <- df16$y10b225[i]*30
  }
  else if (df16$y10b224[i]==5){
    df16$y10b225[i] <- df16$y10b225[i]*df16$y10b22[i]
  }
  else{
    next()
  }
}
# 임금받는 시간 단위 변수 제거
df16 <- subset(df16,select=-c(y10b224))


# 2017년
df17 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w11.xlsx")

## 시간가변 공변량 + sampid+학과,학점,복수/부전공여부(결측치 많은 공변량)
col <- c("y11b224","y11b225","w11edu","sampid","y11g101","y11a034", "y11a065","y11a080",
         "y11b308","y11b156z","y11b161", "y11b220", "y11b221", "w11type", "y11g718", "y11g720","y11f310")
df17 <- subset(df17[id,], select=col)

# 결측치 확인 
## 결측치 많은 변수들 역시나 결측치 많음
colSums(is.na(df17))

# 2. 근로시간: 정규 + 추가 합치기
df17$y11b22 <- sum(df17$y11b220, df17$y11b221, na.rm=T)
df17 <- subset(df17, select=-c(y11b220, y11b221))

# 3. 유보임금: y10g718, y10g720 한줄로 합치기 
df17$y11g7 <- sum(df17$y11g718, df17$y11g720, na.rm=T)
df17 <- subset(df17, select=-c(y11g718, y11g720))

# 4. 임금변수 월평균로 통일
table(df17$y11b224) # 9090908, 9090909: 응답거절, 모름 -> 어떻게 처리할 것인지? 
for (i in range(1,length(df17))){
  if (is.na(df17$y11b224[i])){
    next()
  }
  else if (df17$y11b224[i]==1){
    df17$y11b225[i] <- df17$y11b225[i]/12
  }
  else if (df17$y11b224[i]==3){
    df17$y11b225[i] <- df17$y11b225[i]*4
  }
  else if (df17$y11b224[i]==4){
    df17$y11b225[i] <- df17$y11b225[i]*30
  }
  else if (df17$y11b224[i]==5){
    df17$y11b225[i] <- df17$y11b225[i]*df17$y11b22[i]
  }
  else{
    next()
  }
}
# 임금받는 시간 단위 변수 제거
df17 <- subset(df17,select=-c(y11b224))


# 2018년
df18 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w12.xlsx")
## 시간가변 공변량 + sampid+학과,학점,복수/부전공여부(결측치 많은 공변량)
col <- c("y12b224","y12b225","w12edu","sampid","y12g101","y12a034", "y12a065","y12a080",
         "y12b308","y12b156z","y12b161", "y12b220", "y12b221", "w12type", "y12g718", "y12g720","y12f310")
df18 <- subset(df18[id,], select=col)

# 결측치 확인 
colSums(is.na(df18))

# 2. 근로시간: 정규 + 추가 합치기
df18$y12b22 <- sum(df18$y12b220, df18$y12b221, na.rm=T)
df18 <- subset(df18, select=-c(y12b220, y12b221))

# 3. 유보임금: y10g718, y10g720 한줄로 합치기 
df18$y12g7 <- sum(df18$y12g718, df18$y12g720, na.rm=T)
df18 <- subset(df18, select=-c(y12g718, y12g720))

# 4. 임금변수 월평균로 통일
table(df18$y12b224) # 9090908, 9090909: 응답거절, 모름 -> 어떻게 처리할 것인지? 
for (i in range(1,length(df18))){
  if (is.na(df18$y12b224[i])){
    next()
  }
  else if (df18$y12b224[i]==1){
    df18$y12b225[i] <- df18$y12b225[i]/12
  }
  else if (df18$y12b224[i]==3){
    df18$y12b225[i] <- df18$y12b225[i]*4
  }
  else if (df18$y12b224[i]==4){
    df18$y12b225[i] <- df18$y12b225[i]*30
  }
  else if (df18$y12b224[i]==5){
    df18$y12b225[i] <- df18$y12b225[i]*df18$y12b22[i]
  }
}
    
# 임금받는 시간 단위 변수 제거
df18 <- subset(df18,select=-c(y12b224))


# 2019년
df19 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w13.xlsx")

## 시간가변 공변량 + sampid+학과,학점,복수/부전공여부(결측치 많은 공변량)
col <- c("y13b224","y13b225","w13edu","sampid","y13g101","y13a034", "y13a065","y13a080",
         "y13b308","y13b156z","y13b161", "y13b220", "y13b221", "w13type", "y13g718", "y13g720","y13f310")
df19 <- subset(df19[id,], select=col)

# 결측치 확인 
## 결측치 많은 변수들 역시나 결측치 많음
colSums(is.na(df19))

# 2. 근로시간: 정규 + 추가 합치기
df19$y13b22 <- sum(df19$y13b220, df19$y13b221, na.rm=T)
df19 <- subset(df19, select=-c(y13b220, y13b221))

# 3. 유보임금: y10g718, y10g720 한줄로 합치기 
df19$y13g7 <- sum(df19$y13g718, df19$y13g720, na.rm=T)
df19 <- subset(df19, select=-c(y13g718, y13g720))

# 4. 임금변수 월평균로 통일
table(df19$y13b224) # 9090908, 9090909: 응답거절, 모름 -> 어떻게 처리할 것인지? 
for (i in range(1,length(df19))){
  if (is.na(df19$y13b224[i])){
    next()
  }
  else if (df19$y13b224[i]==1){
    df19$y13b225[i] <- df19$y13b225[i]/12
  }
  else if (df19$y13b224[i]==3){
    df19$y13b225[i] <- df19$y13b225[i]*4
  }
  else if (df19$y13b224[i]==4){
    df19$y13b225[i] <- df19$y13b225[i]*30
  }
  else if (df19$y13b224[i]==5){
    df19$y13b225[i] <- df19$y13b225[i]*df19$y13b22[i]
  }
  else{
    next()
  }
}
# 임금받는 시간 단위 변수 제거
df19 <- subset(df19,select=-c(y13b224))


# 2020년
df20 <- read_excel("C:/Users/kimch/Desktop/원표본/ypdata_w14.xlsx")

## 시간가변 공변량 + sampid+학과,학점,복수/부전공여부(결측치 많은 공변량)
col <- c("y14b224","y14b225","w14edu","sampid","y14g101","y14a034", "y14a065","y14a080",
         "y14b308","y14b156z","y14b161", "y14b220", "y14b221", "w14type", "y14g718", "y14g720","y14f310")
df20 <- subset(df20[id,], select=col)

# 결측치 확인 
## 결측치 많은 변수들 역시나 결측치 많음
colSums(is.na(df20))

# 2. 근로시간: 정규 + 추가 합치기
df20$y14b22 <- sum(df20$y14b220, df20$y14b221, na.rm=T)
df20 <- subset(df20, select=-c(y14b220, y14b221))

# 3. 유보임금: y10g718, y10g720 한줄로 합치기 
df20$y14g7 <- sum(df20$y14g718, df20$y14g720, na.rm=T)
df20 <- subset(df20, select=-c(y14g718, y14g720))

# 4. 임금변수 월평균로 통일
table(df20$y14b224) # 9090908, 9090909: 응답거절, 모름 -> 어떻게 처리할 것인지? 
for (i in range(1,length(df20))){
  if (is.na(df20$y14b224[i])){
    next()
  }
  else if (df20$y14b224[i]==1){
    df20$y14b225[i] <- df20$y14b225[i]/12
  }
  else if (df20$y14b224[i]==3){
    df20$y14b225[i] <- df20$y14b225[i]*4
  }
  else if (df20$y14b224[i]==4){
    df20$y14b225[i] <- df20$y14b225[i]*30
  }
  else if (df20$y14b224[i]==5){
    df20$y14b225[i] <- df20$y14b225[i]*df20$y14b22[i]
  }
  else{
    next()
  }
}
# 임금받는 시간 단위 변수 제거
df20 <- subset(df20,select=-c(y14b224))


# 생성한 데이터프레임 저장
write.csv(df16, file="C:/Users/kimch/Desktop/원표본/df16.csv")
write.csv(df17, file="C:/Users/kimch/Desktop/원표본/df17.csv")
write.csv(df18, file="C:/Users/kimch/Desktop/원표본/df18.csv")
write.csv(df19, file="C:/Users/kimch/Desktop/원표본/df19.csv")
write.csv(df20, file="C:/Users/kimch/Desktop/원표본/df20.csv")
