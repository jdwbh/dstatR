#**********************************************
#*********   ����ͳ�Ʒ�����R����Ӧ�� **********
#*********    ��2�� ����� 2017.2.4  **********
#**********************************************
# 1 ����---��γ�Ϊ���ݷ���ʦ ----
### 1.2.3 ���ݷ������ߵ�ѡ�� ----
X=matrix(1:20,10,2);X
### 1.3.2 ΪʲôҪ��R���� ----
search()
library(MASS)  #�Ѱ�װMASS��
source('dstatR2func.R') #library(dstatR)

### 1.3.4 ��η���R������ ----
# 1. ϵͳ���
options(digits=4)                 #������λ��
par(mar=c(4,4,1.5,1)+0.1,cex=0.8) #ͼ������
# 2. ��ȡ����
UG=read.csv('UGdata.csv');UG
# 3. ͳ�Ʒ���
summary(UG)
t.test(height~sex,data=UG)
fm=lm(weight~height,data=UG);fm
summary(fm)
plot(weight~height,data=UG);abline(fm)    #ģ�ͷ���

# 2 �����ռ����� ----
### 2.3.2 �������� ----
X=c(164,162,186,165,165,187,169,151,157);X

UG=read.table("clipboard",header=T) 
UG=read.table("UGdata.txt",header=T)
UG=read.csv("UGdata.csv")
UG=read.csv('http://202.116.1.177/Rstat/UGdata.csv')
UG

### 2.3.3 R������������ʽ ----
x=c(1,3,5,7,9,2,4,6,8);x
i=1:9;i
j=9:1;j
seq(0.5,9.5,length=20)

class(UG)

data.frame(x,i,j)

UG1=data.frame(g=UG$sex,x=UG$height,y=UG$weight)
head(UG1) #�����ݽ϶�ʱ������head ��ʾ���ݼ���ǰ6�У��ȼ���UG[1:6,]
UG2=UG[,c('sex','height','weight')]
head(UG2)
UG3=UG[1:6,1:5]
UG3

# 3 ���ݴ������� ----
## 3.1 �������� ----
### 3.1.1 �������� ----
setwd("E:/DataStatR")# �ȼ���setwd("E:\\DataStatR")
getwd()              # ��ʾ��ǰ����Ŀ¼

### 3.1.2 �Զ��庯�� ----
install.packages("dstatR")
library(dstatR)

Sq.sum <- function(x){ 
  S=sum(x^2)      # S=???x2
  S               # return(S)
}

Sq.sum <- function(x){ sum(x^2) }
Sq.sum(1:9)
Sq.sum (UG$height)

welcome <- function() print("welcome to use R")
welcome()
welcome <- function(names) print(paste("welcome",names,"to use R"))
welcome("Mr fang")
welcome("Mr Wang")
welcome()
welcome <-function(names="Mr fang") 
  print(paste("welcome",names,"to use R"))
welcome()

sim.t<-function(n){
  mu=5;sigma=2;
  x=rnorm(n,mu,sigma)
  t=(mean(x)-mu)/(sd(x)/sqrt(n))
  t  # return(t)
}
sim.t(10)  #��������Ϊ10����ֵ???=5����׼��???=2��tͳ����

sim.t<-function(n=10,mu=5,sigma=2){ #Ĭ��������Ϊ10����ֵΪ5����׼��Ϊ2
  x=rnorm(n,mu,sigma)
  t=(mean(x)-mu)/(sd(x)/sqrt(n))
  t 
}
sim.t()                      # ��������Ϊ10����ֵΪ5����׼��Ϊ2
sim.t(15)                    # ��������Ϊ15����ֵΪ5����׼��Ϊ2
sim.t(mu=4,sigma=1)          #��������Ϊ10����ֵΪ4����׼��Ϊ1
sim.t(n=25,sigma=20,mu=10)   # ��������Ϊ25����ֵΪ10����׼��Ϊ20

plot.f=function(f,a,b){
  x=seq(a,b,length=100)
  plot(x,f(x),type='l')
}

par(mar=c(4,4,2,1),mfcol=c(1,2),cex=0.8)
  plot.f(sin,0,2*pi)  #��������
  plot.f(cos,0,2*pi)  #��������    
par(mfcol=c(1,1))

par(mfcol=c(1,2))
plot.f(exp,-1,1)     #��-1��1��ָ������
plot.f(log,0,1)      #��0��1�Ķ�������
par(mfrow=c(1,1))

Square<-function(x){ 
  sum2=sum(x^2)
  prod2=prod(x^2) 
  lxx=sum((x-mean(x))^2)
  list(sum2=sum2,prod2=prod2,lxx=lxx)
}
S=Square(UG$height);S

### 3.1.3 ������� ----
x.sum<-function(x){
  n=length(x)
  s = 0
  for(i in 1:n)  
    s = s + x[i]
  s
}
x.sum(1:10)

par(mfrow=c(2,2))
  for(n in c(20,30,50,100)) 
    hist(rnorm(n),xlab='',main=paste('n=',n))
par(mfrow=c(1,1))

f=1; f[2]=1; i=1
while (f[i]+f[i+1]<1000){
  f[i+2]=f[i]+f[i+1]
  i<-i+1;
}
f

f=1; f[2]=1; i=1
repeat {
  f[i+2]=f[i]+f[i+1]
  i=i+1
  if (f[i]+f[i+1]>=1000) break
}
f

abs.x <- function(x){
  if (x<0) {x=-x}
  x
}
abs.x(-3)
abs.x(3)
abs.x(c(3,-3))

abs.x <- function(x){  
  if(x[x<0]){
     x[x<0]=-x[x<0]
  }
  x
}
abs.x(c(3,-3))

if(any(x<=0)) y =log(1+x) else y= log(x)
y
x=c(-3,3)
ifelse(x<=0,-x,x)

## 3.2 ����ѡ�� ----
dim(UG)
nrow(UG)    #dim(UG)[1]
ncol(UG)    #dim(UG)[2]
names(UG)

head(UG)
head(UG,3)
head(UG,-3)
tail(UG)
tail(UG,3)
tail(UG,-3)

### 3.2.1 ѡȡ�۲� ----
UG[1,]          #��1��ѧ���Ļ�����Ϣ
UG[6,]          #��6��ѧ���Ļ�����Ϣ
UG[1:6,]        #ǰ1~6��ѧ���Ļ�����Ϣ���ȼ���head(UG)
UG[c(1,6,20),]   #��1��6��20λѧ���Ļ�����Ϣ

UG[UG$sex=='��',]  #ѡȡ�Ա�Ϊ�еĹ۲�����
UG[UG$weight>80,]   #ѡȡ���ش���80kg��ѧ��

subset(UG,sex=='��')  #ѡȡ��������
subset(UG,sex=='��' & weight>80)  #ѡȡ���ش���80����������

### 3.2.2 ѡȡ���� ----
UG[,7]        #�ȼ���UG$height
UG[,8]        #�ȼ���UG$weight
UG[,7:8]      #�ȼ���UG[,c(7,8)] ��UG[,c('height','weight')]
UG[,c(3,7,8)]  #�ȼ���UG[,c('sex','height','weight')]

mean(UG$height)
mean(UG$weight)
plot(UG$height, UG$weight)
lm(UG$weight~ UG$height)

plot(weight~height,data=UG)
lm(weight~ height,data=UG)

attach(UG)
  mean(height)
  mean(weight)
  plot(height,weight) 
  lm(weight~height)  
detach()

with(UG,{
  mean(height)
  mean(weight)
  plot(height,weight) 
  lm(weight~height)  
})

### 3.2.3 ѡȡ�۲������ ----
UG[UG$sex=='��' & UG$weight>80,c('sex','height','weight')]
subset(UG,sex=='��' & weight>80,c(sex,height,weight)) 

### 3.2.4 �޳��۲������ ----
UG[-1,]            #�޳���1������
UG[c(-1,-6),]        #�޳���1�к͵�6������
UG[,-1]            #�޳���1������
UG[,c(-1,-6)]        #�޳���1�к͵�6������
UG[-1,-1]          #�޳���1�к͵�1������
UG[c(-1,-6),c(-1,-6)]  #�޳���1�С���6�к͵�1�С���6������

## 3.3 ����ת�� ----
### 3.3.1 �޸ı����� ----
names(UG)
vars<-names(UG)
names(UG)<-paste("x",1:9,sep="")# x��1:9�е�ÿ��������ҷָ���Ϊ�ո�
names(UG)
names(UG)<-vars
names(UG)
names(UG)[3]<-"gender"
names(UG)[9]<-"math"
names(UG)
### 3.3.2 �������� ----
UG$logincome=log(UG$income)
UG$logincome<-NULL
UG$GPA=UG$score/10-5  #����=����/10-5
head(UG)
### 3.3.3 �����任 ----
UG=transform(UG,GPA=score/10-5)
tail(UG)

UG$birth=as.Date(UG$birth)
head(UG)
UG$age=as.Date("2012-09-01")-UG$birth
head(UG)
UG$age=as.integer(UG$age/365)   
head(UG)
### 3.3.4 ɾ������ ----
names(UG)
UG$GPA<-NULL;UG$age<-NULL;UG$income_c<-NULL;UG$score_c<-NULL
names(UG)
### 3.3.5 ���±��� ----
UG$income_c=NA
UG$income_c[UG$income<=5]<-"������"
UG$income_c[UG$income>5 & UG$income<=50 ]<-"�е�����"
UG$income_c[UG$income>50]<-"������"
head(UG)

UG<-within(UG,{
    income_c=NA
    income_c[income<=5]<-"������"
    income_c[income>5 & income<=50 ]<-"�е�����"
    income_c[income>50]<-"������"
})
head(UG)
stem(UG$income)

UG$income_c=cut(UG$income,breaks=c(0,5,50,max(UG$income)))
head(UG)
UG$income_c=cut(UG$income,breaks=c(0,5,50,200),
                labels=c("������","�е�����","������"))
head(UG)
UG$score_c=cut(UG$score,breaks=c(0,59,60,70,80,90,100))
head(UG)
## 3.4 �������� ----
### 3.4.1 ���ݼ����� ----
UG[order(UG$income),]  #��income���������У�Ĭ�ϡ�
UG[order(UG$income,decreasing=TRUE),]
UG[order(UG$sex,UG$income),]
### 3.4.2 ���ݼ��ϲ� ----
UG1=UG[,c(1,2:4)]; head(UG1)
UG2=UG[,c(1,7:9)]; head(UG2)
cbind(UG1,UG2)
merge(UG1,UG2,by="id") 
UG3=UG[10:12,c(1,7:9)];UG3
### 3.4.3 ȱʧ���ݵĴ��� ----
UG.NA=UG[,1:9]
UG.NA$income[2]<-NA
UG.NA$weight[4]<-NA
head(UG.NA)
UG_NA=na.omit(UG.NA)
head(UG_NA)

# 4 ����ͳ������  ----
## 4.1 ����ͼ�κ��� ----
### 4.1.1 �߼���ͼ���� ----
X=UG$height   
plot(X)         # plot(X,type='p')
plot(X,type='l')
plot(X,type='b')
plot(X,type='h')

plot(X)         
plot(X,xlab='���',ylab='����',ylim=c(140,200),main='ѧ�����ߵ�ɢ��ͼ')
### 4.1.2 �ͼ���ͼ���� ----
plot(X,type='h');points(X);text(X,cex=0.75)
plot(X,type='h');lines(X);abline(h=170,lty=3)
### 4.1.3 ��ͼ�������� ----
plot(1:24,pch=1:24)
text(1:24,adj=-1)

par(mfrow = c(1,2),mar=c(4,4,2,1)+0.1)              # ����1��2����������ͼ
  plot(X)
par(mar=c(4,4,2,1)+0.1,cex=0.75)  # �޸ı߼ʺ�����
  plot(X)
par(mfrow = c(1,1))             # �ָ�Ϊ����1��1�е���ͼ

layout(matrix(1:4, 2, 2))
layout.show(4)

layout (matrix(c(1,1,1,2,3,4,2,3,4),nr=3,byrow=T))
hist(rnorm(100));hist(rnorm(20));hist(rnorm(40));hist(rnorm(60))
layout(1)

with(UG,{
  par(mfrow=c(1,2),cex=0.8) 
  plot(height,weight);
  plot(height,weight,pch=as.numeric(region));
  legend(150,90,levels(region),pch=1:3,bty='n')
  par(mfrow=c(1,1)) 
})

## 4.2 �����������������ݷ��� ----
### 4.2.1 �������ݷ��� ----
T1=table(UG$region);T1
par(mfcol=c(1,2),cex=0.8)
  barplot(T1,ylim=c(0,20))
  barplot(T1,ylim=c(0,20),col = c("red","yellow","blue"))
par(mfcol=c(1,1))

par(mfcol=c(1,3))
pie(T1)                                # ��һͼ
pie(T1,col=c("red","yellow","blue"))   # �ڶ�ͼ
pct=round(T1/sum(T1)*100,1)          # ����ͼ
lbs=paste(names(T1),pct,"%",sep="")     
pie(T1,lbs)
par(mfcol=c(1,1))

### 4.2.2 �������ݷ��� ----
sum(UG$height)
mean(UG$height)
median(UG$height)
var(UG$height)
sd(UG$height)
quantile(UG$height)
summary(UG$height)
IQR(UG$height)
mad(UG$height)
stem(UG$height)
stem(UG$income)

score_c=cut(UG$score,breaks=c(0,60,70,80,90,100))   #��ʱ����
table(score_c)
score_c=cut(UG$score,breaks=c(0,60,70,80,90,100),right=F)
table(score_c)
par(mfcol=c(1,2),mar=c(4,2,2,1)+0.1,cex=0.8)
  barplot(table(score_c),ylim=c(0,20))
  barplot(table(score_c),ylim=c(0,20),col=2:6)
par(mfcol=c(1,1))

par(mfcol=c(1,2),mar=c(4,4,2,1)+0.1,cex=0.8)
  hist(UG$income,ylim=c(0,30),main='')          # ��Ƶ������ֱ��ͼ
  hist(UG$income,prob=T,ylim=c(0,0.03),main='') # ��Ƶ�ʻ���ֱ��ͼ
  lines(density(UG$income))        # ���Ӹ����ܶ�����
par(mfcol=c(1,1))

rbinom(10,5,0.35)

par(mfcol=c(1,2),cex=0.8)
  hist(UG$height,main='')          # ��Ƶ������ֱ��ͼ
  hist(UG$height,prob=T,main='')   # ��Ƶ�ʻ���ֱ��ͼ
  lines(density(UG$height))        # ���Ӹ����ܶ�����
par(mfcol=c(1,1))

par(mfcol=c(1,2),mar=c(4,4,2,1)+0.5,cex=0.8)
  qqnorm(UG$income);qqline(UG$income)  #�������̬QQͼ
  qqnorm(UG$height);qqline(UG$height)  #���ߵ���̬QQͼ
par(mfcol=c(1,1))

op=par(mfcol=c(1,2),cex=0.8)
  boxplot(UG$income,ylab='income')    
  boxplot(UG$income,ylab='income',horizontal=T)   
par(op)

par(mfcol=c(1,2),cex=0.8)
  boxplot(UG$height,ylab='height')    
  boxplot(UG$weight,ylab='weight')   
par(mfcol=c(1,1))

### 4.2.3 �����Լ��ķ������� ----
Stat1<-function (x){
  cat('n =',length(x),'\n')
  cat('min =',min(x),'\n')
  cat('max =',max(x),'\n')
  cat('mean =',mean(x),'\n')
  cat('sd =',sd(x),'\n')
  cat('median =',median(x),'\n')
  cat('IQR =',IQR(x),'\n')
#  cat('var =',var(x),'\n')
#  cat('CV =',sd(x)/mean(x),'\n')  # ����ϵ��
}
Stat1(UG$height)

Stat2<-function(x){
  c(n=length(x),min=min(x),max=max(x),mean=mean(x),
    sd=sd(x),median=median(x),IQR=IQR(x))  
}
Stat2(UG$height)

Stats(UG[,6])
Stats(UG[,6:9])

EDA(UG$height)
EDA(UG$income)
EDA(log(UG$income))

Ftab(UG$sex)
Ftab(UG$region)

Freq(UG$income)
Freq(UG$height)

## 4.3 ����������ݿ����ݷ��� ----
### 4.3.1 ���������ݷ��� ----
tsr=table(UG$sex,UG$region);tsr #tsr=xtabs(~sex+region,data=UG)
prop.table(tsr)    #�ܵĹ��ɱ�
prop.table(tsr,1)   #���й��ɱ�
prop.table(tsr,2)   #���й��ɱ�

par(mfrow=c(1,2),cex=0.8)          
  barplot(tsr,ylim=c(0,25),legend.text=levels(UG$sex))        
  barplot(tsr,ylim=c(0,20),beside=T,legend.text=levels(UG$sex))  
par(mfrow=c(1,1))

UG$income_c=cut(UG$income,breaks=c(0,5,50,200),
                labels=c("������","�е�����","������"))
table(UG$income_c)

table(UG$region, UG$income_c)
table(UG$region, UG$income_c,UG$sex)
ftable(UG$sex,UG$region, UG$income_c)

attach(UG)
  par(mfrow=c(1,2),cex=0.75)
    barplot(table(region,income_c),legend.text=levels(region))
    barplot(table(region,income_c),beside=T,legend.text=levels(region)) 
  par(mfrow=c(1,1))
detach(UG)

attach(UG)
  par(mfrow=c(1,2),cex=0.8)
    barplot(table(income_c,region),ylim=c(0,25),col=2:6)
    legend('top',levels(income_c),pch=15,col=2:6)
    barplot(table(income_c,region),ylim=c(0,20),beside=T,col=1:3) 
    legend('top',levels(income_c),pch=15,col=1:3)
  par(mfrow=c(1,1))
detach(UG)

### 4.3.2 ���������ݷ��� ----
plot(UG$height,UG$weight) 
plot(UG[,6:9],gap=0)
pairs(UG[,c("income","height","weight","score")],gap=0) 

### 4.3.3 �����Լ������ݷ��� ----
bism=by(UG$height,UG$sex,mean)    # ���Ա����������ľ�ֵ
bism
barplot(bism,ylim=c(0,200),col=1:2)

biss=by(UG$height,UG$sex,sd)      # ���Ա����������ı�׼��
biss
barplot(biss,ylim=c(0,14),col='white')

library(lattice)
histogram(~height|sex,data=UG)

by(UG$height,UG$sex,summary) # ���Ա���������ߵĻ���ͳ����
par(mfrow=c(1,2),cex=0.8)
  boxplot(height~sex,data=UG)   # ���Ա���������ߵ���ʽͼ
  boxplot(height~sex,data=UG,notch=T,col=1:2)   
par(mfrow=c(1,1))

bwplot(~height|sex,data=UG)

by(UG$income,UG$region,summary)
par(mfrow=c(1,2),cex=0.8)
  boxplot(income~region,data=UG)
  boxplot(income~region,data=UG,horizontal=T)
par(mfrow=c(1,1),cex=0.8)

by(UG$income,list(UG$sex,UG$region),summary)
boxplot(income~sex+region,data=UG)

par(mfrow=c(1,2),cex=0.8)
  stripchart(height~sex,data=UG,pch=19)
  stripchart(height~region,data=UG,pch=19)
par(mfrow=c(1,1))

par(mfrow=c(1,2),cex=0.8)
  plot(UG$height,UG$weight);text(UG$height,UG$weight,col=1:2,adj=-0.5,cex=0.75)  
  plot(UG$height,UG$weight);text(UG$height,UG$weight,UG$sex,adj=-0.5,cex=0.75)  
par(mfrow=c(1,1))
coplot(weight~height|sex,data=UG)
coplot(weight~height|region,data=UG, rows=1)

### 4.3.4 Ӧ���ຯ����Ӧ�� ----
UG1=UG[,6:9]
apply(UG1,2,mean)
lapply(UG1,function(x) list(mean=mean(x),sd=sd(x)))
sapply(UG1,function(x) list(mean=mean(x),sd=sd(x)))
tapply(UG$height,INDEX=UG$sex,FUN=mean)
aggregate(UG1,by=list(UG$sex),FUN=mean)
aggregate(UG1,by=list(sex=UG$sex,region=UG$region),FUN=mean)

# 5 �����������ֲ� ----
## 5.1 �����������ֲ� ----
### 5.1.1 ��ɢ��������� ----
par(mar=c(4,4,2,1)+0.2,cex=0.8)
n=20;p=0.5
x=1:5 
P=choose(n,x)*p^x*(1-p)^(n-x)
plot(x,P,'h')

N=100;M=5;n=10
x=1:5 
P=choose(M,x)*choose(N-M,n-x)/choose(N,n)
plot(x,P,'h')

m=10; n=7; k=8
rx=rhyper(15,m,n,k);rx
x=0:(k+1)
px=phyper(x,m,n,k)
dx=dhyper(x,m,n,k)
cbind(px,dx)

lamda=6; x=0:5 
P=lamda^x*exp(-lamda)/factorial(x)
plot(x,P,'h')

### 5.1.2 ������������� ----
x=0:1;y=c(1,1)
plot(x,y,ylim=c(0,1.5),type='l')

x=seq(-4,4,0.1)
y=1/sqrt(2*pi)*exp(-x^2/2)
plot(x,y,type='l')

a=qnorm(0.95);a

x0=4 
z=seq(-2,2,0.1)
x=seq(-x0,x0,0.1)
plot(x,dnorm(x),xlim=c(-x0,x0),ylim=c(0,0.5),type="l",xlab="x",ylab="p(x)")    
abline(h=0,lty=3)

X=UG$height
a=160
Za=(a-mean(X))/sd(X);Za
Pa=pnorm(Za);Pa
b=180
Zb=(mean(b-X))/sd(X);Zb
Pb=1-pnorm(Zb);Pb
P=1-Pa-Pb;P

x=0:50
y=0.3*exp(-0.3*x)
plot(x,y,type='l')

### 5.1.3 R���Էֲ������б� ----
x=rnorm(10);x
y0=dnorm(0);y0   #y=1/sqrt(2*pi)*exp(0)
y1=dnorm(1);y1   #y1=1/sqrt(2*pi*exp(1))
p1=pnorm(-1.96);p1
p2=pnorm(1.96);p2
q1=qnorm(0.05);q1
q2=qnorm(0.95);q2

## 5.2 �������������� ----
### 5.2.1 ��ɢ��������� ----
rbinom(10,1,0.5)  #����10������B(1,0.5)�Ķ���ֲ������
rbinom(10,5,0.35)  #����10������B(5,0.35)�Ķ���ֲ������
par(mfrow=c(1,3))
  p=0.25
  for(n in c(10,20,50)){  
    x=rbinom(100,n,p)
    hist(x,prob=T,main=paste("n =",n))
    xn=0:n
    points(xn,dbinom(xn,n,p),type="h",lwd=3)
  }
par(mfrow=c(1,1))

rhyper(15,10,5,3)
rpois(10,lambda=4)

### 5.2.2 ������������� ----
runif(5,0,1)    # ����5��[0,1]�ľ��ȷֲ��������
runif(3,1,3)    # ����3��[1,3] �ľ��ȷֲ��������
runif(5)       # Ĭ������[0,1]�ϵľ��ȷֲ������

x=runif(10000)
hist(x,prob=T,ylim=c(0,1.5),main='uniform(0,1)')
curve(dunif(x,0,1),add=T)   #���Ӿ��ȷֲ����ܶȺ�����

rnorm(5,10,5)       # ����5����ֵΪ10��׼��Ϊ5����̬�ֲ������
rnorm(5)            # Ĭ�����ɱ�׼��̬�ֲ������

x=rnorm(1000)
hist(x,prob=T,ylim=c(0,0.5),,main='N(0,1)')
curve(dnorm(x),add=T)

x=rexp(1000,1/10)   # ����100����ֵΪ10��ָ���ֲ������
hist(x,prob=T,ylim=c(0,0.1),main="exp(1/10)")
curve(dexp(x,1/10),add=T)

## 5.3 ͳ������������ֲ� ----
### 5.3.1 ������ͳ���� ----
sample(c("H","T"),10,rep=T)
sample(100,10)         #��100����Ʒ���޷Ż������ȡ10��
sample(100,10,rep=T)   #��100����Ʒ���зŻ������ȡ10��
i=sample(48,10); i   #��ȡѧ�����
UG$id[i]       #ѧ�����
UG$name[i]     #ѧ������
UG[i,1:2]      #ѧ����ź�����

### 5.3.2 ���õĳ����ֲ� ----
x=seq(0,20,0.1)
curve(dchisq(x,2),0,20,ylab="p(x)")
curve(dchisq(x,4),add=T,lty=2)
curve(dchisq(x,6),add=T,lty=3)
curve(dchisq(x,8),add=T,lty=4)
curve(dchisq(x,10),add=T,lty=5)
legend(13,0.4,c("n=2","n=4","n=6","n=8","n=10"),lty=1:5,bty="n")

x=seq(-4,4,.01)
plot(x,dnorm(x),type="l",lty=1)
for(i in c(1,5,10)) 
  points(x,dt(x,df=i),type="l",lty=i+1)
legend(2,0.3,c("N(0,1)","t(10)","t(5)","t(1)"),lty=1:4,bty="n") 

x=seq(0,6,0.1)
plot(x,df(x,3,3),type="l",ylim=c(0,1.2),ylab="p(x)")
curve(df(x,5,5),0,6,add=T)
curve(df(x,10,10),0,6,add=T)
curve(df(x,20,20),0,6,add=T)
curve(df(x,30,30),0,6,add=T)

n=10; p=0.25
z=rbinom(1,n,p)
x=(z-n*p)/sqrt(n*p*(1-p)); x

m =100                               # m ģ�����
n = 10; p = 0.25
z = rbinom(m,n,p)                    # ����100�����������
x = (z-n*p)/sqrt(n*p*(1-p))          # ��100�������������׼��
hist(x,prob=T,main=paste("n =",n))
curve(dnorm(x),add=T)             # ������̬����

sim.clt<-function(m=100,n=10,p=0.25){ 
  z = rbinom(m,n,p)               
  x = (z-n*p)/sqrt(n*p*(1-p))        
  hist(x,prob=T,breaks=20,main=paste("n =",n,"p =",p))
 curve(dnorm(x),add=T)             
}
dev.set(2)
par(mar=c(4,4,2,1),mfrow=c(2,2),cex=0.8)
sim.clt()                # Ĭ�� m=100��n=10��p=0.25
sim.clt(1000)            # ȡ m=1000��n=10��p=0.25
sim.clt(1000,30)         # ȡ m=1000��n=30��p=0.25#ͼ���Ƿ����mȡֵ�����
sim.clt(1000,30,0.5)      # ȡ m=1000��n=30��p=0.5
par(mfrow=c(1,1))

### 5.3.3 �����ֲ����ٽ�ֵ ----
u0=seq(0,3,by=0.1);u0             #�ٽ������ֵ
u.0=seq(0,0.1,by=0.01);u.0        #�ٽ������ֵ
u=u0+matrix(u.0,31,11,byrow=T);u  #�ٽ����λ��
p=pnorm(u);p                      #�ٽ������ֵ
colnames(p)<-u.0                  #�ٽ���б�� 
rownames(p)<-u0                   #�ٽ���б�� 
p                                 #�γ���̬�ֲ��ٽ�� 

options(digits=4)
a=c(0.01,0.025,0.05,0.1,0.5,0.95,0.975,0.99);a #β������
n=1:30                                         #������ 
cbind('0.01'=qt(0.01,n),'0.025'=qt(0.025,n),'0.05'=qt(0.05,n),'0.1'=qt(0.1,n),
'0.5'=qt(0.5,n),'0.95'=qt(0.95,n),'0.975'=qt(0.975,n),'0.99'=qt(0.99,n))

# 6 ����ͳ���ƶϷ��� ----
## 6.1 ��̬����Ĳ������� ----
### 6.1.1 �������Ƶķ��� ----
par(mfrow=c(1,2),cex=0.8)
  u.conf.plot(0.95)
  u.conf.plot(0.99)
par(mfrow=c(1,1))

z.conf.int(UG$height,10)

par(mfrow=c(1,2),cex=0.8)
  t.conf.plot(20,0.95)
  t.conf.plot(20,0.99)
par(mfrow=c(1,1))

t.conf.int(UG$height)

t.test(UG$height)

## 6.2 ��̬����ļ������ ----
### 6.2.1 �������ĸ��� ----
### 6.2.2 ��������ֵ�Ƚ�t���� ----
par(mfrow=c(1,2),cex=0.8)
  hist(UG$height,breaks=15,prob=T,main='');lines(density(UG$height))
  qqnorm(UG$height);qqline(UG$height)
par(mfrow=c(1,1))
shapiro.test(UG$height)

t.test(UG$height,mu=170) 
t.test(UG$height,mu=170,alternative="less")

### 6.2.3 ��������ֵ�Ƚ�t���� ----
x1=UG$height[UG$sex=='��']
x2=UG$height[UG$sex=='Ů']
par(mfrow=c(1,2),cex=0.8)
  qqnorm(x1);qqline(x1)
  qqnorm(x2);qqline(x2)
par(mfrow=c(1,1))
shapiro.test(x1)
shapiro.test(x2)

var.test(height~sex,data=UG) # var.test(height[sex=="��"],height[sex=="Ů"])
t.test(height~sex,data=UG,var.equal=T) 
t.test(height~sex,data=UG)    #t.test(height[sex=="��"],height[sex=="Ů"])

### 6.2.4 ��������ֵ������� ----
par(mar=c(4,3,2,1),cex=0.8)
boxplot(weight~region,data=UG)
oneway.test(weight~region, data=UG)

## 6.3 �ֲ����ɵķǲ���ͳ�� ----
### 6.3.1  �ǲ���ͳ�Ƽ�� ----
r1=rank(UG$income,ties.method="first")
r2=rank(UG$income)   # �н�(������ͬ������ƽ��
cbind(x=UG$income,r1,r2)

### 6.3.2 �������ǲ������� ----
par(mfrow=c(1,2),cex=0.8)
  plot(ecdf(UG$income))
  plot(ecdf(UG$height))
par(mfrow=c(1,1))

ks.test(UG$income,"pnorm")
ks.test(UG$height,"pnorm")

EDA.plot(UG$income)
wilcox.test(UG$income,conf.int = T)
t.test(UG$income)
wilcox.test(UG$income,mu=5)
wilcox.test(UG$income,mu=25)

par(cex=0.8)
boxplot(income~sex,data=UG,horizontal=T)

#histogram(~ income|sex,data=UG)
### 6.3.3 �������ǲ������� ----
par(mfrow=c(1,2),cex=0.8)
  hist(UG$income[UG$sex=='��'],prob=T,main='');
  #lines(density(UG$income[UG$sex=='��']))
  hist(UG$income[UG$sex=='Ů'],prob=T,main='');
  #lines(density(UG$income[UG$sex=='Ů']))
par(mfrow=c(1,1))

ks.test(UG$income[UG$sex=='��'],UG$income[UG$sex=='Ů'])
wilcox.test(income~sex,data=UG)

### 6.3.4 �������ǲ������� ----
kruskal.test(income~region, data=UG��

## 6.4 �������ݵ�ͳ���ƶ� ----
### 6.4.1 ���������ݵ�ͳ���ƶ� ----
income_c=cut(UG$income,breaks=c(0,5,50,200),
             labels=c("������","�е�����","������"))
f=table(income_c);f
e=sum(f)/3
cbind(f,e)
X2=sum((f-e)^2/e);X2

prob=c(1,1,1)/3            # ָ�����۸���(���ȷֲ�)
chisq.test(f,p=prob)

### 6.4.2 ���������ݵ�ͳ���ƶ� ----
f2=table(UG$region,income_c);f2
chisq.test(f2)
f3=table(UG$sex,UG$region);f3
chisq.test(f3)

# 7 ����ͳ�Ʒ���ģ�� ----
## 7.1 ��ط���ģ�� ----
### 7.1.1 �������ϵ���ļ��� ----
par(mfrow=c(2,2),mar=c(3,3,1,1),cex=0.8)
x=runif(20)
e=rnorm(20)
y1=3+10*x+e;
plot(x,y=x,type='b');text(0.2,0.8,paste('r =',round(cor(x,x),4)))
plot(x,y=-x,type='b');text(0.8,-0.2,paste('r =',round(cor(x,-x),4)))
plot(x,y1);abline(3,10);text(0.2,10,paste('r =',round(cor(x,y1),4)))
y2=3-10*x+e;
plot(x,y2);abline(3,-10);text(0.8,0,paste('r =',round(cor(x,y2),4)))
par(mfrow=c(1,1))

plot(x,e)
y3=x+e
plot(x,y3);text(0.8,0,paste('r =',round(cor(x,y3),4)))
lines(x,y=5-10*x^2)
plot(x,y=e)

X=UG$height; Y=UG$weight
op<-par(mfrow=c(1,2),cex=0.8)
  plot(X,Y); plot(Y,X)
par(op)

lxy<-function(x,y){ 
	n=length(x); 
	L=sum(x*y)-sum(x)*sum(y)/n   
	L
} 

lxy(X,X)
lxy(Y,Y)
lxy(X,Y)
r=lxy(X,Y)/sqrt(lxy(X,X)*lxy(Y,Y))
r

cor(X,Y)
cor(Y,X)

### 7.1.2 ���ϵ���ļ������ ----
r=cor(X,y)
n=length(X)
tr=r/sqrt((1-r^2)/(n-2));tr  # ����tr
cor.test(X,Y)

### 7.1.4 �������ݵ���ط��� ----
library(lattice)    #����lattice��
xyplot(weight~height|sex,data=UG)

cor.test(~weight+height,data=UG[UG$sex=='��',])
cor.test(~weight+height,data=subset(UG,sex=='Ů'))

## 7.2 �ع����ģ�� ----
### 7.2.1 һԪ���Իع�ģ�� ----
b=lxy(X,Y)/lxy(X,X); b
a=mean(Y)-b*mean(X); a

plot(Y~X)
fm=lm(Y~X)    # ������Իع�ģ��
fm
abline(fm)

plot(weight~height,data=UG)
fm=lm(weight~height,data=UG)    # ������Իع�ģ��
fm
abline(fm)
names(fm)
coef(fm)   #fm$coef
coef(fm)[1]
coef(fm)[2]
resid(fm)
fitted(fm)

plot(weight~height,data=UG);abline(fm)

summary(fm)
predict(fm,data.frame(height=160)) 
predict(fm,data.frame(height=200)) 
predict(fm,data.frame(height=c(150,160,170,180,190,200))) 

sfm=summary(fm);sfm
names(sfm)
sfm$r.sq
sfm$adj.r.sq
sfm$fstat
sfm$coef

t=sfm$coef[2,3];t
P=sfm$coef[2,4];P

### 7.2.2 ��Ԫ���Իع�ģ ----
fm=lm(weight~height+income,data=UG);fm
summary(fm)

###  7.2.3 ��Ԫ�ع�ģ����� ----
y=UG$weight
yhat=fm$fitted
r=y-yhat
cbind(y,yhat,r)
EDA.plot(r)

par(mfrow=c(1,2),mar=c(5,4,2,1),cex=0.8)
  plot(r);abline(h=0)
  plot(y,r);abline(h=0)
par(mfrow=c(1,1))

r=resid(fm);r
plot(r)
rs=rstudent(fm);rs
plot(rs)
cbind(fm$model,r,rs)

rs=rstudent(fm);rs
par(mfrow=c(1,2),mar=c(5,4,2,1),cex=0.8)
  plot(rs,ylim=c(-3,3));abline(h=c(-3,0,3),lty=3)
  qqnorm(rs);qqline(rs)
par(mfrow=c(1,1))

par(mfrow=c(2,2),cex=0.8)
  plot(fm)
par(mfrow=c(1,1))

### 7.2.4 �����Ԫ�ع�ģ�� ----
summary(lm(weight~height,data=UG[UG$sex=='��',]))
summary(lm(weight~height,data=UG[UG$sex=='Ů',]))

### 7.3.2 ����ģ�ͷ��� ----
#bartlett.test(weight~region,data=UG)        
UG[,c('weight','sex','region')]
#oneway.test(weight~region,data=UG,var.equal=T)
summary(aov(weight~region, data=UG))
anova(lm(weight~region, data=UG))

boxplot(weight~region,data=UG)   
summary(aov(weight~sex+region,data=UG))

# 8 R���Եĸ߼�Ӧ�� ----
## 8.1 R���Եı�̸��� ----
### 8.1.1 R���Ա�̻��� ----
find('sd')
args('sd')
apropos('sd')
summary           #It is a generic funciton
methods(sd)   # list of the S3 methods
summary.lm         # may be you want to know the linear models' s summary

### 8.1.2 R���Ա�̶��� ----
x =c(1,3,5,7,9); x
c(1,3,5,7,9) -> y; y
z = c(1,3,5,7,9); z
seq(1,10,2)
seq(1,10)
seq(10,1,-1)
seq(1,by=2,length=10)
t=1:10; t
r=5:1; r   #5:1��ʾ��������
2*1:5
rep(c(1,3),4)
rep(c(1,3),each=4)
rep(1:3,rep(2,3))

matrix(c(1,2,3,4,5,6),nrow=2,ncol=3)
matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE)
matrix(c(1,2,3,4,5,6),nrow=2,ncol=3, dimnames=list(c("R1","R2"),c("C1","C2","C3")))
diag(1:4)
A=matrix(1:16,4,4); A
diag(A)
diag(4)   # ����4�׵�λ��
xx=array(1:24,c(3,4,2))    # ����ά��Ϊ(3,4,2)��3ά����

y=c("Ů","��","��","Ů","Ů","Ů","��") 
f=factor(y); f
levels(f)
score_f=c("B","C","D","B","A","D","A")
score_o=ordered(score_f,levels=c("D","C","B","A") )
score_o

newdata=data.frame(f=UG$sex,x=UG$height,y=UG$weight)
head(newdata)

x=c(1,1,2,2,3,3,3)   
y=c("Ů","��","��","Ů","Ů","Ů","��")  
z=c(80,85,92,76,61,95,83)
LST=list(class=x,sex=y,score=z)
LST
### 8.1.3 R���Ե���ѧ���� ----
integrate(dnorm, -1.96, 1.96)
integrate(dnorm, -Inf, Inf)
f1<-function(x) 1/((x+1)*sqrt(x)) 
integrate(f1, 0 , Inf )

f2=expression(sin(x)*x)
D(f2,"x")

choose(5,3)   #C53
combn(5,3)   #C53�ĸ������
factorial(5)   #5!

f3<-function(x) x^3 - 2*x - 1 
uniroot(f3, c(0,2))
f4<- function(x) x^2 + 2*x + 1
optimize(f, c(-2,2))

## 8.2 R���Ը߼���̾��� ----
### 8.2.1 �Զ��庯������ ----
price<-function(r){ 2000/(1+r)+2000/(1+r)^2+2500/(1+r)^3+4000/(1+r)^4 }
price(0.1)
price(0.14)
price(0.12)

f<-function(r){ 2000/(1+r)+2000/(1+r)^2+2500/(1+r)^3+4000/(1+r)^4-7704}
uniroot(f,c(0,1))

IRR1<-function(C){
  f<-function(r) { 
    n=length(C); 
    S=0
    for(i in 1:(n-1)) 
      S=S+C[i]/(1+r)^i 
    S=S-C[n]
    S
  } 
  uniroot(f,c(0,1))
}
IRR(c(2000,2000,2500,4000,4500,7704))

IRR2<-function(C){
  f<-function(r){
    n=length(C)
    i=1:(n-1)
    sum(C[i]/(1+r)^i)-C[n] 
  } 
  uniroot(f,c(0,1))
}
IRR(c(2000,2000,2500,4000,4500,7704))

## 8.3 R���Ը߼���ͼ���� ----
### 8.3.1 ��������ͳ��ͼ ----
x = 1:10
y = runif(10)
symbols(x, y, circles = y/2 , inches = F, bg = x)

x = rnorm(100)
hist(x,main='')
op<-par(fig=c(.02, .5, .5, .98), new=TRUE)
boxplot(x)
par(op)

par(mar=c(4,4,2,1),cex=0.75)
x = 1:10; plot(x, type = "n")
text(3,2, expression(paste("Temperature (",degree,"C) in 2003")))
text(4,4, expression(bar(x) == sum(frac(x[i],n), i==1, n )))
text(6,6, expression(hat(beta) == (X^ t*X) ^ {.1}*X^ t*y))
text(8,8, expression(z[i] == sqrt(x[i]^2 + y[i]^2)))

x =1:10; names(x) <-letters[1:10]
b = barplot(x, col = rev(heat.colors(10)))
text(b, x, labels = x , pos = 3)

t = seq (0,2*pi, length = 100)
x = 1*sin(t)          
y = 2*cos(t)        
plot(x, y, type = 'l');abline(h=0,v=0,lty=3)

# ��άͳ��ͼ
x <- seq(-10,10,length= 30); y <- x
f <- function(x, y){r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f); z[is.na(z)] <- 1
persp(x, y, z, theta = 30, phi = 30,expand = 0.5, col = 1:8)

# ��ά�ռ�ͼ
z=2*volcano; x=10*(1:nrow(z)); y=10*(1:ncol(z))   
z0=min(z)-20 
z=rbind(z0,cbind(z0,z,z0),z0)
x=c(min(x)-1e-10,x,max(x)+1e-10)
y=c(min(y)-1e-10,y,max(y)+1e-10) 
fill<-matrix("green3", nr = nrow(z)-1, nc = ncol(z)-1) 
fill[,i2<-c(1,ncol(fill))]<-"gray"; fill[i1<-c(1,nrow(fill)),]<-"gray" 
fcol <- fill
zi <- volcano[-1,-1] + volcano[-1,-61] + volcano[-87,-1] + volcano[-87,-61]
fcol[-i1,-i2]<-terrain.colors(20)[cut(zi,quantile(zi,seq(0,1,len=21)),include.lowest=TRUE)]
par(mar=rep(.5,4)) 
persp(x, y, 2*z, theta = 110, phi = 40, col = fcol, scale = FALSE, 
      ltheta = -120, shade = 0.4, border = NA, box = FALSE)

# �����ͼ
library(maps)
par(mar=rep(1,4)) 
us.map <- map("world", plot = FALSE, fill = TRUE)
plot(us.map,type="l",col=1)     #�����ͼ��ͼ

#������ͼ
us.map <- map("state", plot = FALSE, fill = TRUE)
plot(us.map,type="l",col="red") #������ͼ

### 8.3.2 lattice��ͼϵͳ ----
library(lattice)
histogram(~height|sex,data=UG)
xyplot(weight~height|sex,data=UG)
xyplot(weight~height|region,data=UG)
bwplot(~height|sex,data=UG)

n = seq(5,40,5)
x = rnorm(sum(n))
y = factor(rep(n, n), labels=paste("n =", n))
densityplot(~ x | y,
  panel = function(x, ...){
  panel.densityplot(x, col="DarkOliveGreen", ...)
  panel.mathdensity(dmath=dnorm,args=list(mean=mean(x), sd=sd(x)),col="darkblue")
})

### 8.3.3 ggplot2��ͼϵͳ ----
library(ggplot2)
p <- ggplot(data=UG,aes(x=height,y=weight))
p + geom_point()
ggplot(UG,aes(x=height)) + geom_histogram()
ggplot(UG,aes(x=height,y=weight,color=sex)) + geom_point()
ggplot(UG,aes(x=height,y=weight,shape=sex)) + geom_point()
ggplot(UG, aes(x=income))+geom_line(aes(y=height))+geom_line(aes(y=weight))

qplot(height,data=UG,geom=c('histogram'))
qplot(height,data=UG,geom=c('histogram'),colour = I("green"), fill = I("white"))

qplot(region,data=UG,geom='bar',ylim=c(0,20),fill=region)

qplot(sex,height,data=UG,geom=c('boxplot'))
qplot(sex,height,data=UG,geom=c('jitter','boxplot'),alpha=I(0.5),colour=sex)

qplot(height,weight,data=UG,geom=c('point','smooth'),method='lm',colour=sex,se=F)
qplot(height,weight,data=UG,geom=c('point','smooth'),method='lm',facets=sex~.,se=F)

## 8.4 ����ͳ�Ʒ������� ----
### 8.4.1 �ű����������� ----
source("script1.R")
sink("myout",append=T,split=T)
pdf("mygraph.pdf")
source("script1.R")
dev.off()
sink()

### 8.3.2 ʹ��R�������ɱ��� ----


# 9 R���Դ����ݷ������� ----
## 9.1 ͳ��ģ��ʵ�� ----
### 9.1.1 ���ģ�ⷽ����� ----
set.seed(2)
n = 100
x = cumsum(rnorm(n))
plot(x, type = 'l')
y = cumsum(rnorm(n))
plot(x,y,type = 'l')

Bernoulli<-function(m=500){  
  f=rep(0,m)
  for(n in 1:m)  
    f[n]=sum(sample(c(0,1),n,rep=T))/n
  plot(f,type='l',xlab='i',ylim=c(0,1));abline(h=0.5)
}
Bernoulli(100)   # ��Ӳ��100��
Bernoulli(1000)   # ��Ӳ��1000��

plot(seq(-3, 3, 0.1), dnorm(seq(-3, 3, 0.1)), type = "l", xlab = "x", ylab = expression(phi(x)))
text(-3, 0.3, adj = c(0, 1), expression(phi(x) == frac(1,sqrt(2*pi)) ~ e^-frac(x^2,2)))
abline(v =c(-1,1), lty = 3);text(0, 0.1, expression(integral(phi(x)*dx, -1, 1) %~~% 0.68))


g <- function(x) { 1/sqrt(2*pi)*exp(-x^2/2) }
I <- function(n,a,b,g){ 
  x=runif(n)
  sum((b-a)*g(a+(b-a)*x))/n
}
I(10000,-1,1,g)

h<-function(x){sin(x^2+x^3)+cos(1/2+x^0.5)}
curve(h,0,1,xlab="Function",ylab="",lwd=2)
integrate(h,0,1)          #���ú���integrate�����
m=10000
x=h(runif(m))
estint=cumsum(x)/(1:m)
estint[m]             #����ģ�⼼�������
esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
plot(estint,type="l");lines(estint+2*esterr,col=3);lines(estint-2*esterr,col=3)

### 9.1.2 ģ�⺯���Ľ������� ----
fb <- function(n=10,p=0.5) { 
  x=rbinom(1,n,p); 
  z=(x-n*p)/sqrt(n*p*(1-p)) 
  z
}
X=replicate(1000,fb());# ģ��1000��������Ϊ10�Ķ��������
hist(X,prob=T);
u=seq(-4,4,0.01);lines(u,dnorm(u,0,1))

fu<-function(n=10){
  x=runif(n)
  z=(mean(x-1/2)/(1/sqrt(12*n)))
  z
}
X=replicate(1000,fu())         # ģ��1000�����������
hist(X,prob=T,main='n=10')
u=seq(-4,4,0.01);lines(u,dnorm(u,0,1))

fe <- function(n,mu=10){
  x=rexp(n,1/mu)
  z=(mean(x-mu))/(mu/sqrt(n))
  z
}
  
u=seq(-4,4,0.01)
par(mfrow=c(2,2))
  hist(replicate(10000,fe(1)),prob=T,main="n=1");lines(u,dnorm(u,0,1))
  hist(replicate(10000,fe(5)),prob=T,main="n=5");lines(u,dnorm(u,0,1))
  hist(replicate(10000,fe(10)),prob=T,main="n=10");lines(u,dnorm(u,0,1))
  hist(replicate(10000,fe(30)),prob=T,main="n=30");lines(u,dnorm(u,0,1))
par(mfrow=c(1,1))

### 9.1.3 ��ģ��Ľ�һ����ʶ ----
t.stat <- function(x,mu){
  n=length(x)
  z=(mean(x)-mu)/(sd(x)/sqrt(n))
  z
}
mu = 10; 
x=rnorm(100,mu,1)
t.stat(x,mu)

mu = 10; 
x=rexp(100,1/mu); 
t.stat(x,mu)

results = c()     # ��ʼ�����������������һ��������
for (i in 1:200)  results[i] = t.stat(rexp(100,1/mu),mu)
hist(results)    # ֱ��ͼ�����������ε�
boxplot(results)    # �Գ�������β
qqnorm(results)     #����������̬�ֲ�

for (i in 1:200)  results[i] = t.stat(rexp(8,1/mu),mu)
hist(results)   #ֱ��ͼ��������
boxplot(results)   #���Գƣ���β
qqnorm(results)     # ���ӽ���̬�ֲ�
for (i in 1:200) results[i] = t.stat(rt(8,5),0)
hist(results)     # ֱ��ͼ������
boxplot(results)   #�Գƣ���β
qqnorm(results)    # ����̬�ֲ����Ǻܽӽ�
qqplot(results,rt(200,7))    # �����ɶ�Ϊ7��t�ֲ��ӽ�
dev.set(4)
X = runif(100)
EDA.plot(X)
X = rnorm(100)
EDA.plot(X)
X = rt(100,10)
EDA.plot(X)
X = rf(100,10,10)
EDA.plot(X)
X=abs(rnorm(200))
EDA.plot(X)
X=rexp(200)
EDA.plot(X)

n=100           #������
x=rexp(n,1/10)  #����1��������Ϊn��ָ���ֲ�exp(1/10)�����
hist(x,prob=T,main='rexp(100,1/10)');lines(density(x))

m=10000      #ģ��10000������
t=rep(0,m)  #��ʼ����������һ��0����
for(i in 1:m) 
  t[i]=t.stat(rexp(100,1/10),10) #����1000��������Ϊn��ָ���ֲ�exp(1/10)
hist(t,prob=T,main='');lines(density(t))

boxplot(x)    # �Գ�������β
qqnorm(x)     #����������̬�ֲ�
#x=replicate(1000,t.stat(rexp(100,1/10),10))
hist(x,prob=T);curve(dnorm(x),add=T)
EDA.plot(x)

## 9.3 �������ݵ��������� ----
### 9.3.1 ���������� ----
stock.sim()   #stock.sim(N=1136845,File='stock2.csv')

### 9.3.2 �������ݵĹ��� ----
#install.packages("RODBC")
library(RODBC)   #�谲װRODBC��
Rxls = odbcConnectExcel("dstatR2data.xls") 
Rxls1= sqlFetch(Rxls,"Sheet1")
Rxls1
close(Rxls)

Rmdb=odbcConnect("myODBC",uid="",pwd="")
stock2=sqlQuery(Rmdb ,"select * from stock2")

#stock2=sqlQuery(Rmdb, "select * from stock where sex=1 and post=1 order by fund") 
#dim(stock2)
#odbcClose(Rmdb)

stock=read.csv("stock2.csv")
dim(stock)

head(stock)     #��ʾǰ6������
tail(stock)     #��ʾ��6������

Ts=table(stock$sex)

Fs=Ftab(stock$sex)
barplot(Fs)


T1=table(stock1$sex);names(T1)<-c('��','Ů');T1
Freq(stock$age)

age_g=cut(stock$age,breaks=c(0,20,30,40,50,60,70,80),right = FALSE)
table(age_g)
Fa=Ftab(age_g)

barplot(Fa,col=1:7)

levels(stock$result)=c('׬Ǯ','���ⲻ׬','��Ǯ')


factor(stock1$result)
table(result)

Tr=table(stock$result)
Fr=Ftab(stock$result)

pie(Fr,labels=c('׬Ǯ','���ⲻ׬','��Ǯ'))

table(stock$sex,stock$result)

barplot(table(stock$result,stock$sex),beside=T,col=c('red','white','blue'),
  names.arg=c("��","Ů"),legend.text=c('׬Ǯ','���ⲻ׬','��Ǯ'))

barplot(table(stock$sex,stock$result),beside=T,col=c('red','blue'),
  names.arg=c('׬Ǯ','���ⲻ׬','��Ǯ'),legend.text=c("��","Ů"))

ftable(stock$sex,age_g,stock$result)
ftable(age_g,stock$sex,stock$result)

ft=ftable(stock$sex,stock$result,age_g);ft
rowSums(ft)  #�кϼ�
colSums(ft)  
sum(ft)